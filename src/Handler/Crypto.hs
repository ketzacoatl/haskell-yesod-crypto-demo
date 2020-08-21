{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Crypto where

import Crypto.Gpgme
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Either (fromRight)

import Import

type Email = Text

data CryptoFormData = CryptoFormData
  { messageSubject :: Text
  , replyTo :: Email
  , messageContents :: Textarea
  }


cryptoForm :: Form CryptoFormData
cryptoForm = renderDivs $ CryptoFormData
    <$> areq textField "Message Subject" Nothing
    <*> areq emailField "Reply To" Nothing
    <*> areq textareaField "Message Contents" Nothing


getCryptoFormR :: Handler Html
getCryptoFormR = do
    (formWidget, formEnctype) <- generateFormPost cryptoForm

    defaultLayout $ do
        setTitle "Crypto Example!"
        $(widgetFile "crypto-form")


postCryptoFormR :: Handler Html
postCryptoFormR = do
    ((result, formWidget), formEnctype) <- runFormPost cryptoForm
    case result of
      -- Yay! Form is valid
      FormSuccess entry -> defaultLayout $ do
        -- run crypto operation here, and then display the value
        let plaintextMessage = (messageContents entry)
            encryptedMessage = encryptMessage (encodeUtf8 (unTextarea plaintextMessage))
        setTitle "Crypto Example!"
        $(widgetFile "crypto-message")

      -- utoh! Form failed validation
      _ -> defaultLayout $ do
        setTitle "Crypto Example!"
        $(widgetFile "crypto-form")


localTestPubKey = "588EE2A6BA9E259108DF2143556339DC653AA738"

--encryptMessage :: Textarea ->
encryptMessage plaintext = do
    Just enc <- withCtx "crypto" "C" OpenPGP $ \bCtx -> runMaybeT $ do
        aPubKey <- MaybeT $ getKey bCtx localTestPubKey NoSecret
        fromRight $ encrypt bCtx [aPubKey] NoFlag plaintext
    return enc
