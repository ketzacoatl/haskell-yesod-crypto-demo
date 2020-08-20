{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Crypto where

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
            encryptedMessage = (messageContents entry)
        setTitle "Crypto Example!"
        $(widgetFile "crypto-message")

      -- utoh! Form failed validation
      _ -> defaultLayout $ do
        setTitle "Crypto Example!"
        $(widgetFile "crypto-form")
