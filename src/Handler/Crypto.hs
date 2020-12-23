{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Crypto where

-- cryptonite
import Crypto.PubKey.RSA.PKCS15
import Crypto.PubKey.RSA.Types (Error (..), PublicKey (..))
import Data.X509
import Crypto.Store.X509
import Data.ByteString (ByteString (..))
--import Data.Text.Encoding (encodeUtf8)

-- h-gpgme
-- import Crypto.Gpgme
--import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
--import Data.Either (fromRight)

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
        -- retrieve public key
        publicKeyList <- liftIO $ readPubKeyFile "./pub.gpg"
        -- run crypto operation here, and then display the value
        let plaintextMessage = (messageContents entry)
            encryptedMessage = encryptMessage (publicKeyList)
                                              (encodeUtf8 (unTextarea plaintextMessage))
        setTitle "Crypto Example!"
        $(widgetFile "crypto-message")

      -- utoh! Form failed validation
      _ -> defaultLayout $ do
        setTitle "Crypto Example!"
        $(widgetFile "crypto-form")

-- publicKeyAscii :: Text
-- publicKeyAscii = "-----BEGIN PGP PUBLIC KEY BLOCK-----\
-- \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\
-- \=xCmY\
-- \-----END PGP PUBLIC KEY BLOCK-----"


-- cryptonite
encryptMessage :: PublicKey -> ByteString -> ByteString
encryptMessage pubKey plaintext = do
    result <- encrypt pubKey plaintext
    case result of
        encryptedMessage -> return encryptedMessage
        _ -> return plaintext
        
    --where
      -- publicKey = encodeUtf8 publicKeyAscii


-- h-gpgme
--
-- localTestPubKey = "588EE2A6BA9E259108DF2143556339DC653AA738" :: Fpr
-- 
-- gpgHomePath = "./crypto"
-- locale = "C"
-- 
-- --encryptMessage :: Textarea ->
-- -- encryptMessage plaintext = plaintext
-- encryptMessage plaintext = do
--     Just cipherText <- withCtx gpgHomePath locale OpenPGP $ \gpgContext -> do
--         publicKey <- getKey gpgContext localTestPubKey NoSecret
--         case publicKey of
--           Nothing -> return Nothing -- throw exception instead?
--           Just key -> encrypt gpgContext [key] NoFlag plaintext
--     return cipherText
--     --Just cipherText <- encrypt' $ localTestPubKey (unpack plaintext)
--     --return cipherText
--     --Just enc <- withCtx "crypto" "C" OpenPGP $ \bCtx -> do
--     --    aPubKey <- getKey bCtx localTestPubKey NoSecret
--     --    fromRight $ encrypt bCtx [aPubKey] NoFlag plaintext
--     --return enc
