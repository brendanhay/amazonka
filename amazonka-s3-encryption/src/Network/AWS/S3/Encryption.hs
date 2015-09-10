{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.S3.Encryption
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Addons for <http://hackage.haskell.org/package/amazonka-s3 amazonka-s3> to
-- support client-side encryption.
module Network.AWS.S3.Encryption
    (
    -- * Usage
    -- $usage

    -- * Specifying Key Material
    -- $material

      Key
    , kmsKey
    , asymmetricKey
    , symmetricKey
    , newKey

    -- ** Key Environment
    , KeyEnv    (..)
    , HasKeyEnv (..)

    -- ** Modifying Material per Request
    , material

    -- * Handling Errors
    -- $errors

    , EncryptionError   (..)

    -- ** Prisms
    , AsEncryptionError (..)

    -- ** Re-exported Types
    , CryptoError
    , RSA.Error

    -- * Request Encryption/Decryption
    -- $requests

    -- ** Metadata Headers
    , encrypt
    , decrypt
    , initiate

    -- ** Instruction Files
    -- $instructions

    , encryptInstructions
    , decryptInstructions
    , initiateInstructions
    , cleanupInstructions

    -- *** Default Suffix
    , Ext (..)
    , defaultSuffix
    ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS                as AWST
import           Crypto.Error
import           Crypto.PubKey.RSA.Types                as RSA
import           Crypto.Random
import           Network.AWS.Prelude                    hiding (coerce)
import           Network.AWS.S3
import           Network.AWS.S3.Encryption.Decrypt
import           Network.AWS.S3.Encryption.Encrypt
import           Network.AWS.S3.Encryption.Envelope
import           Network.AWS.S3.Encryption.Instructions
import           Network.AWS.S3.Encryption.Types

-- | Set the key material used to encrypt/decrypt a block of actions.
material :: (MonadReader r m, HasKeyEnv r) => Key -> m a -> m a
material k = local (envKey .~ k)

-- | Initialise the AES256 cipher used for ECB encryption with the
-- specified secret key.
--
-- Throws 'EncryptionError', specifically 'CipherFailure'.
symmetricKey :: MonadThrow m => ByteString -> m Key
symmetricKey = fmap (`Symmetric` mempty) . createCipher

-- | Generate a new random secret key. This will need to be stored securely
-- to be able to decrypt any requests that are encrypted with this key.
newKey :: (MonadThrow m, MonadRandom m) => m Key
newKey = getRandomBytes aesKeySize >>= symmetricKey

-- | Specify the asymmetric key material used for RSA encryption.
asymmetricKey :: PrivateKey -> Key
asymmetricKey = (`Asymmetric` mempty) . KeyPair

-- | Specify the KMS master key id to use for encryption.
kmsKey :: Text -> Key
kmsKey = KMS

-- | Encrypt an object, storing the encryption envelope in @x-amz-meta-*@
-- headers.
--
-- Throws 'EncryptionError', 'AWST.Error'.
encrypt :: (AWSConstraint r m, HasKeyEnv r)
        => PutObject
        -> m PutObjectResponse
encrypt x = do
    (a, _) <- encrypted x
    send (set location Metadata a)

-- | Encrypt an object, storing the encryption envelope in an adjacent instructions
-- file with the same 'ObjectKey' and 'defaultSuffix'.
-- This makes two HTTP requests, storing the instructions first and upon success,
-- storing the actual object.
--
-- Throws 'EncryptionError', 'AWST.Error'.
encryptInstructions :: (AWSConstraint r m, HasKeyEnv r)
                    => PutObject
                    -> m PutObjectResponse
encryptInstructions x = do
    (a, b) <- encrypted x
    _      <- send b
    send a

-- | Initiate an encrypted multipart upload, storing the encryption envelope
-- in the @x-amz-meta-*@ headers.
--
-- The returned 'UploadPart' @->@ 'Encrypted' 'UploadPart' function is used to encrypt
-- each part of the object. The same caveats for multipart upload apply, it is
-- assumed that each part is uploaded in order and each part needs to be
-- individually encrypted.
--
-- Throws 'EncryptionError', 'AWST.Error'.
initiate :: (AWSConstraint r m, HasKeyEnv r)
         => CreateMultipartUpload
         -> m ( CreateMultipartUploadResponse
              , UploadPart -> Encrypted UploadPart
              )
initiate x = do
    (a, _) <- encrypted x
    rs     <- send (set location Metadata a)
    return (rs, encryptPart a)

-- | Initiate an encrypted multipart upload, storing the encryption envelope
-- in an adjacent instructions file with the same 'ObjectKey' and 'defaultSuffix'.
--
-- The returned 'UploadPart' @->@ 'Encrypted' 'UploadPart' function is used to encrypt
-- each part of the object. The same caveats for multipart upload apply, it is
-- assumed that each part is uploaded in order and each part needs to be
-- individually encrypted.
--
-- Throws 'EncryptionError', 'AWST.Error'.
initiateInstructions :: (AWSConstraint r m, HasKeyEnv r)
                     => CreateMultipartUpload
                     -> m ( CreateMultipartUploadResponse
                          , UploadPart -> Encrypted UploadPart
                          )
initiateInstructions x = do
    (a, b) <- encrypted x
    rs     <- send a
    _      <- send b
    return (rs, encryptPart a)

-- | Retrieve an object, parsing the envelope from any @x-amz-meta-*@ headers
-- and decrypting the response body.
--
-- Throws 'EncryptionError', 'AWST.Error'.
decrypt :: (AWSConstraint r m, HasKeyEnv r)
        => GetObject
        -> m GetObjectResponse
decrypt x = do
    let (a, _) = decrypted x
    Decrypted f <- send a
    f Nothing

-- | Retrieve an object and its adjacent instructions file. The instructions
-- are retrieved and parsed first.
-- Performs two HTTP requests.
--
-- Throws 'EncryptionError', 'AWST.Error'.
decryptInstructions :: (AWSConstraint r m, HasKeyEnv r)
                    => GetObject
                    -> m GetObjectResponse
decryptInstructions x = do
    let (a, b) = decrypted x
    Instructions g <- send b
    Decrypted    f <- send a
    g >>= f . Just

-- | Given a request to execute, such as 'AbortMultipartUpload' or 'DeleteObject',
-- remove the adjacent instructions file, if it exists with the 'defaultSuffix'.
-- Performs two HTTP requests.
--
-- Throws 'EncryptionError', 'AWST.Error'.
cleanupInstructions :: (AWSConstraint r m, RemoveInstructions a)
                    => a
                    -> m (Rs a)
cleanupInstructions x = do
    rs <- send x
    _  <- send (deleteInstructions x)
    return rs

{- $usage
Encryption and decryption require the use of a client-side secret (symmetric) or
RSA private key (asymmetric) if KMS is not used. You should generate this key and
store it securely - whoever has access to this can decrypt any objects which were
encrypted with the same key. When generating a symmetric key, you should ensure
that the key length is compatible with the underlying 'AES256' cipher.
'newKey' is provided for this reason.

When sending requests that make use of key material - an extension to the underlying
'AWS' environment is required. You can specify this environment as follows:

@
example :: Key -> IO GetObjectResponse
example k = do
    e <- newEnv Frankfurt Discover

    -- The environment needed for encryption is an extended variant
    -- of the typical 'AWS' 'Env':
    runAWS (KeyEnv e k) $ do
        -- To store an encrypted object, 'encrypt' is used inplace
        -- of where you would typically use 'AWST.send':
        _  <- encrypt (putObject "bucket-name" "object-key" body)

        -- To retrieve a previously encrypted object, 'decrypt' is
        -- used, again similarly to how you'd use 'AWST.send':
        rs <- decrypt (getObject "bucket-name" "object-key")

        -- The 'GetObjectResponse' here contains a 'gorsBody' that is decrypted
        -- during read:
        return rs
@

The stored envelope format is designed to be compatible with the official Java
AWS SDK, but only a limited set of the possible encryption options are supported.
Therefore assuming defaults, objects stored with this library should
be retrievable by any of the other official SDKs, and vice versa.
-}

{- $material
You master key should be stored and secured by you alone (or KMS). The specific
key that is used to encrypt an object is required to decrypt the same object.
If you lose this key, you will not be able to decrypt the related objects.
-}

{- $errors
Errors are thrown by the library using 'MonadThrow' and will consist of one of
the branches from 'EncryptionError' for anything crypto related, or an 'AWST.Error'
for anything related to the underlying 'AWS' calls.

You can catch errors and sub-errors via "Control.Exception.Lens.trying" etc,
and the appropriate 'AsEncryptionError' "Control.Lens.Prism":

@
trying '_EncryptionError' (encrypt (putObject "bkt" "key") :: Either 'EncryptionError' PutObjectResponse
@
-}

{- $requests
Only a small number of S3 operations actually utilise encryption/decryption
behaviour, namely 'PutObject', 'GetObject', and the related multipart upload
operations.

When performing encryption, an initialisation vector ('IV') and content encryption key
('CEK') are created per object. The CEK is then encrypted using the key material provided
by the available 'KeyEnv'. The CEK + IV is used to encrypt the content body using
'AES256' in 'ECB' mode and the encrypted CEK + IV are stored with the object
in S3. For decryption, the metadata envelope is parsed and then the CEK is
decrypted using the key material from the 'KeyEnv'. The decrypted CEK is then
used to decrypt the actual content body.
-}

{- $instructions
An alternative method of storing the encryption envelope in an adjacent S3
object is provided for the case when metadata headers are reserved for other
data. This method removes the metadata overhead at the expense of an additional
HTTP request to perform encryption/decryption.
The provided @*Instruction@ functions make the convenient assumption that
the 'defaultSuffix' is desired. If you wish to override the suffix\/extension,
you can simply call the underlying plumbing to modify the
'PutInstructions' or 'GetInstructions' suffix before sending.

An example of encryption with a non-default suffix:

@
(a, b) <- 'encrypted' (x :: 'PutObject')
_      <- 'AWST.send' (b & 'piSuffix' .~ ".envelope" -- Store the instructions.
'AWST.send' a -- Store the actual encrypted object.
@

-}
