{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.S3.Encryption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Addons for <http://hackage.haskell.org/package/amazonka-s3 amazonka-s3> to
-- support client-side encryption.
--
-- Your client-side master keys and your unencrypted data are never sent to AWS;
-- therefore, it is important that you safely manage your encryption keys. If
-- you lose them, you won't be able to decrypt your data.
-- When generating a symmetric key, you should ensure
-- that the key length is compatible with the underlying 'AES256' cipher.
--
-- The encryption procedure is:
--
-- * A one-time-use symmetric key a.k.a. a data encryption key (or data key) and
-- initialisation vector (IV) are generated locally. This data key and IV are used
-- to encrypt the data of a single S3 object using an AES256 cipher in CBC mode,
-- with PKCS5 padding. (For each object sent, a completely separate data key and IV are generated.)
--
-- * The generated data encryption key used above is encrypted using a symmetric
-- AES256 cipher in ECB mode, asymmetric RSA, or KMS facilities, depending on the
-- client-side master key you provide.
--
-- * The encrypted data is uploaded and the encrypted data key and material description
-- are attached as object metadata (either headers or a separate instruction file).
-- If KMS is used, the material description helps determine which client-side master
-- key to later use for decryption, otherwise the configured client-side key at
-- time of decryption is used.
--
-- For decryption:
--
-- The encrypted object is downloaded from Amazon S3 along with any metadata.
-- If KMS was used to encrypt the data then the master key id is taken from the
-- metadata material description, otherwise the client-side master key in the
-- current environment is used to decrypt the data key, which in turn is used
-- to decrypt the object data.
--
-- The client-side master key you provide can be either a symmetric key, an
-- asymmetric public/private key pair, or a KMS master key.
--
-- The stored metadata format is designed to be compatible with the official Java
-- AWS SDK (both V1 and V2 envelopes), but only a limited set of the possible
-- encryption options are supported. Therefore assuming defaults, objects stored
-- with this library should be retrievable by any of the other official SDKs, and
-- vice versa.
module Network.AWS.S3.Encryption
    (
    -- * Usage
    -- $usage

    -- * Specifying Master Keys
    -- $master-key

      KeyEnv
    , Key
    , kmsKey
    , asymmetricKey
    , symmetricKey
    , newSecret

    , master
    , material

    -- * Request Encryption/Decryption
    -- $requests

    , encrypt
    , decrypt
    , initiate

    -- ** Instruction Files
    -- $instructions

    , encryptInstructions
    , decryptInstructions
    , initiateInstructions
    , cleanupInstructions

    -- *** Default Instruction Extension
    , Ext (..)
    , defaultExtension

    -- * Handling Errors
    -- $errors

    , EncryptionError   (..)
    , AsEncryptionError (..)
    ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS                as AWST
import           Crypto.PubKey.RSA.Types                as RSA
import           Crypto.Random
import           Data.ByteString                        (ByteString)
import           Data.Text                              (Text)
import           Network.AWS.S3
import           Network.AWS.S3.Encryption.Decrypt
import           Network.AWS.S3.Encryption.Encrypt
import           Network.AWS.S3.Encryption.Envelope
import           Network.AWS.S3.Encryption.Instructions
import           Network.AWS.S3.Encryption.Types

-- | Set (using 'local') the client-side master key used to encrypt/decrypt
-- a block of actions.
master :: (MonadReader r m, HasKeyEnv r) => Key -> m a -> m a
master k = local (envKey .~ k)

-- | Set (using 'local') any additional material description to be used.
--
-- For encryption, this provides audit and logging information and is logged
-- by CloudTrail, if enabled.
--
-- For decryption when using KMS, this is merged with whatever material description
-- is stored on the envelope and supplied to KMS as a decryption context.
material :: (MonadReader r m, HasKeyEnv r) => Description -> m a -> m a
material d = local (envKey . description .~ d)

-- | Specify a KMS master key to use, with an initially empty material description.
--
-- /See:/ 'description', 'material'.
kmsKey :: Text -> Key
kmsKey k = KMS k mempty

-- | Specify the asymmetric key used for RSA encryption.
--
-- /See:/ 'description', 'material'.
asymmetricKey :: PrivateKey -> Key
asymmetricKey k = Asymmetric (KeyPair k) mempty

-- | Specify the shared secret to use for symmetric key encryption.
-- This must be compatible with the AES256 key size, 32 bytes.
--
-- Throws 'EncryptionError', specifically 'CipherFailure'.
--
-- /See:/ 'newSecret', 'description', 'material'.
symmetricKey :: MonadThrow m => ByteString -> m Key
symmetricKey = fmap (`Symmetric` mempty) . createCipher

-- | Generate a random shared secret that is of the correct length to use with
-- 'symmetricKey'. This will need to be stored securely to enable decryption
-- of any requests that are encrypted using this secret.
newSecret :: (MonadThrow m, MonadRandom m) => m ByteString
newSecret = getRandomBytes aesKeySize

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

-- | Encrypt an object, storing the encryption envelope in an adjacent instruction
-- file with the same 'ObjectKey' and 'defaultExtension'.
-- This makes two HTTP requests, storing the instruction file first and upon success,
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
-- For example:
--
-- @
-- (a', f) <- initiate (a :: CreateMultipartUpload)
-- b'      <- send (f b :: Encrypted UploadPart)
-- @
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
-- in an adjacent instruction file with the same 'ObjectKey' and 'defaultExtension'.
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

-- | Retrieve an object and its adjacent instruction file. The instruction
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
-- remove the adjacent instruction file, if it exists with the 'defaultExtension'.
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
When sending requests that make use of a master key, an extension to the underlying
'AWS' environment is required. You can specify this environment as follows:

@
import Network.AWS
import Network.AWS.S3
import Network.AWS.S3.Encryption
import System.IO

example :: Key -> IO GetObjectResponse
example k = do
    -- A standard AWS environment with credentials is created using 'newEnv':
    e <- newEnv Frankfurt Discover

    -- The environment needed for encryption is then extended using a 'Key':
    runAWS (KeyEnv e (kmsKey "alias/master-key")) $ do
        -- To store an encrypted object, 'encrypt' is used inplace of where you would
        -- typically use 'AWST.send':
        _  <- encrypt (putObject "bucket-name" "object-key" body)

        -- To retrieve a previously encrypted object, 'decrypt' is used, again similarly to
        -- how you'd use 'AWST.send':
        rs <- decrypt (getObject "bucket-name" "object-key")

        -- The 'GetObjectResponse' here contains a 'gorsBody' that is decrypted during read:
        return rs
@
-}

{- $master-key
You master key should be stored and secured by you alone (or KMS). The specific
key that is used to encrypt an object is required to decrypt the same object.
If you lose this key, you will not be able to decrypt the related objects.
-}

{- $errors
Errors are thrown by the library using 'MonadThrow' and will consist of one of
the branches from 'EncryptionError' for anything crypto related, or a disparate
'AWST.Error' anything related to the underlying 'AWS' service calls.

You can catch errors and sub-errors via 'trying' etc. from "Control.Exception.Lens",
and the appropriate 'AsEncryptionError' 'Prism':

@
trying '_EncryptionError' (encrypt (putObject "bkt" "key")) :: Either 'EncryptionError' PutObjectResponse
@
-}

{- $requests
Only a small number of S3 operations actually utilise encryption/decryption
behaviour, namely 'PutObject', 'GetObject', and the related multipart upload
operations. The following functions store the encryption envelope in object
metadata (headers).
-}

{- $instructions
An alternative method of storing the encryption envelope in an adjacent S3
object is provided for the case when metadata headers are reserved for other
data. This method removes the metadata overhead at the expense of an additional
HTTP request to perform encryption/decryption.
The provided @*Instruction@ functions make the convenient assumption that
the 'defaultExtension' is desired. If you wish to override the suffix\/extension,
you can simply call the underlying plumbing to modify the
'PutInstructions' or 'GetInstructions' suffix before sending.

An example of encryption with a non-default instruction extension:

@
(a, b) <- 'encrypted' (x :: 'PutObject')
_      <- 'AWST.send' (b & 'piExtension' .~ ".envelope") -- Store the custom instruction file.
'AWST.send' a -- Store the actual encrypted object.
@
-}
