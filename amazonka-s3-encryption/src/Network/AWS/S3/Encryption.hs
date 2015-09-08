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
module Network.AWS.S3.Encryption
    (

    ) where

import           Control.Monad.Trans.AWS
import           Network.AWS.Prelude                    hiding (coerce)
import           Network.AWS.S3
import           Network.AWS.S3.Encryption.Decrypt
import           Network.AWS.S3.Encryption.Encrypt
import           Network.AWS.S3.Encryption.Instructions
import           Network.AWS.S3.Encryption.Types

-- -- | Set the key material used to encrypt/decrypt a block of actions.
-- material :: (MonadReader r m, HasKeyEnv r) => Key -> m a -> m a
-- material k = local (envKey .~ k)

encrypt :: (AWSConstraint r m, HasKeyEnv r)
        => PutObject
        -> m PutObjectResponse
encrypt x = do
    (a, _) <- encrypted x
    send (set location Metadata a)

encryptInstructions :: (AWSConstraint r m, HasKeyEnv r)
                    => PutObject
                    -> m PutObjectResponse
encryptInstructions x = do
    (a, b) <- encrypted x
    _      <- send b
    send a

-- | Note about parallelism/concurrency, and encryption of parts. If you don't
-- encrypt any of the parts then the entire thing is unencrypted!
initiate :: (AWSConstraint r m, HasKeyEnv r)
         => CreateMultipartUpload
         -> m ( CreateMultipartUploadResponse
              , UploadPart -> Encrypted UploadPart
              )
initiate x = do
    (a, _) <- encrypted x
    rs     <- send (set location Metadata a)
    return (rs, encryptPart a)

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

decrypt :: (AWSConstraint r m, HasKeyEnv r)
        => GetObject
        -> m GetObjectResponse
decrypt x = do
    let (a, _) = decrypted x
    Decrypted f <- send a
    f Nothing

decryptInstructions :: (AWSConstraint r m, HasKeyEnv r)
                    => GetObject
                    -> m GetObjectResponse
decryptInstructions x = do
    let (a, b) = decrypted x
    Instructions g <- send b
    Decrypted    f <- send a
    g >>= f . Just

-- | Given a request to execute, such as AbortMultipartUpload or DeleteObject,
-- remove the relevant adjacent instructions file, if it exists.
cleanup :: (AWSConstraint r m, RemoveInstructions a)
        => a
        -> m (Rs a)
cleanup x = do
    rs <- send x
    _  <- send (deleteInstructions x)
    return rs
