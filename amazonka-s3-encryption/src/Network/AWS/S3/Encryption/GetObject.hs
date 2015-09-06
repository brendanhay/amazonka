{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.S3.Encryption.GetObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.GetObject where

import           Control.Monad.Reader
import           Data.Coerce
import           Data.Conduit
import           Data.Proxy
import           Network.AWS.Prelude  hiding (coerce)
import           Network.AWS.S3       hiding (getObject, putObject)
import qualified Network.AWS.S3       as S3
import           System.IO
-- import Network.AWS.S3.Encryption.Internal

-- data Decrypted a = Decrypted a Env Key (Maybe Envelope)

-- getEncryptedObject :: (MonadReader r m, HasEnv r)
--                    => Key
--                    -> BucketName
--                    -> ObjectKey
--                    -> m (Decrypted GetObject)
-- getEncryptedObject key b k = do
--     env <- view environment
--     return $ Decrypted (getObject b k) env key Nothing

-- instance AWSRequest (Decrypted GetObject) where
--     type Rs (Decrypted GetObject) = GetObjectResponse

--     request (Decrypted x _ _ _) = coerce (request x)

--     response l s (Decrypted x env k m) r = do
--         (n, rs) <- response l s x r
--         e       <- maybe (fromHeaders (rs ^. gorsMetadata) return m
--         return (n, rs & gorsBody %~ decryptBody e)

-- data GetInstruction = GetInstruction BucketName ObjectKey Text

-- -- -- giInstructionExtension :: Lens' GetInstruction Text

-- getEncryptedObjectInstruction :: BucketName
--                                -> ObjectKey
--                                -> GetInstruction
-- getEncryptedObjectInstruction b k = GetInstruction b k instructionSuffix

-- instance AWSRequest GetInstruction where
--     type Rs GetInstruction = Decrypted GetObject

--     request (GetInstruction b k ext) =
--         coerce . request $ getObject b (k) -- <.> ext)

--     response l s rq@(GetInstruction b k _) = receiveJSON (\_ _ -> f) l s rq
--        where
--          rs = fmap (Decrypted (getObject b k) . Just)
--             . fromInstructions env k
