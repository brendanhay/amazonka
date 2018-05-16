{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Instructions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.Instructions where

import           Control.Arrow
import           Control.Lens (Lens', lens, view, (&), (%~))
import           Control.Monad.Trans.AWS
import           Data.Aeson.Types                   (parseEither)
import           Data.Coerce
import           Data.Proxy
import           Network.AWS.Prelude
import           Network.AWS.Response
import           Network.AWS.S3
import           Network.AWS.S3.Encryption.Envelope
import           Network.AWS.S3.Encryption.Types

newtype Instructions = Instructions
    (forall m r. (AWSConstraint r m, HasKeyEnv r) => m Envelope)

class AWSRequest a => AddInstructions a where
    -- | Determine the bucket and key an instructions file is adjacent to.
    add' :: a -> (BucketName, ObjectKey)

instance AddInstructions PutObject             where add' = view poBucket  &&& view poKey
instance AddInstructions GetObject             where add' = view goBucket  &&& view goKey
instance AddInstructions CreateMultipartUpload where add' = view cmuBucket &&& view cmuKey
instance AddInstructions UploadPart            where add' = view upBucket  &&& view upKey

data PutInstructions = PutInstructions
    { _piExt :: Ext
    , _piPut :: PutObject
    } deriving (Show)

putInstructions :: AddInstructions a => a -> Envelope -> PutInstructions
putInstructions (add' -> (b, k)) =
    PutInstructions defaultExtension . putObject b k . toBody

piExtension :: Lens' PutInstructions Ext
piExtension = lens _piExt (\s a -> s { _piExt = a })

instance AWSRequest PutInstructions where
    type Rs PutInstructions = PutObjectResponse

    request x = coerce . request $
        _piPut x & poKey %~ appendExtension (_piExt x)

    response s l _ = response s l (Proxy :: Proxy PutObject)

data GetInstructions = GetInstructions
    { _giExt :: Ext
    , _giGet :: GetObject
    } deriving (Show)

getInstructions :: AddInstructions a => a -> GetInstructions
getInstructions = GetInstructions defaultExtension . uncurry getObject . add'

giExtension :: Lens' GetInstructions Ext
giExtension = lens _giExt (\s a -> s { _giExt = a })

instance AWSRequest GetInstructions where
    type Rs GetInstructions = Instructions

    request x = coerce . request $
        _giGet x & goKey %~ appendExtension (_giExt x)

    response = receiveJSON $ \_ _ o ->
         return $ Instructions $ do
            k <- view envKey
            e <- view environment
            hoistError (EnvelopeInvalid "Instructions")
                       (parseEither parseJSON (Object o))
                >>= fromMetadata k e

class AWSRequest a => RemoveInstructions a where
    -- | Determine the bucket and key an instructions file is adjacent to.
    remove' :: a -> (BucketName, ObjectKey)

instance RemoveInstructions AbortMultipartUpload where remove' = view amuBucket &&& view amuKey
instance RemoveInstructions DeleteObject         where remove' = view doBucket  &&& view doKey

data DeleteInstructions = DeleteInstructions
    { _diExt    :: Ext
    , _diDelete :: DeleteObject
    } deriving (Show)

deleteInstructions :: RemoveInstructions a => a -> DeleteInstructions
deleteInstructions =
    DeleteInstructions defaultExtension . uncurry deleteObject . remove'

diExtension :: Lens' DeleteInstructions Ext
diExtension = lens _diExt (\s a -> s { _diExt = a })

instance AWSRequest DeleteInstructions where
    type Rs DeleteInstructions = DeleteObjectResponse

    request x = coerce . request $
        _diDelete x & doKey %~ appendExtension (_diExt x)

    response s l _ = response s l (Proxy :: Proxy DeleteObject)
