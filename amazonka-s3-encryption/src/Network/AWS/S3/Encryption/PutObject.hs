{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- |
-- Module      : Network.AWS.S3.Encryption.PutObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.PutObject where

import           Control.Arrow
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource
import qualified Data.Aeson.Types                   as Aeson
import           Data.Coerce
import           Data.Conduit
import           Data.Proxy
import           Network.AWS.Prelude                hiding (coerce)
import           Network.AWS.Response
import           Network.AWS.S3
import qualified Network.AWS.S3                     as S3
import           Network.AWS.S3.Encryption.Envelope
import           Network.AWS.S3.Encryption.Types
import           System.IO

data KeyEnv = KeyEnv
    { _envInner :: !Env
    , _envKey   :: !Key
    }

instance HasEnv KeyEnv where
    environment = lens _envInner (\s a -> s { _envInner = a })

class HasKeyEnv a where
    keyEnvironment :: Lens' a KeyEnv

    -- | Key material used to encrypt/decrypt requests.
    envKey :: Lens' a Key
    envKey = keyEnvironment . lens _envKey (\s a -> s { _envKey = a })

-- | Set the key material used to encrypt/decrypt a block of actions.
material :: (MonadReader r m, HasKeyEnv r) => Key -> m a -> m a
material k = local (envKey .~ k)

encrypt :: (AWSConstraint r m, HasKeyEnv r)
        => PutObject
        -> m PutObjectResponse
encrypt = encrypted >=> send . set location Metadata . fst

-- encryptWith :: a -> Location -> Envelope -> Encrypted a

-- encryptUsing :: (AWSConstraint r m, HasKeyEnv r)
--              => PutObject
--              -> m PutObjectResponse
-- encryptUsing x = do
--     (a, b) <- encrypted x
--     send a
--     send b -- this seems super redundant.

-- | Note about parallelism/concurrency, and encryption of parts. If you don't
-- encrypt any of the parts then the entire thing is unencrypted!
initiate :: (AWSConstraint r m, HasKeyEnv r)
         => CreateMultipartUpload
         -> m ( CreateMultipartUploadResponse
              , UploadPart -> Encrypted UploadPart
              )
initiate x = do
    (a, _) <- encrypted x
    rs     <- send (a & location .~ Metadata)
    return (rs, \y -> encryptWith y Discard (a ^. envelope))

initiateInstructions :: (AWSConstraint r m, HasKeyEnv r)
                     => Ext
                     -> CreateMultipartUpload
                     -> m ( CreateMultipartUploadResponse
                          , UploadPart -> Encrypted UploadPart
                          )
initiateInstructions x = do
    (a, b) <- encrypted x
    send a
    send b
    return (rs, \y -> encryptWith y Discard (_encEnvelope a))

-- initiateInstructions :: (AWSConstraint r m, HasKeyEnv r)
--                      => Ext
--                      -> CreateMultipartUpload
--                      -> m ( CreateMultipartUploadResponse
--                           , UploadPart -> Encrypted UploadPart
--                           )
-- initiateInstructions s x = do
--     e  <- genEnvelope
--     rs <- send (encryptWith x Discard e)
--     void $ send (putInstructions x e & piSuffix .~ s)
--     return (rs, \y -> encryptWith y Discard e)

decrypt :: (AWSConstraint r m, HasKeyEnv r)
        => GetObject
        -> m GetObjectResponse
decrypt x = do
    rs <- send x
    r  <- ask
    e  <- fromMetadata (r ^. envKey) (r ^. environment) (rs ^. gorsMetadata)
    return (rs & gorsBody %~ bodyDecrypt e)

decryptUsing :: (AWSConstraint r m, HasKeyEnv r)
             => Ext
             -> GetObject
             -> m GetObjectResponse
decryptUsing s x = do
    rs <- send (getInstructions x & giSuffix .~ s)
    r  <- ask
    e  <- fromInstructions (r ^. envKey) (r ^. environment) rs
    send x <&> gorsBody %~ bodyDecrypt e

-- given a request to execute, such as AbortMultipartUpload or DeleteObject,
-- remove the relevant adjacentinstruction file, if it exists.
cleanup :: (AWSConstraint r m, AWSRequest a, RemoveInstructions a)
        => Ext
        -> a
        -> m (Rs a)
cleanup s x = do
    rs <- send x
    void $ send (deleteInstructions x & diSuffix .~ s)
    return rs

encrypted :: (AWSConstraint r m, HasKeyEnv r, ToEncrypted a)
          => a
          -> m (Encrypted a, PutInstructions)
encrypted x = do
    e <- join $ newEnvelope <$> view envKey <*> view environment
    return ( encryptWith x Discard e
           , putInstructions x e
           )

data Encrypted a = Encrypted
    { _encPayload  :: a
    , _encHeaders  :: [Header]
    , _encLocation :: Location
    , _encEnvelope :: Envelope
    }

location :: Lens' (Encrypted a) Location
location = lens _encLocation (\s a -> s { _encLocation = a })

envelope :: Lens' (Encrypted a) Envelope
envelope = lens _encEnvelope (\s a -> s { _encEnvelope = a })

instance AWSRequest a => AWSRequest (Encrypted a) where
    type Rs (Encrypted a) = Rs a

    request (Encrypted x xs l e) = coerce (request x)
        & rqBody     %~ f
        & rqHeaders <>~ hs
      where
        f b | contentLength b > 0 = bodyEncrypt e b
            | otherwise           = b

        hs  | l == Metadata = xs <> toHeaders e
            | otherwise     = xs

    response l s (Encrypted x _ _ _) = response l s x

class (AWSRequest a, AddInstructions a) => ToEncrypted a where
    -- | Create an encryption context.
    encryptWith :: a -> Location -> Envelope -> Encrypted a

instance ToEncrypted CreateMultipartUpload where
    encryptWith x = Encrypted x []

instance ToEncrypted PutObject where
    encryptWith x = Encrypted x (len : maybeToList md5)
     where
        len = ("X-Amz-Unencrypted-Content-Length",
            toBS (contentLength (x ^. poBody)))

        md5 = ("X-Amz-Unencrypted-Content-MD5",)
            <$> x ^. poBody . to md5Base64

instance ToEncrypted UploadPart where
    encryptWith x = Encrypted x (len : maybeToList md5)
     where
        len = ("X-Amz-Unencrypted-Content-Length",
            toBS (contentLength (x ^. upBody)))

        md5 = ("X-Amz-Unencrypted-Content-MD5",)
            <$> x ^. upBody . to md5Base64

class AddInstructions a where
    -- | Determine the bucket and key an instructions file is adjacent to.
    add' :: a -> (BucketName, ObjectKey)

instance AddInstructions (BucketName, ObjectKey) where
    add' = id

instance AddInstructions PutObject where
    add' = view poBucket &&& view poKey

instance AddInstructions GetObject where
    add' = view goBucket &&& view goKey

instance AddInstructions CreateMultipartUpload where
    add' = view cmuBucket &&& view cmuKey

data PutInstructions = PutInstructions
    { _piExt :: Ext
    , _piPut :: PutObject
    } deriving (Show)

putInstructions :: AddInstructions a => a -> Envelope -> PutInstructions
putInstructions (add' -> (b, k)) =
    PutInstructions defaultSuffix . putObject b k . toBody

piSuffix :: Lens' PutInstructions Ext
piSuffix = lens _piExt (\s a -> s { _piExt = a })

instance AWSRequest PutInstructions where
    type Rs PutInstructions = PutObjectResponse

    request x = coerce . request $
        _piPut x & poKey %~ appendSuffix (_piExt x)

    response s l (PutInstructions _ x) = response s l x

data GetInstructions = GetInstructions
    { _giExt :: Ext
    , _giGet :: GetObject
    } deriving (Show)

getInstructions :: AddInstructions a => a -> GetInstructions
getInstructions = GetInstructions defaultSuffix . uncurry getObject . add'

giSuffix :: Lens' GetInstructions Ext
giSuffix = lens _giExt (\s a -> s { _giExt = a })

instance AWSRequest GetInstructions where
    type Rs GetInstructions = Aeson.Object

    request x = coerce . request $
        _giGet x & goKey %~ appendSuffix (_giExt x)

    response  = receiveJSON (\_ _ -> return)

class RemoveInstructions a where
    -- | Determine the bucket and key an instructions file is adjacent to.
    remove' :: a -> (BucketName, ObjectKey)

instance RemoveInstructions (BucketName, ObjectKey) where
    remove' = id

instance RemoveInstructions AbortMultipartUpload where
    remove' = view amuBucket &&& view amuKey

instance RemoveInstructions DeleteObject where
    remove' = view doBucket &&& view doKey

data DeleteInstructions = DeleteInstructions
    { _diExt    :: Ext
    , _diDelete :: DeleteObject
    } deriving (Show)

deleteInstructions :: RemoveInstructions a => a -> DeleteInstructions
deleteInstructions =
    DeleteInstructions defaultSuffix . uncurry deleteObject . remove'

diSuffix :: Lens' DeleteInstructions Ext
diSuffix = lens _diExt (\s a -> s { _diExt = a })

instance AWSRequest DeleteInstructions where
    type Rs DeleteInstructions = DeleteObjectResponse

    request x = coerce . request $
        _diDelete x & doKey %~ appendSuffix (_diExt x)

    response s l (DeleteInstructions _ x) = response s l x
