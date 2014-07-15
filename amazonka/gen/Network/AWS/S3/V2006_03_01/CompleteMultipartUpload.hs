{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.CompleteMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Completes a multipart upload by assembling previously uploaded parts.
module Network.AWS.S3.V2006_03_01.CompleteMultipartUpload where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (Response)
import           Prelude              hiding (head)

-- | Default CompleteMultipartUpload request.
completeMultipartUpload :: BucketName -- ^ '_cmurBucket'
                        -> Text -- ^ '_cmurUploadId'
                        -> ObjectKey -- ^ '_cmurKey'
                        -> CompletedMultipartUpload -- ^ '_cmurMultipartUpload'
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 p4 = CompleteMultipartUpload
    { _cmurBucket = p1
    , _cmurUploadId = p2
    , _cmurKey = p3
    , _cmurMultipartUpload = p4
    }

data CompleteMultipartUpload = CompleteMultipartUpload
    { _cmurBucket :: BucketName
    , _cmurUploadId :: Text
    , _cmurKey :: ObjectKey
    , _cmurMultipartUpload :: CompletedMultipartUpload
    } deriving (Show, Generic)

instance ToPath CompleteMultipartUpload where
    toPath CompleteMultipartUpload{..} = mconcat
        [ "/"
        , toBS _cmurBucket
        , "/"
        , toBS _cmurKey
        ]

instance ToQuery CompleteMultipartUpload

instance ToHeaders CompleteMultipartUpload

instance ToBody CompleteMultipartUpload where
    toBody = undefined -- toBody . _cmurMultipartUpload

instance AWSRequest CompleteMultipartUpload where
    type Sv CompleteMultipartUpload = S3

    request  = post
    response = cursorResponse $ \hs xml ->
        pure CompleteMultipartUploadResponse
            <*> xml %|? "BucketName"
            <*> xml %|? "ETag"
            <*> xml %|? "Location"
            <*> xml %|? "ObjectKey"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "x-amz-server-side-encryption"

data instance Rs CompleteMultipartUpload = CompleteMultipartUploadResponse
    { _cmuoBucket :: Maybe BucketName
    , _cmuoETag :: Maybe ETag
      -- ^ Entity tag of the object.
    , _cmuoLocation :: Maybe Text
    , _cmuoKey :: Maybe ObjectKey
    , _cmuoExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, this will contain the
      -- expiration date (expiry-date) and rule ID (rule-id). The value of
      -- rule-id is URL encoded.
    , _cmuoVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , _cmuoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Show, Generic)
