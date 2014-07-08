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

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Smart constructor utilising default fields to
-- specify the minimum viable CompleteMultipartUpload request.
completeMultipartUpload :: BucketName -- ^ 'cmurBucket'
                        -> Text -- ^ 'cmurUploadId'
                        -> ObjectKey -- ^ 'cmurKey'
                        -> CompletedMultipartUpload -- ^ 'cmurMultipartUpload'
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 p4 = CompleteMultipartUpload
    { cmurBucket = p1
    , cmurUploadId = p2
    , cmurKey = p3
    , cmurMultipartUpload = p4
    }

data CompleteMultipartUpload = CompleteMultipartUpload
    { cmurBucket :: BucketName
    , cmurUploadId :: Text
    , cmurKey :: ObjectKey
    , cmurMultipartUpload :: CompletedMultipartUpload
    } deriving (Eq, Show, Generic)

instance ToPath CompleteMultipartUpload where
    toPath CompleteMultipartUpload{..} = mconcat
        [ "/"
        , toBS cmurBucket
        , "/"
        , toBS cmurKey
        ]

instance ToQuery CompleteMultipartUpload

instance ToHeaders CompleteMultipartUpload

instance ToBody CompleteMultipartUpload where
    toBody = undefined -- toBody . cmurMultipartUpload

instance AWSRequest CompleteMultipartUpload where
    type Sv CompleteMultipartUpload = S3

    request  = post
    response = response' $

data instance Rs CompleteMultipartUpload = CompleteMultipartUploadResponse
    { cmuoBucket :: Maybe BucketName
    , cmuoETag :: Maybe ETag
      -- ^ Entity tag of the object.
    , cmuoLocation :: Maybe Text
    , cmuoKey :: Maybe ObjectKey
    , cmuoExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, this will contain the
      -- expiration date (expiry-date) and rule ID (rule-id). The value of
      -- rule-id is URL encoded.
    , cmuoVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , cmuoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Eq, Show, Generic)
