{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.AbortMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Aborts a multipart upload. To verify that all parts have been removed, so
-- you don't get charged for the part storage, you should call the List Parts
-- operation and ensure the parts list is empty.
module Network.AWS.S3.V2006_03_01.AbortMultipartUpload where

import           Control.Applicative
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

-- | Default AbortMultipartUpload request.
abortMultipartUpload :: BucketName -- ^ 'amurBucket'
                     -> Text -- ^ 'amurUploadId'
                     -> ObjectKey -- ^ 'amurKey'
                     -> AbortMultipartUpload
abortMultipartUpload p1 p2 p3 = AbortMultipartUpload
    { amurBucket = p1
    , amurUploadId = p2
    , amurKey = p3
    }

data AbortMultipartUpload = AbortMultipartUpload
    { amurBucket :: BucketName
    , amurUploadId :: Text
    , amurKey :: ObjectKey
    } deriving (Eq, Show, Generic)

instance ToPath AbortMultipartUpload where
    toPath AbortMultipartUpload{..} = mconcat
        [ "/"
        , toBS amurBucket
        , "/"
        , toBS amurKey
        ]

instance ToQuery AbortMultipartUpload

instance ToHeaders AbortMultipartUpload

instance ToBody AbortMultipartUpload

instance AWSRequest AbortMultipartUpload where
    type Sv AbortMultipartUpload = S3

    request  = delete
    response = headerResponse . const $ Right AbortMultipartUploadResponse

data instance Rs AbortMultipartUpload = AbortMultipartUploadResponse
    deriving (Eq, Show, Generic)
