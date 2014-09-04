{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.AbortMultipartUpload
    (
    -- * Request
      AbortMultipartUpload
    -- ** Request constructor
    , mkAbortMultipartUploadRequest
    -- ** Request lenses
    , amurBucket
    , amurKey
    , amurUploadId

    -- * Response
    , AbortMultipartUploadResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AbortMultipartUpload' request.
mkAbortMultipartUploadRequest :: BucketName -- ^ 'amurBucket'
                              -> ObjectKey -- ^ 'amurKey'
                              -> Text -- ^ 'amurUploadId'
                              -> AbortMultipartUpload
mkAbortMultipartUploadRequest p1 p2 p3 = AbortMultipartUpload
    { _amurBucket = p1
    , _amurKey = p2
    , _amurUploadId = p3
    }
{-# INLINE mkAbortMultipartUploadRequest #-}

data AbortMultipartUpload = AbortMultipartUpload
    { _amurBucket :: BucketName
    , _amurKey :: ObjectKey
    , _amurUploadId :: Text
    } deriving (Show, Generic)

amurBucket :: Lens' AbortMultipartUpload (BucketName)
amurBucket = lens _amurBucket (\s a -> s { _amurBucket = a })
{-# INLINE amurBucket #-}

amurKey :: Lens' AbortMultipartUpload (ObjectKey)
amurKey = lens _amurKey (\s a -> s { _amurKey = a })
{-# INLINE amurKey #-}

amurUploadId :: Lens' AbortMultipartUpload (Text)
amurUploadId = lens _amurUploadId (\s a -> s { _amurUploadId = a })
{-# INLINE amurUploadId #-}

instance ToPath AbortMultipartUpload where
    toPath AbortMultipartUpload{..} = mconcat
        [ "/"
        , toBS _amurBucket
        , "/"
        , toBS _amurKey
        ]

instance ToQuery AbortMultipartUpload where
    toQuery AbortMultipartUpload{..} = mconcat
        [ "uploadId" =? _amurUploadId
        ]

instance ToHeaders AbortMultipartUpload

instance ToBody AbortMultipartUpload

data AbortMultipartUploadResponse = AbortMultipartUploadResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AbortMultipartUpload where
    type Sv AbortMultipartUpload = S3
    type Rs AbortMultipartUpload = AbortMultipartUploadResponse

    request = delete
    response _ = nullaryResponse AbortMultipartUploadResponse
