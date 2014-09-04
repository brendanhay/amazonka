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
    , abortMultipartUpload
    -- ** Request lenses
    , amurBucket
    , amurUploadId
    , amurKey

    -- * Response
    , AbortMultipartUploadResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AbortMultipartUpload' request.
abortMultipartUpload :: BucketName -- ^ 'amurBucket'
                     -> Text -- ^ 'amurUploadId'
                     -> ObjectKey -- ^ 'amurKey'
                     -> AbortMultipartUpload
abortMultipartUpload p1 p2 p3 = AbortMultipartUpload
    { _amurBucket = p1
    , _amurUploadId = p2
    , _amurKey = p3
    }
{-# INLINE abortMultipartUpload #-}

data AbortMultipartUpload = AbortMultipartUpload
    { _amurBucket :: BucketName
    , _amurUploadId :: Text
    , _amurKey :: ObjectKey
    } deriving (Show, Generic)

amurBucket :: Lens' AbortMultipartUpload (BucketName)
amurBucket f x =
    f (_amurBucket x)
        <&> \y -> x { _amurBucket = y }
{-# INLINE amurBucket #-}

amurUploadId :: Lens' AbortMultipartUpload (Text)
amurUploadId f x =
    f (_amurUploadId x)
        <&> \y -> x { _amurUploadId = y }
{-# INLINE amurUploadId #-}

amurKey :: Lens' AbortMultipartUpload (ObjectKey)
amurKey f x =
    f (_amurKey x)
        <&> \y -> x { _amurKey = y }
{-# INLINE amurKey #-}

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
