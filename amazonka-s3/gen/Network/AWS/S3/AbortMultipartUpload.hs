{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.AbortMultipartUpload
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
module Network.AWS.S3.AbortMultipartUpload
    (
    -- * Request
      AbortMultipartUpload
    -- ** Request constructor
    , abortMultipartUpload
    -- ** Request lenses
    , amurBucket
    , amurKey
    , amurUploadId

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data AbortMultipartUpload = AbortMultipartUpload
    { _amurBucket   :: BucketName
    , _amurKey      :: ObjectKey
    , _amurUploadId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AbortMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amurBucket' @::@ 'BucketName'
--
-- * 'amurKey' @::@ 'ObjectKey'
--
-- * 'amurUploadId' @::@ 'Text'
--
abortMultipartUpload :: BucketName -- ^ 'amurBucket'
                     -> ObjectKey -- ^ 'amurKey'
                     -> Text -- ^ 'amurUploadId'
                     -> AbortMultipartUpload
abortMultipartUpload p1 p2 p3 = AbortMultipartUpload
    { _amurBucket   = p1
    , _amurKey      = p2
    , _amurUploadId = p3
    }

amurBucket :: Lens' AbortMultipartUpload BucketName
amurBucket = lens _amurBucket (\s a -> s { _amurBucket = a })

amurKey :: Lens' AbortMultipartUpload ObjectKey
amurKey = lens _amurKey (\s a -> s { _amurKey = a })

amurUploadId :: Lens' AbortMultipartUpload Text
amurUploadId = lens _amurUploadId (\s a -> s { _amurUploadId = a })

instance ToPath AbortMultipartUpload where
    toPath AbortMultipartUpload{..} = mconcat
        [ "/"
        , toText _amurBucket
        , "/"
        , toText _amurKey
        ]

instance ToQuery AbortMultipartUpload where

instance ToHeaders AbortMultipartUpload

instance AWSRequest AbortMultipartUpload where
    type Sv AbortMultipartUpload = S3
    type Rs AbortMultipartUpload = Empty

    request  = delete
    response = const (nullaryResponse Empty)
