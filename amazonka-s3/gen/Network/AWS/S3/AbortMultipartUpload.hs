{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.S3
    (
    -- * Request
      AbortMultipartUpload
    -- ** Request constructor
    , mkAbortMultipartUpload
    -- ** Request lenses
    , amuBucket
    , amuKey
    , amuUploadId

    -- * Response
    , AbortMultipartUploadResponse
    -- ** Response constructor
    , mkAbortMultipartUploadResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data AbortMultipartUpload = AbortMultipartUpload
    { _amuBucket :: !BucketName
    , _amuKey :: !ObjectKey
    , _amuUploadId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AbortMultipartUpload' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @Key ::@ @ObjectKey@
--
-- * @UploadId ::@ @Text@
--
mkAbortMultipartUpload :: BucketName -- ^ 'amuBucket'
                       -> ObjectKey -- ^ 'amuKey'
                       -> Text -- ^ 'amuUploadId'
                       -> AbortMultipartUpload
mkAbortMultipartUpload p1 p2 p3 = AbortMultipartUpload
    { _amuBucket = p1
    , _amuKey = p2
    , _amuUploadId = p3
    }

amuBucket :: Lens' AbortMultipartUpload BucketName
amuBucket = lens _amuBucket (\s a -> s { _amuBucket = a })

amuKey :: Lens' AbortMultipartUpload ObjectKey
amuKey = lens _amuKey (\s a -> s { _amuKey = a })

amuUploadId :: Lens' AbortMultipartUpload Text
amuUploadId = lens _amuUploadId (\s a -> s { _amuUploadId = a })

instance ToPath AbortMultipartUpload

instance ToQuery AbortMultipartUpload

instance ToHeaders AbortMultipartUpload

instance ToBody AbortMultipartUpload

data AbortMultipartUploadResponse = AbortMultipartUploadResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AbortMultipartUploadResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAbortMultipartUploadResponse :: AbortMultipartUploadResponse
mkAbortMultipartUploadResponse = AbortMultipartUploadResponse

instance AWSRequest AbortMultipartUpload where
    type Sv AbortMultipartUpload = S3
    type Rs AbortMultipartUpload = AbortMultipartUploadResponse

    request = get
    response _ = nullaryResponse AbortMultipartUploadResponse
