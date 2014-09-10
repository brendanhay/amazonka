{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the lifecycle configuration from the bucket.
module Network.AWS.S3
    (
    -- * Request
      DeleteBucketLifecycle
    -- ** Request constructor
    , mkDeleteBucketLifecycle
    -- ** Request lenses
    , dblBucket

    -- * Response
    , DeleteBucketLifecycleResponse
    -- ** Response constructor
    , mkDeleteBucketLifecycleResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype DeleteBucketLifecycle = DeleteBucketLifecycle
    { _dblBucket :: !BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketLifecycle' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
mkDeleteBucketLifecycle :: BucketName -- ^ 'dblBucket'
                        -> DeleteBucketLifecycle
mkDeleteBucketLifecycle p1 = DeleteBucketLifecycle
    { _dblBucket = p1
    }

dblBucket :: Lens' DeleteBucketLifecycle BucketName
dblBucket = lens _dblBucket (\s a -> s { _dblBucket = a })

instance ToPath DeleteBucketLifecycle

instance ToQuery DeleteBucketLifecycle

instance ToHeaders DeleteBucketLifecycle

instance ToBody DeleteBucketLifecycle

data DeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketLifecycleResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteBucketLifecycleResponse :: DeleteBucketLifecycleResponse
mkDeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse

instance AWSRequest DeleteBucketLifecycle where
    type Sv DeleteBucketLifecycle = S3
    type Rs DeleteBucketLifecycle = DeleteBucketLifecycleResponse

    request = get
    response _ = nullaryResponse DeleteBucketLifecycleResponse
