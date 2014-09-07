{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the lifecycle configuration from the bucket.
module Network.AWS.S3.V2006_03_01.DeleteBucketLifecycle
    (
    -- * Request
      DeleteBucketLifecycle
    -- ** Request constructor
    , mkDeleteBucketLifecycle
    -- ** Request lenses
    , dblBucket

    -- * Response
    , DeleteBucketLifecycleResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype DeleteBucketLifecycle = DeleteBucketLifecycle
    { _dblBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketLifecycle' request.
mkDeleteBucketLifecycle :: BucketName -- ^ 'dblBucket'
                        -> DeleteBucketLifecycle
mkDeleteBucketLifecycle p1 = DeleteBucketLifecycle
    { _dblBucket = p1
    }

dblBucket :: Lens' DeleteBucketLifecycle BucketName
dblBucket = lens _dblBucket (\s a -> s { _dblBucket = a })

instance ToPath DeleteBucketLifecycle where
    toPath DeleteBucketLifecycle{..} = mconcat
        [ "/"
        , toBS _dblBucket
        ]

instance ToQuery DeleteBucketLifecycle where
    toQuery DeleteBucketLifecycle{..} = mconcat
        [ "lifecycle"
        ]

instance ToHeaders DeleteBucketLifecycle

instance ToBody DeleteBucketLifecycle

data DeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteBucketLifecycle where
    type Sv DeleteBucketLifecycle = S3
    type Rs DeleteBucketLifecycle = DeleteBucketLifecycleResponse

    request = delete
    response _ = nullaryResponse DeleteBucketLifecycleResponse
