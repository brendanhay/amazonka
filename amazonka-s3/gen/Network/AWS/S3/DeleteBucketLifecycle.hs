{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.S3.DeleteBucketLifecycle
    (
    -- * Request
      DeleteBucketLifecycle
    -- ** Request constructor
    , deleteBucketLifecycle
    -- ** Request lenses
    , dblrBucket

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

newtype DeleteBucketLifecycle = DeleteBucketLifecycle
    { _dblrBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteBucketLifecycle' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dblrBucket' @::@ 'BucketName'
--
deleteBucketLifecycle :: BucketName -- ^ 'dblrBucket'
                      -> DeleteBucketLifecycle
deleteBucketLifecycle p1 = DeleteBucketLifecycle
    { _dblrBucket = p1
    }

dblrBucket :: Lens' DeleteBucketLifecycle BucketName
dblrBucket = lens _dblrBucket (\s a -> s { _dblrBucket = a })

instance ToPath DeleteBucketLifecycle where
    toPath DeleteBucketLifecycle{..} = mconcat
        [ "/"
        , toText _dblrBucket
        ]

instance ToQuery DeleteBucketLifecycle where
    toQuery = const "lifecycle"

instance ToHeaders DeleteBucketLifecycle

instance ToBody DeleteBucketLifecycle

instance AWSRequest DeleteBucketLifecycle where
    type Sv DeleteBucketLifecycle = S3
    type Rs DeleteBucketLifecycle = Empty

    request  = delete
    response = const (nullaryResponse Empty)
