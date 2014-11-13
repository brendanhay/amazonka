{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.S3.DeleteBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the bucket. All objects (including all object versions and Delete
-- Markers) in the bucket must be deleted before the bucket itself can be
-- deleted.
module Network.AWS.S3.DeleteBucket
    (
    -- * Request
      DeleteBucket
    -- ** Request constructor
    , deleteBucket
    -- ** Request lenses
    , dbBucket

    -- * Response
    , DeleteBucketResponse
    -- ** Response constructor
    , deleteBucketResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype DeleteBucket = DeleteBucket
    { _dbBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteBucket' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbBucket' @::@ 'Text'
--
deleteBucket :: Text -- ^ 'dbBucket'
             -> DeleteBucket
deleteBucket p1 = DeleteBucket
    { _dbBucket = p1
    }

dbBucket :: Lens' DeleteBucket Text
dbBucket = lens _dbBucket (\s a -> s { _dbBucket = a })

instance ToPath DeleteBucket where
    toPath DeleteBucket{..} = mconcat
        [ "/"
        , toText _dbBucket
        ]

instance ToQuery DeleteBucket where
    toQuery = const mempty

instance ToHeaders DeleteBucket

data DeleteBucketResponse = DeleteBucketResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteBucketResponse' constructor.
deleteBucketResponse :: DeleteBucketResponse
deleteBucketResponse = DeleteBucketResponse

instance AWSRequest DeleteBucket where
    type Sv DeleteBucket = S3
    type Rs DeleteBucket = DeleteBucketResponse

    request  = delete
    response = nullaryResponse DeleteBucketResponse
