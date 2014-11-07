{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , dbrBucket

    -- * Response
    , DeleteBucketResponse
    -- ** Response constructor
    , deleteBucketResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype DeleteBucket = DeleteBucket
    { _dbrBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteBucket' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbrBucket' @::@ 'Text'
--
deleteBucket :: Text -- ^ 'dbrBucket'
             -> DeleteBucket
deleteBucket p1 = DeleteBucket
    { _dbrBucket = p1
    }

dbrBucket :: Lens' DeleteBucket Text
dbrBucket = lens _dbrBucket (\s a -> s { _dbrBucket = a })

instance ToPath DeleteBucket where
    toPath DeleteBucket{..} = mconcat
        [ "/"
        , toText _dbrBucket
        ]

instance ToQuery DeleteBucket

instance ToHeaders DeleteBucket

deleteBucketResponse :: DeleteBucketResponse
deleteBucketResponse = DeleteBucketResponse

instance AWSRequest DeleteBucket where
    type Sv DeleteBucket = S3
    type Rs DeleteBucket = DeleteBucketResponse

    request  = delete'
    response = const (nullaryResponse DeleteBucketResponse)
