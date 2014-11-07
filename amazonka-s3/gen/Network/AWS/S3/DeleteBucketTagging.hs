{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the tags from the bucket.
module Network.AWS.S3.DeleteBucketTagging
    (
    -- * Request
      DeleteBucketTagging
    -- ** Request constructor
    , deleteBucketTagging
    -- ** Request lenses
    , dbtrBucket

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype DeleteBucketTagging = DeleteBucketTagging
    { _dbtrBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteBucketTagging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtrBucket' @::@ 'Text'
--
deleteBucketTagging :: Text -- ^ 'dbtrBucket'
                    -> DeleteBucketTagging
deleteBucketTagging p1 = DeleteBucketTagging
    { _dbtrBucket = p1
    }

dbtrBucket :: Lens' DeleteBucketTagging Text
dbtrBucket = lens _dbtrBucket (\s a -> s { _dbtrBucket = a })

instance ToPath DeleteBucketTagging where
    toPath DeleteBucketTagging{..} = mconcat
        [ "/"
        , toText _dbtrBucket
        ]

instance ToQuery DeleteBucketTagging where
    toQuery = const "tagging"

instance ToHeaders DeleteBucketTagging


instance AWSRequest DeleteBucketTagging where
    type Sv DeleteBucketTagging = S3
    type Rs DeleteBucketTagging = Empty

    request  = delete'
    response = const (nullaryResponse Empty)
