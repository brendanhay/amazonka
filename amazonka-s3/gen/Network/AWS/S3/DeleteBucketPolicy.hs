{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the policy from the bucket.
module Network.AWS.S3.DeleteBucketPolicy
    (
    -- * Request
      DeleteBucketPolicy
    -- ** Request constructor
    , deleteBucketPolicy
    -- ** Request lenses
    , dbprBucket

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype DeleteBucketPolicy = DeleteBucketPolicy
    { _dbprBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteBucketPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbprBucket' @::@ 'BucketName'
--
deleteBucketPolicy :: BucketName -- ^ 'dbprBucket'
                   -> DeleteBucketPolicy
deleteBucketPolicy p1 = DeleteBucketPolicy
    { _dbprBucket = p1
    }

dbprBucket :: Lens' DeleteBucketPolicy BucketName
dbprBucket = lens _dbprBucket (\s a -> s { _dbprBucket = a })

instance ToPath DeleteBucketPolicy where
    toPath DeleteBucketPolicy{..} = mconcat
        [ "/"
        , toText _dbprBucket
        ]

instance ToQuery DeleteBucketPolicy where
    toQuery = const "policy"

instance ToHeaders DeleteBucketPolicy

instance AWSRequest DeleteBucketPolicy where
    type Sv DeleteBucketPolicy = S3
    type Rs DeleteBucketPolicy = Empty

    request  = delete
    response = const (nullaryResponse Empty)
