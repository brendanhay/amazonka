{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the cors configuration information set for the bucket.
module Network.AWS.S3.DeleteBucketCors
    (
    -- * Request
      DeleteBucketCors
    -- ** Request constructor
    , deleteBucketCors
    -- ** Request lenses
    , dbcrBucket

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

newtype DeleteBucketCors = DeleteBucketCors
    { _dbcrBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteBucketCors' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbcrBucket' @::@ 'BucketName'
--
deleteBucketCors :: BucketName -- ^ 'dbcrBucket'
                 -> DeleteBucketCors
deleteBucketCors p1 = DeleteBucketCors
    { _dbcrBucket = p1
    }

dbcrBucket :: Lens' DeleteBucketCors BucketName
dbcrBucket = lens _dbcrBucket (\s a -> s { _dbcrBucket = a })

instance ToPath DeleteBucketCors where
    toPath DeleteBucketCors{..} = mconcat
        [ "/"
        , toText _dbcrBucket
        ]

instance ToQuery DeleteBucketCors where
    toQuery = const "cors"

instance ToHeaders DeleteBucketCors

instance ToBody DeleteBucketCors

instance AWSRequest DeleteBucketCors where
    type Sv DeleteBucketCors = S3
    type Rs DeleteBucketCors = Empty

    request  = delete
    response = const (nullaryResponse Empty)
