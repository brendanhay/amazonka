{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

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
    , DeleteBucketCorsResponse
    -- ** Response constructor
    , deleteBucketCorsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype DeleteBucketCors = DeleteBucketCors
    { _dbcrBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteBucketCors' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbcrBucket' @::@ 'Text'
--
deleteBucketCors :: Text -- ^ 'dbcrBucket'
                 -> DeleteBucketCors
deleteBucketCors p1 = DeleteBucketCors
    { _dbcrBucket = p1
    }

dbcrBucket :: Lens' DeleteBucketCors Text
dbcrBucket = lens _dbcrBucket (\s a -> s { _dbcrBucket = a })

instance ToPath DeleteBucketCors where
    toPath DeleteBucketCors{..} = mconcat
        [ "/"
        , toText _dbcrBucket
        ]

instance ToQuery DeleteBucketCors where
    toQuery = const "cors"

instance ToHeaders DeleteBucketCors

data DeleteBucketCorsResponse = DeleteBucketCorsResponse
deleteBucketCorsResponse :: DeleteBucketCorsResponse
deleteBucketCorsResponse = DeleteBucketCorsResponse

instance AWSRequest DeleteBucketCors where
    type Sv DeleteBucketCors = S3
    type Rs DeleteBucketCors = DeleteBucketCorsResponse

    request  = delete'
    response = const (nullaryResponse DeleteBucketCorsResponse)
