{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the cors configuration information set for the bucket.
module Network.AWS.S3.V2006_03_01.DeleteBucketCors
    (
    -- * Request
      DeleteBucketCors
    -- ** Request constructor
    , deleteBucketCors
    -- ** Request lenses
    , dbcrBucket

    -- * Response
    , DeleteBucketCorsResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteBucketCors' request.
deleteBucketCors :: BucketName -- ^ 'dbcrBucket'
                 -> DeleteBucketCors
deleteBucketCors p1 = DeleteBucketCors
    { _dbcrBucket = p1
    }

data DeleteBucketCors = DeleteBucketCors
    { _dbcrBucket :: BucketName
    } deriving (Show, Generic)

dbcrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> DeleteBucketCors
    -> f DeleteBucketCors
dbcrBucket f x =
    (\y -> x { _dbcrBucket = y })
       <$> f (_dbcrBucket x)
{-# INLINE dbcrBucket #-}

instance ToPath DeleteBucketCors where
    toPath DeleteBucketCors{..} = mconcat
        [ "/"
        , toBS _dbcrBucket
        ]

instance ToQuery DeleteBucketCors where
    toQuery DeleteBucketCors{..} = mconcat
        [ "cors"
        ]

instance ToHeaders DeleteBucketCors

instance ToBody DeleteBucketCors

data DeleteBucketCorsResponse = DeleteBucketCorsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteBucketCors where
    type Sv DeleteBucketCors = S3
    type Rs DeleteBucketCors = DeleteBucketCorsResponse

    request = delete
    response _ = nullaryResponse DeleteBucketCorsResponse
