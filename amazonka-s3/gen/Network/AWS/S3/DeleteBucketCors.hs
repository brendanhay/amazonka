{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , dbcBucket

    -- * Response
    , DeleteBucketCorsResponse
    -- ** Response constructor
    , deleteBucketCorsResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype DeleteBucketCors = DeleteBucketCors
    { _dbcBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketCors' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
deleteBucketCors :: BucketName -- ^ 'dbcBucket'
                 -> DeleteBucketCors
deleteBucketCors p1 = DeleteBucketCors
    { _dbcBucket = p1
    }

dbcBucket :: Lens' DeleteBucketCors BucketName
dbcBucket = lens _dbcBucket (\s a -> s { _dbcBucket = a })

instance ToPath DeleteBucketCors

instance ToQuery DeleteBucketCors

instance ToHeaders DeleteBucketCors

instance ToBody DeleteBucketCors

data DeleteBucketCorsResponse = DeleteBucketCorsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketCorsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteBucketCorsResponse :: DeleteBucketCorsResponse
deleteBucketCorsResponse = DeleteBucketCorsResponse

instance AWSRequest DeleteBucketCors where
    type Sv DeleteBucketCors = S3
    type Rs DeleteBucketCors = DeleteBucketCorsResponse

    request = get
    response _ = nullaryResponse DeleteBucketCorsResponse
