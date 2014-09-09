{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucket
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
module Network.AWS.S3.V2006_03_01.DeleteBucket
    (
    -- * Request
      DeleteBucket
    -- ** Request constructor
    , mkDeleteBucket
    -- ** Request lenses
    , dbBucket

    -- * Response
    , DeleteBucketResponse
    -- ** Response constructor
    , mkDeleteBucketResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype DeleteBucket = DeleteBucket
    { _dbBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucket' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
mkDeleteBucket :: BucketName -- ^ 'dbBucket'
               -> DeleteBucket
mkDeleteBucket p1 = DeleteBucket
    { _dbBucket = p1
    }

dbBucket :: Lens' DeleteBucket BucketName
dbBucket = lens _dbBucket (\s a -> s { _dbBucket = a })

instance ToPath DeleteBucket

instance ToQuery DeleteBucket

instance ToHeaders DeleteBucket

instance ToBody DeleteBucket

data DeleteBucketResponse = DeleteBucketResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteBucketResponse :: DeleteBucketResponse
mkDeleteBucketResponse = DeleteBucketResponse

instance AWSRequest DeleteBucket where
    type Sv DeleteBucket = S3
    type Rs DeleteBucket = DeleteBucketResponse

    request = get
    response _ = nullaryResponse DeleteBucketResponse
