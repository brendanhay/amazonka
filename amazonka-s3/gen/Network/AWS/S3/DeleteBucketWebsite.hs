{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation removes the website configuration from the bucket.
module Network.AWS.S3
    (
    -- * Request
      DeleteBucketWebsite
    -- ** Request constructor
    , mkDeleteBucketWebsite
    -- ** Request lenses
    , dbwBucket

    -- * Response
    , DeleteBucketWebsiteResponse
    -- ** Response constructor
    , mkDeleteBucketWebsiteResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype DeleteBucketWebsite = DeleteBucketWebsite
    { _dbwBucket :: !BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketWebsite' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
mkDeleteBucketWebsite :: BucketName -- ^ 'dbwBucket'
                      -> DeleteBucketWebsite
mkDeleteBucketWebsite p1 = DeleteBucketWebsite
    { _dbwBucket = p1
    }

dbwBucket :: Lens' DeleteBucketWebsite BucketName
dbwBucket = lens _dbwBucket (\s a -> s { _dbwBucket = a })

instance ToPath DeleteBucketWebsite

instance ToQuery DeleteBucketWebsite

instance ToHeaders DeleteBucketWebsite

instance ToBody DeleteBucketWebsite

data DeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketWebsiteResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteBucketWebsiteResponse :: DeleteBucketWebsiteResponse
mkDeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse

instance AWSRequest DeleteBucketWebsite where
    type Sv DeleteBucketWebsite = S3
    type Rs DeleteBucketWebsite = DeleteBucketWebsiteResponse

    request = get
    response _ = nullaryResponse DeleteBucketWebsiteResponse
