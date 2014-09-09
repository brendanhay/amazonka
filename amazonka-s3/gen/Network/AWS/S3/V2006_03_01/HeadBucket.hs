{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.HeadBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation is useful to determine if a bucket exists and you have
-- permission to access it.
module Network.AWS.S3.V2006_03_01.HeadBucket
    (
    -- * Request
      HeadBucket
    -- ** Request constructor
    , mkHeadBucket
    -- ** Request lenses
    , hbBucket

    -- * Response
    , HeadBucketResponse
    -- ** Response constructor
    , mkHeadBucketResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype HeadBucket = HeadBucket
    { _hbBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'HeadBucket' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
mkHeadBucket :: BucketName -- ^ 'hbBucket'
             -> HeadBucket
mkHeadBucket p1 = HeadBucket
    { _hbBucket = p1
    }

hbBucket :: Lens' HeadBucket BucketName
hbBucket = lens _hbBucket (\s a -> s { _hbBucket = a })

instance ToPath HeadBucket

instance ToQuery HeadBucket

instance ToHeaders HeadBucket

instance ToBody HeadBucket

data HeadBucketResponse = HeadBucketResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'HeadBucketResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkHeadBucketResponse :: HeadBucketResponse
mkHeadBucketResponse = HeadBucketResponse

instance AWSRequest HeadBucket where
    type Sv HeadBucket = S3
    type Rs HeadBucket = HeadBucketResponse

    request = get
    response _ = nullaryResponse HeadBucketResponse
