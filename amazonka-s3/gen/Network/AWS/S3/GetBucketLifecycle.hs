{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the lifecycle configuration information set on the bucket.
module Network.AWS.S3.GetBucketLifecycle
    (
    -- * Request
      GetBucketLifecycle
    -- ** Request constructor
    , getBucketLifecycle
    -- ** Request lenses
    , gblBucket

    -- * Response
    , GetBucketLifecycleResponse
    -- ** Response constructor
    , getBucketLifecycleResponse
    -- ** Response lenses
    , gblrRules
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketLifecycle = GetBucketLifecycle
    { _gblBucket :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketLifecycle' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Text@
--
getBucketLifecycle :: Text -- ^ 'gblBucket'
                   -> GetBucketLifecycle
getBucketLifecycle p1 = GetBucketLifecycle
    { _gblBucket = p1
    }

gblBucket :: Lens' GetBucketLifecycle Text
gblBucket = lens _gblBucket (\s a -> s { _gblBucket = a })

instance ToPath GetBucketLifecycle

instance ToQuery GetBucketLifecycle

instance ToHeaders GetBucketLifecycle

instance ToBody GetBucketLifecycle

newtype GetBucketLifecycleResponse = GetBucketLifecycleResponse
    { _gblrRules :: [Rule]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketLifecycleResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Rules ::@ @[Rule]@
--
getBucketLifecycleResponse :: GetBucketLifecycleResponse
getBucketLifecycleResponse = GetBucketLifecycleResponse
    { _gblrRules = mempty
    }

gblrRules :: Lens' GetBucketLifecycleResponse [Rule]
gblrRules = lens _gblrRules (\s a -> s { _gblrRules = a })

instance FromXML GetBucketLifecycleResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketLifecycle where
    type Sv GetBucketLifecycle = S3
    type Rs GetBucketLifecycle = GetBucketLifecycleResponse

    request = get
    response _ = xmlResponse
