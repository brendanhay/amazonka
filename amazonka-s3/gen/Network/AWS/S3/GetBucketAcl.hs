{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the access control policy for the bucket.
module Network.AWS.S3.GetBucketAcl
    (
    -- * Request
      GetBucketAcl
    -- ** Request constructor
    , getBucketAcl
    -- ** Request lenses
    , gbaBucket

    -- * Response
    , GetBucketAclResponse
    -- ** Response constructor
    , getBucketAclResponse
    -- ** Response lenses
    , gbarOwner
    , gbarGrants
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketAcl = GetBucketAcl
    { _gbaBucket :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketAcl' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Text@
--
getBucketAcl :: Text -- ^ 'gbaBucket'
             -> GetBucketAcl
getBucketAcl p1 = GetBucketAcl
    { _gbaBucket = p1
    }

gbaBucket :: Lens' GetBucketAcl Text
gbaBucket = lens _gbaBucket (\s a -> s { _gbaBucket = a })

instance ToPath GetBucketAcl

instance ToQuery GetBucketAcl

instance ToHeaders GetBucketAcl

instance ToBody GetBucketAcl

data GetBucketAclResponse = GetBucketAclResponse
    { _gbarOwner :: Maybe Owner
    , _gbarGrants :: [Grant]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketAclResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Owner ::@ @Maybe Owner@
--
-- * @Grants ::@ @[Grant]@
--
getBucketAclResponse :: GetBucketAclResponse
getBucketAclResponse = GetBucketAclResponse
    { _gbarOwner = Nothing
    , _gbarGrants = mempty
    }

gbarOwner :: Lens' GetBucketAclResponse (Maybe Owner)
gbarOwner = lens _gbarOwner (\s a -> s { _gbarOwner = a })

-- | A list of grants.
gbarGrants :: Lens' GetBucketAclResponse [Grant]
gbarGrants = lens _gbarGrants (\s a -> s { _gbarGrants = a })

instance FromXML GetBucketAclResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketAcl where
    type Sv GetBucketAcl = S3
    type Rs GetBucketAcl = GetBucketAclResponse

    request = get
    response _ = xmlResponse
