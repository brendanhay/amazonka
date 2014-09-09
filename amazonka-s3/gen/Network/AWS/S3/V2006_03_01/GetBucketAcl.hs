{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the access control policy for the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketAcl
    (
    -- * Request
      GetBucketAcl
    -- ** Request constructor
    , mkGetBucketAcl
    -- ** Request lenses
    , gbaBucket

    -- * Response
    , GetBucketAclResponse
    -- ** Response constructor
    , mkGetBucketAclResponse
    -- ** Response lenses
    , gbarOwner
    , gbarGrants
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketAcl = GetBucketAcl
    { _gbaBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketAcl' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
mkGetBucketAcl :: BucketName -- ^ 'gbaBucket'
               -> GetBucketAcl
mkGetBucketAcl p1 = GetBucketAcl
    { _gbaBucket = p1
    }

gbaBucket :: Lens' GetBucketAcl BucketName
gbaBucket = lens _gbaBucket (\s a -> s { _gbaBucket = a })

instance ToPath GetBucketAcl

instance ToQuery GetBucketAcl

instance ToHeaders GetBucketAcl

instance ToBody GetBucketAcl

data GetBucketAclResponse = GetBucketAclResponse
    { _gbarOwner :: Maybe Owner
    , _gbarGrants :: [Grant]
    } deriving (Show, Generic)

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
mkGetBucketAclResponse :: GetBucketAclResponse
mkGetBucketAclResponse = GetBucketAclResponse
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
