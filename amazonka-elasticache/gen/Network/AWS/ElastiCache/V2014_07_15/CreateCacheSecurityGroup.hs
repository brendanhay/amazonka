{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.CreateCacheSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheSecurityGroup operation creates a new cache security group.
-- Use a cache security group to control access to one or more cache clusters.
-- Cache security groups are only used when you are creating a cluster outside
-- of an Amazon Virtual Private Cloud (VPC). If you are creating a cluster
-- inside of a VPC, use a cache subnet group instead. For more information,
-- see CreateCacheSubnetGroup. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheSecurityGroup
-- &CacheSecurityGroupName=mycachesecuritygroup
-- &Description=My%20cache%20security%20group &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycachesecuritygroup 123456789012 My cache security
-- group 2b1c8035-b7fa-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_07_15.CreateCacheSecurityGroup
    (
    -- * Request
      CreateCacheSecurityGroup
    -- ** Request constructor
    , createCacheSecurityGroup
    -- ** Request lenses
    , ccsgmCacheSecurityGroupName
    , ccsgmDescription

    -- * Response
    , CreateCacheSecurityGroupResponse
    -- ** Response lenses
    , csgxCacheSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateCacheSecurityGroup' request.
createCacheSecurityGroup :: Text -- ^ 'ccsgmCacheSecurityGroupName'
                         -> Text -- ^ 'ccsgmDescription'
                         -> CreateCacheSecurityGroup
createCacheSecurityGroup p1 p2 = CreateCacheSecurityGroup
    { _ccsgmCacheSecurityGroupName = p1
    , _ccsgmDescription = p2
    }

data CreateCacheSecurityGroup = CreateCacheSecurityGroup
    { _ccsgmCacheSecurityGroupName :: Text
      -- ^ A name for the cache security group. This value is stored as a
      -- lowercase string. Constraints: Must contain no more than 255
      -- alphanumeric characters. Must not be the word "Default". Example:
      -- mysecuritygroup.
    , _ccsgmDescription :: Text
      -- ^ A description for the cache security group.
    } deriving (Show, Generic)

-- | A name for the cache security group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric characters.
-- Must not be the word "Default". Example: mysecuritygroup.
ccsgmCacheSecurityGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateCacheSecurityGroup
    -> f CreateCacheSecurityGroup
ccsgmCacheSecurityGroupName f x =
    (\y -> x { _ccsgmCacheSecurityGroupName = y })
       <$> f (_ccsgmCacheSecurityGroupName x)
{-# INLINE ccsgmCacheSecurityGroupName #-}

-- | A description for the cache security group.
ccsgmDescription
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateCacheSecurityGroup
    -> f CreateCacheSecurityGroup
ccsgmDescription f x =
    (\y -> x { _ccsgmDescription = y })
       <$> f (_ccsgmDescription x)
{-# INLINE ccsgmDescription #-}

instance ToQuery CreateCacheSecurityGroup where
    toQuery = genericQuery def

data CreateCacheSecurityGroupResponse = CreateCacheSecurityGroupResponse
    { _csgxCacheSecurityGroup :: Maybe CacheSecurityGroup
      -- ^ Represents the output of one of the following operations:
      -- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
      -- RevokeCacheSecurityGroupIngress.
    } deriving (Show, Generic)

-- | Represents the output of one of the following operations:
-- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
-- RevokeCacheSecurityGroupIngress.
csgxCacheSecurityGroup
    :: Functor f
    => (Maybe CacheSecurityGroup
    -> f (Maybe CacheSecurityGroup))
    -> CreateCacheSecurityGroupResponse
    -> f CreateCacheSecurityGroupResponse
csgxCacheSecurityGroup f x =
    (\y -> x { _csgxCacheSecurityGroup = y })
       <$> f (_csgxCacheSecurityGroup x)
{-# INLINE csgxCacheSecurityGroup #-}

instance FromXML CreateCacheSecurityGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCacheSecurityGroup where
    type Sv CreateCacheSecurityGroup = ElastiCache
    type Rs CreateCacheSecurityGroup = CreateCacheSecurityGroupResponse

    request = post "CreateCacheSecurityGroup"
    response _ = xmlResponse
