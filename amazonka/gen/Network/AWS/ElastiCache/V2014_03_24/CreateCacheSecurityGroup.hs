{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.CreateCacheSecurityGroup
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
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-27T02%3A43%3A10.703Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE mycachesecuritygroup 123456789012 My cache
-- security group 2b1c8035-b7fa-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_03_24.CreateCacheSecurityGroup where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElastiCache.V2014_03_24.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CreateCacheSecurityGroup = CreateCacheSecurityGroup
    { _ccsgmCacheSecurityGroupName :: Text
      -- ^ A name for the cache security group. This value is stored as a
      -- lowercase string. Constraints: Must contain no more than 255
      -- alphanumeric characters. Must not be the word "Default". Example:
      -- mysecuritygroup.
    , _ccsgmDescription :: Text
      -- ^ A description for the cache security group.
    } deriving (Generic)

instance ToQuery CreateCacheSecurityGroup where
    toQuery = genericToQuery def

instance AWSRequest CreateCacheSecurityGroup where
    type Sv CreateCacheSecurityGroup = ElastiCache
    type Rs CreateCacheSecurityGroup = CreateCacheSecurityGroupResponse

    request = post "CreateCacheSecurityGroup"
    response _ = xmlResponse

data CreateCacheSecurityGroupResponse = CreateCacheSecurityGroupResponse
    { _csgxCacheSecurityGroup :: Maybe CacheSecurityGroup
      -- ^ Represents the output of one of the following operations:
      -- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
      -- RevokeCacheSecurityGroupIngress.
    } deriving (Generic)

instance FromXML CreateCacheSecurityGroupResponse where
    fromXMLOptions = xmlOptions
