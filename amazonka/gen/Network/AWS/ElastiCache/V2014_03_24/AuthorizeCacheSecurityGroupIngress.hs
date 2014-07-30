{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.AuthorizeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AuthorizeCacheSecurityGroupIngress operation allows network ingress to
-- a cache security group. Applications using ElastiCache must be running on
-- Amazon EC2, and Amazon EC2 security groups are used as the authorization
-- mechanism. You cannot authorize ingress from an Amazon EC2 security group
-- in one Region to an ElastiCache cluster in another Region.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=AuthorizeCacheSecurityGroupIngress &EC2SecurityGroupName=default
-- &CacheSecurityGroupName=mygroup &EC2SecurityGroupOwnerId=1234-5678-1234
-- &Version=2014-03-24 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-12T01%3A29%3A15.746Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE authorizing default 565419523791 mygroup
-- 123456781234 My security group 817fa999-3647-11e0-ae57-f96cfe56749c.
module Network.AWS.ElastiCache.V2014_03_24.AuthorizeCacheSecurityGroupIngress where

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

data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress
    { _acsgimCacheSecurityGroupName :: Text
      -- ^ The cache security group which will allow network ingress.
    , _acsgimEC2SecurityGroupOwnerId :: Text
      -- ^ The AWS account number of the Amazon EC2 security group owner.
      -- Note that this is not the same thing as an AWS access key ID -
      -- you must provide a valid AWS account number for this parameter.
    , _acsgimEC2SecurityGroupName :: Text
      -- ^ The Amazon EC2 security group to be authorized for ingress to the
      -- cache security group.
    } deriving (Generic)

instance ToQuery AuthorizeCacheSecurityGroupIngress where
    toQuery = genericToQuery def

instance AWSRequest AuthorizeCacheSecurityGroupIngress where
    type Sv AuthorizeCacheSecurityGroupIngress = ElastiCache
    type Rs AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngressResponse

    request = post "AuthorizeCacheSecurityGroupIngress"
    response _ = xmlResponse

data AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse
    { _csgyCacheSecurityGroup :: Maybe CacheSecurityGroup
      -- ^ Represents the output of one of the following operations:
      -- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
      -- RevokeCacheSecurityGroupIngress.
    } deriving (Generic)

instance FromXML AuthorizeCacheSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions
