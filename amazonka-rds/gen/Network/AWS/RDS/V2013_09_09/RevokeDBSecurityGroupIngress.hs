{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.RevokeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Revokes ingress from a DBSecurityGroup for previously authorized IP ranges
-- or EC2 or VPC Security Groups. Required parameters for this API are one of
-- CIDRIP, EC2SecurityGroupId for VPC, or (EC2SecurityGroupOwnerId and either
-- EC2SecurityGroupName or EC2SecurityGroupId). https://rds.amazonaws.com/
-- ?Action=RevokeDBSecurityGroupIngress &DBSecurityGroupName=mydbsecuritygroup
-- &CIDRIP=192.168.1.1%2F24 &Version=2013-05-15
-- &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T22%3A32%3A12.515Z &AWSAccessKeyId= &Signature= My new
-- DBSecurityGroup 192.168.1.1/24 revoking 621567473609 mydbsecuritygroup
-- vpc-1ab2c3d4 beecb8ac-bf5a-11de-9f9f-53d6aee22de9.
module Network.AWS.RDS.V2013_09_09.RevokeDBSecurityGroupIngress
    (
    -- * Request
      RevokeDBSecurityGroupIngress
    -- ** Request constructor
    , revokeDBSecurityGroupIngress
    -- ** Request lenses
    , rdbsgimDBSecurityGroupName
    , rdbsgimCIDRIP
    , rdbsgimEC2SecurityGroupName
    , rdbsgimEC2SecurityGroupId
    , rdbsgimEC2SecurityGroupOwnerId

    -- * Response
    , RevokeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , dbsgdtDBSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RevokeDBSecurityGroupIngress' request.
revokeDBSecurityGroupIngress :: Text -- ^ 'rdbsgimDBSecurityGroupName'
                             -> RevokeDBSecurityGroupIngress
revokeDBSecurityGroupIngress p1 = RevokeDBSecurityGroupIngress
    { _rdbsgimDBSecurityGroupName = p1
    , _rdbsgimCIDRIP = Nothing
    , _rdbsgimEC2SecurityGroupName = Nothing
    , _rdbsgimEC2SecurityGroupId = Nothing
    , _rdbsgimEC2SecurityGroupOwnerId = Nothing
    }
{-# INLINE revokeDBSecurityGroupIngress #-}

data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress
    { _rdbsgimDBSecurityGroupName :: Text
      -- ^ The name of the DB security group to revoke ingress from.
    , _rdbsgimCIDRIP :: Maybe Text
      -- ^ The IP range to revoke access from. Must be a valid CIDR range.
      -- If CIDRIP is specified, EC2SecurityGroupName, EC2SecurityGroupId
      -- and EC2SecurityGroupOwnerId cannot be provided.
    , _rdbsgimEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the EC2 security group to revoke access from. For VPC
      -- DB security groups, EC2SecurityGroupId must be provided.
      -- Otherwise, EC2SecurityGroupOwnerId and either
      -- EC2SecurityGroupName or EC2SecurityGroupId must be provided.
    , _rdbsgimEC2SecurityGroupId :: Maybe Text
      -- ^ The id of the EC2 security group to revoke access from. For VPC
      -- DB security groups, EC2SecurityGroupId must be provided.
      -- Otherwise, EC2SecurityGroupOwnerId and either
      -- EC2SecurityGroupName or EC2SecurityGroupId must be provided.
    , _rdbsgimEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS Account Number of the owner of the EC2 security group
      -- specified in the EC2SecurityGroupName parameter. The AWS Access
      -- Key ID is not an acceptable value. For VPC DB security groups,
      -- EC2SecurityGroupId must be provided. Otherwise,
      -- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
      -- EC2SecurityGroupId must be provided.
    } deriving (Show, Generic)

-- | The name of the DB security group to revoke ingress from.
rdbsgimDBSecurityGroupName :: Lens' RevokeDBSecurityGroupIngress (Text)
rdbsgimDBSecurityGroupName f x =
    f (_rdbsgimDBSecurityGroupName x)
        <&> \y -> x { _rdbsgimDBSecurityGroupName = y }
{-# INLINE rdbsgimDBSecurityGroupName #-}

-- | The IP range to revoke access from. Must be a valid CIDR range. If CIDRIP
-- is specified, EC2SecurityGroupName, EC2SecurityGroupId and
-- EC2SecurityGroupOwnerId cannot be provided.
rdbsgimCIDRIP :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgimCIDRIP f x =
    f (_rdbsgimCIDRIP x)
        <&> \y -> x { _rdbsgimCIDRIP = y }
{-# INLINE rdbsgimCIDRIP #-}

-- | The name of the EC2 security group to revoke access from. For VPC DB
-- security groups, EC2SecurityGroupId must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
rdbsgimEC2SecurityGroupName :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgimEC2SecurityGroupName f x =
    f (_rdbsgimEC2SecurityGroupName x)
        <&> \y -> x { _rdbsgimEC2SecurityGroupName = y }
{-# INLINE rdbsgimEC2SecurityGroupName #-}

-- | The id of the EC2 security group to revoke access from. For VPC DB security
-- groups, EC2SecurityGroupId must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
rdbsgimEC2SecurityGroupId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgimEC2SecurityGroupId f x =
    f (_rdbsgimEC2SecurityGroupId x)
        <&> \y -> x { _rdbsgimEC2SecurityGroupId = y }
{-# INLINE rdbsgimEC2SecurityGroupId #-}

-- | The AWS Account Number of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
-- acceptable value. For VPC DB security groups, EC2SecurityGroupId must be
-- provided. Otherwise, EC2SecurityGroupOwnerId and either
-- EC2SecurityGroupName or EC2SecurityGroupId must be provided.
rdbsgimEC2SecurityGroupOwnerId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgimEC2SecurityGroupOwnerId f x =
    f (_rdbsgimEC2SecurityGroupOwnerId x)
        <&> \y -> x { _rdbsgimEC2SecurityGroupOwnerId = y }
{-# INLINE rdbsgimEC2SecurityGroupOwnerId #-}

instance ToQuery RevokeDBSecurityGroupIngress where
    toQuery = genericQuery def

data RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse
    { _dbsgdtDBSecurityGroup :: Maybe DBSecurityGroup
      -- ^ Contains the result of a successful invocation of the following
      -- actions: DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
      -- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type
      -- is used as a response element in the DescribeDBSecurityGroups
      -- action.
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
-- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
-- as a response element in the DescribeDBSecurityGroups action.
dbsgdtDBSecurityGroup :: Lens' RevokeDBSecurityGroupIngressResponse (Maybe DBSecurityGroup)
dbsgdtDBSecurityGroup f x =
    f (_dbsgdtDBSecurityGroup x)
        <&> \y -> x { _dbsgdtDBSecurityGroup = y }
{-# INLINE dbsgdtDBSecurityGroup #-}

instance FromXML RevokeDBSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeDBSecurityGroupIngress where
    type Sv RevokeDBSecurityGroupIngress = RDS
    type Rs RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngressResponse

    request = post "RevokeDBSecurityGroupIngress"
    response _ = xmlResponse
