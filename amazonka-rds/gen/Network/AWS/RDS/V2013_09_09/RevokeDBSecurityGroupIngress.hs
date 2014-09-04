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
    , mkRevokeDBSecurityGroupIngressMessage
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeDBSecurityGroupIngress' request.
mkRevokeDBSecurityGroupIngressMessage :: Text -- ^ 'rdbsgimDBSecurityGroupName'
                                      -> RevokeDBSecurityGroupIngress
mkRevokeDBSecurityGroupIngressMessage p1 = RevokeDBSecurityGroupIngress
    { _rdbsgimDBSecurityGroupName = p1
    , _rdbsgimCIDRIP = Nothing
    , _rdbsgimEC2SecurityGroupName = Nothing
    , _rdbsgimEC2SecurityGroupId = Nothing
    , _rdbsgimEC2SecurityGroupOwnerId = Nothing
    }
{-# INLINE mkRevokeDBSecurityGroupIngressMessage #-}

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
rdbsgimDBSecurityGroupName = lens _rdbsgimDBSecurityGroupName (\s a -> s { _rdbsgimDBSecurityGroupName = a })
{-# INLINE rdbsgimDBSecurityGroupName #-}

-- | The IP range to revoke access from. Must be a valid CIDR range. If CIDRIP
-- is specified, EC2SecurityGroupName, EC2SecurityGroupId and
-- EC2SecurityGroupOwnerId cannot be provided.
rdbsgimCIDRIP :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgimCIDRIP = lens _rdbsgimCIDRIP (\s a -> s { _rdbsgimCIDRIP = a })
{-# INLINE rdbsgimCIDRIP #-}

-- | The name of the EC2 security group to revoke access from. For VPC DB
-- security groups, EC2SecurityGroupId must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
rdbsgimEC2SecurityGroupName :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgimEC2SecurityGroupName = lens _rdbsgimEC2SecurityGroupName (\s a -> s { _rdbsgimEC2SecurityGroupName = a })
{-# INLINE rdbsgimEC2SecurityGroupName #-}

-- | The id of the EC2 security group to revoke access from. For VPC DB security
-- groups, EC2SecurityGroupId must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
rdbsgimEC2SecurityGroupId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgimEC2SecurityGroupId = lens _rdbsgimEC2SecurityGroupId (\s a -> s { _rdbsgimEC2SecurityGroupId = a })
{-# INLINE rdbsgimEC2SecurityGroupId #-}

-- | The AWS Account Number of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
-- acceptable value. For VPC DB security groups, EC2SecurityGroupId must be
-- provided. Otherwise, EC2SecurityGroupOwnerId and either
-- EC2SecurityGroupName or EC2SecurityGroupId must be provided.
rdbsgimEC2SecurityGroupOwnerId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgimEC2SecurityGroupOwnerId = lens _rdbsgimEC2SecurityGroupOwnerId (\s a -> s { _rdbsgimEC2SecurityGroupOwnerId = a })
{-# INLINE rdbsgimEC2SecurityGroupOwnerId #-}

instance ToQuery RevokeDBSecurityGroupIngress where
    toQuery = genericQuery def

newtype RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse
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
dbsgdtDBSecurityGroup = lens _dbsgdtDBSecurityGroup (\s a -> s { _dbsgdtDBSecurityGroup = a })
{-# INLINE dbsgdtDBSecurityGroup #-}

instance FromXML RevokeDBSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeDBSecurityGroupIngress where
    type Sv RevokeDBSecurityGroupIngress = RDS
    type Rs RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngressResponse

    request = post "RevokeDBSecurityGroupIngress"
    response _ = xmlResponse
