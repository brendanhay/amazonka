{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.RDS.V2013_09_09.RevokeDBSecurityGroupIngress where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RevokeDBSecurityGroupIngress' request.
revokeDBSecurityGroupIngress :: Text -- ^ '_rdbsgimDBSecurityGroupName'
                             -> RevokeDBSecurityGroupIngress
revokeDBSecurityGroupIngress p1 = RevokeDBSecurityGroupIngress
    { _rdbsgimDBSecurityGroupName = p1
    , _rdbsgimCIDRIP = Nothing
    , _rdbsgimEC2SecurityGroupId = Nothing
    , _rdbsgimEC2SecurityGroupName = Nothing
    , _rdbsgimEC2SecurityGroupOwnerId = Nothing
    }

data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress
    { _rdbsgimDBSecurityGroupName :: Text
      -- ^ The name of the DB security group to revoke ingress from.
    , _rdbsgimCIDRIP :: Maybe Text
      -- ^ The IP range to revoke access from. Must be a valid CIDR range.
      -- If CIDRIP is specified, EC2SecurityGroupName, EC2SecurityGroupId
      -- and EC2SecurityGroupOwnerId cannot be provided.
    , _rdbsgimEC2SecurityGroupId :: Maybe Text
      -- ^ The id of the EC2 security group to revoke access from. For VPC
      -- DB security groups, EC2SecurityGroupId must be provided.
      -- Otherwise, EC2SecurityGroupOwnerId and either
      -- EC2SecurityGroupName or EC2SecurityGroupId must be provided.
    , _rdbsgimEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the EC2 security group to revoke access from. For VPC
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

makeLenses ''RevokeDBSecurityGroupIngress

instance ToQuery RevokeDBSecurityGroupIngress where
    toQuery = genericQuery def

data RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse
    { _dbsgyDBSecurityGroup :: Maybe DBSecurityGroup
      -- ^ Contains the result of a successful invocation of the following
      -- actions: DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
      -- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type
      -- is used as a response element in the DescribeDBSecurityGroups
      -- action.
    } deriving (Show, Generic)

makeLenses ''RevokeDBSecurityGroupIngressResponse

instance FromXML RevokeDBSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeDBSecurityGroupIngress where
    type Sv RevokeDBSecurityGroupIngress = RDS
    type Rs RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngressResponse

    request = post "RevokeDBSecurityGroupIngress"
    response _ = xmlResponse
