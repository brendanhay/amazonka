{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.AuthorizeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables ingress to a DBSecurityGroup using one of two forms of
-- authorization. First, EC2 or VPC security groups can be added to the
-- DBSecurityGroup if the application using the database is running on EC2 or
-- VPC instances. Second, IP ranges are available if the application accessing
-- your database is running on the Internet. Required parameters for this API
-- are one of CIDR range, EC2SecurityGroupId for VPC, or
-- (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId for non-VPC). You cannot authorize ingress from an EC2
-- security group in one Region to an Amazon RDS DB instance in another. You
-- cannot authorize ingress from a VPC security group in one VPC to an Amazon
-- RDS DB instance in another. For an overview of CIDR ranges, go to the
-- Wikipedia Tutorial. https://rds.amazonaws.com/ ?CIDRIP=192.168.1.1%2F24
-- &DBSecurityGroupName=mydbsecuritygroup &Version=2013-05-15
-- &Action=AuthorizeDBSecurityGroupIngress &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T17%3A10%3A50.274Z
-- &AWSAccessKeyId= &Signature= My new DBSecurityGroup 192.168.1.1/24
-- authorizing 621567473609 mydbsecuritygroup vpc-1ab2c3d4
-- d9799197-bf2d-11de-b88d-993294bf1c81.
module Network.AWS.RDS.V2013_09_09.AuthorizeDBSecurityGroupIngress
    (
    -- * Request
      AuthorizeDBSecurityGroupIngress
    -- ** Request constructor
    , authorizeDBSecurityGroupIngress
    -- ** Request lenses
    , adbsgimDBSecurityGroupName
    , adbsgimCIDRIP
    , adbsgimEC2SecurityGroupName
    , adbsgimEC2SecurityGroupId
    , adbsgimEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , dbsgwDBSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AuthorizeDBSecurityGroupIngress' request.
authorizeDBSecurityGroupIngress :: Text -- ^ 'adbsgimDBSecurityGroupName'
                                -> AuthorizeDBSecurityGroupIngress
authorizeDBSecurityGroupIngress p1 = AuthorizeDBSecurityGroupIngress
    { _adbsgimDBSecurityGroupName = p1
    , _adbsgimCIDRIP = Nothing
    , _adbsgimEC2SecurityGroupName = Nothing
    , _adbsgimEC2SecurityGroupId = Nothing
    , _adbsgimEC2SecurityGroupOwnerId = Nothing
    }
{-# INLINE authorizeDBSecurityGroupIngress #-}

data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress
    { _adbsgimDBSecurityGroupName :: Text
      -- ^ The name of the DB security group to add authorization to.
    , _adbsgimCIDRIP :: Maybe Text
      -- ^ The IP range to authorize.
    , _adbsgimEC2SecurityGroupName :: Maybe Text
      -- ^ Name of the EC2 security group to authorize. For VPC DB security
      -- groups, EC2SecurityGroupId must be provided. Otherwise,
      -- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
      -- EC2SecurityGroupId must be provided.
    , _adbsgimEC2SecurityGroupId :: Maybe Text
      -- ^ Id of the EC2 security group to authorize. For VPC DB security
      -- groups, EC2SecurityGroupId must be provided. Otherwise,
      -- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
      -- EC2SecurityGroupId must be provided.
    , _adbsgimEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ AWS Account Number of the owner of the EC2 security group
      -- specified in the EC2SecurityGroupName parameter. The AWS Access
      -- Key ID is not an acceptable value. For VPC DB security groups,
      -- EC2SecurityGroupId must be provided. Otherwise,
      -- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
      -- EC2SecurityGroupId must be provided.
    } deriving (Show, Generic)

-- | The name of the DB security group to add authorization to.
adbsgimDBSecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress (Text)
adbsgimDBSecurityGroupName f x =
    f (_adbsgimDBSecurityGroupName x)
        <&> \y -> x { _adbsgimDBSecurityGroupName = y }
{-# INLINE adbsgimDBSecurityGroupName #-}

-- | The IP range to authorize.
adbsgimCIDRIP :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adbsgimCIDRIP f x =
    f (_adbsgimCIDRIP x)
        <&> \y -> x { _adbsgimCIDRIP = y }
{-# INLINE adbsgimCIDRIP #-}

-- | Name of the EC2 security group to authorize. For VPC DB security groups,
-- EC2SecurityGroupId must be provided. Otherwise, EC2SecurityGroupOwnerId and
-- either EC2SecurityGroupName or EC2SecurityGroupId must be provided.
adbsgimEC2SecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adbsgimEC2SecurityGroupName f x =
    f (_adbsgimEC2SecurityGroupName x)
        <&> \y -> x { _adbsgimEC2SecurityGroupName = y }
{-# INLINE adbsgimEC2SecurityGroupName #-}

-- | Id of the EC2 security group to authorize. For VPC DB security groups,
-- EC2SecurityGroupId must be provided. Otherwise, EC2SecurityGroupOwnerId and
-- either EC2SecurityGroupName or EC2SecurityGroupId must be provided.
adbsgimEC2SecurityGroupId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adbsgimEC2SecurityGroupId f x =
    f (_adbsgimEC2SecurityGroupId x)
        <&> \y -> x { _adbsgimEC2SecurityGroupId = y }
{-# INLINE adbsgimEC2SecurityGroupId #-}

-- | AWS Account Number of the owner of the EC2 security group specified in the
-- EC2SecurityGroupName parameter. The AWS Access Key ID is not an acceptable
-- value. For VPC DB security groups, EC2SecurityGroupId must be provided.
-- Otherwise, EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
adbsgimEC2SecurityGroupOwnerId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adbsgimEC2SecurityGroupOwnerId f x =
    f (_adbsgimEC2SecurityGroupOwnerId x)
        <&> \y -> x { _adbsgimEC2SecurityGroupOwnerId = y }
{-# INLINE adbsgimEC2SecurityGroupOwnerId #-}

instance ToQuery AuthorizeDBSecurityGroupIngress where
    toQuery = genericQuery def

data AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse
    { _dbsgwDBSecurityGroup :: Maybe DBSecurityGroup
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
dbsgwDBSecurityGroup :: Lens' AuthorizeDBSecurityGroupIngressResponse (Maybe DBSecurityGroup)
dbsgwDBSecurityGroup f x =
    f (_dbsgwDBSecurityGroup x)
        <&> \y -> x { _dbsgwDBSecurityGroup = y }
{-# INLINE dbsgwDBSecurityGroup #-}

instance FromXML AuthorizeDBSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AuthorizeDBSecurityGroupIngress where
    type Sv AuthorizeDBSecurityGroupIngress = RDS
    type Rs AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngressResponse

    request = post "AuthorizeDBSecurityGroupIngress"
    response _ = xmlResponse
