{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS
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
module Network.AWS.RDS
    (
    -- * Request
      AuthorizeDBSecurityGroupIngress
    -- ** Request constructor
    , mkAuthorizeDBSecurityGroupIngress
    -- ** Request lenses
    , adbsgiDBSecurityGroupName
    , adbsgiCIDRIP
    , adbsgiEC2SecurityGroupName
    , adbsgiEC2SecurityGroupId
    , adbsgiEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeDBSecurityGroupIngressResponse
    -- ** Response constructor
    , mkAuthorizeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , adbsgirDBSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress
    { _adbsgiDBSecurityGroupName :: Text
    , _adbsgiCIDRIP :: Maybe Text
    , _adbsgiEC2SecurityGroupName :: Maybe Text
    , _adbsgiEC2SecurityGroupId :: Maybe Text
    , _adbsgiEC2SecurityGroupOwnerId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeDBSecurityGroupIngress' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSecurityGroupName ::@ @Text@
--
-- * @CIDRIP ::@ @Maybe Text@
--
-- * @EC2SecurityGroupName ::@ @Maybe Text@
--
-- * @EC2SecurityGroupId ::@ @Maybe Text@
--
-- * @EC2SecurityGroupOwnerId ::@ @Maybe Text@
--
mkAuthorizeDBSecurityGroupIngress :: Text -- ^ 'adbsgiDBSecurityGroupName'
                                  -> AuthorizeDBSecurityGroupIngress
mkAuthorizeDBSecurityGroupIngress p1 = AuthorizeDBSecurityGroupIngress
    { _adbsgiDBSecurityGroupName = p1
    , _adbsgiCIDRIP = Nothing
    , _adbsgiEC2SecurityGroupName = Nothing
    , _adbsgiEC2SecurityGroupId = Nothing
    , _adbsgiEC2SecurityGroupOwnerId = Nothing
    }

-- | The name of the DB security group to add authorization to.
adbsgiDBSecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress Text
adbsgiDBSecurityGroupName =
    lens _adbsgiDBSecurityGroupName
         (\s a -> s { _adbsgiDBSecurityGroupName = a })

-- | The IP range to authorize.
adbsgiCIDRIP :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adbsgiCIDRIP = lens _adbsgiCIDRIP (\s a -> s { _adbsgiCIDRIP = a })

-- | Name of the EC2 security group to authorize. For VPC DB security groups,
-- EC2SecurityGroupId must be provided. Otherwise, EC2SecurityGroupOwnerId and
-- either EC2SecurityGroupName or EC2SecurityGroupId must be provided.
adbsgiEC2SecurityGroupName :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adbsgiEC2SecurityGroupName =
    lens _adbsgiEC2SecurityGroupName
         (\s a -> s { _adbsgiEC2SecurityGroupName = a })

-- | Id of the EC2 security group to authorize. For VPC DB security groups,
-- EC2SecurityGroupId must be provided. Otherwise, EC2SecurityGroupOwnerId and
-- either EC2SecurityGroupName or EC2SecurityGroupId must be provided.
adbsgiEC2SecurityGroupId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adbsgiEC2SecurityGroupId =
    lens _adbsgiEC2SecurityGroupId
         (\s a -> s { _adbsgiEC2SecurityGroupId = a })

-- | AWS Account Number of the owner of the EC2 security group specified in the
-- EC2SecurityGroupName parameter. The AWS Access Key ID is not an acceptable
-- value. For VPC DB security groups, EC2SecurityGroupId must be provided.
-- Otherwise, EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
adbsgiEC2SecurityGroupOwnerId :: Lens' AuthorizeDBSecurityGroupIngress (Maybe Text)
adbsgiEC2SecurityGroupOwnerId =
    lens _adbsgiEC2SecurityGroupOwnerId
         (\s a -> s { _adbsgiEC2SecurityGroupOwnerId = a })

instance ToQuery AuthorizeDBSecurityGroupIngress where
    toQuery = genericQuery def

newtype AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse
    { _adbsgirDBSecurityGroup :: Maybe DBSecurityGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeDBSecurityGroupIngressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSecurityGroup ::@ @Maybe DBSecurityGroup@
--
mkAuthorizeDBSecurityGroupIngressResponse :: AuthorizeDBSecurityGroupIngressResponse
mkAuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse
    { _adbsgirDBSecurityGroup = Nothing
    }

-- | Contains the result of a successful invocation of the following actions:
-- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
-- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
-- as a response element in the DescribeDBSecurityGroups action.
adbsgirDBSecurityGroup :: Lens' AuthorizeDBSecurityGroupIngressResponse (Maybe DBSecurityGroup)
adbsgirDBSecurityGroup =
    lens _adbsgirDBSecurityGroup (\s a -> s { _adbsgirDBSecurityGroup = a })

instance FromXML AuthorizeDBSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AuthorizeDBSecurityGroupIngress where
    type Sv AuthorizeDBSecurityGroupIngress = RDS
    type Rs AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngressResponse

    request = post "AuthorizeDBSecurityGroupIngress"
    response _ = xmlResponse
