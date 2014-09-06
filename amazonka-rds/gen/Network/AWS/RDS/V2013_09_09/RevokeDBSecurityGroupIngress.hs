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
    , mkRevokeDBSecurityGroupIngress
    -- ** Request lenses
    , rdbsgiDBSecurityGroupName
    , rdbsgiCIDRIP
    , rdbsgiEC2SecurityGroupName
    , rdbsgiEC2SecurityGroupId
    , rdbsgiEC2SecurityGroupOwnerId

    -- * Response
    , RevokeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , rdbsgirsDBSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress
    { _rdbsgiDBSecurityGroupName :: Text
    , _rdbsgiCIDRIP :: Maybe Text
    , _rdbsgiEC2SecurityGroupName :: Maybe Text
    , _rdbsgiEC2SecurityGroupId :: Maybe Text
    , _rdbsgiEC2SecurityGroupOwnerId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeDBSecurityGroupIngress' request.
mkRevokeDBSecurityGroupIngress :: Text -- ^ 'rdbsgiDBSecurityGroupName'
                               -> RevokeDBSecurityGroupIngress
mkRevokeDBSecurityGroupIngress p1 = RevokeDBSecurityGroupIngress
    { _rdbsgiDBSecurityGroupName = p1
    , _rdbsgiCIDRIP = Nothing
    , _rdbsgiEC2SecurityGroupName = Nothing
    , _rdbsgiEC2SecurityGroupId = Nothing
    , _rdbsgiEC2SecurityGroupOwnerId = Nothing
    }
{-# INLINE mkRevokeDBSecurityGroupIngress #-}

-- | The name of the DB security group to revoke ingress from.
rdbsgiDBSecurityGroupName :: Lens' RevokeDBSecurityGroupIngress Text
rdbsgiDBSecurityGroupName =
    lens _rdbsgiDBSecurityGroupName
         (\s a -> s { _rdbsgiDBSecurityGroupName = a })
{-# INLINE rdbsgiDBSecurityGroupName #-}

-- | The IP range to revoke access from. Must be a valid CIDR range. If CIDRIP
-- is specified, EC2SecurityGroupName, EC2SecurityGroupId and
-- EC2SecurityGroupOwnerId cannot be provided.
rdbsgiCIDRIP :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgiCIDRIP = lens _rdbsgiCIDRIP (\s a -> s { _rdbsgiCIDRIP = a })
{-# INLINE rdbsgiCIDRIP #-}

-- | The name of the EC2 security group to revoke access from. For VPC DB
-- security groups, EC2SecurityGroupId must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
rdbsgiEC2SecurityGroupName :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgiEC2SecurityGroupName =
    lens _rdbsgiEC2SecurityGroupName
         (\s a -> s { _rdbsgiEC2SecurityGroupName = a })
{-# INLINE rdbsgiEC2SecurityGroupName #-}

-- | The id of the EC2 security group to revoke access from. For VPC DB security
-- groups, EC2SecurityGroupId must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId must be provided.
rdbsgiEC2SecurityGroupId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgiEC2SecurityGroupId =
    lens _rdbsgiEC2SecurityGroupId
         (\s a -> s { _rdbsgiEC2SecurityGroupId = a })
{-# INLINE rdbsgiEC2SecurityGroupId #-}

-- | The AWS Account Number of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
-- acceptable value. For VPC DB security groups, EC2SecurityGroupId must be
-- provided. Otherwise, EC2SecurityGroupOwnerId and either
-- EC2SecurityGroupName or EC2SecurityGroupId must be provided.
rdbsgiEC2SecurityGroupOwnerId :: Lens' RevokeDBSecurityGroupIngress (Maybe Text)
rdbsgiEC2SecurityGroupOwnerId =
    lens _rdbsgiEC2SecurityGroupOwnerId
         (\s a -> s { _rdbsgiEC2SecurityGroupOwnerId = a })
{-# INLINE rdbsgiEC2SecurityGroupOwnerId #-}

instance ToQuery RevokeDBSecurityGroupIngress where
    toQuery = genericQuery def

newtype RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse
    { _rdbsgirsDBSecurityGroup :: Maybe DBSecurityGroup
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
-- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
-- as a response element in the DescribeDBSecurityGroups action.
rdbsgirsDBSecurityGroup :: Lens' RevokeDBSecurityGroupIngressResponse (Maybe DBSecurityGroup)
rdbsgirsDBSecurityGroup =
    lens _rdbsgirsDBSecurityGroup
         (\s a -> s { _rdbsgirsDBSecurityGroup = a })
{-# INLINE rdbsgirsDBSecurityGroup #-}

instance FromXML RevokeDBSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeDBSecurityGroupIngress where
    type Sv RevokeDBSecurityGroupIngress = RDS
    type Rs RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngressResponse

    request = post "RevokeDBSecurityGroupIngress"
    response _ = xmlResponse
