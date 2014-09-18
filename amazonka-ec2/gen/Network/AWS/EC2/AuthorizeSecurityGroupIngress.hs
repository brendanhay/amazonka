{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds one or more ingress rules to a security group. EC2-Classic: You can
-- have up to 100 rules per group. EC2-VPC: You can have up to 50 rules per
-- group (covering both ingress and egress rules). Rule changes are propagated
-- to instances within the security group as quickly as possible. However, a
-- small delay might occur. [EC2-Classic] This action gives one or more CIDR
-- IP address ranges permission to access a security group in your account, or
-- gives one or more security groups (called the source groups) permission to
-- access a security group for your account. A source group can be for your
-- own AWS account, or another. [EC2-VPC] This action gives one or more CIDR
-- IP address ranges permission to access a security group in your VPC, or
-- gives one or more other security groups (called the source groups)
-- permission to access a security group for your VPC. The security groups
-- must all be for the same VPC. Example 1 This example request grants TCP
-- port 80 access from the 192.0.2.0/24 and 198.51.100.0/24 address ranges to
-- the security group for EC2-Classic named websrv.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupIngress
-- &amp;GroupName=websrv &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=192.0.2.0/24
-- &amp;IpPermissions.1.IpRanges.2.CidrIp=198.51.100.0/24 &amp;AUTHPARAMS
-- Example 2 This example request grants TCP port 80 access from the source
-- group for EC2-Classic named OtherAccountGroup (in AWS account 123456789012)
-- to the security group for EC2-Classic named websrv.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupIngress
-- &amp;GroupName=websrv &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.Groups.1.GroupName=OtherAccountGroup
-- &amp;IpPermissions.1.Groups.1.UserId=123456789012 &amp;AUTHPARAMS Example 3
-- This example request grants TCP port 80 access from the source group named
-- OtherGroupInMyVPC (with the ID sg-2a2b3c4d) to the security group named
-- VpcWebServers (with the ID sg-1a2b3c4d). In EC2-VPC, you must use the
-- security group IDs in a request, not the security group names. In this
-- example, your AWS account ID is 123456789012.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupIngress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.Groups.1.GroupId=sg-2a2b3c4d
-- &amp;IpPermissions.1.Groups.1.UserId=123456789012 &amp;AUTHPARAMS Example 4
-- This example request grants your local system the ability to use SSH (port
-- 22) to connect to any instance in the security group named default.
-- https://ec2.amazonaws.com/ ?Action=AuthorizeSecurityGroupIngress
-- &amp;GroupName=default &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=22 &amp;IpPermissions.1.ToPort=22
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=your-local-system's-public-ip-address/32
-- &amp;AUTHPARAMS Example 5 This example request grants your local system the
-- ability to use Remote Desktop (port 3389) to connect to any instance in the
-- security group named default. https://ec2.amazonaws.com/
-- ?Action=AuthorizeSecurityGroupIngress &amp;GroupName=default
-- &amp;IpPermissions.1.IpProtocol=tcp &amp;IpPermissions.1.FromPort=3389
-- &amp;IpPermissions.1.ToPort=3389
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=your-local-system's-public-ip-address/32.
-- 
module Network.AWS.EC2.AuthorizeSecurityGroupIngress
    (
    -- * Request
      AuthorizeSecurityGroupIngress
    -- ** Request constructor
    , authorizeSecurityGroupIngress
    -- ** Request lenses
    , asgiGroupName
    , asgiGroupId
    , asgiSourceSecurityGroupName
    , asgiSourceSecurityGroupOwnerId
    , asgiIpProtocol
    , asgiFromPort
    , asgiToPort
    , asgiCidrIp
    , asgiIpPermissions

    -- * Response
    , AuthorizeSecurityGroupIngressResponse
    -- ** Response constructor
    , authorizeSecurityGroupIngressResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { _asgiGroupName :: Maybe Text
    , _asgiGroupId :: Maybe Text
    , _asgiSourceSecurityGroupName :: Maybe Text
    , _asgiSourceSecurityGroupOwnerId :: Maybe Text
    , _asgiIpProtocol :: Maybe Text
    , _asgiFromPort :: Maybe Integer
    , _asgiToPort :: Maybe Integer
    , _asgiCidrIp :: Maybe Text
    , _asgiIpPermissions :: [IpPermission]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeSecurityGroupIngress' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Maybe Text@
--
-- * @GroupId ::@ @Maybe Text@
--
-- * @SourceSecurityGroupName ::@ @Maybe Text@
--
-- * @SourceSecurityGroupOwnerId ::@ @Maybe Text@
--
-- * @IpProtocol ::@ @Maybe Text@
--
-- * @FromPort ::@ @Maybe Integer@
--
-- * @ToPort ::@ @Maybe Integer@
--
-- * @CidrIp ::@ @Maybe Text@
--
-- * @IpPermissions ::@ @[IpPermission]@
--
authorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress
authorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { _asgiGroupName = Nothing
    , _asgiGroupId = Nothing
    , _asgiSourceSecurityGroupName = Nothing
    , _asgiSourceSecurityGroupOwnerId = Nothing
    , _asgiIpProtocol = Nothing
    , _asgiFromPort = Nothing
    , _asgiToPort = Nothing
    , _asgiCidrIp = Nothing
    , _asgiIpPermissions = mempty
    }

-- | [EC2-Classic, default VPC] The name of the security group.
asgiGroupName :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiGroupName = lens _asgiGroupName (\s a -> s { _asgiGroupName = a })

-- | The ID of the security group.
asgiGroupId :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiGroupId = lens _asgiGroupId (\s a -> s { _asgiGroupId = a })

-- | [EC2-Classic, default VPC] The name of the source security group. You can't
-- specify a source security group and a CIDR IP address range.
asgiSourceSecurityGroupName :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiSourceSecurityGroupName =
    lens _asgiSourceSecurityGroupName
         (\s a -> s { _asgiSourceSecurityGroupName = a })

-- | The ID of the source security group. You can't specify a source security
-- group and a CIDR IP address range.
asgiSourceSecurityGroupOwnerId :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiSourceSecurityGroupOwnerId =
    lens _asgiSourceSecurityGroupOwnerId
         (\s a -> s { _asgiSourceSecurityGroupOwnerId = a })

-- | The IP protocol name (tcp, udp, icmp) or number (see Protocol Numbers). Use
-- -1 to specify all.
asgiIpProtocol :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiIpProtocol = lens _asgiIpProtocol (\s a -> s { _asgiIpProtocol = a })

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use -1 to specify all ICMP types.
asgiFromPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Integer)
asgiFromPort = lens _asgiFromPort (\s a -> s { _asgiFromPort = a })

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use -1 to specify all ICMP codes for the
-- ICMP type.
asgiToPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Integer)
asgiToPort = lens _asgiToPort (\s a -> s { _asgiToPort = a })

-- | The CIDR IP address range. You can't specify this parameter when specifying
-- a source security group.
asgiCidrIp :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiCidrIp = lens _asgiCidrIp (\s a -> s { _asgiCidrIp = a })

-- | 
asgiIpPermissions :: Lens' AuthorizeSecurityGroupIngress [IpPermission]
asgiIpPermissions =
    lens _asgiIpPermissions (\s a -> s { _asgiIpPermissions = a })

instance ToQuery AuthorizeSecurityGroupIngress where
    toQuery = genericQuery def

data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeSecurityGroupIngressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
authorizeSecurityGroupIngressResponse :: AuthorizeSecurityGroupIngressResponse
authorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse

instance AWSRequest AuthorizeSecurityGroupIngress where
    type Sv AuthorizeSecurityGroupIngress = EC2
    type Rs AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngressResponse

    request = post "AuthorizeSecurityGroupIngress"
    response _ = nullaryResponse AuthorizeSecurityGroupIngressResponse
