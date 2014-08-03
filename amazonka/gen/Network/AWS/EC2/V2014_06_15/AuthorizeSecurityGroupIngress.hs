{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AuthorizeSecurityGroupIngress
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
module Network.AWS.EC2.V2014_06_15.AuthorizeSecurityGroupIngress where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AuthorizeSecurityGroupIngress' request.
authorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress
authorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { _asgirDryRun = Nothing
    , _asgirFromPort = Nothing
    , _asgirToPort = Nothing
    , _asgirIpPermissions = mempty
    , _asgirIpProtocol = Nothing
    , _asgirGroupId = Nothing
    , _asgirCidrIp = Nothing
    , _asgirSourceSecurityGroupOwnerId = Nothing
    , _asgirGroupName = Nothing
    , _asgirSourceSecurityGroupName = Nothing
    }

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { _asgirDryRun :: Maybe Bool
      -- ^ 
    , _asgirFromPort :: Maybe Integer
      -- ^ The start of port range for the TCP and UDP protocols, or an ICMP
      -- type number. For the ICMP type number, use -1 to specify all ICMP
      -- types.
    , _asgirToPort :: Maybe Integer
      -- ^ The end of port range for the TCP and UDP protocols, or an ICMP
      -- code number. For the ICMP code number, use -1 to specify all ICMP
      -- codes for the ICMP type.
    , _asgirIpPermissions :: [IpPermission]
      -- ^ 
    , _asgirIpProtocol :: Maybe Text
      -- ^ The IP protocol name (tcp, udp, icmp) or number (see Protocol
      -- Numbers). Use -1 to specify all.
    , _asgirGroupId :: Maybe Text
      -- ^ The ID of the security group.
    , _asgirCidrIp :: Maybe Text
      -- ^ The CIDR IP address range. You can't specify this parameter when
      -- specifying a source security group.
    , _asgirSourceSecurityGroupOwnerId :: Maybe Text
      -- ^ The ID of the source security group. You can't specify a source
      -- security group and a CIDR IP address range.
    , _asgirGroupName :: Maybe Text
      -- ^ [EC2-Classic, default VPC] The name of the security group.
    , _asgirSourceSecurityGroupName :: Maybe Text
      -- ^ [EC2-Classic, default VPC] The name of the source security group.
      -- You can't specify a source security group and a CIDR IP address
      -- range.
    } deriving (Generic)

makeLenses ''AuthorizeSecurityGroupIngress

instance ToQuery AuthorizeSecurityGroupIngress where
    toQuery = genericToQuery def

data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
    deriving (Eq, Show, Generic)

makeLenses ''AuthorizeSecurityGroupIngressResponse

instance AWSRequest AuthorizeSecurityGroupIngress where
    type Sv AuthorizeSecurityGroupIngress = EC2
    type Rs AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngressResponse

    request = post "AuthorizeSecurityGroupIngress"
    response _ _ = return (Right AuthorizeSecurityGroupIngressResponse)
