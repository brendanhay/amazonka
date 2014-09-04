{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.RevokeSecurityGroupEgress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes one or more egress rules from a security group for EC2-VPC. The
-- values that you specify in the revoke request (for example, ports) must
-- match the existing rule's values for the rule to be revoked. Each rule
-- consists of the protocol and the CIDR range or source security group. For
-- the TCP and UDP protocols, you must also specify the destination port or
-- range of ports. For the ICMP protocol, you must also specify the ICMP type
-- and code. Rule changes are propagated to instances within the security
-- group as quickly as possible. However, a small delay might occur. Example 1
-- This example revokes the access that the specified security group has to
-- the 205.192.0.0/16 and 205.159.0.0/16 address ranges on TCP port 80.
-- https://ec2.amazonaws.com/?Action=RevokeSecurityGroupEgress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=205.192.0.0/16
-- &amp;IpPermissions.1.IpRanges.2.CidrIp=205.159.0.0/16 &amp;AUTHPARAMS
-- Example 2 This example revokes the access that the specified security group
-- has to the security group with the ID sg-9a8d7f5c on TCP port 1433.
-- https://ec2.amazonaws.com/?Action=RevokeSecurityGroupEgress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=1433 &amp;IpPermissions.1.ToPort=1433
-- &amp;IpPermissions.1.Groups.1.GroupId=sg-9a8d7f5c &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.RevokeSecurityGroupEgress
    (
    -- * Request
      RevokeSecurityGroupEgress
    -- ** Request constructor
    , revokeSecurityGroupEgress
    -- ** Request lenses
    , rsgerGroupId
    , rsgerFromPort
    , rsgerToPort
    , rsgerIpPermissions
    , rsgerSourceSecurityGroupName
    , rsgerSourceSecurityGroupOwnerId
    , rsgerIpProtocol
    , rsgerCidrIp

    -- * Response
    , RevokeSecurityGroupEgressResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RevokeSecurityGroupEgress' request.
revokeSecurityGroupEgress :: Text -- ^ 'rsgerGroupId'
                          -> RevokeSecurityGroupEgress
revokeSecurityGroupEgress p1 = RevokeSecurityGroupEgress
    { _rsgerGroupId = p1
    , _rsgerFromPort = Nothing
    , _rsgerToPort = Nothing
    , _rsgerIpPermissions = mempty
    , _rsgerSourceSecurityGroupName = Nothing
    , _rsgerSourceSecurityGroupOwnerId = Nothing
    , _rsgerIpProtocol = Nothing
    , _rsgerCidrIp = Nothing
    }
{-# INLINE revokeSecurityGroupEgress #-}

data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { _rsgerGroupId :: Text
      -- ^ The ID of the security group.
    , _rsgerFromPort :: Maybe Integer
      -- ^ The start of port range for the TCP and UDP protocols, or an ICMP
      -- type number. For the ICMP type number, use -1 to specify all ICMP
      -- types.
    , _rsgerToPort :: Maybe Integer
      -- ^ The end of port range for the TCP and UDP protocols, or an ICMP
      -- code number. For the ICMP code number, use -1 to specify all ICMP
      -- codes for the ICMP type.
    , _rsgerIpPermissions :: [IpPermission]
      -- ^ 
    , _rsgerSourceSecurityGroupName :: Maybe Text
      -- ^ [EC2-Classic, default VPC] The name of the source security group.
      -- You can't specify a source security group and a CIDR IP address
      -- range.
    , _rsgerSourceSecurityGroupOwnerId :: Maybe Text
      -- ^ The ID of the source security group. You can't specify a source
      -- security group and a CIDR IP address range.
    , _rsgerIpProtocol :: Maybe Text
      -- ^ The IP protocol name (tcp, udp, icmp) or number (see Protocol
      -- Numbers). Use -1 to specify all.
    , _rsgerCidrIp :: Maybe Text
      -- ^ The CIDR IP address range. You can't specify this parameter when
      -- specifying a source security group.
    } deriving (Show, Generic)

-- | The ID of the security group.
rsgerGroupId :: Lens' RevokeSecurityGroupEgress (Text)
rsgerGroupId f x =
    f (_rsgerGroupId x)
        <&> \y -> x { _rsgerGroupId = y }
{-# INLINE rsgerGroupId #-}

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use -1 to specify all ICMP types.
rsgerFromPort :: Lens' RevokeSecurityGroupEgress (Maybe Integer)
rsgerFromPort f x =
    f (_rsgerFromPort x)
        <&> \y -> x { _rsgerFromPort = y }
{-# INLINE rsgerFromPort #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use -1 to specify all ICMP codes for the
-- ICMP type.
rsgerToPort :: Lens' RevokeSecurityGroupEgress (Maybe Integer)
rsgerToPort f x =
    f (_rsgerToPort x)
        <&> \y -> x { _rsgerToPort = y }
{-# INLINE rsgerToPort #-}

-- | 
rsgerIpPermissions :: Lens' RevokeSecurityGroupEgress ([IpPermission])
rsgerIpPermissions f x =
    f (_rsgerIpPermissions x)
        <&> \y -> x { _rsgerIpPermissions = y }
{-# INLINE rsgerIpPermissions #-}

-- | [EC2-Classic, default VPC] The name of the source security group. You can't
-- specify a source security group and a CIDR IP address range.
rsgerSourceSecurityGroupName :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgerSourceSecurityGroupName f x =
    f (_rsgerSourceSecurityGroupName x)
        <&> \y -> x { _rsgerSourceSecurityGroupName = y }
{-# INLINE rsgerSourceSecurityGroupName #-}

-- | The ID of the source security group. You can't specify a source security
-- group and a CIDR IP address range.
rsgerSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgerSourceSecurityGroupOwnerId f x =
    f (_rsgerSourceSecurityGroupOwnerId x)
        <&> \y -> x { _rsgerSourceSecurityGroupOwnerId = y }
{-# INLINE rsgerSourceSecurityGroupOwnerId #-}

-- | The IP protocol name (tcp, udp, icmp) or number (see Protocol Numbers). Use
-- -1 to specify all.
rsgerIpProtocol :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgerIpProtocol f x =
    f (_rsgerIpProtocol x)
        <&> \y -> x { _rsgerIpProtocol = y }
{-# INLINE rsgerIpProtocol #-}

-- | The CIDR IP address range. You can't specify this parameter when specifying
-- a source security group.
rsgerCidrIp :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgerCidrIp f x =
    f (_rsgerCidrIp x)
        <&> \y -> x { _rsgerCidrIp = y }
{-# INLINE rsgerCidrIp #-}

instance ToQuery RevokeSecurityGroupEgress where
    toQuery = genericQuery def

data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RevokeSecurityGroupEgress where
    type Sv RevokeSecurityGroupEgress = EC2
    type Rs RevokeSecurityGroupEgress = RevokeSecurityGroupEgressResponse

    request = post "RevokeSecurityGroupEgress"
    response _ = nullaryResponse RevokeSecurityGroupEgressResponse
