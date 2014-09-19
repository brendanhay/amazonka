{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.RevokeSecurityGroupEgress
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
module Network.AWS.EC2.RevokeSecurityGroupEgress
    (
    -- * Request
      RevokeSecurityGroupEgress
    -- ** Request constructor
    , revokeSecurityGroupEgress
    -- ** Request lenses
    , rsgeGroupId
    , rsgeSourceSecurityGroupName
    , rsgeSourceSecurityGroupOwnerId
    , rsgeIpProtocol
    , rsgeFromPort
    , rsgeToPort
    , rsgeCidrIp
    , rsgeIpPermissions

    -- * Response
    , RevokeSecurityGroupEgressResponse
    -- ** Response constructor
    , revokeSecurityGroupEgressResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { _rsgeGroupId :: Text
    , _rsgeSourceSecurityGroupName :: Maybe Text
    , _rsgeSourceSecurityGroupOwnerId :: Maybe Text
    , _rsgeIpProtocol :: Maybe Text
    , _rsgeFromPort :: Maybe Integer
    , _rsgeToPort :: Maybe Integer
    , _rsgeCidrIp :: Maybe Text
    , _rsgeIpPermissions :: [IpPermission]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeSecurityGroupEgress' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupId ::@ @Text@
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
revokeSecurityGroupEgress :: Text -- ^ 'rsgeGroupId'
                          -> RevokeSecurityGroupEgress
revokeSecurityGroupEgress p1 = RevokeSecurityGroupEgress
    { _rsgeGroupId = p1
    , _rsgeSourceSecurityGroupName = Nothing
    , _rsgeSourceSecurityGroupOwnerId = Nothing
    , _rsgeIpProtocol = Nothing
    , _rsgeFromPort = Nothing
    , _rsgeToPort = Nothing
    , _rsgeCidrIp = Nothing
    , _rsgeIpPermissions = mempty
    }

-- | The ID of the security group.
rsgeGroupId :: Lens' RevokeSecurityGroupEgress Text
rsgeGroupId = lens _rsgeGroupId (\s a -> s { _rsgeGroupId = a })

-- | [EC2-Classic, default VPC] The name of the source security group. You can't
-- specify a source security group and a CIDR IP address range.
rsgeSourceSecurityGroupName :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupName =
    lens _rsgeSourceSecurityGroupName
         (\s a -> s { _rsgeSourceSecurityGroupName = a })

-- | The ID of the source security group. You can't specify a source security
-- group and a CIDR IP address range.
rsgeSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupOwnerId =
    lens _rsgeSourceSecurityGroupOwnerId
         (\s a -> s { _rsgeSourceSecurityGroupOwnerId = a })

-- | The IP protocol name (tcp, udp, icmp) or number (see Protocol Numbers). Use
-- -1 to specify all.
rsgeIpProtocol :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeIpProtocol = lens _rsgeIpProtocol (\s a -> s { _rsgeIpProtocol = a })

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use -1 to specify all ICMP types.
rsgeFromPort :: Lens' RevokeSecurityGroupEgress (Maybe Integer)
rsgeFromPort = lens _rsgeFromPort (\s a -> s { _rsgeFromPort = a })

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use -1 to specify all ICMP codes for the
-- ICMP type.
rsgeToPort :: Lens' RevokeSecurityGroupEgress (Maybe Integer)
rsgeToPort = lens _rsgeToPort (\s a -> s { _rsgeToPort = a })

-- | The CIDR IP address range. You can't specify this parameter when specifying
-- a source security group.
rsgeCidrIp :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeCidrIp = lens _rsgeCidrIp (\s a -> s { _rsgeCidrIp = a })

-- | 
rsgeIpPermissions :: Lens' RevokeSecurityGroupEgress [IpPermission]
rsgeIpPermissions =
    lens _rsgeIpPermissions (\s a -> s { _rsgeIpPermissions = a })

instance ToQuery RevokeSecurityGroupEgress where
    toQuery = genericQuery def

data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeSecurityGroupEgressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
revokeSecurityGroupEgressResponse :: RevokeSecurityGroupEgressResponse
revokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse

instance AWSRequest RevokeSecurityGroupEgress where
    type Sv RevokeSecurityGroupEgress = EC2
    type Rs RevokeSecurityGroupEgress = RevokeSecurityGroupEgressResponse

    request = post "RevokeSecurityGroupEgress"
    response _ = nullaryResponse RevokeSecurityGroupEgressResponse
