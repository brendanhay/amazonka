{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupEgress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds one or more egress rules to a security group for use with a VPC.
-- Specifically, this action permits instances to send traffic to one or more
-- destination CIDR IP address ranges, or to one or more destination security
-- groups for the same VPC. You can have up to 50 rules per security group
-- (covering both ingress and egress rules). A security group is for use with
-- instances either in the EC2-Classic platform or in a specific VPC. This
-- action doesn't apply to security groups for use in EC2-Classic. For more
-- information, see Security Groups for Your VPC in the Amazon Virtual Private
-- Cloud User Guide. Each rule consists of the protocol (for example, TCP),
-- plus either a CIDR range or a source group. For the TCP and UDP protocols,
-- you must also specify the destination port or port range. For the ICMP
-- protocol, you must also specify the ICMP type and code. You can use -1 for
-- the type or code to mean all types or all codes. Rule changes are
-- propagated to affected instances as quickly as possible. However, a small
-- delay might occur.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupEgress.html>
module Network.AWS.EC2.AuthorizeSecurityGroupEgress
    (
    -- * Request
      AuthorizeSecurityGroupEgress
    -- ** Request constructor
    , authorizeSecurityGroupEgress
    -- ** Request lenses
    , asgeCidrIp
    , asgeDryRun
    , asgeFromPort
    , asgeGroupId
    , asgeIpPermissions
    , asgeIpProtocol
    , asgeSourceSecurityGroupName
    , asgeSourceSecurityGroupOwnerId
    , asgeToPort

    -- * Response
    , AuthorizeSecurityGroupEgressResponse
    -- ** Response constructor
    , authorizeSecurityGroupEgressResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
    { _asgeCidrIp                     :: Maybe Text
    , _asgeDryRun                     :: Maybe Bool
    , _asgeFromPort                   :: Maybe Int
    , _asgeGroupId                    :: Text
    , _asgeIpPermissions              :: List "item" IpPermission
    , _asgeIpProtocol                 :: Maybe Text
    , _asgeSourceSecurityGroupName    :: Maybe Text
    , _asgeSourceSecurityGroupOwnerId :: Maybe Text
    , _asgeToPort                     :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'AuthorizeSecurityGroupEgress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgeCidrIp' @::@ 'Maybe' 'Text'
--
-- * 'asgeDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'asgeFromPort' @::@ 'Maybe' 'Int'
--
-- * 'asgeGroupId' @::@ 'Text'
--
-- * 'asgeIpPermissions' @::@ ['IpPermission']
--
-- * 'asgeIpProtocol' @::@ 'Maybe' 'Text'
--
-- * 'asgeSourceSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'asgeSourceSecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'asgeToPort' @::@ 'Maybe' 'Int'
--
authorizeSecurityGroupEgress :: Text -- ^ 'asgeGroupId'
                             -> AuthorizeSecurityGroupEgress
authorizeSecurityGroupEgress p1 = AuthorizeSecurityGroupEgress
    { _asgeGroupId                    = p1
    , _asgeDryRun                     = Nothing
    , _asgeSourceSecurityGroupName    = Nothing
    , _asgeSourceSecurityGroupOwnerId = Nothing
    , _asgeIpProtocol                 = Nothing
    , _asgeFromPort                   = Nothing
    , _asgeToPort                     = Nothing
    , _asgeCidrIp                     = Nothing
    , _asgeIpPermissions              = mempty
    }

-- | The CIDR IP address range. You can't specify this parameter when
-- specifying a source security group.
asgeCidrIp :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeCidrIp = lens _asgeCidrIp (\s a -> s { _asgeCidrIp = a })

asgeDryRun :: Lens' AuthorizeSecurityGroupEgress (Maybe Bool)
asgeDryRun = lens _asgeDryRun (\s a -> s { _asgeDryRun = a })

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use -1 to specify all ICMP types.
asgeFromPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgeFromPort = lens _asgeFromPort (\s a -> s { _asgeFromPort = a })

-- | The ID of the security group.
asgeGroupId :: Lens' AuthorizeSecurityGroupEgress Text
asgeGroupId = lens _asgeGroupId (\s a -> s { _asgeGroupId = a })

-- | A set of IP permissions. You can't specify a destination security group
-- and a CIDR IP address range.
asgeIpPermissions :: Lens' AuthorizeSecurityGroupEgress [IpPermission]
asgeIpPermissions =
    lens _asgeIpPermissions (\s a -> s { _asgeIpPermissions = a })
        . _List

-- | The IP protocol name (tcp, udp, icmp) or number (see Protocol Numbers).
-- Use -1 to specify all.
asgeIpProtocol :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeIpProtocol = lens _asgeIpProtocol (\s a -> s { _asgeIpProtocol = a })

-- | [EC2-Classic, default VPC] The name of the destination security group.
-- You can't specify a destination security group and a CIDR IP address
-- range.
asgeSourceSecurityGroupName :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeSourceSecurityGroupName =
    lens _asgeSourceSecurityGroupName
        (\s a -> s { _asgeSourceSecurityGroupName = a })

-- | The ID of the destination security group. You can't specify a destination
-- security group and a CIDR IP address range.
asgeSourceSecurityGroupOwnerId :: Lens' AuthorizeSecurityGroupEgress (Maybe Text)
asgeSourceSecurityGroupOwnerId =
    lens _asgeSourceSecurityGroupOwnerId
        (\s a -> s { _asgeSourceSecurityGroupOwnerId = a })

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use -1 to specify all ICMP codes for
-- the ICMP type.
asgeToPort :: Lens' AuthorizeSecurityGroupEgress (Maybe Int)
asgeToPort = lens _asgeToPort (\s a -> s { _asgeToPort = a })

data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AuthorizeSecurityGroupEgressResponse' constructor.
authorizeSecurityGroupEgressResponse :: AuthorizeSecurityGroupEgressResponse
authorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse

instance ToPath AuthorizeSecurityGroupEgress where
    toPath = const "/"

instance ToQuery AuthorizeSecurityGroupEgress

instance ToHeaders AuthorizeSecurityGroupEgress

instance AWSRequest AuthorizeSecurityGroupEgress where
    type Sv AuthorizeSecurityGroupEgress = EC2
    type Rs AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgressResponse

    request  = post "AuthorizeSecurityGroupEgress"
    response = nullResponse AuthorizeSecurityGroupEgressResponse
