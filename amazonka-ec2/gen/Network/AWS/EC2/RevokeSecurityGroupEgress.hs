{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
-- values that you specify in the revoke request (for example, ports) must match
-- the existing rule's values for the rule to be revoked.
--
-- Each rule consists of the protocol and the CIDR range or source security
-- group. For the TCP and UDP protocols, you must also specify the destination
-- port or range of ports. For the ICMP protocol, you must also specify the ICMP
-- type and code.
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupEgress.html>
module Network.AWS.EC2.RevokeSecurityGroupEgress
    (
    -- * Request
      RevokeSecurityGroupEgress
    -- ** Request constructor
    , revokeSecurityGroupEgress
    -- ** Request lenses
    , rsgeCidrIp
    , rsgeDryRun
    , rsgeFromPort
    , rsgeGroupId
    , rsgeIpPermissions
    , rsgeIpProtocol
    , rsgeSourceSecurityGroupName
    , rsgeSourceSecurityGroupOwnerId
    , rsgeToPort

    -- * Response
    , RevokeSecurityGroupEgressResponse
    -- ** Response constructor
    , revokeSecurityGroupEgressResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { _rsgeCidrIp                     :: Maybe Text
    , _rsgeDryRun                     :: Maybe Bool
    , _rsgeFromPort                   :: Maybe Int
    , _rsgeGroupId                    :: Text
    , _rsgeIpPermissions              :: List "item" IpPermission
    , _rsgeIpProtocol                 :: Maybe Text
    , _rsgeSourceSecurityGroupName    :: Maybe Text
    , _rsgeSourceSecurityGroupOwnerId :: Maybe Text
    , _rsgeToPort                     :: Maybe Int
    } deriving (Eq, Show)

-- | 'RevokeSecurityGroupEgress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsgeCidrIp' @::@ 'Maybe' 'Text'
--
-- * 'rsgeDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rsgeFromPort' @::@ 'Maybe' 'Int'
--
-- * 'rsgeGroupId' @::@ 'Text'
--
-- * 'rsgeIpPermissions' @::@ ['IpPermission']
--
-- * 'rsgeIpProtocol' @::@ 'Maybe' 'Text'
--
-- * 'rsgeSourceSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rsgeSourceSecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'rsgeToPort' @::@ 'Maybe' 'Int'
--
revokeSecurityGroupEgress :: Text -- ^ 'rsgeGroupId'
                          -> RevokeSecurityGroupEgress
revokeSecurityGroupEgress p1 = RevokeSecurityGroupEgress
    { _rsgeGroupId                    = p1
    , _rsgeDryRun                     = Nothing
    , _rsgeSourceSecurityGroupName    = Nothing
    , _rsgeSourceSecurityGroupOwnerId = Nothing
    , _rsgeIpProtocol                 = Nothing
    , _rsgeFromPort                   = Nothing
    , _rsgeToPort                     = Nothing
    , _rsgeCidrIp                     = Nothing
    , _rsgeIpPermissions              = mempty
    }

-- | The CIDR IP address range. You can't specify this parameter when specifying a
-- source security group.
rsgeCidrIp :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeCidrIp = lens _rsgeCidrIp (\s a -> s { _rsgeCidrIp = a })

rsgeDryRun :: Lens' RevokeSecurityGroupEgress (Maybe Bool)
rsgeDryRun = lens _rsgeDryRun (\s a -> s { _rsgeDryRun = a })

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use '-1' to specify all ICMP types.
rsgeFromPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgeFromPort = lens _rsgeFromPort (\s a -> s { _rsgeFromPort = a })

-- | The ID of the security group.
rsgeGroupId :: Lens' RevokeSecurityGroupEgress Text
rsgeGroupId = lens _rsgeGroupId (\s a -> s { _rsgeGroupId = a })

-- | A set of IP permissions. You can't specify a destination security group and a
-- CIDR IP address range.
rsgeIpPermissions :: Lens' RevokeSecurityGroupEgress [IpPermission]
rsgeIpPermissions =
    lens _rsgeIpPermissions (\s a -> s { _rsgeIpPermissions = a })
        . _List

-- | The IP protocol name ('tcp', 'udp', 'icmp') or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>). Use '-1'
-- to specify all.
rsgeIpProtocol :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeIpProtocol = lens _rsgeIpProtocol (\s a -> s { _rsgeIpProtocol = a })

-- | [EC2-Classic, default VPC] The name of the destination security group. You
-- can't specify a destination security group and a CIDR IP address range.
rsgeSourceSecurityGroupName :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupName =
    lens _rsgeSourceSecurityGroupName
        (\s a -> s { _rsgeSourceSecurityGroupName = a })

-- | The ID of the destination security group. You can't specify a destination
-- security group and a CIDR IP address range.
rsgeSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupOwnerId =
    lens _rsgeSourceSecurityGroupOwnerId
        (\s a -> s { _rsgeSourceSecurityGroupOwnerId = a })

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number.
-- For the ICMP code number, use '-1' to specify all ICMP codes for the ICMP type.
rsgeToPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgeToPort = lens _rsgeToPort (\s a -> s { _rsgeToPort = a })

data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RevokeSecurityGroupEgressResponse' constructor.
revokeSecurityGroupEgressResponse :: RevokeSecurityGroupEgressResponse
revokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse

instance ToPath RevokeSecurityGroupEgress where
    toPath = const "/"

instance ToQuery RevokeSecurityGroupEgress where
    toQuery RevokeSecurityGroupEgress{..} = mconcat
        [ "cidrIp"                     =? _rsgeCidrIp
        , "dryRun"                     =? _rsgeDryRun
        , "fromPort"                   =? _rsgeFromPort
        , "groupId"                    =? _rsgeGroupId
        , "ipPermissions"              =? _rsgeIpPermissions
        , "ipProtocol"                 =? _rsgeIpProtocol
        , "sourceSecurityGroupName"    =? _rsgeSourceSecurityGroupName
        , "sourceSecurityGroupOwnerId" =? _rsgeSourceSecurityGroupOwnerId
        , "toPort"                     =? _rsgeToPort
        ]

instance ToHeaders RevokeSecurityGroupEgress

instance AWSRequest RevokeSecurityGroupEgress where
    type Sv RevokeSecurityGroupEgress = EC2
    type Rs RevokeSecurityGroupEgress = RevokeSecurityGroupEgressResponse

    request  = post "RevokeSecurityGroupEgress"
    response = nullResponse RevokeSecurityGroupEgressResponse
