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

-- Module      : Network.AWS.EC2.RevokeSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes one or more ingress rules from a security group. The values that
-- you specify in the revoke request (for example, ports) must match the
-- existing rule's values for the rule to be removed. Each rule consists of
-- the protocol and the CIDR range or source security group. For the TCP and
-- UDP protocols, you must also specify the destination port or range of
-- ports. For the ICMP protocol, you must also specify the ICMP type and code.
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupIngress.html>
module Network.AWS.EC2.RevokeSecurityGroupIngress
    (
    -- * Request
      RevokeSecurityGroupIngress
    -- ** Request constructor
    , revokeSecurityGroupIngress
    -- ** Request lenses
    , rsgiCidrIp
    , rsgiDryRun
    , rsgiFromPort
    , rsgiGroupId
    , rsgiGroupName
    , rsgiIpPermissions
    , rsgiIpProtocol
    , rsgiSourceSecurityGroupName
    , rsgiSourceSecurityGroupOwnerId
    , rsgiToPort

    -- * Response
    , RevokeSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeSecurityGroupIngressResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { _rsgiCidrIp                     :: Maybe Text
    , _rsgiDryRun                     :: Maybe Bool
    , _rsgiFromPort                   :: Maybe Int
    , _rsgiGroupId                    :: Maybe Text
    , _rsgiGroupName                  :: Maybe Text
    , _rsgiIpPermissions              :: List "item" IpPermission
    , _rsgiIpProtocol                 :: Maybe Text
    , _rsgiSourceSecurityGroupName    :: Maybe Text
    , _rsgiSourceSecurityGroupOwnerId :: Maybe Text
    , _rsgiToPort                     :: Maybe Int
    } deriving (Eq, Show)

-- | 'RevokeSecurityGroupIngress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsgiCidrIp' @::@ 'Maybe' 'Text'
--
-- * 'rsgiDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rsgiFromPort' @::@ 'Maybe' 'Int'
--
-- * 'rsgiGroupId' @::@ 'Maybe' 'Text'
--
-- * 'rsgiGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rsgiIpPermissions' @::@ ['IpPermission']
--
-- * 'rsgiIpProtocol' @::@ 'Maybe' 'Text'
--
-- * 'rsgiSourceSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rsgiSourceSecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'rsgiToPort' @::@ 'Maybe' 'Int'
--
revokeSecurityGroupIngress :: RevokeSecurityGroupIngress
revokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { _rsgiDryRun                     = Nothing
    , _rsgiGroupName                  = Nothing
    , _rsgiGroupId                    = Nothing
    , _rsgiSourceSecurityGroupName    = Nothing
    , _rsgiSourceSecurityGroupOwnerId = Nothing
    , _rsgiIpProtocol                 = Nothing
    , _rsgiFromPort                   = Nothing
    , _rsgiToPort                     = Nothing
    , _rsgiCidrIp                     = Nothing
    , _rsgiIpPermissions              = mempty
    }

-- | The CIDR IP address range. You can't specify this parameter when
-- specifying a source security group.
rsgiCidrIp :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiCidrIp = lens _rsgiCidrIp (\s a -> s { _rsgiCidrIp = a })

rsgiDryRun :: Lens' RevokeSecurityGroupIngress (Maybe Bool)
rsgiDryRun = lens _rsgiDryRun (\s a -> s { _rsgiDryRun = a })

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use -1 to specify all ICMP types.
rsgiFromPort :: Lens' RevokeSecurityGroupIngress (Maybe Int)
rsgiFromPort = lens _rsgiFromPort (\s a -> s { _rsgiFromPort = a })

-- | The ID of the security group.
rsgiGroupId :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiGroupId = lens _rsgiGroupId (\s a -> s { _rsgiGroupId = a })

-- | [EC2-Classic, default VPC] The name of the security group.
rsgiGroupName :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiGroupName = lens _rsgiGroupName (\s a -> s { _rsgiGroupName = a })

-- | A set of IP permissions. You can't specify a source security group and a
-- CIDR IP address range.
rsgiIpPermissions :: Lens' RevokeSecurityGroupIngress [IpPermission]
rsgiIpPermissions =
    lens _rsgiIpPermissions (\s a -> s { _rsgiIpPermissions = a })
        . _List

-- | The IP protocol name (tcp, udp, icmp) or number (see Protocol Numbers).
-- Use -1 to specify all.
rsgiIpProtocol :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiIpProtocol = lens _rsgiIpProtocol (\s a -> s { _rsgiIpProtocol = a })

-- | [EC2-Classic, default VPC] The name of the source security group. You
-- can't specify a source security group and a CIDR IP address range.
rsgiSourceSecurityGroupName :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiSourceSecurityGroupName =
    lens _rsgiSourceSecurityGroupName
        (\s a -> s { _rsgiSourceSecurityGroupName = a })

-- | The ID of the source security group. You can't specify a source security
-- group and a CIDR IP address range.
rsgiSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupIngress (Maybe Text)
rsgiSourceSecurityGroupOwnerId =
    lens _rsgiSourceSecurityGroupOwnerId
        (\s a -> s { _rsgiSourceSecurityGroupOwnerId = a })

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use -1 to specify all ICMP codes for
-- the ICMP type.
rsgiToPort :: Lens' RevokeSecurityGroupIngress (Maybe Int)
rsgiToPort = lens _rsgiToPort (\s a -> s { _rsgiToPort = a })

data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RevokeSecurityGroupIngressResponse' constructor.
revokeSecurityGroupIngressResponse :: RevokeSecurityGroupIngressResponse
revokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse

instance ToPath RevokeSecurityGroupIngress where
    toPath = const "/"

instance ToQuery RevokeSecurityGroupIngress where
    toQuery RevokeSecurityGroupIngress{..} = mconcat
        [ "CidrIp"                     =? _rsgiCidrIp
        , "dryRun"                     =? _rsgiDryRun
        , "FromPort"                   =? _rsgiFromPort
        , "GroupId"                    =? _rsgiGroupId
        , "GroupName"                  =? _rsgiGroupName
        , "IpPermissions"              =? _rsgiIpPermissions
        , "IpProtocol"                 =? _rsgiIpProtocol
        , "SourceSecurityGroupName"    =? _rsgiSourceSecurityGroupName
        , "SourceSecurityGroupOwnerId" =? _rsgiSourceSecurityGroupOwnerId
        , "ToPort"                     =? _rsgiToPort
        ]

instance ToHeaders RevokeSecurityGroupIngress

query

instance AWSRequest RevokeSecurityGroupIngress where
    type Sv RevokeSecurityGroupIngress = EC2
    type Rs RevokeSecurityGroupIngress = RevokeSecurityGroupIngressResponse

    request  = post "RevokeSecurityGroupIngress"
    response = nullResponse RevokeSecurityGroupIngressResponse
