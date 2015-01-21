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

-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds one or more ingress rules to a security group.
--
-- EC2-Classic: You can have up to 100 rules per group.
--
-- EC2-VPC: You can have up to 50 rules per group (covering both ingress and
-- egress rules).
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
--
-- [EC2-Classic] This action gives one or more CIDR IP address ranges
-- permission to access a security group in your account, or gives one or more
-- security groups (called the /source groups/) permission to access a security
-- group for your account. A source group can be for your own AWS account, or
-- another.
--
-- [EC2-VPC] This action gives one or more CIDR IP address ranges permission to
-- access a security group in your VPC, or gives one or more other security
-- groups (called the /source groups/) permission to access a security group for
-- your VPC. The security groups must all be for the same VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupIngress.html>
module Network.AWS.EC2.AuthorizeSecurityGroupIngress
    (
    -- * Request
      AuthorizeSecurityGroupIngress
    -- ** Request constructor
    , authorizeSecurityGroupIngress
    -- ** Request lenses
    , asgiCidrIp
    , asgiDryRun
    , asgiFromPort
    , asgiGroupId
    , asgiGroupName
    , asgiIpPermissions
    , asgiIpProtocol
    , asgiSourceSecurityGroupName
    , asgiSourceSecurityGroupOwnerId
    , asgiToPort

    -- * Response
    , AuthorizeSecurityGroupIngressResponse
    -- ** Response constructor
    , authorizeSecurityGroupIngressResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { _asgiCidrIp                     :: Maybe Text
    , _asgiDryRun                     :: Maybe Bool
    , _asgiFromPort                   :: Maybe Int
    , _asgiGroupId                    :: Maybe Text
    , _asgiGroupName                  :: Maybe Text
    , _asgiIpPermissions              :: List "item" IpPermission
    , _asgiIpProtocol                 :: Maybe Text
    , _asgiSourceSecurityGroupName    :: Maybe Text
    , _asgiSourceSecurityGroupOwnerId :: Maybe Text
    , _asgiToPort                     :: Maybe Int
    } deriving (Eq, Read, Show)

-- | 'AuthorizeSecurityGroupIngress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgiCidrIp' @::@ 'Maybe' 'Text'
--
-- * 'asgiDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'asgiFromPort' @::@ 'Maybe' 'Int'
--
-- * 'asgiGroupId' @::@ 'Maybe' 'Text'
--
-- * 'asgiGroupName' @::@ 'Maybe' 'Text'
--
-- * 'asgiIpPermissions' @::@ ['IpPermission']
--
-- * 'asgiIpProtocol' @::@ 'Maybe' 'Text'
--
-- * 'asgiSourceSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'asgiSourceSecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'asgiToPort' @::@ 'Maybe' 'Int'
--
authorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress
authorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { _asgiDryRun                     = Nothing
    , _asgiGroupName                  = Nothing
    , _asgiGroupId                    = Nothing
    , _asgiSourceSecurityGroupName    = Nothing
    , _asgiSourceSecurityGroupOwnerId = Nothing
    , _asgiIpProtocol                 = Nothing
    , _asgiFromPort                   = Nothing
    , _asgiToPort                     = Nothing
    , _asgiCidrIp                     = Nothing
    , _asgiIpPermissions              = mempty
    }

-- | The CIDR IP address range. You can't specify this parameter when specifying a
-- source security group.
asgiCidrIp :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiCidrIp = lens _asgiCidrIp (\s a -> s { _asgiCidrIp = a })

asgiDryRun :: Lens' AuthorizeSecurityGroupIngress (Maybe Bool)
asgiDryRun = lens _asgiDryRun (\s a -> s { _asgiDryRun = a })

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use '-1' to specify all ICMP types.
asgiFromPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Int)
asgiFromPort = lens _asgiFromPort (\s a -> s { _asgiFromPort = a })

-- | The ID of the security group.
asgiGroupId :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiGroupId = lens _asgiGroupId (\s a -> s { _asgiGroupId = a })

-- | [EC2-Classic, default VPC] The name of the security group.
asgiGroupName :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiGroupName = lens _asgiGroupName (\s a -> s { _asgiGroupName = a })

-- | A set of IP permissions. You can't specify a source security group and a CIDR
-- IP address range.
asgiIpPermissions :: Lens' AuthorizeSecurityGroupIngress [IpPermission]
asgiIpPermissions =
    lens _asgiIpPermissions (\s a -> s { _asgiIpPermissions = a })
        . _List

-- | The IP protocol name ('tcp', 'udp', 'icmp') or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>). Use '-1'
-- to specify all.
asgiIpProtocol :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiIpProtocol = lens _asgiIpProtocol (\s a -> s { _asgiIpProtocol = a })

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

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number.
-- For the ICMP code number, use '-1' to specify all ICMP codes for the ICMP type.
asgiToPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Int)
asgiToPort = lens _asgiToPort (\s a -> s { _asgiToPort = a })

data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AuthorizeSecurityGroupIngressResponse' constructor.
authorizeSecurityGroupIngressResponse :: AuthorizeSecurityGroupIngressResponse
authorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse

instance ToPath AuthorizeSecurityGroupIngress where
    toPath = const "/"

instance ToQuery AuthorizeSecurityGroupIngress where
    toQuery AuthorizeSecurityGroupIngress{..} = mconcat
        [ "CidrIp"                     =? _asgiCidrIp
        , "dryRun"                     =? _asgiDryRun
        , "FromPort"                   =? _asgiFromPort
        , "GroupId"                    =? _asgiGroupId
        , "GroupName"                  =? _asgiGroupName
        , "IpPermissions"              `toQueryList` _asgiIpPermissions
        , "IpProtocol"                 =? _asgiIpProtocol
        , "SourceSecurityGroupName"    =? _asgiSourceSecurityGroupName
        , "SourceSecurityGroupOwnerId" =? _asgiSourceSecurityGroupOwnerId
        , "ToPort"                     =? _asgiToPort
        ]

instance ToHeaders AuthorizeSecurityGroupIngress

instance AWSRequest AuthorizeSecurityGroupIngress where
    type Sv AuthorizeSecurityGroupIngress = EC2
    type Rs AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngressResponse

    request  = post "AuthorizeSecurityGroupIngress"
    response = nullResponse AuthorizeSecurityGroupIngressResponse
