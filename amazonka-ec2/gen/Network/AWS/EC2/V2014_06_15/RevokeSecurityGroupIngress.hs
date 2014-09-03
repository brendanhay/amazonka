{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.RevokeSecurityGroupIngress
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
-- quickly as possible. However, a small delay might occur. Example This
-- example revokes TCP port 80 access from the 205.192.0.0/16 address range
-- for the security group named websrv. If the security group is for a VPC,
-- specify the ID of the security group instead of the name.
-- https://ec2.amazonaws.com/?Action=RevokeSecurityGroupIngress
-- &amp;GroupName=websrv &amp;IpProtocol=tcp &amp;FromPort=80 &amp;ToPort=80
-- &amp;CidrIp=205.192.0.0/16 &amp;AUTHPARAMS
-- &lt;RevokeSecurityGroupIngressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/RevokeSecurityGroupIngressResponse&gt;.
module Network.AWS.EC2.V2014_06_15.RevokeSecurityGroupIngress where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RevokeSecurityGroupIngress' request.
revokeSecurityGroupIngress :: RevokeSecurityGroupIngress
revokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { _rsgirFromPort = Nothing
    , _rsgirToPort = Nothing
    , _rsgirIpPermissions = mempty
    , _rsgirGroupName = Nothing
    , _rsgirGroupId = Nothing
    , _rsgirSourceSecurityGroupName = Nothing
    , _rsgirSourceSecurityGroupOwnerId = Nothing
    , _rsgirIpProtocol = Nothing
    , _rsgirCidrIp = Nothing
    }

data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { _rsgirFromPort :: Maybe Integer
      -- ^ The start of port range for the TCP and UDP protocols, or an ICMP
      -- type number. For the ICMP type number, use -1 to specify all ICMP
      -- types.
    , _rsgirToPort :: Maybe Integer
      -- ^ The end of port range for the TCP and UDP protocols, or an ICMP
      -- code number. For the ICMP code number, use -1 to specify all ICMP
      -- codes for the ICMP type.
    , _rsgirIpPermissions :: [IpPermission]
      -- ^ 
    , _rsgirGroupName :: Maybe Text
      -- ^ [EC2-Classic, default VPC] The name of the security group.
    , _rsgirGroupId :: Maybe Text
      -- ^ The ID of the security group.
    , _rsgirSourceSecurityGroupName :: Maybe Text
      -- ^ [EC2-Classic, default VPC] The name of the source security group.
      -- You can't specify a source security group and a CIDR IP address
      -- range.
    , _rsgirSourceSecurityGroupOwnerId :: Maybe Text
      -- ^ The ID of the source security group. You can't specify a source
      -- security group and a CIDR IP address range.
    , _rsgirIpProtocol :: Maybe Text
      -- ^ The IP protocol name (tcp, udp, icmp) or number (see Protocol
      -- Numbers). Use -1 to specify all.
    , _rsgirCidrIp :: Maybe Text
      -- ^ The CIDR IP address range. You can't specify this parameter when
      -- specifying a source security group.
    } deriving (Show, Generic)

makeLenses ''RevokeSecurityGroupIngress

instance ToQuery RevokeSecurityGroupIngress where
    toQuery = genericQuery def

data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse
    deriving (Eq, Show, Generic)

makeLenses ''RevokeSecurityGroupIngressResponse

instance AWSRequest RevokeSecurityGroupIngress where
    type Sv RevokeSecurityGroupIngress = EC2
    type Rs RevokeSecurityGroupIngress = RevokeSecurityGroupIngressResponse

    request = post "RevokeSecurityGroupIngress"
    response _ = nullaryResponse RevokeSecurityGroupIngressResponse
