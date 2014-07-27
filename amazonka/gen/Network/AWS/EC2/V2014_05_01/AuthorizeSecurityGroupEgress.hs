{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.AuthorizeSecurityGroupEgress
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
-- CIDR IP address ranges, or to one or more security groups for the same VPC.
-- You can have up to 50 rules per security group (covering both ingress and
-- egress rules). A security group is for use with instances either in the
-- EC2-Classic platform or in a specific VPC. This action doesn't apply to
-- security groups for use in EC2-Classic. For more information, see Security
-- Groups for Your VPC in the Amazon Virtual Private Cloud User Guide. Each
-- rule consists of the protocol (for example, TCP), plus either a CIDR range
-- or a source group. For the TCP and UDP protocols, you must also specify the
-- destination port or port range. For the ICMP protocol, you must also
-- specify the ICMP type and code. You can use -1 for the type or code to mean
-- all types or all codes. Rule changes are propagated to affected instances
-- as quickly as possible. However, a small delay might occur. Example 1 This
-- example request grants your security group with the ID sg-1a2b3c4d access
-- to the 192.0.2.0/24 and 198.51.100.0/24 address ranges on TCP port 80.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupEgress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=80 &amp;IpPermissions.1.ToPort=80
-- &amp;IpPermissions.1.IpRanges.1.CidrIp=192.0.2.0/24
-- &amp;IpPermissions.1.IpRanges.2.CidrIp=198.51.100.0/24 &amp;AUTHPARAMS
-- &lt;AuthorizeSecurityGroupEgressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/AuthorizeSecurityGroupEgressResponse&gt; Example 2 This example
-- request grants egress access from the security group with the ID
-- sg-1a2b3c4d to the security group with the ID sg-9a8d7f5c on TCP port 1433.
-- https://ec2.amazonaws.com/?Action=AuthorizeSecurityGroupEgress
-- &amp;GroupId=sg-1a2b3c4d &amp;IpPermissions.1.IpProtocol=tcp
-- &amp;IpPermissions.1.FromPort=1433 &amp;IpPermissions.1.ToPort=1433
-- &amp;IpPermissions.1.Groups.1.GroupId=sg-9a8d7f5c &amp;AUTHPARAMS
-- &lt;AuthorizeSecurityGroupEgressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/AuthorizeSecurityGroupEgressResponse&gt;.
module Network.AWS.EC2.V2014_05_01.AuthorizeSecurityGroupEgress where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
    { _asgerGroupId :: Text
      -- ^ The ID of the security group.
    , _asgerDryRun :: Maybe Bool
      -- ^ 
    , _asgerFromPort :: Maybe Integer
      -- ^ The start of port range for the TCP and UDP protocols, or an ICMP
      -- type number. For the ICMP type number, use -1 to specify all ICMP
      -- types.
    , _asgerToPort :: Maybe Integer
      -- ^ The end of port range for the TCP and UDP protocols, or an ICMP
      -- code number. For the ICMP code number, use -1 to specify all ICMP
      -- codes for the ICMP type.
    , _asgerIpPermissions :: [IpPermission]
      -- ^ 
    , _asgerIpProtocol :: Maybe Text
      -- ^ The IP protocol name (tcp, udp, icmp) or number (see Protocol
      -- Numbers). Use -1 to specify all.
    , _asgerCidrIp :: Maybe Text
      -- ^ The CIDR IP address range. You can't specify this parameter when
      -- specifying a source security group.
    , _asgerSourceSecurityGroupOwnerId :: Maybe Text
      -- ^ The ID of the source security group. You can't specify a source
      -- security group and a CIDR IP address range.
    , _asgerSourceSecurityGroupName :: Maybe Text
      -- ^ [EC2-Classic, default VPC] The name of the source security group.
      -- You can't specify a source security group and a CIDR IP address
      -- range.
    } deriving (Generic)

instance ToQuery AuthorizeSecurityGroupEgress where
    toQuery = genericToQuery def

instance AWSRequest AuthorizeSecurityGroupEgress where
    type Sv AuthorizeSecurityGroupEgress = EC2
    type Rs AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgressResponse

    request = post "AuthorizeSecurityGroupEgress"
    response _ _ = return (Right AuthorizeSecurityGroupEgressResponse)

data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse
    deriving (Eq, Show, Generic)
