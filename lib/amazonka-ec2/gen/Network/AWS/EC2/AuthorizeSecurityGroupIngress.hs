{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified ingress rules to a security group.
--
--
-- An inbound rule permits instances to receive traffic from the specified IPv4 or IPv6 CIDR address ranges, or from the instances associated with the specified destination security groups.
--
-- You specify a protocol for each rule (for example, TCP). For TCP and UDP, you must also specify the destination port or port range. For ICMP/ICMPv6, you must also specify the ICMP/ICMPv6 type and code. You can use -1 to mean all types or all codes.
--
-- Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.
--
-- For more information about VPC security group limits, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html Amazon VPC Limits> .
module Network.AWS.EC2.AuthorizeSecurityGroupIngress
  ( -- * Creating a Request
    authorizeSecurityGroupIngress,
    AuthorizeSecurityGroupIngress,

    -- * Request Lenses
    asgiFromPort,
    asgiIPPermissions,
    asgiIPProtocol,
    asgiGroupId,
    asgiToPort,
    asgiCidrIP,
    asgiSourceSecurityGroupOwnerId,
    asgiGroupName,
    asgiSourceSecurityGroupName,
    asgiDryRun,

    -- * Destructuring the Response
    authorizeSecurityGroupIngressResponse,
    AuthorizeSecurityGroupIngressResponse,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'authorizeSecurityGroupIngress' smart constructor.
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress'
  { _asgiFromPort ::
      !(Maybe Int),
    _asgiIPPermissions ::
      !(Maybe [IPPermission]),
    _asgiIPProtocol ::
      !(Maybe Text),
    _asgiGroupId :: !(Maybe Text),
    _asgiToPort :: !(Maybe Int),
    _asgiCidrIP :: !(Maybe Text),
    _asgiSourceSecurityGroupOwnerId ::
      !(Maybe Text),
    _asgiGroupName :: !(Maybe Text),
    _asgiSourceSecurityGroupName ::
      !(Maybe Text),
    _asgiDryRun :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthorizeSecurityGroupIngress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgiFromPort' - The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all types. If you specify all ICMP types, you must specify all codes. Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- * 'asgiIPPermissions' - The sets of IP permissions.
--
-- * 'asgiIPProtocol' - The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). To specify @icmpv6@ , use a set of IP permissions. [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a protocol other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports you specify. Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- * 'asgiGroupId' - The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- * 'asgiToPort' - The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all codes. If you specify all ICMP types, you must specify all codes. Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- * 'asgiCidrIP' - The IPv4 address range, in CIDR format. You can't specify this parameter when specifying a source security group. To specify an IPv6 address range, use a set of IP permissions. Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- * 'asgiSourceSecurityGroupOwnerId' - [nondefault VPC] The AWS account ID for the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead.
--
-- * 'asgiGroupName' - [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- * 'asgiSourceSecurityGroupName' - [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead. For EC2-VPC, the source security group must be in the same VPC.
--
-- * 'asgiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
authorizeSecurityGroupIngress ::
  AuthorizeSecurityGroupIngress
authorizeSecurityGroupIngress =
  AuthorizeSecurityGroupIngress'
    { _asgiFromPort = Nothing,
      _asgiIPPermissions = Nothing,
      _asgiIPProtocol = Nothing,
      _asgiGroupId = Nothing,
      _asgiToPort = Nothing,
      _asgiCidrIP = Nothing,
      _asgiSourceSecurityGroupOwnerId = Nothing,
      _asgiGroupName = Nothing,
      _asgiSourceSecurityGroupName = Nothing,
      _asgiDryRun = Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all types. If you specify all ICMP types, you must specify all codes. Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
asgiFromPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Int)
asgiFromPort = lens _asgiFromPort (\s a -> s {_asgiFromPort = a})

-- | The sets of IP permissions.
asgiIPPermissions :: Lens' AuthorizeSecurityGroupIngress [IPPermission]
asgiIPPermissions = lens _asgiIPPermissions (\s a -> s {_asgiIPPermissions = a}) . _Default . _Coerce

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). To specify @icmpv6@ , use a set of IP permissions. [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a protocol other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports you specify. Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
asgiIPProtocol :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiIPProtocol = lens _asgiIPProtocol (\s a -> s {_asgiIPProtocol = a})

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
asgiGroupId :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiGroupId = lens _asgiGroupId (\s a -> s {_asgiGroupId = a})

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all codes. If you specify all ICMP types, you must specify all codes. Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
asgiToPort :: Lens' AuthorizeSecurityGroupIngress (Maybe Int)
asgiToPort = lens _asgiToPort (\s a -> s {_asgiToPort = a})

-- | The IPv4 address range, in CIDR format. You can't specify this parameter when specifying a source security group. To specify an IPv6 address range, use a set of IP permissions. Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
asgiCidrIP :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiCidrIP = lens _asgiCidrIP (\s a -> s {_asgiCidrIP = a})

-- | [nondefault VPC] The AWS account ID for the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead.
asgiSourceSecurityGroupOwnerId :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiSourceSecurityGroupOwnerId = lens _asgiSourceSecurityGroupOwnerId (\s a -> s {_asgiSourceSecurityGroupOwnerId = a})

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
asgiGroupName :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiGroupName = lens _asgiGroupName (\s a -> s {_asgiGroupName = a})

-- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead. For EC2-VPC, the source security group must be in the same VPC.
asgiSourceSecurityGroupName :: Lens' AuthorizeSecurityGroupIngress (Maybe Text)
asgiSourceSecurityGroupName = lens _asgiSourceSecurityGroupName (\s a -> s {_asgiSourceSecurityGroupName = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
asgiDryRun :: Lens' AuthorizeSecurityGroupIngress (Maybe Bool)
asgiDryRun = lens _asgiDryRun (\s a -> s {_asgiDryRun = a})

instance AWSRequest AuthorizeSecurityGroupIngress where
  type
    Rs AuthorizeSecurityGroupIngress =
      AuthorizeSecurityGroupIngressResponse
  request = postQuery ec2
  response = receiveNull AuthorizeSecurityGroupIngressResponse'

instance Hashable AuthorizeSecurityGroupIngress

instance NFData AuthorizeSecurityGroupIngress

instance ToHeaders AuthorizeSecurityGroupIngress where
  toHeaders = const mempty

instance ToPath AuthorizeSecurityGroupIngress where
  toPath = const "/"

instance ToQuery AuthorizeSecurityGroupIngress where
  toQuery AuthorizeSecurityGroupIngress' {..} =
    mconcat
      [ "Action" =: ("AuthorizeSecurityGroupIngress" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "FromPort" =: _asgiFromPort,
        toQuery (toQueryList "IpPermissions" <$> _asgiIPPermissions),
        "IpProtocol" =: _asgiIPProtocol,
        "GroupId" =: _asgiGroupId,
        "ToPort" =: _asgiToPort,
        "CidrIp" =: _asgiCidrIP,
        "SourceSecurityGroupOwnerId" =: _asgiSourceSecurityGroupOwnerId,
        "GroupName" =: _asgiGroupName,
        "SourceSecurityGroupName" =: _asgiSourceSecurityGroupName,
        "DryRun" =: _asgiDryRun
      ]

-- | /See:/ 'authorizeSecurityGroupIngressResponse' smart constructor.
data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthorizeSecurityGroupIngressResponse' with the minimum fields required to make a request.
authorizeSecurityGroupIngressResponse ::
  AuthorizeSecurityGroupIngressResponse
authorizeSecurityGroupIngressResponse =
  AuthorizeSecurityGroupIngressResponse'

instance NFData AuthorizeSecurityGroupIngressResponse
