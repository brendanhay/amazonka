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
-- Module      : Network.AWS.EC2.RevokeSecurityGroupEgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Removes the specified egress rules from a security group for EC2-VPC. This action does not apply to security groups for use in EC2-Classic. To remove a rule, the values that you specify (for example, ports) must match the existing rule's values exactly.
--
--
-- Each rule consists of the protocol and the IPv4 or IPv6 CIDR range or source security group. For the TCP and UDP protocols, you must also specify the destination port or range of ports. For the ICMP protocol, you must also specify the ICMP type and code. If the security group rule has a description, you do not have to specify the description to revoke the rule.
--
-- Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.
module Network.AWS.EC2.RevokeSecurityGroupEgress
  ( -- * Creating a Request
    revokeSecurityGroupEgress,
    RevokeSecurityGroupEgress,

    -- * Request Lenses
    rsgeFromPort,
    rsgeIPPermissions,
    rsgeIPProtocol,
    rsgeToPort,
    rsgeCidrIP,
    rsgeSourceSecurityGroupOwnerId,
    rsgeSourceSecurityGroupName,
    rsgeDryRun,
    rsgeGroupId,

    -- * Destructuring the Response
    revokeSecurityGroupEgressResponse,
    RevokeSecurityGroupEgressResponse,

    -- * Response Lenses
    rsgersReturn,
    rsgersUnknownIPPermissions,
    rsgersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'revokeSecurityGroupEgress' smart constructor.
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress'
  { _rsgeFromPort ::
      !(Maybe Int),
    _rsgeIPPermissions ::
      !(Maybe [IPPermission]),
    _rsgeIPProtocol :: !(Maybe Text),
    _rsgeToPort :: !(Maybe Int),
    _rsgeCidrIP :: !(Maybe Text),
    _rsgeSourceSecurityGroupOwnerId ::
      !(Maybe Text),
    _rsgeSourceSecurityGroupName ::
      !(Maybe Text),
    _rsgeDryRun :: !(Maybe Bool),
    _rsgeGroupId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RevokeSecurityGroupEgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsgeFromPort' - Not supported. Use a set of IP permissions to specify the port.
--
-- * 'rsgeIPPermissions' - The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
--
-- * 'rsgeIPProtocol' - Not supported. Use a set of IP permissions to specify the protocol name or number.
--
-- * 'rsgeToPort' - Not supported. Use a set of IP permissions to specify the port.
--
-- * 'rsgeCidrIP' - Not supported. Use a set of IP permissions to specify the CIDR.
--
-- * 'rsgeSourceSecurityGroupOwnerId' - Not supported. Use a set of IP permissions to specify a destination security group.
--
-- * 'rsgeSourceSecurityGroupName' - Not supported. Use a set of IP permissions to specify a destination security group.
--
-- * 'rsgeDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rsgeGroupId' - The ID of the security group.
revokeSecurityGroupEgress ::
  -- | 'rsgeGroupId'
  Text ->
  RevokeSecurityGroupEgress
revokeSecurityGroupEgress pGroupId_ =
  RevokeSecurityGroupEgress'
    { _rsgeFromPort = Nothing,
      _rsgeIPPermissions = Nothing,
      _rsgeIPProtocol = Nothing,
      _rsgeToPort = Nothing,
      _rsgeCidrIP = Nothing,
      _rsgeSourceSecurityGroupOwnerId = Nothing,
      _rsgeSourceSecurityGroupName = Nothing,
      _rsgeDryRun = Nothing,
      _rsgeGroupId = pGroupId_
    }

-- | Not supported. Use a set of IP permissions to specify the port.
rsgeFromPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgeFromPort = lens _rsgeFromPort (\s a -> s {_rsgeFromPort = a})

-- | The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
rsgeIPPermissions :: Lens' RevokeSecurityGroupEgress [IPPermission]
rsgeIPPermissions = lens _rsgeIPPermissions (\s a -> s {_rsgeIPPermissions = a}) . _Default . _Coerce

-- | Not supported. Use a set of IP permissions to specify the protocol name or number.
rsgeIPProtocol :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeIPProtocol = lens _rsgeIPProtocol (\s a -> s {_rsgeIPProtocol = a})

-- | Not supported. Use a set of IP permissions to specify the port.
rsgeToPort :: Lens' RevokeSecurityGroupEgress (Maybe Int)
rsgeToPort = lens _rsgeToPort (\s a -> s {_rsgeToPort = a})

-- | Not supported. Use a set of IP permissions to specify the CIDR.
rsgeCidrIP :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeCidrIP = lens _rsgeCidrIP (\s a -> s {_rsgeCidrIP = a})

-- | Not supported. Use a set of IP permissions to specify a destination security group.
rsgeSourceSecurityGroupOwnerId :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupOwnerId = lens _rsgeSourceSecurityGroupOwnerId (\s a -> s {_rsgeSourceSecurityGroupOwnerId = a})

-- | Not supported. Use a set of IP permissions to specify a destination security group.
rsgeSourceSecurityGroupName :: Lens' RevokeSecurityGroupEgress (Maybe Text)
rsgeSourceSecurityGroupName = lens _rsgeSourceSecurityGroupName (\s a -> s {_rsgeSourceSecurityGroupName = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rsgeDryRun :: Lens' RevokeSecurityGroupEgress (Maybe Bool)
rsgeDryRun = lens _rsgeDryRun (\s a -> s {_rsgeDryRun = a})

-- | The ID of the security group.
rsgeGroupId :: Lens' RevokeSecurityGroupEgress Text
rsgeGroupId = lens _rsgeGroupId (\s a -> s {_rsgeGroupId = a})

instance AWSRequest RevokeSecurityGroupEgress where
  type
    Rs RevokeSecurityGroupEgress =
      RevokeSecurityGroupEgressResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          RevokeSecurityGroupEgressResponse'
            <$> (x .@? "return")
            <*> ( x .@? "unknownIpPermissionSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable RevokeSecurityGroupEgress

instance NFData RevokeSecurityGroupEgress

instance ToHeaders RevokeSecurityGroupEgress where
  toHeaders = const mempty

instance ToPath RevokeSecurityGroupEgress where
  toPath = const "/"

instance ToQuery RevokeSecurityGroupEgress where
  toQuery RevokeSecurityGroupEgress' {..} =
    mconcat
      [ "Action" =: ("RevokeSecurityGroupEgress" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "FromPort" =: _rsgeFromPort,
        toQuery (toQueryList "IpPermissions" <$> _rsgeIPPermissions),
        "IpProtocol" =: _rsgeIPProtocol,
        "ToPort" =: _rsgeToPort,
        "CidrIp" =: _rsgeCidrIP,
        "SourceSecurityGroupOwnerId" =: _rsgeSourceSecurityGroupOwnerId,
        "SourceSecurityGroupName" =: _rsgeSourceSecurityGroupName,
        "DryRun" =: _rsgeDryRun,
        "GroupId" =: _rsgeGroupId
      ]

-- | /See:/ 'revokeSecurityGroupEgressResponse' smart constructor.
data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse'
  { _rsgersReturn ::
      !(Maybe Bool),
    _rsgersUnknownIPPermissions ::
      !(Maybe [IPPermission]),
    _rsgersResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RevokeSecurityGroupEgressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsgersReturn' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- * 'rsgersUnknownIPPermissions' - The outbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter.
--
-- * 'rsgersResponseStatus' - -- | The response status code.
revokeSecurityGroupEgressResponse ::
  -- | 'rsgersResponseStatus'
  Int ->
  RevokeSecurityGroupEgressResponse
revokeSecurityGroupEgressResponse pResponseStatus_ =
  RevokeSecurityGroupEgressResponse'
    { _rsgersReturn = Nothing,
      _rsgersUnknownIPPermissions = Nothing,
      _rsgersResponseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
rsgersReturn :: Lens' RevokeSecurityGroupEgressResponse (Maybe Bool)
rsgersReturn = lens _rsgersReturn (\s a -> s {_rsgersReturn = a})

-- | The outbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter.
rsgersUnknownIPPermissions :: Lens' RevokeSecurityGroupEgressResponse [IPPermission]
rsgersUnknownIPPermissions = lens _rsgersUnknownIPPermissions (\s a -> s {_rsgersUnknownIPPermissions = a}) . _Default . _Coerce

-- | -- | The response status code.
rsgersResponseStatus :: Lens' RevokeSecurityGroupEgressResponse Int
rsgersResponseStatus = lens _rsgersResponseStatus (\s a -> s {_rsgersResponseStatus = a})

instance NFData RevokeSecurityGroupEgressResponse
