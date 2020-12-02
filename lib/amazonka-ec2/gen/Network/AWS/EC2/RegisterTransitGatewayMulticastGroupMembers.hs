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
-- Module      : Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers members (network interfaces) with the transit gateway multicast group. A member is a network interface associated with a supported EC2 instance that receives multicast traffic. For information about supported instances, see <https://docs.aws.amazon.com/vpc/latest/tgw/transit-gateway-limits.html#multicast-limits Multicast Consideration> in /Amazon VPC Transit Gateways/ .
--
--
-- After you add the members, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SearchTransitGatewayMulticastGroups.html SearchTransitGatewayMulticastGroups> to verify that the members were added to the transit gateway multicast group.
module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
  ( -- * Creating a Request
    registerTransitGatewayMulticastGroupMembers,
    RegisterTransitGatewayMulticastGroupMembers,

    -- * Request Lenses
    rtgmgmNetworkInterfaceIds,
    rtgmgmTransitGatewayMulticastDomainId,
    rtgmgmGroupIPAddress,
    rtgmgmDryRun,

    -- * Destructuring the Response
    registerTransitGatewayMulticastGroupMembersResponse,
    RegisterTransitGatewayMulticastGroupMembersResponse,

    -- * Response Lenses
    rtgmgmrsRegisteredMulticastGroupMembers,
    rtgmgmrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerTransitGatewayMulticastGroupMembers' smart constructor.
data RegisterTransitGatewayMulticastGroupMembers = RegisterTransitGatewayMulticastGroupMembers'
  { _rtgmgmNetworkInterfaceIds ::
      !( Maybe
           [Text]
       ),
    _rtgmgmTransitGatewayMulticastDomainId ::
      !( Maybe
           Text
       ),
    _rtgmgmGroupIPAddress ::
      !( Maybe
           Text
       ),
    _rtgmgmDryRun ::
      !( Maybe
           Bool
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'RegisterTransitGatewayMulticastGroupMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtgmgmNetworkInterfaceIds' - The group members' network interface IDs to register with the transit gateway multicast group.
--
-- * 'rtgmgmTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'rtgmgmGroupIPAddress' - The IP address assigned to the transit gateway multicast group.
--
-- * 'rtgmgmDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
registerTransitGatewayMulticastGroupMembers ::
  RegisterTransitGatewayMulticastGroupMembers
registerTransitGatewayMulticastGroupMembers =
  RegisterTransitGatewayMulticastGroupMembers'
    { _rtgmgmNetworkInterfaceIds =
        Nothing,
      _rtgmgmTransitGatewayMulticastDomainId = Nothing,
      _rtgmgmGroupIPAddress = Nothing,
      _rtgmgmDryRun = Nothing
    }

-- | The group members' network interface IDs to register with the transit gateway multicast group.
rtgmgmNetworkInterfaceIds :: Lens' RegisterTransitGatewayMulticastGroupMembers [Text]
rtgmgmNetworkInterfaceIds = lens _rtgmgmNetworkInterfaceIds (\s a -> s {_rtgmgmNetworkInterfaceIds = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
rtgmgmTransitGatewayMulticastDomainId :: Lens' RegisterTransitGatewayMulticastGroupMembers (Maybe Text)
rtgmgmTransitGatewayMulticastDomainId = lens _rtgmgmTransitGatewayMulticastDomainId (\s a -> s {_rtgmgmTransitGatewayMulticastDomainId = a})

-- | The IP address assigned to the transit gateway multicast group.
rtgmgmGroupIPAddress :: Lens' RegisterTransitGatewayMulticastGroupMembers (Maybe Text)
rtgmgmGroupIPAddress = lens _rtgmgmGroupIPAddress (\s a -> s {_rtgmgmGroupIPAddress = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rtgmgmDryRun :: Lens' RegisterTransitGatewayMulticastGroupMembers (Maybe Bool)
rtgmgmDryRun = lens _rtgmgmDryRun (\s a -> s {_rtgmgmDryRun = a})

instance AWSRequest RegisterTransitGatewayMulticastGroupMembers where
  type
    Rs RegisterTransitGatewayMulticastGroupMembers =
      RegisterTransitGatewayMulticastGroupMembersResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          RegisterTransitGatewayMulticastGroupMembersResponse'
            <$> (x .@? "registeredMulticastGroupMembers") <*> (pure (fromEnum s))
      )

instance Hashable RegisterTransitGatewayMulticastGroupMembers

instance NFData RegisterTransitGatewayMulticastGroupMembers

instance ToHeaders RegisterTransitGatewayMulticastGroupMembers where
  toHeaders = const mempty

instance ToPath RegisterTransitGatewayMulticastGroupMembers where
  toPath = const "/"

instance ToQuery RegisterTransitGatewayMulticastGroupMembers where
  toQuery RegisterTransitGatewayMulticastGroupMembers' {..} =
    mconcat
      [ "Action"
          =: ("RegisterTransitGatewayMulticastGroupMembers" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          (toQueryList "NetworkInterfaceIds" <$> _rtgmgmNetworkInterfaceIds),
        "TransitGatewayMulticastDomainId"
          =: _rtgmgmTransitGatewayMulticastDomainId,
        "GroupIpAddress" =: _rtgmgmGroupIPAddress,
        "DryRun" =: _rtgmgmDryRun
      ]

-- | /See:/ 'registerTransitGatewayMulticastGroupMembersResponse' smart constructor.
data RegisterTransitGatewayMulticastGroupMembersResponse = RegisterTransitGatewayMulticastGroupMembersResponse'
  { _rtgmgmrsRegisteredMulticastGroupMembers ::
      !( Maybe
           TransitGatewayMulticastRegisteredGroupMembers
       ),
    _rtgmgmrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'RegisterTransitGatewayMulticastGroupMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtgmgmrsRegisteredMulticastGroupMembers' - Information about the registered transit gateway multicast group members.
--
-- * 'rtgmgmrsResponseStatus' - -- | The response status code.
registerTransitGatewayMulticastGroupMembersResponse ::
  -- | 'rtgmgmrsResponseStatus'
  Int ->
  RegisterTransitGatewayMulticastGroupMembersResponse
registerTransitGatewayMulticastGroupMembersResponse
  pResponseStatus_ =
    RegisterTransitGatewayMulticastGroupMembersResponse'
      { _rtgmgmrsRegisteredMulticastGroupMembers =
          Nothing,
        _rtgmgmrsResponseStatus = pResponseStatus_
      }

-- | Information about the registered transit gateway multicast group members.
rtgmgmrsRegisteredMulticastGroupMembers :: Lens' RegisterTransitGatewayMulticastGroupMembersResponse (Maybe TransitGatewayMulticastRegisteredGroupMembers)
rtgmgmrsRegisteredMulticastGroupMembers = lens _rtgmgmrsRegisteredMulticastGroupMembers (\s a -> s {_rtgmgmrsRegisteredMulticastGroupMembers = a})

-- | -- | The response status code.
rtgmgmrsResponseStatus :: Lens' RegisterTransitGatewayMulticastGroupMembersResponse Int
rtgmgmrsResponseStatus = lens _rtgmgmrsResponseStatus (\s a -> s {_rtgmgmrsResponseStatus = a})

instance NFData RegisterTransitGatewayMulticastGroupMembersResponse
