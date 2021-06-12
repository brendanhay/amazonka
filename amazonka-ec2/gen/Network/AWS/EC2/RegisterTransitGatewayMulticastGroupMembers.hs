{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers members (network interfaces) with the transit gateway
-- multicast group. A member is a network interface associated with a
-- supported EC2 instance that receives multicast traffic. For information
-- about supported instances, see
-- <https://docs.aws.amazon.com/vpc/latest/tgw/transit-gateway-limits.html#multicast-limits Multicast Consideration>
-- in /Amazon VPC Transit Gateways/.
--
-- After you add the members, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SearchTransitGatewayMulticastGroups.html SearchTransitGatewayMulticastGroups>
-- to verify that the members were added to the transit gateway multicast
-- group.
module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
  ( -- * Creating a Request
    RegisterTransitGatewayMulticastGroupMembers (..),
    newRegisterTransitGatewayMulticastGroupMembers,

    -- * Request Lenses
    registerTransitGatewayMulticastGroupMembers_dryRun,
    registerTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    registerTransitGatewayMulticastGroupMembers_groupIpAddress,

    -- * Destructuring the Response
    RegisterTransitGatewayMulticastGroupMembersResponse (..),
    newRegisterTransitGatewayMulticastGroupMembersResponse,

    -- * Response Lenses
    registerTransitGatewayMulticastGroupMembersResponse_registeredMulticastGroupMembers,
    registerTransitGatewayMulticastGroupMembersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterTransitGatewayMulticastGroupMembers' smart constructor.
data RegisterTransitGatewayMulticastGroupMembers = RegisterTransitGatewayMulticastGroupMembers'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The group members\' network interface IDs to register with the transit
    -- gateway multicast group.
    networkInterfaceIds :: Core.Maybe [Core.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterTransitGatewayMulticastGroupMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'registerTransitGatewayMulticastGroupMembers_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayMulticastDomainId', 'registerTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'networkInterfaceIds', 'registerTransitGatewayMulticastGroupMembers_networkInterfaceIds' - The group members\' network interface IDs to register with the transit
-- gateway multicast group.
--
-- 'groupIpAddress', 'registerTransitGatewayMulticastGroupMembers_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
newRegisterTransitGatewayMulticastGroupMembers ::
  RegisterTransitGatewayMulticastGroupMembers
newRegisterTransitGatewayMulticastGroupMembers =
  RegisterTransitGatewayMulticastGroupMembers'
    { dryRun =
        Core.Nothing,
      transitGatewayMulticastDomainId =
        Core.Nothing,
      networkInterfaceIds =
        Core.Nothing,
      groupIpAddress = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
registerTransitGatewayMulticastGroupMembers_dryRun :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Bool)
registerTransitGatewayMulticastGroupMembers_dryRun = Lens.lens (\RegisterTransitGatewayMulticastGroupMembers' {dryRun} -> dryRun) (\s@RegisterTransitGatewayMulticastGroupMembers' {} a -> s {dryRun = a} :: RegisterTransitGatewayMulticastGroupMembers)

-- | The ID of the transit gateway multicast domain.
registerTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Text)
registerTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId = Lens.lens (\RegisterTransitGatewayMulticastGroupMembers' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@RegisterTransitGatewayMulticastGroupMembers' {} a -> s {transitGatewayMulticastDomainId = a} :: RegisterTransitGatewayMulticastGroupMembers)

-- | The group members\' network interface IDs to register with the transit
-- gateway multicast group.
registerTransitGatewayMulticastGroupMembers_networkInterfaceIds :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe [Core.Text])
registerTransitGatewayMulticastGroupMembers_networkInterfaceIds = Lens.lens (\RegisterTransitGatewayMulticastGroupMembers' {networkInterfaceIds} -> networkInterfaceIds) (\s@RegisterTransitGatewayMulticastGroupMembers' {} a -> s {networkInterfaceIds = a} :: RegisterTransitGatewayMulticastGroupMembers) Core.. Lens.mapping Lens._Coerce

-- | The IP address assigned to the transit gateway multicast group.
registerTransitGatewayMulticastGroupMembers_groupIpAddress :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Text)
registerTransitGatewayMulticastGroupMembers_groupIpAddress = Lens.lens (\RegisterTransitGatewayMulticastGroupMembers' {groupIpAddress} -> groupIpAddress) (\s@RegisterTransitGatewayMulticastGroupMembers' {} a -> s {groupIpAddress = a} :: RegisterTransitGatewayMulticastGroupMembers)

instance
  Core.AWSRequest
    RegisterTransitGatewayMulticastGroupMembers
  where
  type
    AWSResponse
      RegisterTransitGatewayMulticastGroupMembers =
      RegisterTransitGatewayMulticastGroupMembersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterTransitGatewayMulticastGroupMembersResponse'
            Core.<$> (x Core..@? "registeredMulticastGroupMembers")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RegisterTransitGatewayMulticastGroupMembers

instance
  Core.NFData
    RegisterTransitGatewayMulticastGroupMembers

instance
  Core.ToHeaders
    RegisterTransitGatewayMulticastGroupMembers
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    RegisterTransitGatewayMulticastGroupMembers
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    RegisterTransitGatewayMulticastGroupMembers
  where
  toQuery
    RegisterTransitGatewayMulticastGroupMembers' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "RegisterTransitGatewayMulticastGroupMembers" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "DryRun" Core.=: dryRun,
          "TransitGatewayMulticastDomainId"
            Core.=: transitGatewayMulticastDomainId,
          Core.toQuery
            ( Core.toQueryList "NetworkInterfaceIds"
                Core.<$> networkInterfaceIds
            ),
          "GroupIpAddress" Core.=: groupIpAddress
        ]

-- | /See:/ 'newRegisterTransitGatewayMulticastGroupMembersResponse' smart constructor.
data RegisterTransitGatewayMulticastGroupMembersResponse = RegisterTransitGatewayMulticastGroupMembersResponse'
  { -- | Information about the registered transit gateway multicast group
    -- members.
    registeredMulticastGroupMembers :: Core.Maybe TransitGatewayMulticastRegisteredGroupMembers,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterTransitGatewayMulticastGroupMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registeredMulticastGroupMembers', 'registerTransitGatewayMulticastGroupMembersResponse_registeredMulticastGroupMembers' - Information about the registered transit gateway multicast group
-- members.
--
-- 'httpStatus', 'registerTransitGatewayMulticastGroupMembersResponse_httpStatus' - The response's http status code.
newRegisterTransitGatewayMulticastGroupMembersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterTransitGatewayMulticastGroupMembersResponse
newRegisterTransitGatewayMulticastGroupMembersResponse
  pHttpStatus_ =
    RegisterTransitGatewayMulticastGroupMembersResponse'
      { registeredMulticastGroupMembers =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the registered transit gateway multicast group
-- members.
registerTransitGatewayMulticastGroupMembersResponse_registeredMulticastGroupMembers :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembersResponse (Core.Maybe TransitGatewayMulticastRegisteredGroupMembers)
registerTransitGatewayMulticastGroupMembersResponse_registeredMulticastGroupMembers = Lens.lens (\RegisterTransitGatewayMulticastGroupMembersResponse' {registeredMulticastGroupMembers} -> registeredMulticastGroupMembers) (\s@RegisterTransitGatewayMulticastGroupMembersResponse' {} a -> s {registeredMulticastGroupMembers = a} :: RegisterTransitGatewayMulticastGroupMembersResponse)

-- | The response's http status code.
registerTransitGatewayMulticastGroupMembersResponse_httpStatus :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembersResponse Core.Int
registerTransitGatewayMulticastGroupMembersResponse_httpStatus = Lens.lens (\RegisterTransitGatewayMulticastGroupMembersResponse' {httpStatus} -> httpStatus) (\s@RegisterTransitGatewayMulticastGroupMembersResponse' {} a -> s {httpStatus = a} :: RegisterTransitGatewayMulticastGroupMembersResponse)

instance
  Core.NFData
    RegisterTransitGatewayMulticastGroupMembersResponse
