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
-- Module      : Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified members (network interfaces) from the transit
-- gateway multicast group.
module Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers
  ( -- * Creating a Request
    DeregisterTransitGatewayMulticastGroupMembers (..),
    newDeregisterTransitGatewayMulticastGroupMembers,

    -- * Request Lenses
    deregisterTransitGatewayMulticastGroupMembers_dryRun,
    deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupMembers_groupIpAddress,

    -- * Destructuring the Response
    DeregisterTransitGatewayMulticastGroupMembersResponse (..),
    newDeregisterTransitGatewayMulticastGroupMembersResponse,

    -- * Response Lenses
    deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers,
    deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterTransitGatewayMulticastGroupMembers' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembers = DeregisterTransitGatewayMulticastGroupMembers'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The IDs of the group members\' network interfaces.
    networkInterfaceIds :: Core.Maybe [Core.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterTransitGatewayMulticastGroupMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deregisterTransitGatewayMulticastGroupMembers_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayMulticastDomainId', 'deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'networkInterfaceIds', 'deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds' - The IDs of the group members\' network interfaces.
--
-- 'groupIpAddress', 'deregisterTransitGatewayMulticastGroupMembers_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
newDeregisterTransitGatewayMulticastGroupMembers ::
  DeregisterTransitGatewayMulticastGroupMembers
newDeregisterTransitGatewayMulticastGroupMembers =
  DeregisterTransitGatewayMulticastGroupMembers'
    { dryRun =
        Core.Nothing,
      transitGatewayMulticastDomainId =
        Core.Nothing,
      networkInterfaceIds =
        Core.Nothing,
      groupIpAddress =
        Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deregisterTransitGatewayMulticastGroupMembers_dryRun :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Bool)
deregisterTransitGatewayMulticastGroupMembers_dryRun = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembers' {dryRun} -> dryRun) (\s@DeregisterTransitGatewayMulticastGroupMembers' {} a -> s {dryRun = a} :: DeregisterTransitGatewayMulticastGroupMembers)

-- | The ID of the transit gateway multicast domain.
deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Text)
deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembers' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@DeregisterTransitGatewayMulticastGroupMembers' {} a -> s {transitGatewayMulticastDomainId = a} :: DeregisterTransitGatewayMulticastGroupMembers)

-- | The IDs of the group members\' network interfaces.
deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe [Core.Text])
deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembers' {networkInterfaceIds} -> networkInterfaceIds) (\s@DeregisterTransitGatewayMulticastGroupMembers' {} a -> s {networkInterfaceIds = a} :: DeregisterTransitGatewayMulticastGroupMembers) Core.. Lens.mapping Lens._Coerce

-- | The IP address assigned to the transit gateway multicast group.
deregisterTransitGatewayMulticastGroupMembers_groupIpAddress :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Text)
deregisterTransitGatewayMulticastGroupMembers_groupIpAddress = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembers' {groupIpAddress} -> groupIpAddress) (\s@DeregisterTransitGatewayMulticastGroupMembers' {} a -> s {groupIpAddress = a} :: DeregisterTransitGatewayMulticastGroupMembers)

instance
  Core.AWSRequest
    DeregisterTransitGatewayMulticastGroupMembers
  where
  type
    AWSResponse
      DeregisterTransitGatewayMulticastGroupMembers =
      DeregisterTransitGatewayMulticastGroupMembersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeregisterTransitGatewayMulticastGroupMembersResponse'
            Core.<$> (x Core..@? "deregisteredMulticastGroupMembers")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeregisterTransitGatewayMulticastGroupMembers

instance
  Core.NFData
    DeregisterTransitGatewayMulticastGroupMembers

instance
  Core.ToHeaders
    DeregisterTransitGatewayMulticastGroupMembers
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeregisterTransitGatewayMulticastGroupMembers
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeregisterTransitGatewayMulticastGroupMembers
  where
  toQuery
    DeregisterTransitGatewayMulticastGroupMembers' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "DeregisterTransitGatewayMulticastGroupMembers" ::
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

-- | /See:/ 'newDeregisterTransitGatewayMulticastGroupMembersResponse' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembersResponse = DeregisterTransitGatewayMulticastGroupMembersResponse'
  { -- | Information about the deregistered members.
    deregisteredMulticastGroupMembers :: Core.Maybe TransitGatewayMulticastDeregisteredGroupMembers,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterTransitGatewayMulticastGroupMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deregisteredMulticastGroupMembers', 'deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers' - Information about the deregistered members.
--
-- 'httpStatus', 'deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus' - The response's http status code.
newDeregisterTransitGatewayMulticastGroupMembersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeregisterTransitGatewayMulticastGroupMembersResponse
newDeregisterTransitGatewayMulticastGroupMembersResponse
  pHttpStatus_ =
    DeregisterTransitGatewayMulticastGroupMembersResponse'
      { deregisteredMulticastGroupMembers =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the deregistered members.
deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse (Core.Maybe TransitGatewayMulticastDeregisteredGroupMembers)
deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembersResponse' {deregisteredMulticastGroupMembers} -> deregisteredMulticastGroupMembers) (\s@DeregisterTransitGatewayMulticastGroupMembersResponse' {} a -> s {deregisteredMulticastGroupMembers = a} :: DeregisterTransitGatewayMulticastGroupMembersResponse)

-- | The response's http status code.
deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse Core.Int
deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembersResponse' {httpStatus} -> httpStatus) (\s@DeregisterTransitGatewayMulticastGroupMembersResponse' {} a -> s {httpStatus = a} :: DeregisterTransitGatewayMulticastGroupMembersResponse)

instance
  Core.NFData
    DeregisterTransitGatewayMulticastGroupMembersResponse
