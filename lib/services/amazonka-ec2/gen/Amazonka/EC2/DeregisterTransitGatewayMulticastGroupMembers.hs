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
-- Module      : Amazonka.EC2.DeregisterTransitGatewayMulticastGroupMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified members (network interfaces) from the transit
-- gateway multicast group.
module Amazonka.EC2.DeregisterTransitGatewayMulticastGroupMembers
  ( -- * Creating a Request
    DeregisterTransitGatewayMulticastGroupMembers (..),
    newDeregisterTransitGatewayMulticastGroupMembers,

    -- * Request Lenses
    deregisterTransitGatewayMulticastGroupMembers_dryRun,
    deregisterTransitGatewayMulticastGroupMembers_groupIpAddress,
    deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,

    -- * Destructuring the Response
    DeregisterTransitGatewayMulticastGroupMembersResponse (..),
    newDeregisterTransitGatewayMulticastGroupMembersResponse,

    -- * Response Lenses
    deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers,
    deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterTransitGatewayMulticastGroupMembers' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembers = DeregisterTransitGatewayMulticastGroupMembers'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the group members\' network interfaces.
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'groupIpAddress', 'deregisterTransitGatewayMulticastGroupMembers_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
--
-- 'networkInterfaceIds', 'deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds' - The IDs of the group members\' network interfaces.
--
-- 'transitGatewayMulticastDomainId', 'deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newDeregisterTransitGatewayMulticastGroupMembers ::
  DeregisterTransitGatewayMulticastGroupMembers
newDeregisterTransitGatewayMulticastGroupMembers =
  DeregisterTransitGatewayMulticastGroupMembers'
    { dryRun =
        Prelude.Nothing,
      groupIpAddress =
        Prelude.Nothing,
      networkInterfaceIds =
        Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deregisterTransitGatewayMulticastGroupMembers_dryRun :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Prelude.Maybe Prelude.Bool)
deregisterTransitGatewayMulticastGroupMembers_dryRun = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembers' {dryRun} -> dryRun) (\s@DeregisterTransitGatewayMulticastGroupMembers' {} a -> s {dryRun = a} :: DeregisterTransitGatewayMulticastGroupMembers)

-- | The IP address assigned to the transit gateway multicast group.
deregisterTransitGatewayMulticastGroupMembers_groupIpAddress :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Prelude.Maybe Prelude.Text)
deregisterTransitGatewayMulticastGroupMembers_groupIpAddress = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembers' {groupIpAddress} -> groupIpAddress) (\s@DeregisterTransitGatewayMulticastGroupMembers' {} a -> s {groupIpAddress = a} :: DeregisterTransitGatewayMulticastGroupMembers)

-- | The IDs of the group members\' network interfaces.
deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Prelude.Maybe [Prelude.Text])
deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembers' {networkInterfaceIds} -> networkInterfaceIds) (\s@DeregisterTransitGatewayMulticastGroupMembers' {} a -> s {networkInterfaceIds = a} :: DeregisterTransitGatewayMulticastGroupMembers) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway multicast domain.
deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Prelude.Maybe Prelude.Text)
deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembers' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@DeregisterTransitGatewayMulticastGroupMembers' {} a -> s {transitGatewayMulticastDomainId = a} :: DeregisterTransitGatewayMulticastGroupMembers)

instance
  Core.AWSRequest
    DeregisterTransitGatewayMulticastGroupMembers
  where
  type
    AWSResponse
      DeregisterTransitGatewayMulticastGroupMembers =
      DeregisterTransitGatewayMulticastGroupMembersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeregisterTransitGatewayMulticastGroupMembersResponse'
            Prelude.<$> (x Data..@? "deregisteredMulticastGroupMembers")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterTransitGatewayMulticastGroupMembers
  where
  hashWithSalt
    _salt
    DeregisterTransitGatewayMulticastGroupMembers' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` groupIpAddress
        `Prelude.hashWithSalt` networkInterfaceIds
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    DeregisterTransitGatewayMulticastGroupMembers
  where
  rnf
    DeregisterTransitGatewayMulticastGroupMembers' {..} =
      Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf groupIpAddress
        `Prelude.seq` Prelude.rnf networkInterfaceIds
        `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainId

instance
  Data.ToHeaders
    DeregisterTransitGatewayMulticastGroupMembers
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeregisterTransitGatewayMulticastGroupMembers
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeregisterTransitGatewayMulticastGroupMembers
  where
  toQuery
    DeregisterTransitGatewayMulticastGroupMembers' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DeregisterTransitGatewayMulticastGroupMembers" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          "GroupIpAddress" Data.=: groupIpAddress,
          Data.toQuery
            ( Data.toQueryList "NetworkInterfaceIds"
                Prelude.<$> networkInterfaceIds
            ),
          "TransitGatewayMulticastDomainId"
            Data.=: transitGatewayMulticastDomainId
        ]

-- | /See:/ 'newDeregisterTransitGatewayMulticastGroupMembersResponse' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembersResponse = DeregisterTransitGatewayMulticastGroupMembersResponse'
  { -- | Information about the deregistered members.
    deregisteredMulticastGroupMembers :: Prelude.Maybe TransitGatewayMulticastDeregisteredGroupMembers,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeregisterTransitGatewayMulticastGroupMembersResponse
newDeregisterTransitGatewayMulticastGroupMembersResponse
  pHttpStatus_ =
    DeregisterTransitGatewayMulticastGroupMembersResponse'
      { deregisteredMulticastGroupMembers =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the deregistered members.
deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse (Prelude.Maybe TransitGatewayMulticastDeregisteredGroupMembers)
deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembersResponse' {deregisteredMulticastGroupMembers} -> deregisteredMulticastGroupMembers) (\s@DeregisterTransitGatewayMulticastGroupMembersResponse' {} a -> s {deregisteredMulticastGroupMembers = a} :: DeregisterTransitGatewayMulticastGroupMembersResponse)

-- | The response's http status code.
deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse Prelude.Int
deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus = Lens.lens (\DeregisterTransitGatewayMulticastGroupMembersResponse' {httpStatus} -> httpStatus) (\s@DeregisterTransitGatewayMulticastGroupMembersResponse' {} a -> s {httpStatus = a} :: DeregisterTransitGatewayMulticastGroupMembersResponse)

instance
  Prelude.NFData
    DeregisterTransitGatewayMulticastGroupMembersResponse
  where
  rnf
    DeregisterTransitGatewayMulticastGroupMembersResponse' {..} =
      Prelude.rnf deregisteredMulticastGroupMembers
        `Prelude.seq` Prelude.rnf httpStatus
