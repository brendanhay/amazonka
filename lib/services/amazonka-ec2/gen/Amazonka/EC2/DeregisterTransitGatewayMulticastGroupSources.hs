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
-- Module      : Amazonka.EC2.DeregisterTransitGatewayMulticastGroupSources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified sources (network interfaces) from the transit
-- gateway multicast group.
module Amazonka.EC2.DeregisterTransitGatewayMulticastGroupSources
  ( -- * Creating a Request
    DeregisterTransitGatewayMulticastGroupSources (..),
    newDeregisterTransitGatewayMulticastGroupSources,

    -- * Request Lenses
    deregisterTransitGatewayMulticastGroupSources_groupIpAddress,
    deregisterTransitGatewayMulticastGroupSources_dryRun,
    deregisterTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupSources_networkInterfaceIds,

    -- * Destructuring the Response
    DeregisterTransitGatewayMulticastGroupSourcesResponse (..),
    newDeregisterTransitGatewayMulticastGroupSourcesResponse,

    -- * Response Lenses
    deregisterTransitGatewayMulticastGroupSourcesResponse_deregisteredMulticastGroupSources,
    deregisterTransitGatewayMulticastGroupSourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterTransitGatewayMulticastGroupSources' smart constructor.
data DeregisterTransitGatewayMulticastGroupSources = DeregisterTransitGatewayMulticastGroupSources'
  { -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the group sources\' network interfaces.
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTransitGatewayMulticastGroupSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupIpAddress', 'deregisterTransitGatewayMulticastGroupSources_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
--
-- 'dryRun', 'deregisterTransitGatewayMulticastGroupSources_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayMulticastDomainId', 'deregisterTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'networkInterfaceIds', 'deregisterTransitGatewayMulticastGroupSources_networkInterfaceIds' - The IDs of the group sources\' network interfaces.
newDeregisterTransitGatewayMulticastGroupSources ::
  DeregisterTransitGatewayMulticastGroupSources
newDeregisterTransitGatewayMulticastGroupSources =
  DeregisterTransitGatewayMulticastGroupSources'
    { groupIpAddress =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing,
      networkInterfaceIds =
        Prelude.Nothing
    }

-- | The IP address assigned to the transit gateway multicast group.
deregisterTransitGatewayMulticastGroupSources_groupIpAddress :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Prelude.Maybe Prelude.Text)
deregisterTransitGatewayMulticastGroupSources_groupIpAddress = Lens.lens (\DeregisterTransitGatewayMulticastGroupSources' {groupIpAddress} -> groupIpAddress) (\s@DeregisterTransitGatewayMulticastGroupSources' {} a -> s {groupIpAddress = a} :: DeregisterTransitGatewayMulticastGroupSources)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deregisterTransitGatewayMulticastGroupSources_dryRun :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Prelude.Maybe Prelude.Bool)
deregisterTransitGatewayMulticastGroupSources_dryRun = Lens.lens (\DeregisterTransitGatewayMulticastGroupSources' {dryRun} -> dryRun) (\s@DeregisterTransitGatewayMulticastGroupSources' {} a -> s {dryRun = a} :: DeregisterTransitGatewayMulticastGroupSources)

-- | The ID of the transit gateway multicast domain.
deregisterTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Prelude.Maybe Prelude.Text)
deregisterTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId = Lens.lens (\DeregisterTransitGatewayMulticastGroupSources' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@DeregisterTransitGatewayMulticastGroupSources' {} a -> s {transitGatewayMulticastDomainId = a} :: DeregisterTransitGatewayMulticastGroupSources)

-- | The IDs of the group sources\' network interfaces.
deregisterTransitGatewayMulticastGroupSources_networkInterfaceIds :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Prelude.Maybe [Prelude.Text])
deregisterTransitGatewayMulticastGroupSources_networkInterfaceIds = Lens.lens (\DeregisterTransitGatewayMulticastGroupSources' {networkInterfaceIds} -> networkInterfaceIds) (\s@DeregisterTransitGatewayMulticastGroupSources' {} a -> s {networkInterfaceIds = a} :: DeregisterTransitGatewayMulticastGroupSources) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    DeregisterTransitGatewayMulticastGroupSources
  where
  type
    AWSResponse
      DeregisterTransitGatewayMulticastGroupSources =
      DeregisterTransitGatewayMulticastGroupSourcesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeregisterTransitGatewayMulticastGroupSourcesResponse'
            Prelude.<$> (x Core..@? "deregisteredMulticastGroupSources")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterTransitGatewayMulticastGroupSources
  where
  hashWithSalt
    _salt
    DeregisterTransitGatewayMulticastGroupSources' {..} =
      _salt `Prelude.hashWithSalt` groupIpAddress
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId
        `Prelude.hashWithSalt` networkInterfaceIds

instance
  Prelude.NFData
    DeregisterTransitGatewayMulticastGroupSources
  where
  rnf
    DeregisterTransitGatewayMulticastGroupSources' {..} =
      Prelude.rnf groupIpAddress
        `Prelude.seq` Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainId
        `Prelude.seq` Prelude.rnf networkInterfaceIds

instance
  Core.ToHeaders
    DeregisterTransitGatewayMulticastGroupSources
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeregisterTransitGatewayMulticastGroupSources
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeregisterTransitGatewayMulticastGroupSources
  where
  toQuery
    DeregisterTransitGatewayMulticastGroupSources' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "DeregisterTransitGatewayMulticastGroupSources" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          "GroupIpAddress" Core.=: groupIpAddress,
          "DryRun" Core.=: dryRun,
          "TransitGatewayMulticastDomainId"
            Core.=: transitGatewayMulticastDomainId,
          Core.toQuery
            ( Core.toQueryList "NetworkInterfaceIds"
                Prelude.<$> networkInterfaceIds
            )
        ]

-- | /See:/ 'newDeregisterTransitGatewayMulticastGroupSourcesResponse' smart constructor.
data DeregisterTransitGatewayMulticastGroupSourcesResponse = DeregisterTransitGatewayMulticastGroupSourcesResponse'
  { -- | Information about the deregistered group sources.
    deregisteredMulticastGroupSources :: Prelude.Maybe TransitGatewayMulticastDeregisteredGroupSources,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTransitGatewayMulticastGroupSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deregisteredMulticastGroupSources', 'deregisterTransitGatewayMulticastGroupSourcesResponse_deregisteredMulticastGroupSources' - Information about the deregistered group sources.
--
-- 'httpStatus', 'deregisterTransitGatewayMulticastGroupSourcesResponse_httpStatus' - The response's http status code.
newDeregisterTransitGatewayMulticastGroupSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterTransitGatewayMulticastGroupSourcesResponse
newDeregisterTransitGatewayMulticastGroupSourcesResponse
  pHttpStatus_ =
    DeregisterTransitGatewayMulticastGroupSourcesResponse'
      { deregisteredMulticastGroupSources =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the deregistered group sources.
deregisterTransitGatewayMulticastGroupSourcesResponse_deregisteredMulticastGroupSources :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSourcesResponse (Prelude.Maybe TransitGatewayMulticastDeregisteredGroupSources)
deregisterTransitGatewayMulticastGroupSourcesResponse_deregisteredMulticastGroupSources = Lens.lens (\DeregisterTransitGatewayMulticastGroupSourcesResponse' {deregisteredMulticastGroupSources} -> deregisteredMulticastGroupSources) (\s@DeregisterTransitGatewayMulticastGroupSourcesResponse' {} a -> s {deregisteredMulticastGroupSources = a} :: DeregisterTransitGatewayMulticastGroupSourcesResponse)

-- | The response's http status code.
deregisterTransitGatewayMulticastGroupSourcesResponse_httpStatus :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSourcesResponse Prelude.Int
deregisterTransitGatewayMulticastGroupSourcesResponse_httpStatus = Lens.lens (\DeregisterTransitGatewayMulticastGroupSourcesResponse' {httpStatus} -> httpStatus) (\s@DeregisterTransitGatewayMulticastGroupSourcesResponse' {} a -> s {httpStatus = a} :: DeregisterTransitGatewayMulticastGroupSourcesResponse)

instance
  Prelude.NFData
    DeregisterTransitGatewayMulticastGroupSourcesResponse
  where
  rnf
    DeregisterTransitGatewayMulticastGroupSourcesResponse' {..} =
      Prelude.rnf deregisteredMulticastGroupSources
        `Prelude.seq` Prelude.rnf httpStatus
