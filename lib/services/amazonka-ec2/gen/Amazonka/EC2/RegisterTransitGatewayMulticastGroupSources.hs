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
-- Module      : Amazonka.EC2.RegisterTransitGatewayMulticastGroupSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers sources (network interfaces) with the specified transit
-- gateway multicast group.
--
-- A multicast source is a network interface attached to a supported
-- instance that sends multicast traffic. For information about supported
-- instances, see
-- <https://docs.aws.amazon.com/vpc/latest/tgw/transit-gateway-limits.html#multicast-limits Multicast Considerations>
-- in /Amazon VPC Transit Gateways/.
--
-- After you add the source, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SearchTransitGatewayMulticastGroups.html SearchTransitGatewayMulticastGroups>
-- to verify that the source was added to the multicast group.
module Amazonka.EC2.RegisterTransitGatewayMulticastGroupSources
  ( -- * Creating a Request
    RegisterTransitGatewayMulticastGroupSources (..),
    newRegisterTransitGatewayMulticastGroupSources,

    -- * Request Lenses
    registerTransitGatewayMulticastGroupSources_dryRun,
    registerTransitGatewayMulticastGroupSources_groupIpAddress,
    registerTransitGatewayMulticastGroupSources_networkInterfaceIds,
    registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,

    -- * Destructuring the Response
    RegisterTransitGatewayMulticastGroupSourcesResponse (..),
    newRegisterTransitGatewayMulticastGroupSourcesResponse,

    -- * Response Lenses
    registerTransitGatewayMulticastGroupSourcesResponse_registeredMulticastGroupSources,
    registerTransitGatewayMulticastGroupSourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterTransitGatewayMulticastGroupSources' smart constructor.
data RegisterTransitGatewayMulticastGroupSources = RegisterTransitGatewayMulticastGroupSources'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The group sources\' network interface IDs to register with the transit
    -- gateway multicast group.
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTransitGatewayMulticastGroupSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'registerTransitGatewayMulticastGroupSources_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupIpAddress', 'registerTransitGatewayMulticastGroupSources_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
--
-- 'networkInterfaceIds', 'registerTransitGatewayMulticastGroupSources_networkInterfaceIds' - The group sources\' network interface IDs to register with the transit
-- gateway multicast group.
--
-- 'transitGatewayMulticastDomainId', 'registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newRegisterTransitGatewayMulticastGroupSources ::
  RegisterTransitGatewayMulticastGroupSources
newRegisterTransitGatewayMulticastGroupSources =
  RegisterTransitGatewayMulticastGroupSources'
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
registerTransitGatewayMulticastGroupSources_dryRun :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Prelude.Maybe Prelude.Bool)
registerTransitGatewayMulticastGroupSources_dryRun = Lens.lens (\RegisterTransitGatewayMulticastGroupSources' {dryRun} -> dryRun) (\s@RegisterTransitGatewayMulticastGroupSources' {} a -> s {dryRun = a} :: RegisterTransitGatewayMulticastGroupSources)

-- | The IP address assigned to the transit gateway multicast group.
registerTransitGatewayMulticastGroupSources_groupIpAddress :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Prelude.Maybe Prelude.Text)
registerTransitGatewayMulticastGroupSources_groupIpAddress = Lens.lens (\RegisterTransitGatewayMulticastGroupSources' {groupIpAddress} -> groupIpAddress) (\s@RegisterTransitGatewayMulticastGroupSources' {} a -> s {groupIpAddress = a} :: RegisterTransitGatewayMulticastGroupSources)

-- | The group sources\' network interface IDs to register with the transit
-- gateway multicast group.
registerTransitGatewayMulticastGroupSources_networkInterfaceIds :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Prelude.Maybe [Prelude.Text])
registerTransitGatewayMulticastGroupSources_networkInterfaceIds = Lens.lens (\RegisterTransitGatewayMulticastGroupSources' {networkInterfaceIds} -> networkInterfaceIds) (\s@RegisterTransitGatewayMulticastGroupSources' {} a -> s {networkInterfaceIds = a} :: RegisterTransitGatewayMulticastGroupSources) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway multicast domain.
registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Prelude.Maybe Prelude.Text)
registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId = Lens.lens (\RegisterTransitGatewayMulticastGroupSources' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@RegisterTransitGatewayMulticastGroupSources' {} a -> s {transitGatewayMulticastDomainId = a} :: RegisterTransitGatewayMulticastGroupSources)

instance
  Core.AWSRequest
    RegisterTransitGatewayMulticastGroupSources
  where
  type
    AWSResponse
      RegisterTransitGatewayMulticastGroupSources =
      RegisterTransitGatewayMulticastGroupSourcesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterTransitGatewayMulticastGroupSourcesResponse'
            Prelude.<$> (x Data..@? "registeredMulticastGroupSources")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterTransitGatewayMulticastGroupSources
  where
  hashWithSalt
    _salt
    RegisterTransitGatewayMulticastGroupSources' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` groupIpAddress
        `Prelude.hashWithSalt` networkInterfaceIds
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    RegisterTransitGatewayMulticastGroupSources
  where
  rnf RegisterTransitGatewayMulticastGroupSources' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf groupIpAddress
      `Prelude.seq` Prelude.rnf networkInterfaceIds
      `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainId

instance
  Data.ToHeaders
    RegisterTransitGatewayMulticastGroupSources
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    RegisterTransitGatewayMulticastGroupSources
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RegisterTransitGatewayMulticastGroupSources
  where
  toQuery
    RegisterTransitGatewayMulticastGroupSources' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "RegisterTransitGatewayMulticastGroupSources" ::
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

-- | /See:/ 'newRegisterTransitGatewayMulticastGroupSourcesResponse' smart constructor.
data RegisterTransitGatewayMulticastGroupSourcesResponse = RegisterTransitGatewayMulticastGroupSourcesResponse'
  { -- | Information about the transit gateway multicast group sources.
    registeredMulticastGroupSources :: Prelude.Maybe TransitGatewayMulticastRegisteredGroupSources,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTransitGatewayMulticastGroupSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registeredMulticastGroupSources', 'registerTransitGatewayMulticastGroupSourcesResponse_registeredMulticastGroupSources' - Information about the transit gateway multicast group sources.
--
-- 'httpStatus', 'registerTransitGatewayMulticastGroupSourcesResponse_httpStatus' - The response's http status code.
newRegisterTransitGatewayMulticastGroupSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterTransitGatewayMulticastGroupSourcesResponse
newRegisterTransitGatewayMulticastGroupSourcesResponse
  pHttpStatus_ =
    RegisterTransitGatewayMulticastGroupSourcesResponse'
      { registeredMulticastGroupSources =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the transit gateway multicast group sources.
registerTransitGatewayMulticastGroupSourcesResponse_registeredMulticastGroupSources :: Lens.Lens' RegisterTransitGatewayMulticastGroupSourcesResponse (Prelude.Maybe TransitGatewayMulticastRegisteredGroupSources)
registerTransitGatewayMulticastGroupSourcesResponse_registeredMulticastGroupSources = Lens.lens (\RegisterTransitGatewayMulticastGroupSourcesResponse' {registeredMulticastGroupSources} -> registeredMulticastGroupSources) (\s@RegisterTransitGatewayMulticastGroupSourcesResponse' {} a -> s {registeredMulticastGroupSources = a} :: RegisterTransitGatewayMulticastGroupSourcesResponse)

-- | The response's http status code.
registerTransitGatewayMulticastGroupSourcesResponse_httpStatus :: Lens.Lens' RegisterTransitGatewayMulticastGroupSourcesResponse Prelude.Int
registerTransitGatewayMulticastGroupSourcesResponse_httpStatus = Lens.lens (\RegisterTransitGatewayMulticastGroupSourcesResponse' {httpStatus} -> httpStatus) (\s@RegisterTransitGatewayMulticastGroupSourcesResponse' {} a -> s {httpStatus = a} :: RegisterTransitGatewayMulticastGroupSourcesResponse)

instance
  Prelude.NFData
    RegisterTransitGatewayMulticastGroupSourcesResponse
  where
  rnf
    RegisterTransitGatewayMulticastGroupSourcesResponse' {..} =
      Prelude.rnf registeredMulticastGroupSources
        `Prelude.seq` Prelude.rnf httpStatus
