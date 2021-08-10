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
-- Module      : Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources
  ( -- * Creating a Request
    RegisterTransitGatewayMulticastGroupSources (..),
    newRegisterTransitGatewayMulticastGroupSources,

    -- * Request Lenses
    registerTransitGatewayMulticastGroupSources_dryRun,
    registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupSources_networkInterfaceIds,
    registerTransitGatewayMulticastGroupSources_groupIpAddress,

    -- * Destructuring the Response
    RegisterTransitGatewayMulticastGroupSourcesResponse (..),
    newRegisterTransitGatewayMulticastGroupSourcesResponse,

    -- * Response Lenses
    registerTransitGatewayMulticastGroupSourcesResponse_registeredMulticastGroupSources,
    registerTransitGatewayMulticastGroupSourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterTransitGatewayMulticastGroupSources' smart constructor.
data RegisterTransitGatewayMulticastGroupSources = RegisterTransitGatewayMulticastGroupSources'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The group sources\' network interface IDs to register with the transit
    -- gateway multicast group.
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text
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
-- 'transitGatewayMulticastDomainId', 'registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'networkInterfaceIds', 'registerTransitGatewayMulticastGroupSources_networkInterfaceIds' - The group sources\' network interface IDs to register with the transit
-- gateway multicast group.
--
-- 'groupIpAddress', 'registerTransitGatewayMulticastGroupSources_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
newRegisterTransitGatewayMulticastGroupSources ::
  RegisterTransitGatewayMulticastGroupSources
newRegisterTransitGatewayMulticastGroupSources =
  RegisterTransitGatewayMulticastGroupSources'
    { dryRun =
        Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing,
      networkInterfaceIds =
        Prelude.Nothing,
      groupIpAddress =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
registerTransitGatewayMulticastGroupSources_dryRun :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Prelude.Maybe Prelude.Bool)
registerTransitGatewayMulticastGroupSources_dryRun = Lens.lens (\RegisterTransitGatewayMulticastGroupSources' {dryRun} -> dryRun) (\s@RegisterTransitGatewayMulticastGroupSources' {} a -> s {dryRun = a} :: RegisterTransitGatewayMulticastGroupSources)

-- | The ID of the transit gateway multicast domain.
registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Prelude.Maybe Prelude.Text)
registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId = Lens.lens (\RegisterTransitGatewayMulticastGroupSources' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@RegisterTransitGatewayMulticastGroupSources' {} a -> s {transitGatewayMulticastDomainId = a} :: RegisterTransitGatewayMulticastGroupSources)

-- | The group sources\' network interface IDs to register with the transit
-- gateway multicast group.
registerTransitGatewayMulticastGroupSources_networkInterfaceIds :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Prelude.Maybe [Prelude.Text])
registerTransitGatewayMulticastGroupSources_networkInterfaceIds = Lens.lens (\RegisterTransitGatewayMulticastGroupSources' {networkInterfaceIds} -> networkInterfaceIds) (\s@RegisterTransitGatewayMulticastGroupSources' {} a -> s {networkInterfaceIds = a} :: RegisterTransitGatewayMulticastGroupSources) Prelude.. Lens.mapping Lens._Coerce

-- | The IP address assigned to the transit gateway multicast group.
registerTransitGatewayMulticastGroupSources_groupIpAddress :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Prelude.Maybe Prelude.Text)
registerTransitGatewayMulticastGroupSources_groupIpAddress = Lens.lens (\RegisterTransitGatewayMulticastGroupSources' {groupIpAddress} -> groupIpAddress) (\s@RegisterTransitGatewayMulticastGroupSources' {} a -> s {groupIpAddress = a} :: RegisterTransitGatewayMulticastGroupSources)

instance
  Core.AWSRequest
    RegisterTransitGatewayMulticastGroupSources
  where
  type
    AWSResponse
      RegisterTransitGatewayMulticastGroupSources =
      RegisterTransitGatewayMulticastGroupSourcesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterTransitGatewayMulticastGroupSourcesResponse'
            Prelude.<$> (x Core..@? "registeredMulticastGroupSources")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterTransitGatewayMulticastGroupSources

instance
  Prelude.NFData
    RegisterTransitGatewayMulticastGroupSources

instance
  Core.ToHeaders
    RegisterTransitGatewayMulticastGroupSources
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    RegisterTransitGatewayMulticastGroupSources
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    RegisterTransitGatewayMulticastGroupSources
  where
  toQuery
    RegisterTransitGatewayMulticastGroupSources' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "RegisterTransitGatewayMulticastGroupSources" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Core.=: dryRun,
          "TransitGatewayMulticastDomainId"
            Core.=: transitGatewayMulticastDomainId,
          Core.toQuery
            ( Core.toQueryList "NetworkInterfaceIds"
                Prelude.<$> networkInterfaceIds
            ),
          "GroupIpAddress" Core.=: groupIpAddress
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
