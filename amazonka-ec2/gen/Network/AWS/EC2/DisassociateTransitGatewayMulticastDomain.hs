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
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified subnets from the transit gateway multicast
-- domain.
module Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
  ( -- * Creating a Request
    DisassociateTransitGatewayMulticastDomain (..),
    newDisassociateTransitGatewayMulticastDomain,

    -- * Request Lenses
    disassociateTransitGatewayMulticastDomain_dryRun,
    disassociateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    disassociateTransitGatewayMulticastDomain_subnetIds,
    disassociateTransitGatewayMulticastDomain_transitGatewayAttachmentId,

    -- * Destructuring the Response
    DisassociateTransitGatewayMulticastDomainResponse (..),
    newDisassociateTransitGatewayMulticastDomainResponse,

    -- * Response Lenses
    disassociateTransitGatewayMulticastDomainResponse_associations,
    disassociateTransitGatewayMulticastDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateTransitGatewayMulticastDomain' smart constructor.
data DisassociateTransitGatewayMulticastDomain = DisassociateTransitGatewayMulticastDomain'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The IDs of the subnets;
    subnetIds :: Core.Maybe [Core.Text],
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateTransitGatewayMulticastDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disassociateTransitGatewayMulticastDomain_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayMulticastDomainId', 'disassociateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'subnetIds', 'disassociateTransitGatewayMulticastDomain_subnetIds' - The IDs of the subnets;
--
-- 'transitGatewayAttachmentId', 'disassociateTransitGatewayMulticastDomain_transitGatewayAttachmentId' - The ID of the attachment.
newDisassociateTransitGatewayMulticastDomain ::
  DisassociateTransitGatewayMulticastDomain
newDisassociateTransitGatewayMulticastDomain =
  DisassociateTransitGatewayMulticastDomain'
    { dryRun =
        Core.Nothing,
      transitGatewayMulticastDomainId =
        Core.Nothing,
      subnetIds = Core.Nothing,
      transitGatewayAttachmentId =
        Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disassociateTransitGatewayMulticastDomain_dryRun :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
disassociateTransitGatewayMulticastDomain_dryRun = Lens.lens (\DisassociateTransitGatewayMulticastDomain' {dryRun} -> dryRun) (\s@DisassociateTransitGatewayMulticastDomain' {} a -> s {dryRun = a} :: DisassociateTransitGatewayMulticastDomain)

-- | The ID of the transit gateway multicast domain.
disassociateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe Core.Text)
disassociateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId = Lens.lens (\DisassociateTransitGatewayMulticastDomain' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@DisassociateTransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainId = a} :: DisassociateTransitGatewayMulticastDomain)

-- | The IDs of the subnets;
disassociateTransitGatewayMulticastDomain_subnetIds :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe [Core.Text])
disassociateTransitGatewayMulticastDomain_subnetIds = Lens.lens (\DisassociateTransitGatewayMulticastDomain' {subnetIds} -> subnetIds) (\s@DisassociateTransitGatewayMulticastDomain' {} a -> s {subnetIds = a} :: DisassociateTransitGatewayMulticastDomain) Core.. Lens.mapping Lens._Coerce

-- | The ID of the attachment.
disassociateTransitGatewayMulticastDomain_transitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe Core.Text)
disassociateTransitGatewayMulticastDomain_transitGatewayAttachmentId = Lens.lens (\DisassociateTransitGatewayMulticastDomain' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DisassociateTransitGatewayMulticastDomain' {} a -> s {transitGatewayAttachmentId = a} :: DisassociateTransitGatewayMulticastDomain)

instance
  Core.AWSRequest
    DisassociateTransitGatewayMulticastDomain
  where
  type
    AWSResponse
      DisassociateTransitGatewayMulticastDomain =
      DisassociateTransitGatewayMulticastDomainResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateTransitGatewayMulticastDomainResponse'
            Core.<$> (x Core..@? "associations")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisassociateTransitGatewayMulticastDomain

instance
  Core.NFData
    DisassociateTransitGatewayMulticastDomain

instance
  Core.ToHeaders
    DisassociateTransitGatewayMulticastDomain
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DisassociateTransitGatewayMulticastDomain
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DisassociateTransitGatewayMulticastDomain
  where
  toQuery
    DisassociateTransitGatewayMulticastDomain' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "DisassociateTransitGatewayMulticastDomain" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "DryRun" Core.=: dryRun,
          "TransitGatewayMulticastDomainId"
            Core.=: transitGatewayMulticastDomainId,
          Core.toQuery
            (Core.toQueryList "SubnetIds" Core.<$> subnetIds),
          "TransitGatewayAttachmentId"
            Core.=: transitGatewayAttachmentId
        ]

-- | /See:/ 'newDisassociateTransitGatewayMulticastDomainResponse' smart constructor.
data DisassociateTransitGatewayMulticastDomainResponse = DisassociateTransitGatewayMulticastDomainResponse'
  { -- | Information about the association.
    associations :: Core.Maybe TransitGatewayMulticastDomainAssociations,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateTransitGatewayMulticastDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'disassociateTransitGatewayMulticastDomainResponse_associations' - Information about the association.
--
-- 'httpStatus', 'disassociateTransitGatewayMulticastDomainResponse_httpStatus' - The response's http status code.
newDisassociateTransitGatewayMulticastDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateTransitGatewayMulticastDomainResponse
newDisassociateTransitGatewayMulticastDomainResponse
  pHttpStatus_ =
    DisassociateTransitGatewayMulticastDomainResponse'
      { associations =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the association.
disassociateTransitGatewayMulticastDomainResponse_associations :: Lens.Lens' DisassociateTransitGatewayMulticastDomainResponse (Core.Maybe TransitGatewayMulticastDomainAssociations)
disassociateTransitGatewayMulticastDomainResponse_associations = Lens.lens (\DisassociateTransitGatewayMulticastDomainResponse' {associations} -> associations) (\s@DisassociateTransitGatewayMulticastDomainResponse' {} a -> s {associations = a} :: DisassociateTransitGatewayMulticastDomainResponse)

-- | The response's http status code.
disassociateTransitGatewayMulticastDomainResponse_httpStatus :: Lens.Lens' DisassociateTransitGatewayMulticastDomainResponse Core.Int
disassociateTransitGatewayMulticastDomainResponse_httpStatus = Lens.lens (\DisassociateTransitGatewayMulticastDomainResponse' {httpStatus} -> httpStatus) (\s@DisassociateTransitGatewayMulticastDomainResponse' {} a -> s {httpStatus = a} :: DisassociateTransitGatewayMulticastDomainResponse)

instance
  Core.NFData
    DisassociateTransitGatewayMulticastDomainResponse
