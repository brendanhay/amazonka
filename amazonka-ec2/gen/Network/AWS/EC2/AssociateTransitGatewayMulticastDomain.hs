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
-- Module      : Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified subnets and transit gateway attachments with
-- the specified transit gateway multicast domain.
--
-- The transit gateway attachment must be in the available state before you
-- can add a resource. Use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeTransitGatewayAttachments.html DescribeTransitGatewayAttachments>
-- to see the state of the attachment.
module Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
  ( -- * Creating a Request
    AssociateTransitGatewayMulticastDomain (..),
    newAssociateTransitGatewayMulticastDomain,

    -- * Request Lenses
    associateTransitGatewayMulticastDomain_dryRun,
    associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    associateTransitGatewayMulticastDomain_subnetIds,
    associateTransitGatewayMulticastDomain_transitGatewayAttachmentId,

    -- * Destructuring the Response
    AssociateTransitGatewayMulticastDomainResponse (..),
    newAssociateTransitGatewayMulticastDomainResponse,

    -- * Response Lenses
    associateTransitGatewayMulticastDomainResponse_associations,
    associateTransitGatewayMulticastDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateTransitGatewayMulticastDomain' smart constructor.
data AssociateTransitGatewayMulticastDomain = AssociateTransitGatewayMulticastDomain'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The IDs of the subnets to associate with the transit gateway multicast
    -- domain.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The ID of the transit gateway attachment to associate with the transit
    -- gateway multicast domain.
    transitGatewayAttachmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateTransitGatewayMulticastDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateTransitGatewayMulticastDomain_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayMulticastDomainId', 'associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'subnetIds', 'associateTransitGatewayMulticastDomain_subnetIds' - The IDs of the subnets to associate with the transit gateway multicast
-- domain.
--
-- 'transitGatewayAttachmentId', 'associateTransitGatewayMulticastDomain_transitGatewayAttachmentId' - The ID of the transit gateway attachment to associate with the transit
-- gateway multicast domain.
newAssociateTransitGatewayMulticastDomain ::
  AssociateTransitGatewayMulticastDomain
newAssociateTransitGatewayMulticastDomain =
  AssociateTransitGatewayMulticastDomain'
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
associateTransitGatewayMulticastDomain_dryRun :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
associateTransitGatewayMulticastDomain_dryRun = Lens.lens (\AssociateTransitGatewayMulticastDomain' {dryRun} -> dryRun) (\s@AssociateTransitGatewayMulticastDomain' {} a -> s {dryRun = a} :: AssociateTransitGatewayMulticastDomain)

-- | The ID of the transit gateway multicast domain.
associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Core.Maybe Core.Text)
associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId = Lens.lens (\AssociateTransitGatewayMulticastDomain' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@AssociateTransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainId = a} :: AssociateTransitGatewayMulticastDomain)

-- | The IDs of the subnets to associate with the transit gateway multicast
-- domain.
associateTransitGatewayMulticastDomain_subnetIds :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Core.Maybe [Core.Text])
associateTransitGatewayMulticastDomain_subnetIds = Lens.lens (\AssociateTransitGatewayMulticastDomain' {subnetIds} -> subnetIds) (\s@AssociateTransitGatewayMulticastDomain' {} a -> s {subnetIds = a} :: AssociateTransitGatewayMulticastDomain) Core.. Lens.mapping Lens._Coerce

-- | The ID of the transit gateway attachment to associate with the transit
-- gateway multicast domain.
associateTransitGatewayMulticastDomain_transitGatewayAttachmentId :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Core.Maybe Core.Text)
associateTransitGatewayMulticastDomain_transitGatewayAttachmentId = Lens.lens (\AssociateTransitGatewayMulticastDomain' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AssociateTransitGatewayMulticastDomain' {} a -> s {transitGatewayAttachmentId = a} :: AssociateTransitGatewayMulticastDomain)

instance
  Core.AWSRequest
    AssociateTransitGatewayMulticastDomain
  where
  type
    AWSResponse
      AssociateTransitGatewayMulticastDomain =
      AssociateTransitGatewayMulticastDomainResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateTransitGatewayMulticastDomainResponse'
            Core.<$> (x Core..@? "associations")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AssociateTransitGatewayMulticastDomain

instance
  Core.NFData
    AssociateTransitGatewayMulticastDomain

instance
  Core.ToHeaders
    AssociateTransitGatewayMulticastDomain
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    AssociateTransitGatewayMulticastDomain
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AssociateTransitGatewayMulticastDomain
  where
  toQuery AssociateTransitGatewayMulticastDomain' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "AssociateTransitGatewayMulticastDomain" ::
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

-- | /See:/ 'newAssociateTransitGatewayMulticastDomainResponse' smart constructor.
data AssociateTransitGatewayMulticastDomainResponse = AssociateTransitGatewayMulticastDomainResponse'
  { -- | Information about the transit gateway multicast domain associations.
    associations :: Core.Maybe TransitGatewayMulticastDomainAssociations,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateTransitGatewayMulticastDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'associateTransitGatewayMulticastDomainResponse_associations' - Information about the transit gateway multicast domain associations.
--
-- 'httpStatus', 'associateTransitGatewayMulticastDomainResponse_httpStatus' - The response's http status code.
newAssociateTransitGatewayMulticastDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateTransitGatewayMulticastDomainResponse
newAssociateTransitGatewayMulticastDomainResponse
  pHttpStatus_ =
    AssociateTransitGatewayMulticastDomainResponse'
      { associations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the transit gateway multicast domain associations.
associateTransitGatewayMulticastDomainResponse_associations :: Lens.Lens' AssociateTransitGatewayMulticastDomainResponse (Core.Maybe TransitGatewayMulticastDomainAssociations)
associateTransitGatewayMulticastDomainResponse_associations = Lens.lens (\AssociateTransitGatewayMulticastDomainResponse' {associations} -> associations) (\s@AssociateTransitGatewayMulticastDomainResponse' {} a -> s {associations = a} :: AssociateTransitGatewayMulticastDomainResponse)

-- | The response's http status code.
associateTransitGatewayMulticastDomainResponse_httpStatus :: Lens.Lens' AssociateTransitGatewayMulticastDomainResponse Core.Int
associateTransitGatewayMulticastDomainResponse_httpStatus = Lens.lens (\AssociateTransitGatewayMulticastDomainResponse' {httpStatus} -> httpStatus) (\s@AssociateTransitGatewayMulticastDomainResponse' {} a -> s {httpStatus = a} :: AssociateTransitGatewayMulticastDomainResponse)

instance
  Core.NFData
    AssociateTransitGatewayMulticastDomainResponse
