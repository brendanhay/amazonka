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
-- Module      : Network.AWS.EC2.AcceptTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a request to associate subnets with a transit gateway multicast
-- domain.
module Network.AWS.EC2.AcceptTransitGatewayMulticastDomainAssociations
  ( -- * Creating a Request
    AcceptTransitGatewayMulticastDomainAssociations (..),
    newAcceptTransitGatewayMulticastDomainAssociations,

    -- * Request Lenses
    acceptTransitGatewayMulticastDomainAssociations_dryRun,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    acceptTransitGatewayMulticastDomainAssociations_subnetIds,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,

    -- * Destructuring the Response
    AcceptTransitGatewayMulticastDomainAssociationsResponse (..),
    newAcceptTransitGatewayMulticastDomainAssociationsResponse,

    -- * Response Lenses
    acceptTransitGatewayMulticastDomainAssociationsResponse_associations,
    acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptTransitGatewayMulticastDomainAssociations' smart constructor.
data AcceptTransitGatewayMulticastDomainAssociations = AcceptTransitGatewayMulticastDomainAssociations'
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
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptTransitGatewayMulticastDomainAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'acceptTransitGatewayMulticastDomainAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayMulticastDomainId', 'acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'subnetIds', 'acceptTransitGatewayMulticastDomainAssociations_subnetIds' - The IDs of the subnets to associate with the transit gateway multicast
-- domain.
--
-- 'transitGatewayAttachmentId', 'acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
newAcceptTransitGatewayMulticastDomainAssociations ::
  AcceptTransitGatewayMulticastDomainAssociations
newAcceptTransitGatewayMulticastDomainAssociations =
  AcceptTransitGatewayMulticastDomainAssociations'
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
acceptTransitGatewayMulticastDomainAssociations_dryRun :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Bool)
acceptTransitGatewayMulticastDomainAssociations_dryRun = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociations' {dryRun} -> dryRun) (\s@AcceptTransitGatewayMulticastDomainAssociations' {} a -> s {dryRun = a} :: AcceptTransitGatewayMulticastDomainAssociations)

-- | The ID of the transit gateway multicast domain.
acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Text)
acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociations' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@AcceptTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayMulticastDomainId = a} :: AcceptTransitGatewayMulticastDomainAssociations)

-- | The IDs of the subnets to associate with the transit gateway multicast
-- domain.
acceptTransitGatewayMulticastDomainAssociations_subnetIds :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociations (Core.Maybe [Core.Text])
acceptTransitGatewayMulticastDomainAssociations_subnetIds = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociations' {subnetIds} -> subnetIds) (\s@AcceptTransitGatewayMulticastDomainAssociations' {} a -> s {subnetIds = a} :: AcceptTransitGatewayMulticastDomainAssociations) Core.. Lens.mapping Lens._Coerce

-- | The ID of the transit gateway attachment.
acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Text)
acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociations' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AcceptTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayAttachmentId = a} :: AcceptTransitGatewayMulticastDomainAssociations)

instance
  Core.AWSRequest
    AcceptTransitGatewayMulticastDomainAssociations
  where
  type
    AWSResponse
      AcceptTransitGatewayMulticastDomainAssociations =
      AcceptTransitGatewayMulticastDomainAssociationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptTransitGatewayMulticastDomainAssociationsResponse'
            Core.<$> (x Core..@? "associations")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AcceptTransitGatewayMulticastDomainAssociations

instance
  Core.NFData
    AcceptTransitGatewayMulticastDomainAssociations

instance
  Core.ToHeaders
    AcceptTransitGatewayMulticastDomainAssociations
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    AcceptTransitGatewayMulticastDomainAssociations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AcceptTransitGatewayMulticastDomainAssociations
  where
  toQuery
    AcceptTransitGatewayMulticastDomainAssociations' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "AcceptTransitGatewayMulticastDomainAssociations" ::
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

-- | /See:/ 'newAcceptTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data AcceptTransitGatewayMulticastDomainAssociationsResponse = AcceptTransitGatewayMulticastDomainAssociationsResponse'
  { associations :: Core.Maybe TransitGatewayMulticastDomainAssociations,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptTransitGatewayMulticastDomainAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'acceptTransitGatewayMulticastDomainAssociationsResponse_associations' - Undocumented member.
--
-- 'httpStatus', 'acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus' - The response's http status code.
newAcceptTransitGatewayMulticastDomainAssociationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptTransitGatewayMulticastDomainAssociationsResponse
newAcceptTransitGatewayMulticastDomainAssociationsResponse
  pHttpStatus_ =
    AcceptTransitGatewayMulticastDomainAssociationsResponse'
      { associations =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
acceptTransitGatewayMulticastDomainAssociationsResponse_associations :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociationsResponse (Core.Maybe TransitGatewayMulticastDomainAssociations)
acceptTransitGatewayMulticastDomainAssociationsResponse_associations = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociationsResponse' {associations} -> associations) (\s@AcceptTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {associations = a} :: AcceptTransitGatewayMulticastDomainAssociationsResponse)

-- | The response's http status code.
acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociationsResponse Core.Int
acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociationsResponse' {httpStatus} -> httpStatus) (\s@AcceptTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {httpStatus = a} :: AcceptTransitGatewayMulticastDomainAssociationsResponse)

instance
  Core.NFData
    AcceptTransitGatewayMulticastDomainAssociationsResponse
