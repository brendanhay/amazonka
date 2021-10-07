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
-- Module      : Network.AWS.EC2.RejectTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a request to associate cross-account subnets with a transit
-- gateway multicast domain.
module Network.AWS.EC2.RejectTransitGatewayMulticastDomainAssociations
  ( -- * Creating a Request
    RejectTransitGatewayMulticastDomainAssociations (..),
    newRejectTransitGatewayMulticastDomainAssociations,

    -- * Request Lenses
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    rejectTransitGatewayMulticastDomainAssociations_dryRun,
    rejectTransitGatewayMulticastDomainAssociations_subnetIds,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,

    -- * Destructuring the Response
    RejectTransitGatewayMulticastDomainAssociationsResponse (..),
    newRejectTransitGatewayMulticastDomainAssociationsResponse,

    -- * Response Lenses
    rejectTransitGatewayMulticastDomainAssociationsResponse_associations,
    rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectTransitGatewayMulticastDomainAssociations' smart constructor.
data RejectTransitGatewayMulticastDomainAssociations = RejectTransitGatewayMulticastDomainAssociations'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the subnets to associate with the transit gateway multicast
    -- domain.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectTransitGatewayMulticastDomainAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayMulticastDomainId', 'rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'dryRun', 'rejectTransitGatewayMulticastDomainAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'subnetIds', 'rejectTransitGatewayMulticastDomainAssociations_subnetIds' - The IDs of the subnets to associate with the transit gateway multicast
-- domain.
--
-- 'transitGatewayAttachmentId', 'rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
newRejectTransitGatewayMulticastDomainAssociations ::
  RejectTransitGatewayMulticastDomainAssociations
newRejectTransitGatewayMulticastDomainAssociations =
  RejectTransitGatewayMulticastDomainAssociations'
    { transitGatewayMulticastDomainId =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      subnetIds =
        Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing
    }

-- | The ID of the transit gateway multicast domain.
rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId = Lens.lens (\RejectTransitGatewayMulticastDomainAssociations' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@RejectTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayMulticastDomainId = a} :: RejectTransitGatewayMulticastDomainAssociations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rejectTransitGatewayMulticastDomainAssociations_dryRun :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Bool)
rejectTransitGatewayMulticastDomainAssociations_dryRun = Lens.lens (\RejectTransitGatewayMulticastDomainAssociations' {dryRun} -> dryRun) (\s@RejectTransitGatewayMulticastDomainAssociations' {} a -> s {dryRun = a} :: RejectTransitGatewayMulticastDomainAssociations)

-- | The IDs of the subnets to associate with the transit gateway multicast
-- domain.
rejectTransitGatewayMulticastDomainAssociations_subnetIds :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociations (Prelude.Maybe [Prelude.Text])
rejectTransitGatewayMulticastDomainAssociations_subnetIds = Lens.lens (\RejectTransitGatewayMulticastDomainAssociations' {subnetIds} -> subnetIds) (\s@RejectTransitGatewayMulticastDomainAssociations' {} a -> s {subnetIds = a} :: RejectTransitGatewayMulticastDomainAssociations) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the transit gateway attachment.
rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId = Lens.lens (\RejectTransitGatewayMulticastDomainAssociations' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@RejectTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayAttachmentId = a} :: RejectTransitGatewayMulticastDomainAssociations)

instance
  Core.AWSRequest
    RejectTransitGatewayMulticastDomainAssociations
  where
  type
    AWSResponse
      RejectTransitGatewayMulticastDomainAssociations =
      RejectTransitGatewayMulticastDomainAssociationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RejectTransitGatewayMulticastDomainAssociationsResponse'
            Prelude.<$> (x Core..@? "associations")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RejectTransitGatewayMulticastDomainAssociations

instance
  Prelude.NFData
    RejectTransitGatewayMulticastDomainAssociations

instance
  Core.ToHeaders
    RejectTransitGatewayMulticastDomainAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    RejectTransitGatewayMulticastDomainAssociations
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    RejectTransitGatewayMulticastDomainAssociations
  where
  toQuery
    RejectTransitGatewayMulticastDomainAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "RejectTransitGatewayMulticastDomainAssociations" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          "TransitGatewayMulticastDomainId"
            Core.=: transitGatewayMulticastDomainId,
          "DryRun" Core.=: dryRun,
          Core.toQuery
            (Core.toQueryList "SubnetIds" Prelude.<$> subnetIds),
          "TransitGatewayAttachmentId"
            Core.=: transitGatewayAttachmentId
        ]

-- | /See:/ 'newRejectTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data RejectTransitGatewayMulticastDomainAssociationsResponse = RejectTransitGatewayMulticastDomainAssociationsResponse'
  { associations :: Prelude.Maybe TransitGatewayMulticastDomainAssociations,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectTransitGatewayMulticastDomainAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'rejectTransitGatewayMulticastDomainAssociationsResponse_associations' - Undocumented member.
--
-- 'httpStatus', 'rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus' - The response's http status code.
newRejectTransitGatewayMulticastDomainAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectTransitGatewayMulticastDomainAssociationsResponse
newRejectTransitGatewayMulticastDomainAssociationsResponse
  pHttpStatus_ =
    RejectTransitGatewayMulticastDomainAssociationsResponse'
      { associations =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
rejectTransitGatewayMulticastDomainAssociationsResponse_associations :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociationsResponse (Prelude.Maybe TransitGatewayMulticastDomainAssociations)
rejectTransitGatewayMulticastDomainAssociationsResponse_associations = Lens.lens (\RejectTransitGatewayMulticastDomainAssociationsResponse' {associations} -> associations) (\s@RejectTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {associations = a} :: RejectTransitGatewayMulticastDomainAssociationsResponse)

-- | The response's http status code.
rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociationsResponse Prelude.Int
rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus = Lens.lens (\RejectTransitGatewayMulticastDomainAssociationsResponse' {httpStatus} -> httpStatus) (\s@RejectTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {httpStatus = a} :: RejectTransitGatewayMulticastDomainAssociationsResponse)

instance
  Prelude.NFData
    RejectTransitGatewayMulticastDomainAssociationsResponse
