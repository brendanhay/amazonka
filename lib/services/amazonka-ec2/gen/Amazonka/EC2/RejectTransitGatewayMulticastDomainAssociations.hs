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
-- Module      : Amazonka.EC2.RejectTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a request to associate cross-account subnets with a transit
-- gateway multicast domain.
module Amazonka.EC2.RejectTransitGatewayMulticastDomainAssociations
  ( -- * Creating a Request
    RejectTransitGatewayMulticastDomainAssociations (..),
    newRejectTransitGatewayMulticastDomainAssociations,

    -- * Request Lenses
    rejectTransitGatewayMulticastDomainAssociations_dryRun,
    rejectTransitGatewayMulticastDomainAssociations_subnetIds,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,

    -- * Destructuring the Response
    RejectTransitGatewayMulticastDomainAssociationsResponse (..),
    newRejectTransitGatewayMulticastDomainAssociationsResponse,

    -- * Response Lenses
    rejectTransitGatewayMulticastDomainAssociationsResponse_associations,
    rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectTransitGatewayMulticastDomainAssociations' smart constructor.
data RejectTransitGatewayMulticastDomainAssociations = RejectTransitGatewayMulticastDomainAssociations'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the subnets to associate with the transit gateway multicast
    -- domain.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text
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
-- 'dryRun', 'rejectTransitGatewayMulticastDomainAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'subnetIds', 'rejectTransitGatewayMulticastDomainAssociations_subnetIds' - The IDs of the subnets to associate with the transit gateway multicast
-- domain.
--
-- 'transitGatewayAttachmentId', 'rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
--
-- 'transitGatewayMulticastDomainId', 'rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newRejectTransitGatewayMulticastDomainAssociations ::
  RejectTransitGatewayMulticastDomainAssociations
newRejectTransitGatewayMulticastDomainAssociations =
  RejectTransitGatewayMulticastDomainAssociations'
    { dryRun =
        Prelude.Nothing,
      subnetIds =
        Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rejectTransitGatewayMulticastDomainAssociations_dryRun :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Bool)
rejectTransitGatewayMulticastDomainAssociations_dryRun = Lens.lens (\RejectTransitGatewayMulticastDomainAssociations' {dryRun} -> dryRun) (\s@RejectTransitGatewayMulticastDomainAssociations' {} a -> s {dryRun = a} :: RejectTransitGatewayMulticastDomainAssociations)

-- | The IDs of the subnets to associate with the transit gateway multicast
-- domain.
rejectTransitGatewayMulticastDomainAssociations_subnetIds :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociations (Prelude.Maybe [Prelude.Text])
rejectTransitGatewayMulticastDomainAssociations_subnetIds = Lens.lens (\RejectTransitGatewayMulticastDomainAssociations' {subnetIds} -> subnetIds) (\s@RejectTransitGatewayMulticastDomainAssociations' {} a -> s {subnetIds = a} :: RejectTransitGatewayMulticastDomainAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway attachment.
rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId = Lens.lens (\RejectTransitGatewayMulticastDomainAssociations' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@RejectTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayAttachmentId = a} :: RejectTransitGatewayMulticastDomainAssociations)

-- | The ID of the transit gateway multicast domain.
rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId = Lens.lens (\RejectTransitGatewayMulticastDomainAssociations' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@RejectTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayMulticastDomainId = a} :: RejectTransitGatewayMulticastDomainAssociations)

instance
  Core.AWSRequest
    RejectTransitGatewayMulticastDomainAssociations
  where
  type
    AWSResponse
      RejectTransitGatewayMulticastDomainAssociations =
      RejectTransitGatewayMulticastDomainAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RejectTransitGatewayMulticastDomainAssociationsResponse'
            Prelude.<$> (x Data..@? "associations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RejectTransitGatewayMulticastDomainAssociations
  where
  hashWithSalt
    _salt
    RejectTransitGatewayMulticastDomainAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` subnetIds
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    RejectTransitGatewayMulticastDomainAssociations
  where
  rnf
    RejectTransitGatewayMulticastDomainAssociations' {..} =
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf subnetIds `Prelude.seq`
          Prelude.rnf transitGatewayAttachmentId `Prelude.seq`
            Prelude.rnf transitGatewayMulticastDomainId

instance
  Data.ToHeaders
    RejectTransitGatewayMulticastDomainAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    RejectTransitGatewayMulticastDomainAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RejectTransitGatewayMulticastDomainAssociations
  where
  toQuery
    RejectTransitGatewayMulticastDomainAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "RejectTransitGatewayMulticastDomainAssociations" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          Data.toQuery
            (Data.toQueryList "SubnetIds" Prelude.<$> subnetIds),
          "TransitGatewayAttachmentId"
            Data.=: transitGatewayAttachmentId,
          "TransitGatewayMulticastDomainId"
            Data.=: transitGatewayMulticastDomainId
        ]

-- | /See:/ 'newRejectTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data RejectTransitGatewayMulticastDomainAssociationsResponse = RejectTransitGatewayMulticastDomainAssociationsResponse'
  { -- | Information about the multicast domain associations.
    associations :: Prelude.Maybe TransitGatewayMulticastDomainAssociations,
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
-- 'associations', 'rejectTransitGatewayMulticastDomainAssociationsResponse_associations' - Information about the multicast domain associations.
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

-- | Information about the multicast domain associations.
rejectTransitGatewayMulticastDomainAssociationsResponse_associations :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociationsResponse (Prelude.Maybe TransitGatewayMulticastDomainAssociations)
rejectTransitGatewayMulticastDomainAssociationsResponse_associations = Lens.lens (\RejectTransitGatewayMulticastDomainAssociationsResponse' {associations} -> associations) (\s@RejectTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {associations = a} :: RejectTransitGatewayMulticastDomainAssociationsResponse)

-- | The response's http status code.
rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus :: Lens.Lens' RejectTransitGatewayMulticastDomainAssociationsResponse Prelude.Int
rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus = Lens.lens (\RejectTransitGatewayMulticastDomainAssociationsResponse' {httpStatus} -> httpStatus) (\s@RejectTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {httpStatus = a} :: RejectTransitGatewayMulticastDomainAssociationsResponse)

instance
  Prelude.NFData
    RejectTransitGatewayMulticastDomainAssociationsResponse
  where
  rnf
    RejectTransitGatewayMulticastDomainAssociationsResponse' {..} =
      Prelude.rnf associations `Prelude.seq`
        Prelude.rnf httpStatus
