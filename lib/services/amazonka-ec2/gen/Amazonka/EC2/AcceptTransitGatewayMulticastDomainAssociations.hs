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
-- Module      : Amazonka.EC2.AcceptTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a request to associate subnets with a transit gateway multicast
-- domain.
module Amazonka.EC2.AcceptTransitGatewayMulticastDomainAssociations
  ( -- * Creating a Request
    AcceptTransitGatewayMulticastDomainAssociations (..),
    newAcceptTransitGatewayMulticastDomainAssociations,

    -- * Request Lenses
    acceptTransitGatewayMulticastDomainAssociations_dryRun,
    acceptTransitGatewayMulticastDomainAssociations_subnetIds,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,

    -- * Destructuring the Response
    AcceptTransitGatewayMulticastDomainAssociationsResponse (..),
    newAcceptTransitGatewayMulticastDomainAssociationsResponse,

    -- * Response Lenses
    acceptTransitGatewayMulticastDomainAssociationsResponse_associations,
    acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptTransitGatewayMulticastDomainAssociations' smart constructor.
data AcceptTransitGatewayMulticastDomainAssociations = AcceptTransitGatewayMulticastDomainAssociations'
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
-- 'subnetIds', 'acceptTransitGatewayMulticastDomainAssociations_subnetIds' - The IDs of the subnets to associate with the transit gateway multicast
-- domain.
--
-- 'transitGatewayAttachmentId', 'acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
--
-- 'transitGatewayMulticastDomainId', 'acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newAcceptTransitGatewayMulticastDomainAssociations ::
  AcceptTransitGatewayMulticastDomainAssociations
newAcceptTransitGatewayMulticastDomainAssociations =
  AcceptTransitGatewayMulticastDomainAssociations'
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
acceptTransitGatewayMulticastDomainAssociations_dryRun :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Bool)
acceptTransitGatewayMulticastDomainAssociations_dryRun = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociations' {dryRun} -> dryRun) (\s@AcceptTransitGatewayMulticastDomainAssociations' {} a -> s {dryRun = a} :: AcceptTransitGatewayMulticastDomainAssociations)

-- | The IDs of the subnets to associate with the transit gateway multicast
-- domain.
acceptTransitGatewayMulticastDomainAssociations_subnetIds :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociations (Prelude.Maybe [Prelude.Text])
acceptTransitGatewayMulticastDomainAssociations_subnetIds = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociations' {subnetIds} -> subnetIds) (\s@AcceptTransitGatewayMulticastDomainAssociations' {} a -> s {subnetIds = a} :: AcceptTransitGatewayMulticastDomainAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway attachment.
acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociations' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AcceptTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayAttachmentId = a} :: AcceptTransitGatewayMulticastDomainAssociations)

-- | The ID of the transit gateway multicast domain.
acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociations' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@AcceptTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayMulticastDomainId = a} :: AcceptTransitGatewayMulticastDomainAssociations)

instance
  Core.AWSRequest
    AcceptTransitGatewayMulticastDomainAssociations
  where
  type
    AWSResponse
      AcceptTransitGatewayMulticastDomainAssociations =
      AcceptTransitGatewayMulticastDomainAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptTransitGatewayMulticastDomainAssociationsResponse'
            Prelude.<$> (x Data..@? "associations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptTransitGatewayMulticastDomainAssociations
  where
  hashWithSalt
    _salt
    AcceptTransitGatewayMulticastDomainAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` subnetIds
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    AcceptTransitGatewayMulticastDomainAssociations
  where
  rnf
    AcceptTransitGatewayMulticastDomainAssociations' {..} =
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf subnetIds `Prelude.seq`
          Prelude.rnf transitGatewayAttachmentId `Prelude.seq`
            Prelude.rnf transitGatewayMulticastDomainId

instance
  Data.ToHeaders
    AcceptTransitGatewayMulticastDomainAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    AcceptTransitGatewayMulticastDomainAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AcceptTransitGatewayMulticastDomainAssociations
  where
  toQuery
    AcceptTransitGatewayMulticastDomainAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "AcceptTransitGatewayMulticastDomainAssociations" ::
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

-- | /See:/ 'newAcceptTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data AcceptTransitGatewayMulticastDomainAssociationsResponse = AcceptTransitGatewayMulticastDomainAssociationsResponse'
  { -- | Information about the multicast domain associations.
    associations :: Prelude.Maybe TransitGatewayMulticastDomainAssociations,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptTransitGatewayMulticastDomainAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'acceptTransitGatewayMulticastDomainAssociationsResponse_associations' - Information about the multicast domain associations.
--
-- 'httpStatus', 'acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus' - The response's http status code.
newAcceptTransitGatewayMulticastDomainAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptTransitGatewayMulticastDomainAssociationsResponse
newAcceptTransitGatewayMulticastDomainAssociationsResponse
  pHttpStatus_ =
    AcceptTransitGatewayMulticastDomainAssociationsResponse'
      { associations =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the multicast domain associations.
acceptTransitGatewayMulticastDomainAssociationsResponse_associations :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociationsResponse (Prelude.Maybe TransitGatewayMulticastDomainAssociations)
acceptTransitGatewayMulticastDomainAssociationsResponse_associations = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociationsResponse' {associations} -> associations) (\s@AcceptTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {associations = a} :: AcceptTransitGatewayMulticastDomainAssociationsResponse)

-- | The response's http status code.
acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus :: Lens.Lens' AcceptTransitGatewayMulticastDomainAssociationsResponse Prelude.Int
acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus = Lens.lens (\AcceptTransitGatewayMulticastDomainAssociationsResponse' {httpStatus} -> httpStatus) (\s@AcceptTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {httpStatus = a} :: AcceptTransitGatewayMulticastDomainAssociationsResponse)

instance
  Prelude.NFData
    AcceptTransitGatewayMulticastDomainAssociationsResponse
  where
  rnf
    AcceptTransitGatewayMulticastDomainAssociationsResponse' {..} =
      Prelude.rnf associations `Prelude.seq`
        Prelude.rnf httpStatus
