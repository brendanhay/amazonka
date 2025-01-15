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
-- Module      : Amazonka.EC2.AssociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.AssociateTransitGatewayMulticastDomain
  ( -- * Creating a Request
    AssociateTransitGatewayMulticastDomain (..),
    newAssociateTransitGatewayMulticastDomain,

    -- * Request Lenses
    associateTransitGatewayMulticastDomain_dryRun,
    associateTransitGatewayMulticastDomain_subnetIds,
    associateTransitGatewayMulticastDomain_transitGatewayAttachmentId,
    associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,

    -- * Destructuring the Response
    AssociateTransitGatewayMulticastDomainResponse (..),
    newAssociateTransitGatewayMulticastDomainResponse,

    -- * Response Lenses
    associateTransitGatewayMulticastDomainResponse_associations,
    associateTransitGatewayMulticastDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateTransitGatewayMulticastDomain' smart constructor.
data AssociateTransitGatewayMulticastDomain = AssociateTransitGatewayMulticastDomain'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the subnets to associate with the transit gateway multicast
    -- domain.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the transit gateway attachment to associate with the transit
    -- gateway multicast domain.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'subnetIds', 'associateTransitGatewayMulticastDomain_subnetIds' - The IDs of the subnets to associate with the transit gateway multicast
-- domain.
--
-- 'transitGatewayAttachmentId', 'associateTransitGatewayMulticastDomain_transitGatewayAttachmentId' - The ID of the transit gateway attachment to associate with the transit
-- gateway multicast domain.
--
-- 'transitGatewayMulticastDomainId', 'associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newAssociateTransitGatewayMulticastDomain ::
  AssociateTransitGatewayMulticastDomain
newAssociateTransitGatewayMulticastDomain =
  AssociateTransitGatewayMulticastDomain'
    { dryRun =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateTransitGatewayMulticastDomain_dryRun :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Prelude.Maybe Prelude.Bool)
associateTransitGatewayMulticastDomain_dryRun = Lens.lens (\AssociateTransitGatewayMulticastDomain' {dryRun} -> dryRun) (\s@AssociateTransitGatewayMulticastDomain' {} a -> s {dryRun = a} :: AssociateTransitGatewayMulticastDomain)

-- | The IDs of the subnets to associate with the transit gateway multicast
-- domain.
associateTransitGatewayMulticastDomain_subnetIds :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Prelude.Maybe [Prelude.Text])
associateTransitGatewayMulticastDomain_subnetIds = Lens.lens (\AssociateTransitGatewayMulticastDomain' {subnetIds} -> subnetIds) (\s@AssociateTransitGatewayMulticastDomain' {} a -> s {subnetIds = a} :: AssociateTransitGatewayMulticastDomain) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway attachment to associate with the transit
-- gateway multicast domain.
associateTransitGatewayMulticastDomain_transitGatewayAttachmentId :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
associateTransitGatewayMulticastDomain_transitGatewayAttachmentId = Lens.lens (\AssociateTransitGatewayMulticastDomain' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AssociateTransitGatewayMulticastDomain' {} a -> s {transitGatewayAttachmentId = a} :: AssociateTransitGatewayMulticastDomain)

-- | The ID of the transit gateway multicast domain.
associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId = Lens.lens (\AssociateTransitGatewayMulticastDomain' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@AssociateTransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainId = a} :: AssociateTransitGatewayMulticastDomain)

instance
  Core.AWSRequest
    AssociateTransitGatewayMulticastDomain
  where
  type
    AWSResponse
      AssociateTransitGatewayMulticastDomain =
      AssociateTransitGatewayMulticastDomainResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateTransitGatewayMulticastDomainResponse'
            Prelude.<$> (x Data..@? "associations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateTransitGatewayMulticastDomain
  where
  hashWithSalt
    _salt
    AssociateTransitGatewayMulticastDomain' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` subnetIds
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    AssociateTransitGatewayMulticastDomain
  where
  rnf AssociateTransitGatewayMulticastDomain' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf subnetIds `Prelude.seq`
        Prelude.rnf transitGatewayAttachmentId `Prelude.seq`
          Prelude.rnf transitGatewayMulticastDomainId

instance
  Data.ToHeaders
    AssociateTransitGatewayMulticastDomain
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    AssociateTransitGatewayMulticastDomain
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AssociateTransitGatewayMulticastDomain
  where
  toQuery AssociateTransitGatewayMulticastDomain' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AssociateTransitGatewayMulticastDomain" ::
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

-- | /See:/ 'newAssociateTransitGatewayMulticastDomainResponse' smart constructor.
data AssociateTransitGatewayMulticastDomainResponse = AssociateTransitGatewayMulticastDomainResponse'
  { -- | Information about the transit gateway multicast domain associations.
    associations :: Prelude.Maybe TransitGatewayMulticastDomainAssociations,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AssociateTransitGatewayMulticastDomainResponse
newAssociateTransitGatewayMulticastDomainResponse
  pHttpStatus_ =
    AssociateTransitGatewayMulticastDomainResponse'
      { associations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the transit gateway multicast domain associations.
associateTransitGatewayMulticastDomainResponse_associations :: Lens.Lens' AssociateTransitGatewayMulticastDomainResponse (Prelude.Maybe TransitGatewayMulticastDomainAssociations)
associateTransitGatewayMulticastDomainResponse_associations = Lens.lens (\AssociateTransitGatewayMulticastDomainResponse' {associations} -> associations) (\s@AssociateTransitGatewayMulticastDomainResponse' {} a -> s {associations = a} :: AssociateTransitGatewayMulticastDomainResponse)

-- | The response's http status code.
associateTransitGatewayMulticastDomainResponse_httpStatus :: Lens.Lens' AssociateTransitGatewayMulticastDomainResponse Prelude.Int
associateTransitGatewayMulticastDomainResponse_httpStatus = Lens.lens (\AssociateTransitGatewayMulticastDomainResponse' {httpStatus} -> httpStatus) (\s@AssociateTransitGatewayMulticastDomainResponse' {} a -> s {httpStatus = a} :: AssociateTransitGatewayMulticastDomainResponse)

instance
  Prelude.NFData
    AssociateTransitGatewayMulticastDomainResponse
  where
  rnf
    AssociateTransitGatewayMulticastDomainResponse' {..} =
      Prelude.rnf associations `Prelude.seq`
        Prelude.rnf httpStatus
