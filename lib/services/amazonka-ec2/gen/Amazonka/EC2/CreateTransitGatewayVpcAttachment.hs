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
-- Module      : Amazonka.EC2.CreateTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified VPC to the specified transit gateway.
--
-- If you attach a VPC with a CIDR range that overlaps the CIDR range of a
-- VPC that is already attached, the new VPC CIDR range is not propagated
-- to the default propagation route table.
--
-- To send VPC traffic to an attached transit gateway, add a route to the
-- VPC route table using CreateRoute.
module Amazonka.EC2.CreateTransitGatewayVpcAttachment
  ( -- * Creating a Request
    CreateTransitGatewayVpcAttachment (..),
    newCreateTransitGatewayVpcAttachment,

    -- * Request Lenses
    createTransitGatewayVpcAttachment_dryRun,
    createTransitGatewayVpcAttachment_options,
    createTransitGatewayVpcAttachment_tagSpecifications,
    createTransitGatewayVpcAttachment_transitGatewayId,
    createTransitGatewayVpcAttachment_vpcId,
    createTransitGatewayVpcAttachment_subnetIds,

    -- * Destructuring the Response
    CreateTransitGatewayVpcAttachmentResponse (..),
    newCreateTransitGatewayVpcAttachmentResponse,

    -- * Response Lenses
    createTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    createTransitGatewayVpcAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayVpcAttachment' smart constructor.
data CreateTransitGatewayVpcAttachment = CreateTransitGatewayVpcAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The VPC attachment options.
    options :: Prelude.Maybe CreateTransitGatewayVpcAttachmentRequestOptions,
    -- | The tags to apply to the VPC attachment.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text,
    -- | The IDs of one or more subnets. You can specify only one subnet per
    -- Availability Zone. You must specify at least one subnet, but we
    -- recommend that you specify two subnets for better availability. The
    -- transit gateway uses one IP address from each specified subnet.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createTransitGatewayVpcAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'options', 'createTransitGatewayVpcAttachment_options' - The VPC attachment options.
--
-- 'tagSpecifications', 'createTransitGatewayVpcAttachment_tagSpecifications' - The tags to apply to the VPC attachment.
--
-- 'transitGatewayId', 'createTransitGatewayVpcAttachment_transitGatewayId' - The ID of the transit gateway.
--
-- 'vpcId', 'createTransitGatewayVpcAttachment_vpcId' - The ID of the VPC.
--
-- 'subnetIds', 'createTransitGatewayVpcAttachment_subnetIds' - The IDs of one or more subnets. You can specify only one subnet per
-- Availability Zone. You must specify at least one subnet, but we
-- recommend that you specify two subnets for better availability. The
-- transit gateway uses one IP address from each specified subnet.
newCreateTransitGatewayVpcAttachment ::
  -- | 'transitGatewayId'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  CreateTransitGatewayVpcAttachment
newCreateTransitGatewayVpcAttachment
  pTransitGatewayId_
  pVpcId_ =
    CreateTransitGatewayVpcAttachment'
      { dryRun =
          Prelude.Nothing,
        options = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        transitGatewayId = pTransitGatewayId_,
        vpcId = pVpcId_,
        subnetIds = Prelude.mempty
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayVpcAttachment_dryRun :: Lens.Lens' CreateTransitGatewayVpcAttachment (Prelude.Maybe Prelude.Bool)
createTransitGatewayVpcAttachment_dryRun = Lens.lens (\CreateTransitGatewayVpcAttachment' {dryRun} -> dryRun) (\s@CreateTransitGatewayVpcAttachment' {} a -> s {dryRun = a} :: CreateTransitGatewayVpcAttachment)

-- | The VPC attachment options.
createTransitGatewayVpcAttachment_options :: Lens.Lens' CreateTransitGatewayVpcAttachment (Prelude.Maybe CreateTransitGatewayVpcAttachmentRequestOptions)
createTransitGatewayVpcAttachment_options = Lens.lens (\CreateTransitGatewayVpcAttachment' {options} -> options) (\s@CreateTransitGatewayVpcAttachment' {} a -> s {options = a} :: CreateTransitGatewayVpcAttachment)

-- | The tags to apply to the VPC attachment.
createTransitGatewayVpcAttachment_tagSpecifications :: Lens.Lens' CreateTransitGatewayVpcAttachment (Prelude.Maybe [TagSpecification])
createTransitGatewayVpcAttachment_tagSpecifications = Lens.lens (\CreateTransitGatewayVpcAttachment' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayVpcAttachment' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayVpcAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway.
createTransitGatewayVpcAttachment_transitGatewayId :: Lens.Lens' CreateTransitGatewayVpcAttachment Prelude.Text
createTransitGatewayVpcAttachment_transitGatewayId = Lens.lens (\CreateTransitGatewayVpcAttachment' {transitGatewayId} -> transitGatewayId) (\s@CreateTransitGatewayVpcAttachment' {} a -> s {transitGatewayId = a} :: CreateTransitGatewayVpcAttachment)

-- | The ID of the VPC.
createTransitGatewayVpcAttachment_vpcId :: Lens.Lens' CreateTransitGatewayVpcAttachment Prelude.Text
createTransitGatewayVpcAttachment_vpcId = Lens.lens (\CreateTransitGatewayVpcAttachment' {vpcId} -> vpcId) (\s@CreateTransitGatewayVpcAttachment' {} a -> s {vpcId = a} :: CreateTransitGatewayVpcAttachment)

-- | The IDs of one or more subnets. You can specify only one subnet per
-- Availability Zone. You must specify at least one subnet, but we
-- recommend that you specify two subnets for better availability. The
-- transit gateway uses one IP address from each specified subnet.
createTransitGatewayVpcAttachment_subnetIds :: Lens.Lens' CreateTransitGatewayVpcAttachment [Prelude.Text]
createTransitGatewayVpcAttachment_subnetIds = Lens.lens (\CreateTransitGatewayVpcAttachment' {subnetIds} -> subnetIds) (\s@CreateTransitGatewayVpcAttachment' {} a -> s {subnetIds = a} :: CreateTransitGatewayVpcAttachment) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CreateTransitGatewayVpcAttachment
  where
  type
    AWSResponse CreateTransitGatewayVpcAttachment =
      CreateTransitGatewayVpcAttachmentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayVpcAttachmentResponse'
            Prelude.<$> (x Data..@? "transitGatewayVpcAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayVpcAttachment
  where
  hashWithSalt
    _salt
    CreateTransitGatewayVpcAttachment' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` options
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` transitGatewayId
        `Prelude.hashWithSalt` vpcId
        `Prelude.hashWithSalt` subnetIds

instance
  Prelude.NFData
    CreateTransitGatewayVpcAttachment
  where
  rnf CreateTransitGatewayVpcAttachment' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnetIds

instance
  Data.ToHeaders
    CreateTransitGatewayVpcAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateTransitGatewayVpcAttachment
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateTransitGatewayVpcAttachment
  where
  toQuery CreateTransitGatewayVpcAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateTransitGatewayVpcAttachment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Options" Data.=: options,
        Data.toQuery
          ( Data.toQueryList "TagSpecifications"
              Prelude.<$> tagSpecifications
          ),
        "TransitGatewayId" Data.=: transitGatewayId,
        "VpcId" Data.=: vpcId,
        Data.toQueryList "SubnetIds" subnetIds
      ]

-- | /See:/ 'newCreateTransitGatewayVpcAttachmentResponse' smart constructor.
data CreateTransitGatewayVpcAttachmentResponse = CreateTransitGatewayVpcAttachmentResponse'
  { -- | Information about the VPC attachment.
    transitGatewayVpcAttachment :: Prelude.Maybe TransitGatewayVpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayVpcAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayVpcAttachment', 'createTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment' - Information about the VPC attachment.
--
-- 'httpStatus', 'createTransitGatewayVpcAttachmentResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayVpcAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayVpcAttachmentResponse
newCreateTransitGatewayVpcAttachmentResponse
  pHttpStatus_ =
    CreateTransitGatewayVpcAttachmentResponse'
      { transitGatewayVpcAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the VPC attachment.
createTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment :: Lens.Lens' CreateTransitGatewayVpcAttachmentResponse (Prelude.Maybe TransitGatewayVpcAttachment)
createTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment = Lens.lens (\CreateTransitGatewayVpcAttachmentResponse' {transitGatewayVpcAttachment} -> transitGatewayVpcAttachment) (\s@CreateTransitGatewayVpcAttachmentResponse' {} a -> s {transitGatewayVpcAttachment = a} :: CreateTransitGatewayVpcAttachmentResponse)

-- | The response's http status code.
createTransitGatewayVpcAttachmentResponse_httpStatus :: Lens.Lens' CreateTransitGatewayVpcAttachmentResponse Prelude.Int
createTransitGatewayVpcAttachmentResponse_httpStatus = Lens.lens (\CreateTransitGatewayVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayVpcAttachmentResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayVpcAttachmentResponse)

instance
  Prelude.NFData
    CreateTransitGatewayVpcAttachmentResponse
  where
  rnf CreateTransitGatewayVpcAttachmentResponse' {..} =
    Prelude.rnf transitGatewayVpcAttachment
      `Prelude.seq` Prelude.rnf httpStatus
