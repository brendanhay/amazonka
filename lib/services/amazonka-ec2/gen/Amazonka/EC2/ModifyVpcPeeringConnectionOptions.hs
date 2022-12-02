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
-- Module      : Amazonka.EC2.ModifyVpcPeeringConnectionOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Modifies the VPC peering connection options on one side of a VPC peering
-- connection. You can do the following:
--
-- -   Enable\/disable communication over the peering connection between an
--     EC2-Classic instance that\'s linked to your VPC (using ClassicLink)
--     and instances in the peer VPC.
--
-- -   Enable\/disable communication over the peering connection between
--     instances in your VPC and an EC2-Classic instance that\'s linked to
--     the peer VPC.
--
-- -   Enable\/disable the ability to resolve public DNS hostnames to
--     private IP addresses when queried from instances in the peer VPC.
--
-- If the peered VPCs are in the same Amazon Web Services account, you can
-- enable DNS resolution for queries from the local VPC. This ensures that
-- queries from the local VPC resolve to private IP addresses in the peer
-- VPC. This option is not available if the peered VPCs are in different
-- different Amazon Web Services accounts or different Regions. For peered
-- VPCs in different Amazon Web Services accounts, each Amazon Web Services
-- account owner must initiate a separate request to modify the peering
-- connection options. For inter-region peering connections, you must use
-- the Region for the requester VPC to modify the requester VPC peering
-- options and the Region for the accepter VPC to modify the accepter VPC
-- peering options. To verify which VPCs are the accepter and the requester
-- for a VPC peering connection, use the DescribeVpcPeeringConnections
-- command.
module Amazonka.EC2.ModifyVpcPeeringConnectionOptions
  ( -- * Creating a Request
    ModifyVpcPeeringConnectionOptions (..),
    newModifyVpcPeeringConnectionOptions,

    -- * Request Lenses
    modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_dryRun,
    modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId,

    -- * Destructuring the Response
    ModifyVpcPeeringConnectionOptionsResponse (..),
    newModifyVpcPeeringConnectionOptionsResponse,

    -- * Response Lenses
    modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpcPeeringConnectionOptions' smart constructor.
data ModifyVpcPeeringConnectionOptions = ModifyVpcPeeringConnectionOptions'
  { -- | The VPC peering connection options for the requester VPC.
    requesterPeeringConnectionOptions :: Prelude.Maybe PeeringConnectionOptionsRequest,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The VPC peering connection options for the accepter VPC.
    accepterPeeringConnectionOptions :: Prelude.Maybe PeeringConnectionOptionsRequest,
    -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcPeeringConnectionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requesterPeeringConnectionOptions', 'modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions' - The VPC peering connection options for the requester VPC.
--
-- 'dryRun', 'modifyVpcPeeringConnectionOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'accepterPeeringConnectionOptions', 'modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions' - The VPC peering connection options for the accepter VPC.
--
-- 'vpcPeeringConnectionId', 'modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId' - The ID of the VPC peering connection.
newModifyVpcPeeringConnectionOptions ::
  -- | 'vpcPeeringConnectionId'
  Prelude.Text ->
  ModifyVpcPeeringConnectionOptions
newModifyVpcPeeringConnectionOptions
  pVpcPeeringConnectionId_ =
    ModifyVpcPeeringConnectionOptions'
      { requesterPeeringConnectionOptions =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        accepterPeeringConnectionOptions =
          Prelude.Nothing,
        vpcPeeringConnectionId =
          pVpcPeeringConnectionId_
      }

-- | The VPC peering connection options for the requester VPC.
modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Prelude.Maybe PeeringConnectionOptionsRequest)
modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptions' {requesterPeeringConnectionOptions} -> requesterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {requesterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptions)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcPeeringConnectionOptions_dryRun :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Prelude.Maybe Prelude.Bool)
modifyVpcPeeringConnectionOptions_dryRun = Lens.lens (\ModifyVpcPeeringConnectionOptions' {dryRun} -> dryRun) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {dryRun = a} :: ModifyVpcPeeringConnectionOptions)

-- | The VPC peering connection options for the accepter VPC.
modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Prelude.Maybe PeeringConnectionOptionsRequest)
modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptions' {accepterPeeringConnectionOptions} -> accepterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {accepterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptions)

-- | The ID of the VPC peering connection.
modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId :: Lens.Lens' ModifyVpcPeeringConnectionOptions Prelude.Text
modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId = Lens.lens (\ModifyVpcPeeringConnectionOptions' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {vpcPeeringConnectionId = a} :: ModifyVpcPeeringConnectionOptions)

instance
  Core.AWSRequest
    ModifyVpcPeeringConnectionOptions
  where
  type
    AWSResponse ModifyVpcPeeringConnectionOptions =
      ModifyVpcPeeringConnectionOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcPeeringConnectionOptionsResponse'
            Prelude.<$> (x Data..@? "requesterPeeringConnectionOptions")
              Prelude.<*> (x Data..@? "accepterPeeringConnectionOptions")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVpcPeeringConnectionOptions
  where
  hashWithSalt
    _salt
    ModifyVpcPeeringConnectionOptions' {..} =
      _salt
        `Prelude.hashWithSalt` requesterPeeringConnectionOptions
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` accepterPeeringConnectionOptions
        `Prelude.hashWithSalt` vpcPeeringConnectionId

instance
  Prelude.NFData
    ModifyVpcPeeringConnectionOptions
  where
  rnf ModifyVpcPeeringConnectionOptions' {..} =
    Prelude.rnf requesterPeeringConnectionOptions
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf accepterPeeringConnectionOptions
      `Prelude.seq` Prelude.rnf vpcPeeringConnectionId

instance
  Data.ToHeaders
    ModifyVpcPeeringConnectionOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyVpcPeeringConnectionOptions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyVpcPeeringConnectionOptions
  where
  toQuery ModifyVpcPeeringConnectionOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyVpcPeeringConnectionOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "RequesterPeeringConnectionOptions"
          Data.=: requesterPeeringConnectionOptions,
        "DryRun" Data.=: dryRun,
        "AccepterPeeringConnectionOptions"
          Data.=: accepterPeeringConnectionOptions,
        "VpcPeeringConnectionId"
          Data.=: vpcPeeringConnectionId
      ]

-- | /See:/ 'newModifyVpcPeeringConnectionOptionsResponse' smart constructor.
data ModifyVpcPeeringConnectionOptionsResponse = ModifyVpcPeeringConnectionOptionsResponse'
  { -- | Information about the VPC peering connection options for the requester
    -- VPC.
    requesterPeeringConnectionOptions :: Prelude.Maybe PeeringConnectionOptions,
    -- | Information about the VPC peering connection options for the accepter
    -- VPC.
    accepterPeeringConnectionOptions :: Prelude.Maybe PeeringConnectionOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcPeeringConnectionOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requesterPeeringConnectionOptions', 'modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions' - Information about the VPC peering connection options for the requester
-- VPC.
--
-- 'accepterPeeringConnectionOptions', 'modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions' - Information about the VPC peering connection options for the accepter
-- VPC.
--
-- 'httpStatus', 'modifyVpcPeeringConnectionOptionsResponse_httpStatus' - The response's http status code.
newModifyVpcPeeringConnectionOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVpcPeeringConnectionOptionsResponse
newModifyVpcPeeringConnectionOptionsResponse
  pHttpStatus_ =
    ModifyVpcPeeringConnectionOptionsResponse'
      { requesterPeeringConnectionOptions =
          Prelude.Nothing,
        accepterPeeringConnectionOptions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the VPC peering connection options for the requester
-- VPC.
modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptionsResponse (Prelude.Maybe PeeringConnectionOptions)
modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptionsResponse' {requesterPeeringConnectionOptions} -> requesterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptionsResponse' {} a -> s {requesterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptionsResponse)

-- | Information about the VPC peering connection options for the accepter
-- VPC.
modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptionsResponse (Prelude.Maybe PeeringConnectionOptions)
modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptionsResponse' {accepterPeeringConnectionOptions} -> accepterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptionsResponse' {} a -> s {accepterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptionsResponse)

-- | The response's http status code.
modifyVpcPeeringConnectionOptionsResponse_httpStatus :: Lens.Lens' ModifyVpcPeeringConnectionOptionsResponse Prelude.Int
modifyVpcPeeringConnectionOptionsResponse_httpStatus = Lens.lens (\ModifyVpcPeeringConnectionOptionsResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcPeeringConnectionOptionsResponse' {} a -> s {httpStatus = a} :: ModifyVpcPeeringConnectionOptionsResponse)

instance
  Prelude.NFData
    ModifyVpcPeeringConnectionOptionsResponse
  where
  rnf ModifyVpcPeeringConnectionOptionsResponse' {..} =
    Prelude.rnf requesterPeeringConnectionOptions
      `Prelude.seq` Prelude.rnf accepterPeeringConnectionOptions
      `Prelude.seq` Prelude.rnf httpStatus
