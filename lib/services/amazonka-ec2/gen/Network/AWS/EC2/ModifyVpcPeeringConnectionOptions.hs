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
-- Module      : Network.AWS.EC2.ModifyVpcPeeringConnectionOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
module Network.AWS.EC2.ModifyVpcPeeringConnectionOptions
  ( -- * Creating a Request
    ModifyVpcPeeringConnectionOptions (..),
    newModifyVpcPeeringConnectionOptions,

    -- * Request Lenses
    modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_dryRun,
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpcPeeringConnectionOptions' smart constructor.
data ModifyVpcPeeringConnectionOptions = ModifyVpcPeeringConnectionOptions'
  { -- | The VPC peering connection options for the requester VPC.
    requesterPeeringConnectionOptions :: Prelude.Maybe PeeringConnectionOptionsRequest,
    -- | The VPC peering connection options for the accepter VPC.
    accepterPeeringConnectionOptions :: Prelude.Maybe PeeringConnectionOptionsRequest,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
-- 'accepterPeeringConnectionOptions', 'modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions' - The VPC peering connection options for the accepter VPC.
--
-- 'dryRun', 'modifyVpcPeeringConnectionOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
        accepterPeeringConnectionOptions =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        vpcPeeringConnectionId =
          pVpcPeeringConnectionId_
      }

-- | The VPC peering connection options for the requester VPC.
modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Prelude.Maybe PeeringConnectionOptionsRequest)
modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptions' {requesterPeeringConnectionOptions} -> requesterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {requesterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptions)

-- | The VPC peering connection options for the accepter VPC.
modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Prelude.Maybe PeeringConnectionOptionsRequest)
modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptions' {accepterPeeringConnectionOptions} -> accepterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {accepterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptions)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcPeeringConnectionOptions_dryRun :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Prelude.Maybe Prelude.Bool)
modifyVpcPeeringConnectionOptions_dryRun = Lens.lens (\ModifyVpcPeeringConnectionOptions' {dryRun} -> dryRun) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {dryRun = a} :: ModifyVpcPeeringConnectionOptions)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcPeeringConnectionOptionsResponse'
            Prelude.<$> (x Core..@? "requesterPeeringConnectionOptions")
              Prelude.<*> (x Core..@? "accepterPeeringConnectionOptions")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVpcPeeringConnectionOptions

instance
  Prelude.NFData
    ModifyVpcPeeringConnectionOptions

instance
  Core.ToHeaders
    ModifyVpcPeeringConnectionOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ModifyVpcPeeringConnectionOptions
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ModifyVpcPeeringConnectionOptions
  where
  toQuery ModifyVpcPeeringConnectionOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyVpcPeeringConnectionOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "RequesterPeeringConnectionOptions"
          Core.=: requesterPeeringConnectionOptions,
        "AccepterPeeringConnectionOptions"
          Core.=: accepterPeeringConnectionOptions,
        "DryRun" Core.=: dryRun,
        "VpcPeeringConnectionId"
          Core.=: vpcPeeringConnectionId
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
