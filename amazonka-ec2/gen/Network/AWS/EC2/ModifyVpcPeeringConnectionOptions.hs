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
-- If the peered VPCs are in the same AWS account, you can enable DNS
-- resolution for queries from the local VPC. This ensures that queries
-- from the local VPC resolve to private IP addresses in the peer VPC. This
-- option is not available if the peered VPCs are in different AWS accounts
-- or different Regions. For peered VPCs in different AWS accounts, each
-- AWS account owner must initiate a separate request to modify the peering
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
    modifyVpcPeeringConnectionOptions_dryRun,
    modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId,

    -- * Destructuring the Response
    ModifyVpcPeeringConnectionOptionsResponse (..),
    newModifyVpcPeeringConnectionOptionsResponse,

    -- * Response Lenses
    modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpcPeeringConnectionOptions' smart constructor.
data ModifyVpcPeeringConnectionOptions = ModifyVpcPeeringConnectionOptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The VPC peering connection options for the accepter VPC.
    accepterPeeringConnectionOptions :: Core.Maybe PeeringConnectionOptionsRequest,
    -- | The VPC peering connection options for the requester VPC.
    requesterPeeringConnectionOptions :: Core.Maybe PeeringConnectionOptionsRequest,
    -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpcPeeringConnectionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyVpcPeeringConnectionOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'accepterPeeringConnectionOptions', 'modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions' - The VPC peering connection options for the accepter VPC.
--
-- 'requesterPeeringConnectionOptions', 'modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions' - The VPC peering connection options for the requester VPC.
--
-- 'vpcPeeringConnectionId', 'modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId' - The ID of the VPC peering connection.
newModifyVpcPeeringConnectionOptions ::
  -- | 'vpcPeeringConnectionId'
  Core.Text ->
  ModifyVpcPeeringConnectionOptions
newModifyVpcPeeringConnectionOptions
  pVpcPeeringConnectionId_ =
    ModifyVpcPeeringConnectionOptions'
      { dryRun =
          Core.Nothing,
        accepterPeeringConnectionOptions =
          Core.Nothing,
        requesterPeeringConnectionOptions =
          Core.Nothing,
        vpcPeeringConnectionId =
          pVpcPeeringConnectionId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcPeeringConnectionOptions_dryRun :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Core.Maybe Core.Bool)
modifyVpcPeeringConnectionOptions_dryRun = Lens.lens (\ModifyVpcPeeringConnectionOptions' {dryRun} -> dryRun) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {dryRun = a} :: ModifyVpcPeeringConnectionOptions)

-- | The VPC peering connection options for the accepter VPC.
modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Core.Maybe PeeringConnectionOptionsRequest)
modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptions' {accepterPeeringConnectionOptions} -> accepterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {accepterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptions)

-- | The VPC peering connection options for the requester VPC.
modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Core.Maybe PeeringConnectionOptionsRequest)
modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptions' {requesterPeeringConnectionOptions} -> requesterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptions' {} a -> s {requesterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptions)

-- | The ID of the VPC peering connection.
modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId :: Lens.Lens' ModifyVpcPeeringConnectionOptions Core.Text
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
            Core.<$> (x Core..@? "accepterPeeringConnectionOptions")
            Core.<*> (x Core..@? "requesterPeeringConnectionOptions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ModifyVpcPeeringConnectionOptions

instance
  Core.NFData
    ModifyVpcPeeringConnectionOptions

instance
  Core.ToHeaders
    ModifyVpcPeeringConnectionOptions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ModifyVpcPeeringConnectionOptions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ModifyVpcPeeringConnectionOptions
  where
  toQuery ModifyVpcPeeringConnectionOptions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ModifyVpcPeeringConnectionOptions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "AccepterPeeringConnectionOptions"
          Core.=: accepterPeeringConnectionOptions,
        "RequesterPeeringConnectionOptions"
          Core.=: requesterPeeringConnectionOptions,
        "VpcPeeringConnectionId"
          Core.=: vpcPeeringConnectionId
      ]

-- | /See:/ 'newModifyVpcPeeringConnectionOptionsResponse' smart constructor.
data ModifyVpcPeeringConnectionOptionsResponse = ModifyVpcPeeringConnectionOptionsResponse'
  { -- | Information about the VPC peering connection options for the accepter
    -- VPC.
    accepterPeeringConnectionOptions :: Core.Maybe PeeringConnectionOptions,
    -- | Information about the VPC peering connection options for the requester
    -- VPC.
    requesterPeeringConnectionOptions :: Core.Maybe PeeringConnectionOptions,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpcPeeringConnectionOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accepterPeeringConnectionOptions', 'modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions' - Information about the VPC peering connection options for the accepter
-- VPC.
--
-- 'requesterPeeringConnectionOptions', 'modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions' - Information about the VPC peering connection options for the requester
-- VPC.
--
-- 'httpStatus', 'modifyVpcPeeringConnectionOptionsResponse_httpStatus' - The response's http status code.
newModifyVpcPeeringConnectionOptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyVpcPeeringConnectionOptionsResponse
newModifyVpcPeeringConnectionOptionsResponse
  pHttpStatus_ =
    ModifyVpcPeeringConnectionOptionsResponse'
      { accepterPeeringConnectionOptions =
          Core.Nothing,
        requesterPeeringConnectionOptions =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the VPC peering connection options for the accepter
-- VPC.
modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptionsResponse (Core.Maybe PeeringConnectionOptions)
modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptionsResponse' {accepterPeeringConnectionOptions} -> accepterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptionsResponse' {} a -> s {accepterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptionsResponse)

-- | Information about the VPC peering connection options for the requester
-- VPC.
modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptionsResponse (Core.Maybe PeeringConnectionOptions)
modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions = Lens.lens (\ModifyVpcPeeringConnectionOptionsResponse' {requesterPeeringConnectionOptions} -> requesterPeeringConnectionOptions) (\s@ModifyVpcPeeringConnectionOptionsResponse' {} a -> s {requesterPeeringConnectionOptions = a} :: ModifyVpcPeeringConnectionOptionsResponse)

-- | The response's http status code.
modifyVpcPeeringConnectionOptionsResponse_httpStatus :: Lens.Lens' ModifyVpcPeeringConnectionOptionsResponse Core.Int
modifyVpcPeeringConnectionOptionsResponse_httpStatus = Lens.lens (\ModifyVpcPeeringConnectionOptionsResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcPeeringConnectionOptionsResponse' {} a -> s {httpStatus = a} :: ModifyVpcPeeringConnectionOptionsResponse)

instance
  Core.NFData
    ModifyVpcPeeringConnectionOptionsResponse
