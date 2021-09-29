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
-- Module      : Network.AWS.EC2.ModifyTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified VPC attachment.
module Network.AWS.EC2.ModifyTransitGatewayVpcAttachment
  ( -- * Creating a Request
    ModifyTransitGatewayVpcAttachment (..),
    newModifyTransitGatewayVpcAttachment,

    -- * Request Lenses
    modifyTransitGatewayVpcAttachment_dryRun,
    modifyTransitGatewayVpcAttachment_removeSubnetIds,
    modifyTransitGatewayVpcAttachment_options,
    modifyTransitGatewayVpcAttachment_addSubnetIds,
    modifyTransitGatewayVpcAttachment_transitGatewayAttachmentId,

    -- * Destructuring the Response
    ModifyTransitGatewayVpcAttachmentResponse (..),
    newModifyTransitGatewayVpcAttachmentResponse,

    -- * Response Lenses
    modifyTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    modifyTransitGatewayVpcAttachmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyTransitGatewayVpcAttachment' smart constructor.
data ModifyTransitGatewayVpcAttachment = ModifyTransitGatewayVpcAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of one or more subnets to remove.
    removeSubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The new VPC attachment options.
    options :: Prelude.Maybe ModifyTransitGatewayVpcAttachmentRequestOptions,
    -- | The IDs of one or more subnets to add. You can specify at most one
    -- subnet per Availability Zone.
    addSubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTransitGatewayVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyTransitGatewayVpcAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'removeSubnetIds', 'modifyTransitGatewayVpcAttachment_removeSubnetIds' - The IDs of one or more subnets to remove.
--
-- 'options', 'modifyTransitGatewayVpcAttachment_options' - The new VPC attachment options.
--
-- 'addSubnetIds', 'modifyTransitGatewayVpcAttachment_addSubnetIds' - The IDs of one or more subnets to add. You can specify at most one
-- subnet per Availability Zone.
--
-- 'transitGatewayAttachmentId', 'modifyTransitGatewayVpcAttachment_transitGatewayAttachmentId' - The ID of the attachment.
newModifyTransitGatewayVpcAttachment ::
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  ModifyTransitGatewayVpcAttachment
newModifyTransitGatewayVpcAttachment
  pTransitGatewayAttachmentId_ =
    ModifyTransitGatewayVpcAttachment'
      { dryRun =
          Prelude.Nothing,
        removeSubnetIds = Prelude.Nothing,
        options = Prelude.Nothing,
        addSubnetIds = Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTransitGatewayVpcAttachment_dryRun :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Prelude.Maybe Prelude.Bool)
modifyTransitGatewayVpcAttachment_dryRun = Lens.lens (\ModifyTransitGatewayVpcAttachment' {dryRun} -> dryRun) (\s@ModifyTransitGatewayVpcAttachment' {} a -> s {dryRun = a} :: ModifyTransitGatewayVpcAttachment)

-- | The IDs of one or more subnets to remove.
modifyTransitGatewayVpcAttachment_removeSubnetIds :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Prelude.Maybe [Prelude.Text])
modifyTransitGatewayVpcAttachment_removeSubnetIds = Lens.lens (\ModifyTransitGatewayVpcAttachment' {removeSubnetIds} -> removeSubnetIds) (\s@ModifyTransitGatewayVpcAttachment' {} a -> s {removeSubnetIds = a} :: ModifyTransitGatewayVpcAttachment) Prelude.. Lens.mapping Lens._Coerce

-- | The new VPC attachment options.
modifyTransitGatewayVpcAttachment_options :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Prelude.Maybe ModifyTransitGatewayVpcAttachmentRequestOptions)
modifyTransitGatewayVpcAttachment_options = Lens.lens (\ModifyTransitGatewayVpcAttachment' {options} -> options) (\s@ModifyTransitGatewayVpcAttachment' {} a -> s {options = a} :: ModifyTransitGatewayVpcAttachment)

-- | The IDs of one or more subnets to add. You can specify at most one
-- subnet per Availability Zone.
modifyTransitGatewayVpcAttachment_addSubnetIds :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Prelude.Maybe [Prelude.Text])
modifyTransitGatewayVpcAttachment_addSubnetIds = Lens.lens (\ModifyTransitGatewayVpcAttachment' {addSubnetIds} -> addSubnetIds) (\s@ModifyTransitGatewayVpcAttachment' {} a -> s {addSubnetIds = a} :: ModifyTransitGatewayVpcAttachment) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the attachment.
modifyTransitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' ModifyTransitGatewayVpcAttachment Prelude.Text
modifyTransitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\ModifyTransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@ModifyTransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: ModifyTransitGatewayVpcAttachment)

instance
  Core.AWSRequest
    ModifyTransitGatewayVpcAttachment
  where
  type
    AWSResponse ModifyTransitGatewayVpcAttachment =
      ModifyTransitGatewayVpcAttachmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTransitGatewayVpcAttachmentResponse'
            Prelude.<$> (x Core..@? "transitGatewayVpcAttachment")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyTransitGatewayVpcAttachment

instance
  Prelude.NFData
    ModifyTransitGatewayVpcAttachment

instance
  Core.ToHeaders
    ModifyTransitGatewayVpcAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ModifyTransitGatewayVpcAttachment
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ModifyTransitGatewayVpcAttachment
  where
  toQuery ModifyTransitGatewayVpcAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyTransitGatewayVpcAttachment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "RemoveSubnetIds"
              Prelude.<$> removeSubnetIds
          ),
        "Options" Core.=: options,
        Core.toQuery
          ( Core.toQueryList "AddSubnetIds"
              Prelude.<$> addSubnetIds
          ),
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newModifyTransitGatewayVpcAttachmentResponse' smart constructor.
data ModifyTransitGatewayVpcAttachmentResponse = ModifyTransitGatewayVpcAttachmentResponse'
  { -- | Information about the modified attachment.
    transitGatewayVpcAttachment :: Prelude.Maybe TransitGatewayVpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTransitGatewayVpcAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayVpcAttachment', 'modifyTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment' - Information about the modified attachment.
--
-- 'httpStatus', 'modifyTransitGatewayVpcAttachmentResponse_httpStatus' - The response's http status code.
newModifyTransitGatewayVpcAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyTransitGatewayVpcAttachmentResponse
newModifyTransitGatewayVpcAttachmentResponse
  pHttpStatus_ =
    ModifyTransitGatewayVpcAttachmentResponse'
      { transitGatewayVpcAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the modified attachment.
modifyTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment :: Lens.Lens' ModifyTransitGatewayVpcAttachmentResponse (Prelude.Maybe TransitGatewayVpcAttachment)
modifyTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment = Lens.lens (\ModifyTransitGatewayVpcAttachmentResponse' {transitGatewayVpcAttachment} -> transitGatewayVpcAttachment) (\s@ModifyTransitGatewayVpcAttachmentResponse' {} a -> s {transitGatewayVpcAttachment = a} :: ModifyTransitGatewayVpcAttachmentResponse)

-- | The response's http status code.
modifyTransitGatewayVpcAttachmentResponse_httpStatus :: Lens.Lens' ModifyTransitGatewayVpcAttachmentResponse Prelude.Int
modifyTransitGatewayVpcAttachmentResponse_httpStatus = Lens.lens (\ModifyTransitGatewayVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@ModifyTransitGatewayVpcAttachmentResponse' {} a -> s {httpStatus = a} :: ModifyTransitGatewayVpcAttachmentResponse)

instance
  Prelude.NFData
    ModifyTransitGatewayVpcAttachmentResponse
