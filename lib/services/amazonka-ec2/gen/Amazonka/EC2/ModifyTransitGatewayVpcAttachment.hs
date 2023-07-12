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
-- Module      : Amazonka.EC2.ModifyTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified VPC attachment.
module Amazonka.EC2.ModifyTransitGatewayVpcAttachment
  ( -- * Creating a Request
    ModifyTransitGatewayVpcAttachment (..),
    newModifyTransitGatewayVpcAttachment,

    -- * Request Lenses
    modifyTransitGatewayVpcAttachment_addSubnetIds,
    modifyTransitGatewayVpcAttachment_dryRun,
    modifyTransitGatewayVpcAttachment_options,
    modifyTransitGatewayVpcAttachment_removeSubnetIds,
    modifyTransitGatewayVpcAttachment_transitGatewayAttachmentId,

    -- * Destructuring the Response
    ModifyTransitGatewayVpcAttachmentResponse (..),
    newModifyTransitGatewayVpcAttachmentResponse,

    -- * Response Lenses
    modifyTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    modifyTransitGatewayVpcAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyTransitGatewayVpcAttachment' smart constructor.
data ModifyTransitGatewayVpcAttachment = ModifyTransitGatewayVpcAttachment'
  { -- | The IDs of one or more subnets to add. You can specify at most one
    -- subnet per Availability Zone.
    addSubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The new VPC attachment options.
    options :: Prelude.Maybe ModifyTransitGatewayVpcAttachmentRequestOptions,
    -- | The IDs of one or more subnets to remove.
    removeSubnetIds :: Prelude.Maybe [Prelude.Text],
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
-- 'addSubnetIds', 'modifyTransitGatewayVpcAttachment_addSubnetIds' - The IDs of one or more subnets to add. You can specify at most one
-- subnet per Availability Zone.
--
-- 'dryRun', 'modifyTransitGatewayVpcAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'options', 'modifyTransitGatewayVpcAttachment_options' - The new VPC attachment options.
--
-- 'removeSubnetIds', 'modifyTransitGatewayVpcAttachment_removeSubnetIds' - The IDs of one or more subnets to remove.
--
-- 'transitGatewayAttachmentId', 'modifyTransitGatewayVpcAttachment_transitGatewayAttachmentId' - The ID of the attachment.
newModifyTransitGatewayVpcAttachment ::
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  ModifyTransitGatewayVpcAttachment
newModifyTransitGatewayVpcAttachment
  pTransitGatewayAttachmentId_ =
    ModifyTransitGatewayVpcAttachment'
      { addSubnetIds =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        options = Prelude.Nothing,
        removeSubnetIds = Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | The IDs of one or more subnets to add. You can specify at most one
-- subnet per Availability Zone.
modifyTransitGatewayVpcAttachment_addSubnetIds :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Prelude.Maybe [Prelude.Text])
modifyTransitGatewayVpcAttachment_addSubnetIds = Lens.lens (\ModifyTransitGatewayVpcAttachment' {addSubnetIds} -> addSubnetIds) (\s@ModifyTransitGatewayVpcAttachment' {} a -> s {addSubnetIds = a} :: ModifyTransitGatewayVpcAttachment) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTransitGatewayVpcAttachment_dryRun :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Prelude.Maybe Prelude.Bool)
modifyTransitGatewayVpcAttachment_dryRun = Lens.lens (\ModifyTransitGatewayVpcAttachment' {dryRun} -> dryRun) (\s@ModifyTransitGatewayVpcAttachment' {} a -> s {dryRun = a} :: ModifyTransitGatewayVpcAttachment)

-- | The new VPC attachment options.
modifyTransitGatewayVpcAttachment_options :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Prelude.Maybe ModifyTransitGatewayVpcAttachmentRequestOptions)
modifyTransitGatewayVpcAttachment_options = Lens.lens (\ModifyTransitGatewayVpcAttachment' {options} -> options) (\s@ModifyTransitGatewayVpcAttachment' {} a -> s {options = a} :: ModifyTransitGatewayVpcAttachment)

-- | The IDs of one or more subnets to remove.
modifyTransitGatewayVpcAttachment_removeSubnetIds :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Prelude.Maybe [Prelude.Text])
modifyTransitGatewayVpcAttachment_removeSubnetIds = Lens.lens (\ModifyTransitGatewayVpcAttachment' {removeSubnetIds} -> removeSubnetIds) (\s@ModifyTransitGatewayVpcAttachment' {} a -> s {removeSubnetIds = a} :: ModifyTransitGatewayVpcAttachment) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTransitGatewayVpcAttachmentResponse'
            Prelude.<$> (x Data..@? "transitGatewayVpcAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyTransitGatewayVpcAttachment
  where
  hashWithSalt
    _salt
    ModifyTransitGatewayVpcAttachment' {..} =
      _salt
        `Prelude.hashWithSalt` addSubnetIds
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` options
        `Prelude.hashWithSalt` removeSubnetIds
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    ModifyTransitGatewayVpcAttachment
  where
  rnf ModifyTransitGatewayVpcAttachment' {..} =
    Prelude.rnf addSubnetIds
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf removeSubnetIds
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId

instance
  Data.ToHeaders
    ModifyTransitGatewayVpcAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyTransitGatewayVpcAttachment
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyTransitGatewayVpcAttachment
  where
  toQuery ModifyTransitGatewayVpcAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyTransitGatewayVpcAttachment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AddSubnetIds"
              Prelude.<$> addSubnetIds
          ),
        "DryRun" Data.=: dryRun,
        "Options" Data.=: options,
        Data.toQuery
          ( Data.toQueryList "RemoveSubnetIds"
              Prelude.<$> removeSubnetIds
          ),
        "TransitGatewayAttachmentId"
          Data.=: transitGatewayAttachmentId
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
  where
  rnf ModifyTransitGatewayVpcAttachmentResponse' {..} =
    Prelude.rnf transitGatewayVpcAttachment
      `Prelude.seq` Prelude.rnf httpStatus
