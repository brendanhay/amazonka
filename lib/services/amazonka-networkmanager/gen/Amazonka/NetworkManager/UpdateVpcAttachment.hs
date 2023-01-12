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
-- Module      : Amazonka.NetworkManager.UpdateVpcAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a VPC attachment.
module Amazonka.NetworkManager.UpdateVpcAttachment
  ( -- * Creating a Request
    UpdateVpcAttachment (..),
    newUpdateVpcAttachment,

    -- * Request Lenses
    updateVpcAttachment_addSubnetArns,
    updateVpcAttachment_options,
    updateVpcAttachment_removeSubnetArns,
    updateVpcAttachment_attachmentId,

    -- * Destructuring the Response
    UpdateVpcAttachmentResponse (..),
    newUpdateVpcAttachmentResponse,

    -- * Response Lenses
    updateVpcAttachmentResponse_vpcAttachment,
    updateVpcAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVpcAttachment' smart constructor.
data UpdateVpcAttachment = UpdateVpcAttachment'
  { -- | Adds a subnet ARN to the VPC attachment.
    addSubnetArns :: Prelude.Maybe [Prelude.Text],
    -- | Additional options for updating the VPC attachment.
    options :: Prelude.Maybe VpcOptions,
    -- | Removes a subnet ARN from the attachment.
    removeSubnetArns :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the attachment.
    attachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addSubnetArns', 'updateVpcAttachment_addSubnetArns' - Adds a subnet ARN to the VPC attachment.
--
-- 'options', 'updateVpcAttachment_options' - Additional options for updating the VPC attachment.
--
-- 'removeSubnetArns', 'updateVpcAttachment_removeSubnetArns' - Removes a subnet ARN from the attachment.
--
-- 'attachmentId', 'updateVpcAttachment_attachmentId' - The ID of the attachment.
newUpdateVpcAttachment ::
  -- | 'attachmentId'
  Prelude.Text ->
  UpdateVpcAttachment
newUpdateVpcAttachment pAttachmentId_ =
  UpdateVpcAttachment'
    { addSubnetArns =
        Prelude.Nothing,
      options = Prelude.Nothing,
      removeSubnetArns = Prelude.Nothing,
      attachmentId = pAttachmentId_
    }

-- | Adds a subnet ARN to the VPC attachment.
updateVpcAttachment_addSubnetArns :: Lens.Lens' UpdateVpcAttachment (Prelude.Maybe [Prelude.Text])
updateVpcAttachment_addSubnetArns = Lens.lens (\UpdateVpcAttachment' {addSubnetArns} -> addSubnetArns) (\s@UpdateVpcAttachment' {} a -> s {addSubnetArns = a} :: UpdateVpcAttachment) Prelude.. Lens.mapping Lens.coerced

-- | Additional options for updating the VPC attachment.
updateVpcAttachment_options :: Lens.Lens' UpdateVpcAttachment (Prelude.Maybe VpcOptions)
updateVpcAttachment_options = Lens.lens (\UpdateVpcAttachment' {options} -> options) (\s@UpdateVpcAttachment' {} a -> s {options = a} :: UpdateVpcAttachment)

-- | Removes a subnet ARN from the attachment.
updateVpcAttachment_removeSubnetArns :: Lens.Lens' UpdateVpcAttachment (Prelude.Maybe [Prelude.Text])
updateVpcAttachment_removeSubnetArns = Lens.lens (\UpdateVpcAttachment' {removeSubnetArns} -> removeSubnetArns) (\s@UpdateVpcAttachment' {} a -> s {removeSubnetArns = a} :: UpdateVpcAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the attachment.
updateVpcAttachment_attachmentId :: Lens.Lens' UpdateVpcAttachment Prelude.Text
updateVpcAttachment_attachmentId = Lens.lens (\UpdateVpcAttachment' {attachmentId} -> attachmentId) (\s@UpdateVpcAttachment' {} a -> s {attachmentId = a} :: UpdateVpcAttachment)

instance Core.AWSRequest UpdateVpcAttachment where
  type
    AWSResponse UpdateVpcAttachment =
      UpdateVpcAttachmentResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVpcAttachmentResponse'
            Prelude.<$> (x Data..?> "VpcAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVpcAttachment where
  hashWithSalt _salt UpdateVpcAttachment' {..} =
    _salt `Prelude.hashWithSalt` addSubnetArns
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` removeSubnetArns
      `Prelude.hashWithSalt` attachmentId

instance Prelude.NFData UpdateVpcAttachment where
  rnf UpdateVpcAttachment' {..} =
    Prelude.rnf addSubnetArns
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf removeSubnetArns
      `Prelude.seq` Prelude.rnf attachmentId

instance Data.ToHeaders UpdateVpcAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVpcAttachment where
  toJSON UpdateVpcAttachment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddSubnetArns" Data..=) Prelude.<$> addSubnetArns,
            ("Options" Data..=) Prelude.<$> options,
            ("RemoveSubnetArns" Data..=)
              Prelude.<$> removeSubnetArns
          ]
      )

instance Data.ToPath UpdateVpcAttachment where
  toPath UpdateVpcAttachment' {..} =
    Prelude.mconcat
      ["/vpc-attachments/", Data.toBS attachmentId]

instance Data.ToQuery UpdateVpcAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVpcAttachmentResponse' smart constructor.
data UpdateVpcAttachmentResponse = UpdateVpcAttachmentResponse'
  { -- | Describes the updated VPC attachment.
    vpcAttachment :: Prelude.Maybe VpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVpcAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcAttachment', 'updateVpcAttachmentResponse_vpcAttachment' - Describes the updated VPC attachment.
--
-- 'httpStatus', 'updateVpcAttachmentResponse_httpStatus' - The response's http status code.
newUpdateVpcAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVpcAttachmentResponse
newUpdateVpcAttachmentResponse pHttpStatus_ =
  UpdateVpcAttachmentResponse'
    { vpcAttachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the updated VPC attachment.
updateVpcAttachmentResponse_vpcAttachment :: Lens.Lens' UpdateVpcAttachmentResponse (Prelude.Maybe VpcAttachment)
updateVpcAttachmentResponse_vpcAttachment = Lens.lens (\UpdateVpcAttachmentResponse' {vpcAttachment} -> vpcAttachment) (\s@UpdateVpcAttachmentResponse' {} a -> s {vpcAttachment = a} :: UpdateVpcAttachmentResponse)

-- | The response's http status code.
updateVpcAttachmentResponse_httpStatus :: Lens.Lens' UpdateVpcAttachmentResponse Prelude.Int
updateVpcAttachmentResponse_httpStatus = Lens.lens (\UpdateVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@UpdateVpcAttachmentResponse' {} a -> s {httpStatus = a} :: UpdateVpcAttachmentResponse)

instance Prelude.NFData UpdateVpcAttachmentResponse where
  rnf UpdateVpcAttachmentResponse' {..} =
    Prelude.rnf vpcAttachment
      `Prelude.seq` Prelude.rnf httpStatus
