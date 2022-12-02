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
-- Module      : Amazonka.NetworkManager.GetVpcAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a VPC attachment.
module Amazonka.NetworkManager.GetVpcAttachment
  ( -- * Creating a Request
    GetVpcAttachment (..),
    newGetVpcAttachment,

    -- * Request Lenses
    getVpcAttachment_attachmentId,

    -- * Destructuring the Response
    GetVpcAttachmentResponse (..),
    newGetVpcAttachmentResponse,

    -- * Response Lenses
    getVpcAttachmentResponse_vpcAttachment,
    getVpcAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVpcAttachment' smart constructor.
data GetVpcAttachment = GetVpcAttachment'
  { -- | The ID of the attachment.
    attachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'getVpcAttachment_attachmentId' - The ID of the attachment.
newGetVpcAttachment ::
  -- | 'attachmentId'
  Prelude.Text ->
  GetVpcAttachment
newGetVpcAttachment pAttachmentId_ =
  GetVpcAttachment' {attachmentId = pAttachmentId_}

-- | The ID of the attachment.
getVpcAttachment_attachmentId :: Lens.Lens' GetVpcAttachment Prelude.Text
getVpcAttachment_attachmentId = Lens.lens (\GetVpcAttachment' {attachmentId} -> attachmentId) (\s@GetVpcAttachment' {} a -> s {attachmentId = a} :: GetVpcAttachment)

instance Core.AWSRequest GetVpcAttachment where
  type
    AWSResponse GetVpcAttachment =
      GetVpcAttachmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVpcAttachmentResponse'
            Prelude.<$> (x Data..?> "VpcAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVpcAttachment where
  hashWithSalt _salt GetVpcAttachment' {..} =
    _salt `Prelude.hashWithSalt` attachmentId

instance Prelude.NFData GetVpcAttachment where
  rnf GetVpcAttachment' {..} = Prelude.rnf attachmentId

instance Data.ToHeaders GetVpcAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetVpcAttachment where
  toPath GetVpcAttachment' {..} =
    Prelude.mconcat
      ["/vpc-attachments/", Data.toBS attachmentId]

instance Data.ToQuery GetVpcAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVpcAttachmentResponse' smart constructor.
data GetVpcAttachmentResponse = GetVpcAttachmentResponse'
  { -- | Returns details about a VPC attachment.
    vpcAttachment :: Prelude.Maybe VpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpcAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcAttachment', 'getVpcAttachmentResponse_vpcAttachment' - Returns details about a VPC attachment.
--
-- 'httpStatus', 'getVpcAttachmentResponse_httpStatus' - The response's http status code.
newGetVpcAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVpcAttachmentResponse
newGetVpcAttachmentResponse pHttpStatus_ =
  GetVpcAttachmentResponse'
    { vpcAttachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns details about a VPC attachment.
getVpcAttachmentResponse_vpcAttachment :: Lens.Lens' GetVpcAttachmentResponse (Prelude.Maybe VpcAttachment)
getVpcAttachmentResponse_vpcAttachment = Lens.lens (\GetVpcAttachmentResponse' {vpcAttachment} -> vpcAttachment) (\s@GetVpcAttachmentResponse' {} a -> s {vpcAttachment = a} :: GetVpcAttachmentResponse)

-- | The response's http status code.
getVpcAttachmentResponse_httpStatus :: Lens.Lens' GetVpcAttachmentResponse Prelude.Int
getVpcAttachmentResponse_httpStatus = Lens.lens (\GetVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@GetVpcAttachmentResponse' {} a -> s {httpStatus = a} :: GetVpcAttachmentResponse)

instance Prelude.NFData GetVpcAttachmentResponse where
  rnf GetVpcAttachmentResponse' {..} =
    Prelude.rnf vpcAttachment
      `Prelude.seq` Prelude.rnf httpStatus
