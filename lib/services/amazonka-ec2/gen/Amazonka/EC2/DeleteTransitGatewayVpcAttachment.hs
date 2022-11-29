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
-- Module      : Amazonka.EC2.DeleteTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC attachment.
module Amazonka.EC2.DeleteTransitGatewayVpcAttachment
  ( -- * Creating a Request
    DeleteTransitGatewayVpcAttachment (..),
    newDeleteTransitGatewayVpcAttachment,

    -- * Request Lenses
    deleteTransitGatewayVpcAttachment_dryRun,
    deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId,

    -- * Destructuring the Response
    DeleteTransitGatewayVpcAttachmentResponse (..),
    newDeleteTransitGatewayVpcAttachmentResponse,

    -- * Response Lenses
    deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    deleteTransitGatewayVpcAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTransitGatewayVpcAttachment' smart constructor.
data DeleteTransitGatewayVpcAttachment = DeleteTransitGatewayVpcAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayVpcAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayAttachmentId', 'deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId' - The ID of the attachment.
newDeleteTransitGatewayVpcAttachment ::
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  DeleteTransitGatewayVpcAttachment
newDeleteTransitGatewayVpcAttachment
  pTransitGatewayAttachmentId_ =
    DeleteTransitGatewayVpcAttachment'
      { dryRun =
          Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayVpcAttachment_dryRun :: Lens.Lens' DeleteTransitGatewayVpcAttachment (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayVpcAttachment_dryRun = Lens.lens (\DeleteTransitGatewayVpcAttachment' {dryRun} -> dryRun) (\s@DeleteTransitGatewayVpcAttachment' {} a -> s {dryRun = a} :: DeleteTransitGatewayVpcAttachment)

-- | The ID of the attachment.
deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayVpcAttachment Prelude.Text
deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\DeleteTransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DeleteTransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: DeleteTransitGatewayVpcAttachment)

instance
  Core.AWSRequest
    DeleteTransitGatewayVpcAttachment
  where
  type
    AWSResponse DeleteTransitGatewayVpcAttachment =
      DeleteTransitGatewayVpcAttachmentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayVpcAttachmentResponse'
            Prelude.<$> (x Core..@? "transitGatewayVpcAttachment")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayVpcAttachment
  where
  hashWithSalt
    _salt
    DeleteTransitGatewayVpcAttachment' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    DeleteTransitGatewayVpcAttachment
  where
  rnf DeleteTransitGatewayVpcAttachment' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId

instance
  Core.ToHeaders
    DeleteTransitGatewayVpcAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeleteTransitGatewayVpcAttachment
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteTransitGatewayVpcAttachment
  where
  toQuery DeleteTransitGatewayVpcAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteTransitGatewayVpcAttachment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDeleteTransitGatewayVpcAttachmentResponse' smart constructor.
data DeleteTransitGatewayVpcAttachmentResponse = DeleteTransitGatewayVpcAttachmentResponse'
  { -- | Information about the deleted VPC attachment.
    transitGatewayVpcAttachment :: Prelude.Maybe TransitGatewayVpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayVpcAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayVpcAttachment', 'deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment' - Information about the deleted VPC attachment.
--
-- 'httpStatus', 'deleteTransitGatewayVpcAttachmentResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayVpcAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayVpcAttachmentResponse
newDeleteTransitGatewayVpcAttachmentResponse
  pHttpStatus_ =
    DeleteTransitGatewayVpcAttachmentResponse'
      { transitGatewayVpcAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted VPC attachment.
deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment :: Lens.Lens' DeleteTransitGatewayVpcAttachmentResponse (Prelude.Maybe TransitGatewayVpcAttachment)
deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment = Lens.lens (\DeleteTransitGatewayVpcAttachmentResponse' {transitGatewayVpcAttachment} -> transitGatewayVpcAttachment) (\s@DeleteTransitGatewayVpcAttachmentResponse' {} a -> s {transitGatewayVpcAttachment = a} :: DeleteTransitGatewayVpcAttachmentResponse)

-- | The response's http status code.
deleteTransitGatewayVpcAttachmentResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayVpcAttachmentResponse Prelude.Int
deleteTransitGatewayVpcAttachmentResponse_httpStatus = Lens.lens (\DeleteTransitGatewayVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayVpcAttachmentResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayVpcAttachmentResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayVpcAttachmentResponse
  where
  rnf DeleteTransitGatewayVpcAttachmentResponse' {..} =
    Prelude.rnf transitGatewayVpcAttachment
      `Prelude.seq` Prelude.rnf httpStatus
