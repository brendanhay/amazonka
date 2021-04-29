{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC attachment.
module Network.AWS.EC2.DeleteTransitGatewayVpcAttachment
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteTransitGatewayVpcAttachment
  where
  type
    Rs DeleteTransitGatewayVpcAttachment =
      DeleteTransitGatewayVpcAttachmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayVpcAttachmentResponse'
            Prelude.<$> (x Prelude..@? "transitGatewayVpcAttachment")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayVpcAttachment

instance
  Prelude.NFData
    DeleteTransitGatewayVpcAttachment

instance
  Prelude.ToHeaders
    DeleteTransitGatewayVpcAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteTransitGatewayVpcAttachment
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteTransitGatewayVpcAttachment
  where
  toQuery DeleteTransitGatewayVpcAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteTransitGatewayVpcAttachment" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "TransitGatewayAttachmentId"
          Prelude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDeleteTransitGatewayVpcAttachmentResponse' smart constructor.
data DeleteTransitGatewayVpcAttachmentResponse = DeleteTransitGatewayVpcAttachmentResponse'
  { -- | Information about the deleted VPC attachment.
    transitGatewayVpcAttachment :: Prelude.Maybe TransitGatewayVpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
