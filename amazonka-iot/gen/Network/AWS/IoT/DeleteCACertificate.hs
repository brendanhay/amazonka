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
-- Module      : Network.AWS.IoT.DeleteCACertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a registered CA certificate.
module Network.AWS.IoT.DeleteCACertificate
  ( -- * Creating a Request
    DeleteCACertificate (..),
    newDeleteCACertificate,

    -- * Request Lenses
    deleteCACertificate_certificateId,

    -- * Destructuring the Response
    DeleteCACertificateResponse (..),
    newDeleteCACertificateResponse,

    -- * Response Lenses
    deleteCACertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input for the DeleteCACertificate operation.
--
-- /See:/ 'newDeleteCACertificate' smart constructor.
data DeleteCACertificate = DeleteCACertificate'
  { -- | The ID of the certificate to delete. (The last part of the certificate
    -- ARN contains the certificate ID.)
    certificateId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateId', 'deleteCACertificate_certificateId' - The ID of the certificate to delete. (The last part of the certificate
-- ARN contains the certificate ID.)
newDeleteCACertificate ::
  -- | 'certificateId'
  Core.Text ->
  DeleteCACertificate
newDeleteCACertificate pCertificateId_ =
  DeleteCACertificate'
    { certificateId =
        pCertificateId_
    }

-- | The ID of the certificate to delete. (The last part of the certificate
-- ARN contains the certificate ID.)
deleteCACertificate_certificateId :: Lens.Lens' DeleteCACertificate Core.Text
deleteCACertificate_certificateId = Lens.lens (\DeleteCACertificate' {certificateId} -> certificateId) (\s@DeleteCACertificate' {} a -> s {certificateId = a} :: DeleteCACertificate)

instance Core.AWSRequest DeleteCACertificate where
  type
    AWSResponse DeleteCACertificate =
      DeleteCACertificateResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCACertificateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCACertificate

instance Core.NFData DeleteCACertificate

instance Core.ToHeaders DeleteCACertificate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteCACertificate where
  toPath DeleteCACertificate' {..} =
    Core.mconcat
      ["/cacertificate/", Core.toBS certificateId]

instance Core.ToQuery DeleteCACertificate where
  toQuery = Core.const Core.mempty

-- | The output for the DeleteCACertificate operation.
--
-- /See:/ 'newDeleteCACertificateResponse' smart constructor.
data DeleteCACertificateResponse = DeleteCACertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCACertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCACertificateResponse_httpStatus' - The response's http status code.
newDeleteCACertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteCACertificateResponse
newDeleteCACertificateResponse pHttpStatus_ =
  DeleteCACertificateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCACertificateResponse_httpStatus :: Lens.Lens' DeleteCACertificateResponse Core.Int
deleteCACertificateResponse_httpStatus = Lens.lens (\DeleteCACertificateResponse' {httpStatus} -> httpStatus) (\s@DeleteCACertificateResponse' {} a -> s {httpStatus = a} :: DeleteCACertificateResponse)

instance Core.NFData DeleteCACertificateResponse
