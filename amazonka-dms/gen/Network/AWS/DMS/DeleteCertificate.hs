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
-- Module      : Network.AWS.DMS.DeleteCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified certificate.
module Network.AWS.DMS.DeleteCertificate
  ( -- * Creating a Request
    DeleteCertificate (..),
    newDeleteCertificate,

    -- * Request Lenses
    deleteCertificate_certificateArn,

    -- * Destructuring the Response
    DeleteCertificateResponse (..),
    newDeleteCertificateResponse,

    -- * Response Lenses
    deleteCertificateResponse_certificate,
    deleteCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCertificate' smart constructor.
data DeleteCertificate = DeleteCertificate'
  { -- | The Amazon Resource Name (ARN) of the deleted certificate.
    certificateArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'deleteCertificate_certificateArn' - The Amazon Resource Name (ARN) of the deleted certificate.
newDeleteCertificate ::
  -- | 'certificateArn'
  Core.Text ->
  DeleteCertificate
newDeleteCertificate pCertificateArn_ =
  DeleteCertificate'
    { certificateArn =
        pCertificateArn_
    }

-- | The Amazon Resource Name (ARN) of the deleted certificate.
deleteCertificate_certificateArn :: Lens.Lens' DeleteCertificate Core.Text
deleteCertificate_certificateArn = Lens.lens (\DeleteCertificate' {certificateArn} -> certificateArn) (\s@DeleteCertificate' {} a -> s {certificateArn = a} :: DeleteCertificate)

instance Core.AWSRequest DeleteCertificate where
  type
    AWSResponse DeleteCertificate =
      DeleteCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCertificateResponse'
            Core.<$> (x Core..?> "Certificate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCertificate

instance Core.NFData DeleteCertificate

instance Core.ToHeaders DeleteCertificate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DeleteCertificate" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteCertificate where
  toJSON DeleteCertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateArn" Core..= certificateArn)
          ]
      )

instance Core.ToPath DeleteCertificate where
  toPath = Core.const "/"

instance Core.ToQuery DeleteCertificate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  { -- | The Secure Sockets Layer (SSL) certificate.
    certificate :: Core.Maybe Certificate,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'deleteCertificateResponse_certificate' - The Secure Sockets Layer (SSL) certificate.
--
-- 'httpStatus', 'deleteCertificateResponse_httpStatus' - The response's http status code.
newDeleteCertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteCertificateResponse
newDeleteCertificateResponse pHttpStatus_ =
  DeleteCertificateResponse'
    { certificate =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Secure Sockets Layer (SSL) certificate.
deleteCertificateResponse_certificate :: Lens.Lens' DeleteCertificateResponse (Core.Maybe Certificate)
deleteCertificateResponse_certificate = Lens.lens (\DeleteCertificateResponse' {certificate} -> certificate) (\s@DeleteCertificateResponse' {} a -> s {certificate = a} :: DeleteCertificateResponse)

-- | The response's http status code.
deleteCertificateResponse_httpStatus :: Lens.Lens' DeleteCertificateResponse Core.Int
deleteCertificateResponse_httpStatus = Lens.lens (\DeleteCertificateResponse' {httpStatus} -> httpStatus) (\s@DeleteCertificateResponse' {} a -> s {httpStatus = a} :: DeleteCertificateResponse)

instance Core.NFData DeleteCertificateResponse
