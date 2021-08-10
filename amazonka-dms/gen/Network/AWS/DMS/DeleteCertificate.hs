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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCertificate' smart constructor.
data DeleteCertificate = DeleteCertificate'
  { -- | The Amazon Resource Name (ARN) of the deleted certificate.
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteCertificate
newDeleteCertificate pCertificateArn_ =
  DeleteCertificate'
    { certificateArn =
        pCertificateArn_
    }

-- | The Amazon Resource Name (ARN) of the deleted certificate.
deleteCertificate_certificateArn :: Lens.Lens' DeleteCertificate Prelude.Text
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
            Prelude.<$> (x Core..?> "Certificate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCertificate

instance Prelude.NFData DeleteCertificate

instance Core.ToHeaders DeleteCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DeleteCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteCertificate where
  toJSON DeleteCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Core..= certificateArn)
          ]
      )

instance Core.ToPath DeleteCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  { -- | The Secure Sockets Layer (SSL) certificate.
    certificate :: Prelude.Maybe Certificate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteCertificateResponse
newDeleteCertificateResponse pHttpStatus_ =
  DeleteCertificateResponse'
    { certificate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Secure Sockets Layer (SSL) certificate.
deleteCertificateResponse_certificate :: Lens.Lens' DeleteCertificateResponse (Prelude.Maybe Certificate)
deleteCertificateResponse_certificate = Lens.lens (\DeleteCertificateResponse' {certificate} -> certificate) (\s@DeleteCertificateResponse' {} a -> s {certificate = a} :: DeleteCertificateResponse)

-- | The response's http status code.
deleteCertificateResponse_httpStatus :: Lens.Lens' DeleteCertificateResponse Prelude.Int
deleteCertificateResponse_httpStatus = Lens.lens (\DeleteCertificateResponse' {httpStatus} -> httpStatus) (\s@DeleteCertificateResponse' {} a -> s {httpStatus = a} :: DeleteCertificateResponse)

instance Prelude.NFData DeleteCertificateResponse
