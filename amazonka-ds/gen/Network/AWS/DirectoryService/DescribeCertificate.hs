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
-- Module      : Network.AWS.DirectoryService.DescribeCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays information about the certificate registered for secure LDAP or
-- client certificate authentication.
module Network.AWS.DirectoryService.DescribeCertificate
  ( -- * Creating a Request
    DescribeCertificate (..),
    newDescribeCertificate,

    -- * Request Lenses
    describeCertificate_directoryId,
    describeCertificate_certificateId,

    -- * Destructuring the Response
    DescribeCertificateResponse (..),
    newDescribeCertificateResponse,

    -- * Response Lenses
    describeCertificateResponse_certificate,
    describeCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCertificate' smart constructor.
data DescribeCertificate = DescribeCertificate'
  { -- | The identifier of the directory.
    directoryId :: Core.Text,
    -- | The identifier of the certificate.
    certificateId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'describeCertificate_directoryId' - The identifier of the directory.
--
-- 'certificateId', 'describeCertificate_certificateId' - The identifier of the certificate.
newDescribeCertificate ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'certificateId'
  Core.Text ->
  DescribeCertificate
newDescribeCertificate pDirectoryId_ pCertificateId_ =
  DescribeCertificate'
    { directoryId = pDirectoryId_,
      certificateId = pCertificateId_
    }

-- | The identifier of the directory.
describeCertificate_directoryId :: Lens.Lens' DescribeCertificate Core.Text
describeCertificate_directoryId = Lens.lens (\DescribeCertificate' {directoryId} -> directoryId) (\s@DescribeCertificate' {} a -> s {directoryId = a} :: DescribeCertificate)

-- | The identifier of the certificate.
describeCertificate_certificateId :: Lens.Lens' DescribeCertificate Core.Text
describeCertificate_certificateId = Lens.lens (\DescribeCertificate' {certificateId} -> certificateId) (\s@DescribeCertificate' {} a -> s {certificateId = a} :: DescribeCertificate)

instance Core.AWSRequest DescribeCertificate where
  type
    AWSResponse DescribeCertificate =
      DescribeCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificateResponse'
            Core.<$> (x Core..?> "Certificate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCertificate

instance Core.NFData DescribeCertificate

instance Core.ToHeaders DescribeCertificate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeCertificate" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCertificate where
  toJSON DescribeCertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("CertificateId" Core..= certificateId)
          ]
      )

instance Core.ToPath DescribeCertificate where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCertificate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { -- | Information about the certificate, including registered date time,
    -- certificate state, the reason for the state, expiration date time, and
    -- certificate common name.
    certificate :: Core.Maybe Certificate,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'describeCertificateResponse_certificate' - Information about the certificate, including registered date time,
-- certificate state, the reason for the state, expiration date time, and
-- certificate common name.
--
-- 'httpStatus', 'describeCertificateResponse_httpStatus' - The response's http status code.
newDescribeCertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCertificateResponse
newDescribeCertificateResponse pHttpStatus_ =
  DescribeCertificateResponse'
    { certificate =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the certificate, including registered date time,
-- certificate state, the reason for the state, expiration date time, and
-- certificate common name.
describeCertificateResponse_certificate :: Lens.Lens' DescribeCertificateResponse (Core.Maybe Certificate)
describeCertificateResponse_certificate = Lens.lens (\DescribeCertificateResponse' {certificate} -> certificate) (\s@DescribeCertificateResponse' {} a -> s {certificate = a} :: DescribeCertificateResponse)

-- | The response's http status code.
describeCertificateResponse_httpStatus :: Lens.Lens' DescribeCertificateResponse Core.Int
describeCertificateResponse_httpStatus = Lens.lens (\DescribeCertificateResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificateResponse' {} a -> s {httpStatus = a} :: DescribeCertificateResponse)

instance Core.NFData DescribeCertificateResponse
