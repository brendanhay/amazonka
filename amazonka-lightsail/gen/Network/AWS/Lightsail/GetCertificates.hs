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
-- Module      : Network.AWS.Lightsail.GetCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more Amazon Lightsail SSL\/TLS
-- certificates.
--
-- To get a summary of a certificate, ommit @includeCertificateDetails@
-- from your request. The response will include only the certificate Amazon
-- Resource Name (ARN), certificate name, domain name, and tags.
module Network.AWS.Lightsail.GetCertificates
  ( -- * Creating a Request
    GetCertificates (..),
    newGetCertificates,

    -- * Request Lenses
    getCertificates_includeCertificateDetails,
    getCertificates_certificateStatuses,
    getCertificates_certificateName,

    -- * Destructuring the Response
    GetCertificatesResponse (..),
    newGetCertificatesResponse,

    -- * Response Lenses
    getCertificatesResponse_certificates,
    getCertificatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCertificates' smart constructor.
data GetCertificates = GetCertificates'
  { -- | Indicates whether to include detailed information about the certificates
    -- in the response.
    --
    -- When omitted, the response includes only the certificate names, Amazon
    -- Resource Names (ARNs), domain names, and tags.
    includeCertificateDetails :: Prelude.Maybe Prelude.Bool,
    -- | The status of the certificates for which to return information.
    --
    -- For example, specify @ISSUED@ to return only certificates with an
    -- @ISSUED@ status.
    --
    -- When omitted, the response includes all of your certificates in the AWS
    -- Region where the request is made, regardless of their current status.
    certificateStatuses :: Prelude.Maybe [CertificateStatus],
    -- | The name for the certificate for which to return information.
    --
    -- When omitted, the response includes all of your certificates in the AWS
    -- Region where the request is made.
    certificateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeCertificateDetails', 'getCertificates_includeCertificateDetails' - Indicates whether to include detailed information about the certificates
-- in the response.
--
-- When omitted, the response includes only the certificate names, Amazon
-- Resource Names (ARNs), domain names, and tags.
--
-- 'certificateStatuses', 'getCertificates_certificateStatuses' - The status of the certificates for which to return information.
--
-- For example, specify @ISSUED@ to return only certificates with an
-- @ISSUED@ status.
--
-- When omitted, the response includes all of your certificates in the AWS
-- Region where the request is made, regardless of their current status.
--
-- 'certificateName', 'getCertificates_certificateName' - The name for the certificate for which to return information.
--
-- When omitted, the response includes all of your certificates in the AWS
-- Region where the request is made.
newGetCertificates ::
  GetCertificates
newGetCertificates =
  GetCertificates'
    { includeCertificateDetails =
        Prelude.Nothing,
      certificateStatuses = Prelude.Nothing,
      certificateName = Prelude.Nothing
    }

-- | Indicates whether to include detailed information about the certificates
-- in the response.
--
-- When omitted, the response includes only the certificate names, Amazon
-- Resource Names (ARNs), domain names, and tags.
getCertificates_includeCertificateDetails :: Lens.Lens' GetCertificates (Prelude.Maybe Prelude.Bool)
getCertificates_includeCertificateDetails = Lens.lens (\GetCertificates' {includeCertificateDetails} -> includeCertificateDetails) (\s@GetCertificates' {} a -> s {includeCertificateDetails = a} :: GetCertificates)

-- | The status of the certificates for which to return information.
--
-- For example, specify @ISSUED@ to return only certificates with an
-- @ISSUED@ status.
--
-- When omitted, the response includes all of your certificates in the AWS
-- Region where the request is made, regardless of their current status.
getCertificates_certificateStatuses :: Lens.Lens' GetCertificates (Prelude.Maybe [CertificateStatus])
getCertificates_certificateStatuses = Lens.lens (\GetCertificates' {certificateStatuses} -> certificateStatuses) (\s@GetCertificates' {} a -> s {certificateStatuses = a} :: GetCertificates) Prelude.. Lens.mapping Lens._Coerce

-- | The name for the certificate for which to return information.
--
-- When omitted, the response includes all of your certificates in the AWS
-- Region where the request is made.
getCertificates_certificateName :: Lens.Lens' GetCertificates (Prelude.Maybe Prelude.Text)
getCertificates_certificateName = Lens.lens (\GetCertificates' {certificateName} -> certificateName) (\s@GetCertificates' {} a -> s {certificateName = a} :: GetCertificates)

instance Core.AWSRequest GetCertificates where
  type
    AWSResponse GetCertificates =
      GetCertificatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCertificatesResponse'
            Prelude.<$> (x Core..?> "certificates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCertificates

instance Prelude.NFData GetCertificates

instance Core.ToHeaders GetCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetCertificates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCertificates where
  toJSON GetCertificates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("includeCertificateDetails" Core..=)
              Prelude.<$> includeCertificateDetails,
            ("certificateStatuses" Core..=)
              Prelude.<$> certificateStatuses,
            ("certificateName" Core..=)
              Prelude.<$> certificateName
          ]
      )

instance Core.ToPath GetCertificates where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCertificates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCertificatesResponse' smart constructor.
data GetCertificatesResponse = GetCertificatesResponse'
  { -- | An object that describes certificates.
    certificates :: Prelude.Maybe [CertificateSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificates', 'getCertificatesResponse_certificates' - An object that describes certificates.
--
-- 'httpStatus', 'getCertificatesResponse_httpStatus' - The response's http status code.
newGetCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCertificatesResponse
newGetCertificatesResponse pHttpStatus_ =
  GetCertificatesResponse'
    { certificates =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes certificates.
getCertificatesResponse_certificates :: Lens.Lens' GetCertificatesResponse (Prelude.Maybe [CertificateSummary])
getCertificatesResponse_certificates = Lens.lens (\GetCertificatesResponse' {certificates} -> certificates) (\s@GetCertificatesResponse' {} a -> s {certificates = a} :: GetCertificatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCertificatesResponse_httpStatus :: Lens.Lens' GetCertificatesResponse Prelude.Int
getCertificatesResponse_httpStatus = Lens.lens (\GetCertificatesResponse' {httpStatus} -> httpStatus) (\s@GetCertificatesResponse' {} a -> s {httpStatus = a} :: GetCertificatesResponse)

instance Prelude.NFData GetCertificatesResponse
