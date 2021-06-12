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
-- Module      : Network.AWS.CertificateManager.DescribeCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed metadata about the specified ACM certificate.
module Network.AWS.CertificateManager.DescribeCertificate
  ( -- * Creating a Request
    DescribeCertificate (..),
    newDescribeCertificate,

    -- * Request Lenses
    describeCertificate_certificateArn,

    -- * Destructuring the Response
    DescribeCertificateResponse (..),
    newDescribeCertificateResponse,

    -- * Response Lenses
    describeCertificateResponse_certificate,
    describeCertificateResponse_httpStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCertificate' smart constructor.
data DescribeCertificate = DescribeCertificate'
  { -- | The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have
    -- the following form:
    --
    -- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    certificateArn :: Core.Text
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
-- 'certificateArn', 'describeCertificate_certificateArn' - The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have
-- the following form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
newDescribeCertificate ::
  -- | 'certificateArn'
  Core.Text ->
  DescribeCertificate
newDescribeCertificate pCertificateArn_ =
  DescribeCertificate'
    { certificateArn =
        pCertificateArn_
    }

-- | The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have
-- the following form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
describeCertificate_certificateArn :: Lens.Lens' DescribeCertificate Core.Text
describeCertificate_certificateArn = Lens.lens (\DescribeCertificate' {certificateArn} -> certificateArn) (\s@DescribeCertificate' {} a -> s {certificateArn = a} :: DescribeCertificate)

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
              Core.=# ( "CertificateManager.DescribeCertificate" ::
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
          [ Core.Just
              ("CertificateArn" Core..= certificateArn)
          ]
      )

instance Core.ToPath DescribeCertificate where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCertificate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { -- | Metadata about an ACM certificate.
    certificate :: Core.Maybe CertificateDetail,
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
-- 'certificate', 'describeCertificateResponse_certificate' - Metadata about an ACM certificate.
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

-- | Metadata about an ACM certificate.
describeCertificateResponse_certificate :: Lens.Lens' DescribeCertificateResponse (Core.Maybe CertificateDetail)
describeCertificateResponse_certificate = Lens.lens (\DescribeCertificateResponse' {certificate} -> certificate) (\s@DescribeCertificateResponse' {} a -> s {certificate = a} :: DescribeCertificateResponse)

-- | The response's http status code.
describeCertificateResponse_httpStatus :: Lens.Lens' DescribeCertificateResponse Core.Int
describeCertificateResponse_httpStatus = Lens.lens (\DescribeCertificateResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificateResponse' {} a -> s {httpStatus = a} :: DescribeCertificateResponse)

instance Core.NFData DescribeCertificateResponse
