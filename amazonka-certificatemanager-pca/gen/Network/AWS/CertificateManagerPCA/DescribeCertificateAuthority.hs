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
-- Module      : Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about your private certificate authority (CA) or one
-- that has been shared with you. You specify the private CA on input by
-- its ARN (Amazon Resource Name). The output contains the status of your
-- CA. This can be any of the following:
--
-- -   @CREATING@ - ACM Private CA is creating your private certificate
--     authority.
--
-- -   @PENDING_CERTIFICATE@ - The certificate is pending. You must use
--     your ACM Private CA-hosted or on-premises root or subordinate CA to
--     sign your private CA CSR and then import it into PCA.
--
-- -   @ACTIVE@ - Your private CA is active.
--
-- -   @DISABLED@ - Your private CA has been disabled.
--
-- -   @EXPIRED@ - Your private CA certificate has expired.
--
-- -   @FAILED@ - Your private CA has failed. Your CA can fail because of
--     problems such a network outage or back-end AWS failure or other
--     errors. A failed CA can never return to the pending state. You must
--     create a new CA.
--
-- -   @DELETED@ - Your private CA is within the restoration period, after
--     which it is permanently deleted. The length of time remaining in the
--     CA\'s restoration period is also included in this action\'s output.
module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
  ( -- * Creating a Request
    DescribeCertificateAuthority (..),
    newDescribeCertificateAuthority,

    -- * Request Lenses
    describeCertificateAuthority_certificateAuthorityArn,

    -- * Destructuring the Response
    DescribeCertificateAuthorityResponse (..),
    newDescribeCertificateAuthorityResponse,

    -- * Response Lenses
    describeCertificateAuthorityResponse_certificateAuthority,
    describeCertificateAuthorityResponse_httpStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCertificateAuthority' smart constructor.
data DescribeCertificateAuthority = DescribeCertificateAuthority'
  { -- | The Amazon Resource Name (ARN) that was returned when you called
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
    -- This must be of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
    certificateAuthorityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'describeCertificateAuthority_certificateAuthorityArn' - The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
newDescribeCertificateAuthority ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  DescribeCertificateAuthority
newDescribeCertificateAuthority
  pCertificateAuthorityArn_ =
    DescribeCertificateAuthority'
      { certificateAuthorityArn =
          pCertificateAuthorityArn_
      }

-- | The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
describeCertificateAuthority_certificateAuthorityArn :: Lens.Lens' DescribeCertificateAuthority Prelude.Text
describeCertificateAuthority_certificateAuthorityArn = Lens.lens (\DescribeCertificateAuthority' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@DescribeCertificateAuthority' {} a -> s {certificateAuthorityArn = a} :: DescribeCertificateAuthority)

instance Core.AWSRequest DescribeCertificateAuthority where
  type
    AWSResponse DescribeCertificateAuthority =
      DescribeCertificateAuthorityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificateAuthorityResponse'
            Prelude.<$> (x Core..?> "CertificateAuthority")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCertificateAuthority

instance Prelude.NFData DescribeCertificateAuthority

instance Core.ToHeaders DescribeCertificateAuthority where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ACMPrivateCA.DescribeCertificateAuthority" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCertificateAuthority where
  toJSON DescribeCertificateAuthority' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Core..= certificateAuthorityArn
              )
          ]
      )

instance Core.ToPath DescribeCertificateAuthority where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCertificateAuthority where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCertificateAuthorityResponse' smart constructor.
data DescribeCertificateAuthorityResponse = DescribeCertificateAuthorityResponse'
  { -- | A
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CertificateAuthority.html CertificateAuthority>
    -- structure that contains information about your private CA.
    certificateAuthority :: Prelude.Maybe CertificateAuthority,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthority', 'describeCertificateAuthorityResponse_certificateAuthority' - A
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CertificateAuthority.html CertificateAuthority>
-- structure that contains information about your private CA.
--
-- 'httpStatus', 'describeCertificateAuthorityResponse_httpStatus' - The response's http status code.
newDescribeCertificateAuthorityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCertificateAuthorityResponse
newDescribeCertificateAuthorityResponse pHttpStatus_ =
  DescribeCertificateAuthorityResponse'
    { certificateAuthority =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CertificateAuthority.html CertificateAuthority>
-- structure that contains information about your private CA.
describeCertificateAuthorityResponse_certificateAuthority :: Lens.Lens' DescribeCertificateAuthorityResponse (Prelude.Maybe CertificateAuthority)
describeCertificateAuthorityResponse_certificateAuthority = Lens.lens (\DescribeCertificateAuthorityResponse' {certificateAuthority} -> certificateAuthority) (\s@DescribeCertificateAuthorityResponse' {} a -> s {certificateAuthority = a} :: DescribeCertificateAuthorityResponse)

-- | The response's http status code.
describeCertificateAuthorityResponse_httpStatus :: Lens.Lens' DescribeCertificateAuthorityResponse Prelude.Int
describeCertificateAuthorityResponse_httpStatus = Lens.lens (\DescribeCertificateAuthorityResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificateAuthorityResponse' {} a -> s {httpStatus = a} :: DescribeCertificateAuthorityResponse)

instance
  Prelude.NFData
    DescribeCertificateAuthorityResponse
