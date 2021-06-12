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
-- Module      : Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status or configuration of a private certificate authority
-- (CA). Your private CA must be in the @ACTIVE@ or @DISABLED@ state before
-- you can update it. You can disable a private CA that is in the @ACTIVE@
-- state or make a CA that is in the @DISABLED@ state active again.
--
-- Both PCA and the IAM principal must have permission to write to the S3
-- bucket that you specify. If the IAM principal making the call does not
-- have permission to write to the bucket, then an exception is thrown. For
-- more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaAuthAccess.html Configure Access to ACM Private CA>.
module Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
  ( -- * Creating a Request
    UpdateCertificateAuthority (..),
    newUpdateCertificateAuthority,

    -- * Request Lenses
    updateCertificateAuthority_status,
    updateCertificateAuthority_revocationConfiguration,
    updateCertificateAuthority_certificateAuthorityArn,

    -- * Destructuring the Response
    UpdateCertificateAuthorityResponse (..),
    newUpdateCertificateAuthorityResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCertificateAuthority' smart constructor.
data UpdateCertificateAuthority = UpdateCertificateAuthority'
  { -- | Status of your private CA.
    status :: Core.Maybe CertificateAuthorityStatus,
    -- | Revocation information for your private CA.
    revocationConfiguration :: Core.Maybe RevocationConfiguration,
    -- | Amazon Resource Name (ARN) of the private CA that issued the certificate
    -- to be revoked. This must be of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
    certificateAuthorityArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateCertificateAuthority_status' - Status of your private CA.
--
-- 'revocationConfiguration', 'updateCertificateAuthority_revocationConfiguration' - Revocation information for your private CA.
--
-- 'certificateAuthorityArn', 'updateCertificateAuthority_certificateAuthorityArn' - Amazon Resource Name (ARN) of the private CA that issued the certificate
-- to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
newUpdateCertificateAuthority ::
  -- | 'certificateAuthorityArn'
  Core.Text ->
  UpdateCertificateAuthority
newUpdateCertificateAuthority
  pCertificateAuthorityArn_ =
    UpdateCertificateAuthority'
      { status = Core.Nothing,
        revocationConfiguration = Core.Nothing,
        certificateAuthorityArn =
          pCertificateAuthorityArn_
      }

-- | Status of your private CA.
updateCertificateAuthority_status :: Lens.Lens' UpdateCertificateAuthority (Core.Maybe CertificateAuthorityStatus)
updateCertificateAuthority_status = Lens.lens (\UpdateCertificateAuthority' {status} -> status) (\s@UpdateCertificateAuthority' {} a -> s {status = a} :: UpdateCertificateAuthority)

-- | Revocation information for your private CA.
updateCertificateAuthority_revocationConfiguration :: Lens.Lens' UpdateCertificateAuthority (Core.Maybe RevocationConfiguration)
updateCertificateAuthority_revocationConfiguration = Lens.lens (\UpdateCertificateAuthority' {revocationConfiguration} -> revocationConfiguration) (\s@UpdateCertificateAuthority' {} a -> s {revocationConfiguration = a} :: UpdateCertificateAuthority)

-- | Amazon Resource Name (ARN) of the private CA that issued the certificate
-- to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
updateCertificateAuthority_certificateAuthorityArn :: Lens.Lens' UpdateCertificateAuthority Core.Text
updateCertificateAuthority_certificateAuthorityArn = Lens.lens (\UpdateCertificateAuthority' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@UpdateCertificateAuthority' {} a -> s {certificateAuthorityArn = a} :: UpdateCertificateAuthority)

instance Core.AWSRequest UpdateCertificateAuthority where
  type
    AWSResponse UpdateCertificateAuthority =
      UpdateCertificateAuthorityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateCertificateAuthorityResponse'

instance Core.Hashable UpdateCertificateAuthority

instance Core.NFData UpdateCertificateAuthority

instance Core.ToHeaders UpdateCertificateAuthority where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ACMPrivateCA.UpdateCertificateAuthority" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateCertificateAuthority where
  toJSON UpdateCertificateAuthority' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("RevocationConfiguration" Core..=)
              Core.<$> revocationConfiguration,
            Core.Just
              ( "CertificateAuthorityArn"
                  Core..= certificateAuthorityArn
              )
          ]
      )

instance Core.ToPath UpdateCertificateAuthority where
  toPath = Core.const "/"

instance Core.ToQuery UpdateCertificateAuthority where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCertificateAuthorityResponse' smart constructor.
data UpdateCertificateAuthorityResponse = UpdateCertificateAuthorityResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCertificateAuthorityResponse ::
  UpdateCertificateAuthorityResponse
newUpdateCertificateAuthorityResponse =
  UpdateCertificateAuthorityResponse'

instance
  Core.NFData
    UpdateCertificateAuthorityResponse
