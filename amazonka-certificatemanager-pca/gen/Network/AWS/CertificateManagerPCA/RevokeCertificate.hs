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
-- Module      : Network.AWS.CertificateManagerPCA.RevokeCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes a certificate that was issued inside ACM Private CA. If you
-- enable a certificate revocation list (CRL) when you create or update
-- your private CA, information about the revoked certificates will be
-- included in the CRL. ACM Private CA writes the CRL to an S3 bucket that
-- you specify. A CRL is typically updated approximately 30 minutes after a
-- certificate is revoked. If for any reason the CRL update fails, ACM
-- Private CA attempts makes further attempts every 15 minutes. With Amazon
-- CloudWatch, you can create alarms for the metrics @CRLGenerated@ and
-- @MisconfiguredCRLBucket@. For more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaCloudWatch.html Supported CloudWatch Metrics>.
--
-- Both PCA and the IAM principal must have permission to write to the S3
-- bucket that you specify. If the IAM principal making the call does not
-- have permission to write to the bucket, then an exception is thrown. For
-- more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaAuthAccess.html Configure Access to ACM Private CA>.
--
-- ACM Private CA also writes revocation information to the audit report.
-- For more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport>.
--
-- You cannot revoke a root CA self-signed certificate.
module Network.AWS.CertificateManagerPCA.RevokeCertificate
  ( -- * Creating a Request
    RevokeCertificate (..),
    newRevokeCertificate,

    -- * Request Lenses
    revokeCertificate_certificateAuthorityArn,
    revokeCertificate_certificateSerial,
    revokeCertificate_revocationReason,

    -- * Destructuring the Response
    RevokeCertificateResponse (..),
    newRevokeCertificateResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRevokeCertificate' smart constructor.
data RevokeCertificate = RevokeCertificate'
  { -- | Amazon Resource Name (ARN) of the private CA that issued the certificate
    -- to be revoked. This must be of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
    certificateAuthorityArn :: Prelude.Text,
    -- | Serial number of the certificate to be revoked. This must be in
    -- hexadecimal format. You can retrieve the serial number by calling
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate>
    -- with the Amazon Resource Name (ARN) of the certificate you want and the
    -- ARN of your private CA. The __GetCertificate__ action retrieves the
    -- certificate in the PEM format. You can use the following OpenSSL command
    -- to list the certificate in text format and copy the hexadecimal serial
    -- number.
    --
    -- @openssl x509 -in file_path -text -noout@
    --
    -- You can also copy the serial number from the console or use the
    -- <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate>
    -- action in the /AWS Certificate Manager API Reference/.
    certificateSerial :: Prelude.Text,
    -- | Specifies why you revoked the certificate.
    revocationReason :: RevocationReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RevokeCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'revokeCertificate_certificateAuthorityArn' - Amazon Resource Name (ARN) of the private CA that issued the certificate
-- to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
--
-- 'certificateSerial', 'revokeCertificate_certificateSerial' - Serial number of the certificate to be revoked. This must be in
-- hexadecimal format. You can retrieve the serial number by calling
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate>
-- with the Amazon Resource Name (ARN) of the certificate you want and the
-- ARN of your private CA. The __GetCertificate__ action retrieves the
-- certificate in the PEM format. You can use the following OpenSSL command
-- to list the certificate in text format and copy the hexadecimal serial
-- number.
--
-- @openssl x509 -in file_path -text -noout@
--
-- You can also copy the serial number from the console or use the
-- <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate>
-- action in the /AWS Certificate Manager API Reference/.
--
-- 'revocationReason', 'revokeCertificate_revocationReason' - Specifies why you revoked the certificate.
newRevokeCertificate ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  -- | 'certificateSerial'
  Prelude.Text ->
  -- | 'revocationReason'
  RevocationReason ->
  RevokeCertificate
newRevokeCertificate
  pCertificateAuthorityArn_
  pCertificateSerial_
  pRevocationReason_ =
    RevokeCertificate'
      { certificateAuthorityArn =
          pCertificateAuthorityArn_,
        certificateSerial = pCertificateSerial_,
        revocationReason = pRevocationReason_
      }

-- | Amazon Resource Name (ARN) of the private CA that issued the certificate
-- to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
revokeCertificate_certificateAuthorityArn :: Lens.Lens' RevokeCertificate Prelude.Text
revokeCertificate_certificateAuthorityArn = Lens.lens (\RevokeCertificate' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@RevokeCertificate' {} a -> s {certificateAuthorityArn = a} :: RevokeCertificate)

-- | Serial number of the certificate to be revoked. This must be in
-- hexadecimal format. You can retrieve the serial number by calling
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate>
-- with the Amazon Resource Name (ARN) of the certificate you want and the
-- ARN of your private CA. The __GetCertificate__ action retrieves the
-- certificate in the PEM format. You can use the following OpenSSL command
-- to list the certificate in text format and copy the hexadecimal serial
-- number.
--
-- @openssl x509 -in file_path -text -noout@
--
-- You can also copy the serial number from the console or use the
-- <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate>
-- action in the /AWS Certificate Manager API Reference/.
revokeCertificate_certificateSerial :: Lens.Lens' RevokeCertificate Prelude.Text
revokeCertificate_certificateSerial = Lens.lens (\RevokeCertificate' {certificateSerial} -> certificateSerial) (\s@RevokeCertificate' {} a -> s {certificateSerial = a} :: RevokeCertificate)

-- | Specifies why you revoked the certificate.
revokeCertificate_revocationReason :: Lens.Lens' RevokeCertificate RevocationReason
revokeCertificate_revocationReason = Lens.lens (\RevokeCertificate' {revocationReason} -> revocationReason) (\s@RevokeCertificate' {} a -> s {revocationReason = a} :: RevokeCertificate)

instance Prelude.AWSRequest RevokeCertificate where
  type Rs RevokeCertificate = RevokeCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RevokeCertificateResponse'

instance Prelude.Hashable RevokeCertificate

instance Prelude.NFData RevokeCertificate

instance Prelude.ToHeaders RevokeCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ACMPrivateCA.RevokeCertificate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RevokeCertificate where
  toJSON RevokeCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Prelude..= certificateAuthorityArn
              ),
            Prelude.Just
              ("CertificateSerial" Prelude..= certificateSerial),
            Prelude.Just
              ("RevocationReason" Prelude..= revocationReason)
          ]
      )

instance Prelude.ToPath RevokeCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RevokeCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRevokeCertificateResponse' smart constructor.
data RevokeCertificateResponse = RevokeCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RevokeCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRevokeCertificateResponse ::
  RevokeCertificateResponse
newRevokeCertificateResponse =
  RevokeCertificateResponse'

instance Prelude.NFData RevokeCertificateResponse
