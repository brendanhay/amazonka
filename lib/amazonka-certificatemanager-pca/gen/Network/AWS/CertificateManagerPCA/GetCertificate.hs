{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.GetCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a certificate from your private CA or one that has been shared with you. The ARN of the certificate is returned when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_IssueCertificate.html IssueCertificate> action. You must specify both the ARN of your private CA and the ARN of the issued certificate when calling the __GetCertificate__ action. You can retrieve the certificate if it is in the __ISSUED__ state. You can call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport> action to create a report that contains information about all of the certificates issued and revoked by your private CA.
module Network.AWS.CertificateManagerPCA.GetCertificate
  ( -- * Creating a request
    GetCertificate (..),
    mkGetCertificate,

    -- ** Request lenses
    gcCertificateARN,
    gcCertificateAuthorityARN,

    -- * Destructuring the response
    GetCertificateResponse (..),
    mkGetCertificateResponse,

    -- ** Response lenses
    gcrsCertificate,
    gcrsCertificateChain,
    gcrsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCertificate' smart constructor.
data GetCertificate = GetCertificate'
  { -- | The ARN of the issued certificate. The ARN contains the certificate serial number and must be in the following form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
    certificateARN :: Lude.Text,
    -- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the issued certificate. The ARN contains the certificate serial number and must be in the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
mkGetCertificate ::
  -- | 'certificateARN'
  Lude.Text ->
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  GetCertificate
mkGetCertificate pCertificateARN_ pCertificateAuthorityARN_ =
  GetCertificate'
    { certificateARN = pCertificateARN_,
      certificateAuthorityARN = pCertificateAuthorityARN_
    }

-- | The ARN of the issued certificate. The ARN contains the certificate serial number and must be in the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateARN :: Lens.Lens' GetCertificate Lude.Text
gcCertificateARN = Lens.lens (certificateARN :: GetCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: GetCertificate)
{-# DEPRECATED gcCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateAuthorityARN :: Lens.Lens' GetCertificate Lude.Text
gcCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: GetCertificate -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: GetCertificate)
{-# DEPRECATED gcCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest GetCertificate where
  type Rs GetCertificate = GetCertificateResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCertificateResponse'
            Lude.<$> (x Lude..?> "Certificate")
            Lude.<*> (x Lude..?> "CertificateChain")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.GetCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCertificate where
  toJSON GetCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CertificateArn" Lude..= certificateARN),
            Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath GetCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { -- | The base64 PEM-encoded certificate specified by the @CertificateArn@ parameter.
    certificate :: Lude.Maybe Lude.Text,
    -- | The base64 PEM-encoded certificate chain that chains up to the on-premises root CA certificate that you used to sign your private CA certificate.
    certificateChain :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - The base64 PEM-encoded certificate specified by the @CertificateArn@ parameter.
-- * 'certificateChain' - The base64 PEM-encoded certificate chain that chains up to the on-premises root CA certificate that you used to sign your private CA certificate.
-- * 'responseStatus' - The response status code.
mkGetCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCertificateResponse
mkGetCertificateResponse pResponseStatus_ =
  GetCertificateResponse'
    { certificate = Lude.Nothing,
      certificateChain = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The base64 PEM-encoded certificate specified by the @CertificateArn@ parameter.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsCertificate :: Lens.Lens' GetCertificateResponse (Lude.Maybe Lude.Text)
gcrsCertificate = Lens.lens (certificate :: GetCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: GetCertificateResponse)
{-# DEPRECATED gcrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The base64 PEM-encoded certificate chain that chains up to the on-premises root CA certificate that you used to sign your private CA certificate.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsCertificateChain :: Lens.Lens' GetCertificateResponse (Lude.Maybe Lude.Text)
gcrsCertificateChain = Lens.lens (certificateChain :: GetCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateChain = a} :: GetCertificateResponse)
{-# DEPRECATED gcrsCertificateChain "Use generic-lens or generic-optics with 'certificateChain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetCertificateResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCertificateResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
