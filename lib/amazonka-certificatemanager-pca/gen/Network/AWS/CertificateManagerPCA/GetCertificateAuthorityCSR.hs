{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the certificate signing request (CSR) for your private certificate authority (CA). The CSR is created when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. Sign the CSR with your ACM Private CA-hosted or on-premises root or subordinate CA. Then import the signed certificate back into ACM Private CA by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ImportCertificateAuthorityCertificate.html ImportCertificateAuthorityCertificate> action. The CSR is returned as a base64 PEM-encoded string.
module Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR
  ( -- * Creating a request
    GetCertificateAuthorityCSR (..),
    mkGetCertificateAuthorityCSR,

    -- ** Request lenses
    gcacsrCertificateAuthorityARN,

    -- * Destructuring the response
    GetCertificateAuthorityCSRResponse (..),
    mkGetCertificateAuthorityCSRResponse,

    -- ** Response lenses
    gcacsrrsCSR,
    gcacsrrsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCertificateAuthorityCSR' smart constructor.
newtype GetCertificateAuthorityCSR = GetCertificateAuthorityCSR'
  { -- | The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCertificateAuthorityCSR' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
mkGetCertificateAuthorityCSR ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  GetCertificateAuthorityCSR
mkGetCertificateAuthorityCSR pCertificateAuthorityARN_ =
  GetCertificateAuthorityCSR'
    { certificateAuthorityARN =
        pCertificateAuthorityARN_
    }

-- | The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacsrCertificateAuthorityARN :: Lens.Lens' GetCertificateAuthorityCSR Lude.Text
gcacsrCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: GetCertificateAuthorityCSR -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: GetCertificateAuthorityCSR)
{-# DEPRECATED gcacsrCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest GetCertificateAuthorityCSR where
  type
    Rs GetCertificateAuthorityCSR =
      GetCertificateAuthorityCSRResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCertificateAuthorityCSRResponse'
            Lude.<$> (x Lude..?> "Csr") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCertificateAuthorityCSR where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.GetCertificateAuthorityCsr" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCertificateAuthorityCSR where
  toJSON GetCertificateAuthorityCSR' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath GetCertificateAuthorityCSR where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCertificateAuthorityCSR where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCertificateAuthorityCSRResponse' smart constructor.
data GetCertificateAuthorityCSRResponse = GetCertificateAuthorityCSRResponse'
  { -- | The base64 PEM-encoded certificate signing request (CSR) for your private CA certificate.
    cSR :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCertificateAuthorityCSRResponse' with the minimum fields required to make a request.
--
-- * 'cSR' - The base64 PEM-encoded certificate signing request (CSR) for your private CA certificate.
-- * 'responseStatus' - The response status code.
mkGetCertificateAuthorityCSRResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCertificateAuthorityCSRResponse
mkGetCertificateAuthorityCSRResponse pResponseStatus_ =
  GetCertificateAuthorityCSRResponse'
    { cSR = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The base64 PEM-encoded certificate signing request (CSR) for your private CA certificate.
--
-- /Note:/ Consider using 'cSR' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacsrrsCSR :: Lens.Lens' GetCertificateAuthorityCSRResponse (Lude.Maybe Lude.Text)
gcacsrrsCSR = Lens.lens (cSR :: GetCertificateAuthorityCSRResponse -> Lude.Maybe Lude.Text) (\s a -> s {cSR = a} :: GetCertificateAuthorityCSRResponse)
{-# DEPRECATED gcacsrrsCSR "Use generic-lens or generic-optics with 'cSR' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacsrrsResponseStatus :: Lens.Lens' GetCertificateAuthorityCSRResponse Lude.Int
gcacsrrsResponseStatus = Lens.lens (responseStatus :: GetCertificateAuthorityCSRResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCertificateAuthorityCSRResponse)
{-# DEPRECATED gcacsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
