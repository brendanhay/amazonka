{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.GetCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an Amazon-issued certificate and its certificate chain. The chain consists of the certificate of the issuing CA and the intermediate certificates of any other subordinate CAs. All of the certificates are base64 encoded. You can use <https://wiki.openssl.org/index.php/Command_Line_Utilities OpenSSL> to decode the certificates and inspect individual fields.
module Network.AWS.CertificateManager.GetCertificate
  ( -- * Creating a request
    GetCertificate (..),
    mkGetCertificate,

    -- ** Request lenses
    gcCertificateARN,

    -- * Destructuring the response
    GetCertificateResponse (..),
    mkGetCertificateResponse,

    -- ** Response lenses
    gcrsCertificate,
    gcrsCertificateChain,
    gcrsResponseStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCertificate' smart constructor.
newtype GetCertificate = GetCertificate'
  { certificateARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - String that contains a certificate ARN in the following format:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkGetCertificate ::
  -- | 'certificateARN'
  Lude.Text ->
  GetCertificate
mkGetCertificate pCertificateARN_ =
  GetCertificate' {certificateARN = pCertificateARN_}

-- | String that contains a certificate ARN in the following format:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateARN :: Lens.Lens' GetCertificate Lude.Text
gcCertificateARN = Lens.lens (certificateARN :: GetCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: GetCertificate)
{-# DEPRECATED gcCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.AWSRequest GetCertificate where
  type Rs GetCertificate = GetCertificateResponse
  request = Req.postJSON certificateManagerService
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
              Lude.=# ("CertificateManager.GetCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCertificate where
  toJSON GetCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CertificateArn" Lude..= certificateARN)]
      )

instance Lude.ToPath GetCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { certificate ::
      Lude.Maybe Lude.Text,
    certificateChain :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - The ACM-issued certificate corresponding to the ARN specified as input.
-- * 'certificateChain' - Certificates forming the requested certificate's chain of trust. The chain consists of the certificate of the issuing CA and the intermediate certificates of any other subordinate CAs.
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

-- | The ACM-issued certificate corresponding to the ARN specified as input.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsCertificate :: Lens.Lens' GetCertificateResponse (Lude.Maybe Lude.Text)
gcrsCertificate = Lens.lens (certificate :: GetCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: GetCertificateResponse)
{-# DEPRECATED gcrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | Certificates forming the requested certificate's chain of trust. The chain consists of the certificate of the issuing CA and the intermediate certificates of any other subordinate CAs.
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
