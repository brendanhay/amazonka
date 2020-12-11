{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the certificate and certificate chain for your private certificate authority (CA) or one that has been shared with you. Both the certificate and the chain are base64 PEM-encoded. The chain does not include the CA certificate. Each certificate in the chain signs the one before it.
module Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCertificate
  ( -- * Creating a request
    GetCertificateAuthorityCertificate (..),
    mkGetCertificateAuthorityCertificate,

    -- ** Request lenses
    gcacCertificateAuthorityARN,

    -- * Destructuring the response
    GetCertificateAuthorityCertificateResponse (..),
    mkGetCertificateAuthorityCertificateResponse,

    -- ** Response lenses
    gcacrsCertificate,
    gcacrsCertificateChain,
    gcacrsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCertificateAuthorityCertificate' smart constructor.
newtype GetCertificateAuthorityCertificate = GetCertificateAuthorityCertificate'
  { certificateAuthorityARN ::
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

-- | Creates a value of 'GetCertificateAuthorityCertificate' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) of your private CA. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
mkGetCertificateAuthorityCertificate ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  GetCertificateAuthorityCertificate
mkGetCertificateAuthorityCertificate pCertificateAuthorityARN_ =
  GetCertificateAuthorityCertificate'
    { certificateAuthorityARN =
        pCertificateAuthorityARN_
    }

-- | The Amazon Resource Name (ARN) of your private CA. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacCertificateAuthorityARN :: Lens.Lens' GetCertificateAuthorityCertificate Lude.Text
gcacCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: GetCertificateAuthorityCertificate -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: GetCertificateAuthorityCertificate)
{-# DEPRECATED gcacCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest GetCertificateAuthorityCertificate where
  type
    Rs GetCertificateAuthorityCertificate =
      GetCertificateAuthorityCertificateResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCertificateAuthorityCertificateResponse'
            Lude.<$> (x Lude..?> "Certificate")
            Lude.<*> (x Lude..?> "CertificateChain")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCertificateAuthorityCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ACMPrivateCA.GetCertificateAuthorityCertificate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCertificateAuthorityCertificate where
  toJSON GetCertificateAuthorityCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath GetCertificateAuthorityCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCertificateAuthorityCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCertificateAuthorityCertificateResponse' smart constructor.
data GetCertificateAuthorityCertificateResponse = GetCertificateAuthorityCertificateResponse'
  { certificate ::
      Lude.Maybe
        Lude.Text,
    certificateChain ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCertificateAuthorityCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - Base64-encoded certificate authority (CA) certificate.
-- * 'certificateChain' - Base64-encoded certificate chain that includes any intermediate certificates and chains up to root on-premises certificate that you used to sign your private CA certificate. The chain does not include your private CA certificate. If this is a root CA, the value will be null.
-- * 'responseStatus' - The response status code.
mkGetCertificateAuthorityCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCertificateAuthorityCertificateResponse
mkGetCertificateAuthorityCertificateResponse pResponseStatus_ =
  GetCertificateAuthorityCertificateResponse'
    { certificate =
        Lude.Nothing,
      certificateChain = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Base64-encoded certificate authority (CA) certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacrsCertificate :: Lens.Lens' GetCertificateAuthorityCertificateResponse (Lude.Maybe Lude.Text)
gcacrsCertificate = Lens.lens (certificate :: GetCertificateAuthorityCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: GetCertificateAuthorityCertificateResponse)
{-# DEPRECATED gcacrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | Base64-encoded certificate chain that includes any intermediate certificates and chains up to root on-premises certificate that you used to sign your private CA certificate. The chain does not include your private CA certificate. If this is a root CA, the value will be null.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacrsCertificateChain :: Lens.Lens' GetCertificateAuthorityCertificateResponse (Lude.Maybe Lude.Text)
gcacrsCertificateChain = Lens.lens (certificateChain :: GetCertificateAuthorityCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateChain = a} :: GetCertificateAuthorityCertificateResponse)
{-# DEPRECATED gcacrsCertificateChain "Use generic-lens or generic-optics with 'certificateChain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacrsResponseStatus :: Lens.Lens' GetCertificateAuthorityCertificateResponse Lude.Int
gcacrsResponseStatus = Lens.lens (responseStatus :: GetCertificateAuthorityCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCertificateAuthorityCertificateResponse)
{-# DEPRECATED gcacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
