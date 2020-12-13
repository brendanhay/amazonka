{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about your private certificate authority (CA) or one that has been shared with you. You specify the private CA on input by its ARN (Amazon Resource Name). The output contains the status of your CA. This can be any of the following:
--
--
--     * @CREATING@ - ACM Private CA is creating your private certificate authority.
--
--
--     * @PENDING_CERTIFICATE@ - The certificate is pending. You must use your ACM Private CA-hosted or on-premises root or subordinate CA to sign your private CA CSR and then import it into PCA.
--
--
--     * @ACTIVE@ - Your private CA is active.
--
--
--     * @DISABLED@ - Your private CA has been disabled.
--
--
--     * @EXPIRED@ - Your private CA certificate has expired.
--
--
--     * @FAILED@ - Your private CA has failed. Your CA can fail because of problems such a network outage or backend AWS failure or other errors. A failed CA can never return to the pending state. You must create a new CA.
--
--
--     * @DELETED@ - Your private CA is within the restoration period, after which it is permanently deleted. The length of time remaining in the CA's restoration period is also included in this action's output.
module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
  ( -- * Creating a request
    DescribeCertificateAuthority (..),
    mkDescribeCertificateAuthority,

    -- ** Request lenses
    dCertificateAuthorityARN,

    -- * Destructuring the response
    DescribeCertificateAuthorityResponse (..),
    mkDescribeCertificateAuthorityResponse,

    -- ** Response lenses
    dcarsCertificateAuthority,
    dcarsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCertificateAuthority' smart constructor.
newtype DescribeCertificateAuthority = DescribeCertificateAuthority'
  { -- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificateAuthority' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
mkDescribeCertificateAuthority ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  DescribeCertificateAuthority
mkDescribeCertificateAuthority pCertificateAuthorityARN_ =
  DescribeCertificateAuthority'
    { certificateAuthorityARN =
        pCertificateAuthorityARN_
    }

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateAuthorityARN :: Lens.Lens' DescribeCertificateAuthority Lude.Text
dCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: DescribeCertificateAuthority -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: DescribeCertificateAuthority)
{-# DEPRECATED dCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest DescribeCertificateAuthority where
  type
    Rs DescribeCertificateAuthority =
      DescribeCertificateAuthorityResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCertificateAuthorityResponse'
            Lude.<$> (x Lude..?> "CertificateAuthority")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCertificateAuthority where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.DescribeCertificateAuthority" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCertificateAuthority where
  toJSON DescribeCertificateAuthority' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath DescribeCertificateAuthority where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCertificateAuthority where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCertificateAuthorityResponse' smart constructor.
data DescribeCertificateAuthorityResponse = DescribeCertificateAuthorityResponse'
  { -- | A <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CertificateAuthority.html CertificateAuthority> structure that contains information about your private CA.
    certificateAuthority :: Lude.Maybe CertificateAuthority,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificateAuthorityResponse' with the minimum fields required to make a request.
--
-- * 'certificateAuthority' - A <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CertificateAuthority.html CertificateAuthority> structure that contains information about your private CA.
-- * 'responseStatus' - The response status code.
mkDescribeCertificateAuthorityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCertificateAuthorityResponse
mkDescribeCertificateAuthorityResponse pResponseStatus_ =
  DescribeCertificateAuthorityResponse'
    { certificateAuthority =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CertificateAuthority.html CertificateAuthority> structure that contains information about your private CA.
--
-- /Note:/ Consider using 'certificateAuthority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarsCertificateAuthority :: Lens.Lens' DescribeCertificateAuthorityResponse (Lude.Maybe CertificateAuthority)
dcarsCertificateAuthority = Lens.lens (certificateAuthority :: DescribeCertificateAuthorityResponse -> Lude.Maybe CertificateAuthority) (\s a -> s {certificateAuthority = a} :: DescribeCertificateAuthorityResponse)
{-# DEPRECATED dcarsCertificateAuthority "Use generic-lens or generic-optics with 'certificateAuthority' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarsResponseStatus :: Lens.Lens' DescribeCertificateAuthorityResponse Lude.Int
dcarsResponseStatus = Lens.lens (responseStatus :: DescribeCertificateAuthorityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCertificateAuthorityResponse)
{-# DEPRECATED dcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
