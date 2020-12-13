{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.DescribeCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed metadata about the specified ACM certificate.
module Network.AWS.CertificateManager.DescribeCertificate
  ( -- * Creating a request
    DescribeCertificate (..),
    mkDescribeCertificate,

    -- ** Request lenses
    dCertificateARN,

    -- * Destructuring the response
    DescribeCertificateResponse (..),
    mkDescribeCertificateResponse,

    -- ** Response lenses
    dcrsCertificate,
    dcrsResponseStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCertificate' smart constructor.
newtype DescribeCertificate = DescribeCertificate'
  { -- | The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have the following form:
    --
    -- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    certificateARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkDescribeCertificate ::
  -- | 'certificateARN'
  Lude.Text ->
  DescribeCertificate
mkDescribeCertificate pCertificateARN_ =
  DescribeCertificate' {certificateARN = pCertificateARN_}

-- | The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateARN :: Lens.Lens' DescribeCertificate Lude.Text
dCertificateARN = Lens.lens (certificateARN :: DescribeCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: DescribeCertificate)
{-# DEPRECATED dCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.AWSRequest DescribeCertificate where
  type Rs DescribeCertificate = DescribeCertificateResponse
  request = Req.postJSON certificateManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCertificateResponse'
            Lude.<$> (x Lude..?> "Certificate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.DescribeCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCertificate where
  toJSON DescribeCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CertificateArn" Lude..= certificateARN)]
      )

instance Lude.ToPath DescribeCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { -- | Metadata about an ACM certificate.
    certificate :: Lude.Maybe CertificateDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - Metadata about an ACM certificate.
-- * 'responseStatus' - The response status code.
mkDescribeCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCertificateResponse
mkDescribeCertificateResponse pResponseStatus_ =
  DescribeCertificateResponse'
    { certificate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Metadata about an ACM certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCertificate :: Lens.Lens' DescribeCertificateResponse (Lude.Maybe CertificateDetail)
dcrsCertificate = Lens.lens (certificate :: DescribeCertificateResponse -> Lude.Maybe CertificateDetail) (\s a -> s {certificate = a} :: DescribeCertificateResponse)
{-# DEPRECATED dcrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeCertificateResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCertificateResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
