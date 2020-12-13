{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.RenewCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renews an eligable ACM certificate. At this time, only exported private certificates can be renewed with this operation. In order to renew your ACM PCA certificates with ACM, you must first <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaPermissions.html grant the ACM service principal permission to do so> . For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/manual-renewal.html Testing Managed Renewal> in the ACM User Guide.
module Network.AWS.CertificateManager.RenewCertificate
  ( -- * Creating a request
    RenewCertificate (..),
    mkRenewCertificate,

    -- ** Request lenses
    rcCertificateARN,

    -- * Destructuring the response
    RenewCertificateResponse (..),
    mkRenewCertificateResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRenewCertificate' smart constructor.
newtype RenewCertificate = RenewCertificate'
  { -- | String that contains the ARN of the ACM certificate to be renewed. This must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    certificateARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenewCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - String that contains the ARN of the ACM certificate to be renewed. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkRenewCertificate ::
  -- | 'certificateARN'
  Lude.Text ->
  RenewCertificate
mkRenewCertificate pCertificateARN_ =
  RenewCertificate' {certificateARN = pCertificateARN_}

-- | String that contains the ARN of the ACM certificate to be renewed. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateARN :: Lens.Lens' RenewCertificate Lude.Text
rcCertificateARN = Lens.lens (certificateARN :: RenewCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: RenewCertificate)
{-# DEPRECATED rcCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.AWSRequest RenewCertificate where
  type Rs RenewCertificate = RenewCertificateResponse
  request = Req.postJSON certificateManagerService
  response = Res.receiveNull RenewCertificateResponse'

instance Lude.ToHeaders RenewCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.RenewCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RenewCertificate where
  toJSON RenewCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CertificateArn" Lude..= certificateARN)]
      )

instance Lude.ToPath RenewCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery RenewCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRenewCertificateResponse' smart constructor.
data RenewCertificateResponse = RenewCertificateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenewCertificateResponse' with the minimum fields required to make a request.
mkRenewCertificateResponse ::
  RenewCertificateResponse
mkRenewCertificateResponse = RenewCertificateResponse'
