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
    rcCertificateArn,

    -- * Destructuring the response
    RenewCertificateResponse (..),
    mkRenewCertificateResponse,
  )
where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRenewCertificate' smart constructor.
newtype RenewCertificate = RenewCertificate'
  { -- | String that contains the ARN of the ACM certificate to be renewed. This must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    certificateArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RenewCertificate' value with any optional fields omitted.
mkRenewCertificate ::
  -- | 'certificateArn'
  Types.Arn ->
  RenewCertificate
mkRenewCertificate certificateArn =
  RenewCertificate' {certificateArn}

-- | String that contains the ARN of the ACM certificate to be renewed. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateArn :: Lens.Lens' RenewCertificate Types.Arn
rcCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED rcCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

instance Core.FromJSON RenewCertificate where
  toJSON RenewCertificate {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CertificateArn" Core..= certificateArn)]
      )

instance Core.AWSRequest RenewCertificate where
  type Rs RenewCertificate = RenewCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CertificateManager.RenewCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull RenewCertificateResponse'

-- | /See:/ 'mkRenewCertificateResponse' smart constructor.
data RenewCertificateResponse = RenewCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenewCertificateResponse' value with any optional fields omitted.
mkRenewCertificateResponse ::
  RenewCertificateResponse
mkRenewCertificateResponse = RenewCertificateResponse'
