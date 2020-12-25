{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.DeleteCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a certificate and its associated private key. If this action succeeds, the certificate no longer appears in the list that can be displayed by calling the 'ListCertificates' action or be retrieved by calling the 'GetCertificate' action. The certificate will not be available for use by AWS services integrated with ACM.
module Network.AWS.CertificateManager.DeleteCertificate
  ( -- * Creating a request
    DeleteCertificate (..),
    mkDeleteCertificate,

    -- ** Request lenses
    dcCertificateArn,

    -- * Destructuring the response
    DeleteCertificateResponse (..),
    mkDeleteCertificateResponse,
  )
where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'
  { -- | String that contains the ARN of the ACM certificate to be deleted. This must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    certificateArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCertificate' value with any optional fields omitted.
mkDeleteCertificate ::
  -- | 'certificateArn'
  Types.Arn ->
  DeleteCertificate
mkDeleteCertificate certificateArn =
  DeleteCertificate' {certificateArn}

-- | String that contains the ARN of the ACM certificate to be deleted. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateArn :: Lens.Lens' DeleteCertificate Types.Arn
dcCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED dcCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

instance Core.FromJSON DeleteCertificate where
  toJSON DeleteCertificate {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CertificateArn" Core..= certificateArn)]
      )

instance Core.AWSRequest DeleteCertificate where
  type Rs DeleteCertificate = DeleteCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CertificateManager.DeleteCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteCertificateResponse'

-- | /See:/ 'mkDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCertificateResponse' value with any optional fields omitted.
mkDeleteCertificateResponse ::
  DeleteCertificateResponse
mkDeleteCertificateResponse = DeleteCertificateResponse'
