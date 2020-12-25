{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a certificate authority (CA) that is in the @DELETED@ state. You can restore a CA during the period that you defined in the __PermanentDeletionTimeInDays__ parameter of the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthority.html DeleteCertificateAuthority> action. Currently, you can specify 7 to 30 days. If you did not specify a __PermanentDeletionTimeInDays__ value, by default you can restore the CA at any time in a 30 day period. You can check the time remaining in the restoration period of a private CA in the @DELETED@ state by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DescribeCertificateAuthority.html DescribeCertificateAuthority> or <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> actions. The status of a restored CA is set to its pre-deletion status when the __RestoreCertificateAuthority__ action returns. To change its status to @ACTIVE@ , call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> action. If the private CA was in the @PENDING_CERTIFICATE@ state at deletion, you must use the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ImportCertificateAuthorityCertificate.html ImportCertificateAuthorityCertificate> action to import a certificate authority into the private CA before it can be activated. You cannot restore a CA after the restoration period has ended.
module Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
  ( -- * Creating a request
    RestoreCertificateAuthority (..),
    mkRestoreCertificateAuthority,

    -- ** Request lenses
    rcaCertificateAuthorityArn,

    -- * Destructuring the response
    RestoreCertificateAuthorityResponse (..),
    mkRestoreCertificateAuthorityResponse,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreCertificateAuthority' smart constructor.
newtype RestoreCertificateAuthority = RestoreCertificateAuthority'
  { -- | The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
    certificateAuthorityArn :: Types.CertificateAuthorityArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreCertificateAuthority' value with any optional fields omitted.
mkRestoreCertificateAuthority ::
  -- | 'certificateAuthorityArn'
  Types.CertificateAuthorityArn ->
  RestoreCertificateAuthority
mkRestoreCertificateAuthority certificateAuthorityArn =
  RestoreCertificateAuthority' {certificateAuthorityArn}

-- | The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcaCertificateAuthorityArn :: Lens.Lens' RestoreCertificateAuthority Types.CertificateAuthorityArn
rcaCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED rcaCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

instance Core.FromJSON RestoreCertificateAuthority where
  toJSON RestoreCertificateAuthority {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateAuthorityArn" Core..= certificateAuthorityArn)
          ]
      )

instance Core.AWSRequest RestoreCertificateAuthority where
  type
    Rs RestoreCertificateAuthority =
      RestoreCertificateAuthorityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ACMPrivateCA.RestoreCertificateAuthority")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull RestoreCertificateAuthorityResponse'

-- | /See:/ 'mkRestoreCertificateAuthorityResponse' smart constructor.
data RestoreCertificateAuthorityResponse = RestoreCertificateAuthorityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreCertificateAuthorityResponse' value with any optional fields omitted.
mkRestoreCertificateAuthorityResponse ::
  RestoreCertificateAuthorityResponse
mkRestoreCertificateAuthorityResponse =
  RestoreCertificateAuthorityResponse'
