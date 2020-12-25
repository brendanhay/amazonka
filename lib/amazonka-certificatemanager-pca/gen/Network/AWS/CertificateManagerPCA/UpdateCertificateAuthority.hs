{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status or configuration of a private certificate authority (CA). Your private CA must be in the @ACTIVE@ or @DISABLED@ state before you can update it. You can disable a private CA that is in the @ACTIVE@ state or make a CA that is in the @DISABLED@ state active again.
module Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
  ( -- * Creating a request
    UpdateCertificateAuthority (..),
    mkUpdateCertificateAuthority,

    -- ** Request lenses
    ucaCertificateAuthorityArn,
    ucaRevocationConfiguration,
    ucaStatus,

    -- * Destructuring the response
    UpdateCertificateAuthorityResponse (..),
    mkUpdateCertificateAuthorityResponse,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCertificateAuthority' smart constructor.
data UpdateCertificateAuthority = UpdateCertificateAuthority'
  { -- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
    certificateAuthorityArn :: Types.CertificateAuthorityArn,
    -- | Revocation information for your private CA.
    revocationConfiguration :: Core.Maybe Types.RevocationConfiguration,
    -- | Status of your private CA.
    status :: Core.Maybe Types.CertificateAuthorityStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCertificateAuthority' value with any optional fields omitted.
mkUpdateCertificateAuthority ::
  -- | 'certificateAuthorityArn'
  Types.CertificateAuthorityArn ->
  UpdateCertificateAuthority
mkUpdateCertificateAuthority certificateAuthorityArn =
  UpdateCertificateAuthority'
    { certificateAuthorityArn,
      revocationConfiguration = Core.Nothing,
      status = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaCertificateAuthorityArn :: Lens.Lens' UpdateCertificateAuthority Types.CertificateAuthorityArn
ucaCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED ucaCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

-- | Revocation information for your private CA.
--
-- /Note:/ Consider using 'revocationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaRevocationConfiguration :: Lens.Lens' UpdateCertificateAuthority (Core.Maybe Types.RevocationConfiguration)
ucaRevocationConfiguration = Lens.field @"revocationConfiguration"
{-# DEPRECATED ucaRevocationConfiguration "Use generic-lens or generic-optics with 'revocationConfiguration' instead." #-}

-- | Status of your private CA.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaStatus :: Lens.Lens' UpdateCertificateAuthority (Core.Maybe Types.CertificateAuthorityStatus)
ucaStatus = Lens.field @"status"
{-# DEPRECATED ucaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON UpdateCertificateAuthority where
  toJSON UpdateCertificateAuthority {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
            ("RevocationConfiguration" Core..=)
              Core.<$> revocationConfiguration,
            ("Status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest UpdateCertificateAuthority where
  type
    Rs UpdateCertificateAuthority =
      UpdateCertificateAuthorityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ACMPrivateCA.UpdateCertificateAuthority")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateCertificateAuthorityResponse'

-- | /See:/ 'mkUpdateCertificateAuthorityResponse' smart constructor.
data UpdateCertificateAuthorityResponse = UpdateCertificateAuthorityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCertificateAuthorityResponse' value with any optional fields omitted.
mkUpdateCertificateAuthorityResponse ::
  UpdateCertificateAuthorityResponse
mkUpdateCertificateAuthorityResponse =
  UpdateCertificateAuthorityResponse'
