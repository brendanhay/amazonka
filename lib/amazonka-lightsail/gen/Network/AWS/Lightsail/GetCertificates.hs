{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more Amazon Lightsail SSL/TLS certificates.
module Network.AWS.Lightsail.GetCertificates
  ( -- * Creating a request
    GetCertificates (..),
    mkGetCertificates,

    -- ** Request lenses
    gcCertificateName,
    gcCertificateStatuses,
    gcIncludeCertificateDetails,

    -- * Destructuring the response
    GetCertificatesResponse (..),
    mkGetCertificatesResponse,

    -- ** Response lenses
    gcrrsCertificates,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCertificates' smart constructor.
data GetCertificates = GetCertificates'
  { -- | The name for the certificate for which to return information.
    --
    -- When omitted, the response includes all of your certificates in the AWS Region where the request is made.
    certificateName :: Core.Maybe Types.CertificateName,
    -- | The status of the certificates for which to return information.
    --
    -- For example, specify @ISSUED@ to return only certificates with an @ISSUED@ status.
    -- When omitted, the response includes all of your certificates in the AWS Region where the request is made, regardless of their current status.
    certificateStatuses :: Core.Maybe [Types.CertificateStatus],
    -- | Indicates whether to include detailed information about the certificates in the response.
    --
    -- When omitted, the response includes only the certificate names, Amazon Resource Names (ARNs), domain names, and tags.
    includeCertificateDetails :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCertificates' value with any optional fields omitted.
mkGetCertificates ::
  GetCertificates
mkGetCertificates =
  GetCertificates'
    { certificateName = Core.Nothing,
      certificateStatuses = Core.Nothing,
      includeCertificateDetails = Core.Nothing
    }

-- | The name for the certificate for which to return information.
--
-- When omitted, the response includes all of your certificates in the AWS Region where the request is made.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateName :: Lens.Lens' GetCertificates (Core.Maybe Types.CertificateName)
gcCertificateName = Lens.field @"certificateName"
{-# DEPRECATED gcCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The status of the certificates for which to return information.
--
-- For example, specify @ISSUED@ to return only certificates with an @ISSUED@ status.
-- When omitted, the response includes all of your certificates in the AWS Region where the request is made, regardless of their current status.
--
-- /Note:/ Consider using 'certificateStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateStatuses :: Lens.Lens' GetCertificates (Core.Maybe [Types.CertificateStatus])
gcCertificateStatuses = Lens.field @"certificateStatuses"
{-# DEPRECATED gcCertificateStatuses "Use generic-lens or generic-optics with 'certificateStatuses' instead." #-}

-- | Indicates whether to include detailed information about the certificates in the response.
--
-- When omitted, the response includes only the certificate names, Amazon Resource Names (ARNs), domain names, and tags.
--
-- /Note:/ Consider using 'includeCertificateDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcIncludeCertificateDetails :: Lens.Lens' GetCertificates (Core.Maybe Core.Bool)
gcIncludeCertificateDetails = Lens.field @"includeCertificateDetails"
{-# DEPRECATED gcIncludeCertificateDetails "Use generic-lens or generic-optics with 'includeCertificateDetails' instead." #-}

instance Core.FromJSON GetCertificates where
  toJSON GetCertificates {..} =
    Core.object
      ( Core.catMaybes
          [ ("certificateName" Core..=) Core.<$> certificateName,
            ("certificateStatuses" Core..=) Core.<$> certificateStatuses,
            ("includeCertificateDetails" Core..=)
              Core.<$> includeCertificateDetails
          ]
      )

instance Core.AWSRequest GetCertificates where
  type Rs GetCertificates = GetCertificatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetCertificates")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCertificatesResponse'
            Core.<$> (x Core..:? "certificates") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCertificatesResponse' smart constructor.
data GetCertificatesResponse = GetCertificatesResponse'
  { -- | An object that describes certificates.
    certificates :: Core.Maybe [Types.CertificateSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetCertificatesResponse' value with any optional fields omitted.
mkGetCertificatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCertificatesResponse
mkGetCertificatesResponse responseStatus =
  GetCertificatesResponse'
    { certificates = Core.Nothing,
      responseStatus
    }

-- | An object that describes certificates.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCertificates :: Lens.Lens' GetCertificatesResponse (Core.Maybe [Types.CertificateSummary])
gcrrsCertificates = Lens.field @"certificates"
{-# DEPRECATED gcrrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetCertificatesResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
