{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeCACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a registered CA certificate.
module Network.AWS.IoT.DescribeCACertificate
  ( -- * Creating a request
    DescribeCACertificate (..),
    mkDescribeCACertificate,

    -- ** Request lenses
    dCertificateId,

    -- * Destructuring the response
    DescribeCACertificateResponse (..),
    mkDescribeCACertificateResponse,

    -- ** Response lenses
    dcacrfrsCertificateDescription,
    dcacrfrsRegistrationConfig,
    dcacrfrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeCACertificate operation.
--
-- /See:/ 'mkDescribeCACertificate' smart constructor.
newtype DescribeCACertificate = DescribeCACertificate'
  { -- | The CA certificate identifier.
    certificateId :: Types.CertificateId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCACertificate' value with any optional fields omitted.
mkDescribeCACertificate ::
  -- | 'certificateId'
  Types.CertificateId ->
  DescribeCACertificate
mkDescribeCACertificate certificateId =
  DescribeCACertificate' {certificateId}

-- | The CA certificate identifier.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateId :: Lens.Lens' DescribeCACertificate Types.CertificateId
dCertificateId = Lens.field @"certificateId"
{-# DEPRECATED dCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Core.AWSRequest DescribeCACertificate where
  type Rs DescribeCACertificate = DescribeCACertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/cacertificate/" Core.<> (Core.toText certificateId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCACertificateResponse'
            Core.<$> (x Core..:? "certificateDescription")
            Core.<*> (x Core..:? "registrationConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the DescribeCACertificate operation.
--
-- /See:/ 'mkDescribeCACertificateResponse' smart constructor.
data DescribeCACertificateResponse = DescribeCACertificateResponse'
  { -- | The CA certificate description.
    certificateDescription :: Core.Maybe Types.CACertificateDescription,
    -- | Information about the registration configuration.
    registrationConfig :: Core.Maybe Types.RegistrationConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeCACertificateResponse' value with any optional fields omitted.
mkDescribeCACertificateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCACertificateResponse
mkDescribeCACertificateResponse responseStatus =
  DescribeCACertificateResponse'
    { certificateDescription =
        Core.Nothing,
      registrationConfig = Core.Nothing,
      responseStatus
    }

-- | The CA certificate description.
--
-- /Note:/ Consider using 'certificateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacrfrsCertificateDescription :: Lens.Lens' DescribeCACertificateResponse (Core.Maybe Types.CACertificateDescription)
dcacrfrsCertificateDescription = Lens.field @"certificateDescription"
{-# DEPRECATED dcacrfrsCertificateDescription "Use generic-lens or generic-optics with 'certificateDescription' instead." #-}

-- | Information about the registration configuration.
--
-- /Note:/ Consider using 'registrationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacrfrsRegistrationConfig :: Lens.Lens' DescribeCACertificateResponse (Core.Maybe Types.RegistrationConfig)
dcacrfrsRegistrationConfig = Lens.field @"registrationConfig"
{-# DEPRECATED dcacrfrsRegistrationConfig "Use generic-lens or generic-optics with 'registrationConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacrfrsResponseStatus :: Lens.Lens' DescribeCACertificateResponse Core.Int
dcacrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcacrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
