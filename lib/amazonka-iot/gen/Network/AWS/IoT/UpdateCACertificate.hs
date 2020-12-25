{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateCACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered CA certificate.
module Network.AWS.IoT.UpdateCACertificate
  ( -- * Creating a request
    UpdateCACertificate (..),
    mkUpdateCACertificate,

    -- ** Request lenses
    ucacCertificateId,
    ucacNewAutoRegistrationStatus,
    ucacNewStatus,
    ucacRegistrationConfig,
    ucacRemoveAutoRegistration,

    -- * Destructuring the response
    UpdateCACertificateResponse (..),
    mkUpdateCACertificateResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the UpdateCACertificate operation.
--
-- /See:/ 'mkUpdateCACertificate' smart constructor.
data UpdateCACertificate = UpdateCACertificate'
  { -- | The CA certificate identifier.
    certificateId :: Types.CertificateId,
    -- | The new value for the auto registration status. Valid values are: "ENABLE" or "DISABLE".
    newAutoRegistrationStatus :: Core.Maybe Types.AutoRegistrationStatus,
    -- | The updated status of the CA certificate.
    --
    -- __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
    newStatus :: Core.Maybe Types.CACertificateStatus,
    -- | Information about the registration configuration.
    registrationConfig :: Core.Maybe Types.RegistrationConfig,
    -- | If true, removes auto registration.
    removeAutoRegistration :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCACertificate' value with any optional fields omitted.
mkUpdateCACertificate ::
  -- | 'certificateId'
  Types.CertificateId ->
  UpdateCACertificate
mkUpdateCACertificate certificateId =
  UpdateCACertificate'
    { certificateId,
      newAutoRegistrationStatus = Core.Nothing,
      newStatus = Core.Nothing,
      registrationConfig = Core.Nothing,
      removeAutoRegistration = Core.Nothing
    }

-- | The CA certificate identifier.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacCertificateId :: Lens.Lens' UpdateCACertificate Types.CertificateId
ucacCertificateId = Lens.field @"certificateId"
{-# DEPRECATED ucacCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The new value for the auto registration status. Valid values are: "ENABLE" or "DISABLE".
--
-- /Note:/ Consider using 'newAutoRegistrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacNewAutoRegistrationStatus :: Lens.Lens' UpdateCACertificate (Core.Maybe Types.AutoRegistrationStatus)
ucacNewAutoRegistrationStatus = Lens.field @"newAutoRegistrationStatus"
{-# DEPRECATED ucacNewAutoRegistrationStatus "Use generic-lens or generic-optics with 'newAutoRegistrationStatus' instead." #-}

-- | The updated status of the CA certificate.
--
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- /Note:/ Consider using 'newStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacNewStatus :: Lens.Lens' UpdateCACertificate (Core.Maybe Types.CACertificateStatus)
ucacNewStatus = Lens.field @"newStatus"
{-# DEPRECATED ucacNewStatus "Use generic-lens or generic-optics with 'newStatus' instead." #-}

-- | Information about the registration configuration.
--
-- /Note:/ Consider using 'registrationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacRegistrationConfig :: Lens.Lens' UpdateCACertificate (Core.Maybe Types.RegistrationConfig)
ucacRegistrationConfig = Lens.field @"registrationConfig"
{-# DEPRECATED ucacRegistrationConfig "Use generic-lens or generic-optics with 'registrationConfig' instead." #-}

-- | If true, removes auto registration.
--
-- /Note:/ Consider using 'removeAutoRegistration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacRemoveAutoRegistration :: Lens.Lens' UpdateCACertificate (Core.Maybe Core.Bool)
ucacRemoveAutoRegistration = Lens.field @"removeAutoRegistration"
{-# DEPRECATED ucacRemoveAutoRegistration "Use generic-lens or generic-optics with 'removeAutoRegistration' instead." #-}

instance Core.FromJSON UpdateCACertificate where
  toJSON UpdateCACertificate {..} =
    Core.object
      ( Core.catMaybes
          [ ("registrationConfig" Core..=) Core.<$> registrationConfig,
            ("removeAutoRegistration" Core..=)
              Core.<$> removeAutoRegistration
          ]
      )

instance Core.AWSRequest UpdateCACertificate where
  type Rs UpdateCACertificate = UpdateCACertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/cacertificate/" Core.<> (Core.toText certificateId)),
        Core._rqQuery =
          Core.toQueryValue "newAutoRegistrationStatus"
            Core.<$> newAutoRegistrationStatus
            Core.<> (Core.toQueryValue "newStatus" Core.<$> newStatus),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateCACertificateResponse'

-- | /See:/ 'mkUpdateCACertificateResponse' smart constructor.
data UpdateCACertificateResponse = UpdateCACertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCACertificateResponse' value with any optional fields omitted.
mkUpdateCACertificateResponse ::
  UpdateCACertificateResponse
mkUpdateCACertificateResponse = UpdateCACertificateResponse'
