{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RegisterCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a certificate for secured LDAP connection.
module Network.AWS.DirectoryService.RegisterCertificate
  ( -- * Creating a request
    RegisterCertificate (..),
    mkRegisterCertificate,

    -- ** Request lenses
    rcDirectoryId,
    rcCertificateData,

    -- * Destructuring the response
    RegisterCertificateResponse (..),
    mkRegisterCertificateResponse,

    -- ** Response lenses
    rcrrsCertificateId,
    rcrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterCertificate' smart constructor.
data RegisterCertificate = RegisterCertificate'
  { -- | The identifier of the directory.
    directoryId :: Types.DirectoryId,
    -- | The certificate PEM string that needs to be registered.
    certificateData :: Types.CertificateData
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCertificate' value with any optional fields omitted.
mkRegisterCertificate ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'certificateData'
  Types.CertificateData ->
  RegisterCertificate
mkRegisterCertificate directoryId certificateData =
  RegisterCertificate' {directoryId, certificateData}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDirectoryId :: Lens.Lens' RegisterCertificate Types.DirectoryId
rcDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED rcDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The certificate PEM string that needs to be registered.
--
-- /Note:/ Consider using 'certificateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateData :: Lens.Lens' RegisterCertificate Types.CertificateData
rcCertificateData = Lens.field @"certificateData"
{-# DEPRECATED rcCertificateData "Use generic-lens or generic-optics with 'certificateData' instead." #-}

instance Core.FromJSON RegisterCertificate where
  toJSON RegisterCertificate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("CertificateData" Core..= certificateData)
          ]
      )

instance Core.AWSRequest RegisterCertificate where
  type Rs RegisterCertificate = RegisterCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.RegisterCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterCertificateResponse'
            Core.<$> (x Core..:? "CertificateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterCertificateResponse' smart constructor.
data RegisterCertificateResponse = RegisterCertificateResponse'
  { -- | The identifier of the certificate.
    certificateId :: Core.Maybe Types.CertificateId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCertificateResponse' value with any optional fields omitted.
mkRegisterCertificateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterCertificateResponse
mkRegisterCertificateResponse responseStatus =
  RegisterCertificateResponse'
    { certificateId = Core.Nothing,
      responseStatus
    }

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsCertificateId :: Lens.Lens' RegisterCertificateResponse (Core.Maybe Types.CertificateId)
rcrrsCertificateId = Lens.field @"certificateId"
{-# DEPRECATED rcrrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsResponseStatus :: Lens.Lens' RegisterCertificateResponse Core.Int
rcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
