{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified server certificate stored in IAM.
--
-- For more information about working with server certificates, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic includes a list of AWS services that can use the server certificates that you manage with IAM.
module Network.AWS.IAM.GetServerCertificate
  ( -- * Creating a request
    GetServerCertificate (..),
    mkGetServerCertificate,

    -- ** Request lenses
    gscServerCertificateName,

    -- * Destructuring the response
    GetServerCertificateResponse (..),
    mkGetServerCertificateResponse,

    -- ** Response lenses
    gscrrsServerCertificate,
    gscrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetServerCertificate' smart constructor.
newtype GetServerCertificate = GetServerCertificate'
  { -- | The name of the server certificate you want to retrieve information about.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    serverCertificateName :: Types.ServerCertificateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetServerCertificate' value with any optional fields omitted.
mkGetServerCertificate ::
  -- | 'serverCertificateName'
  Types.ServerCertificateName ->
  GetServerCertificate
mkGetServerCertificate serverCertificateName =
  GetServerCertificate' {serverCertificateName}

-- | The name of the server certificate you want to retrieve information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscServerCertificateName :: Lens.Lens' GetServerCertificate Types.ServerCertificateName
gscServerCertificateName = Lens.field @"serverCertificateName"
{-# DEPRECATED gscServerCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead." #-}

instance Core.AWSRequest GetServerCertificate where
  type Rs GetServerCertificate = GetServerCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetServerCertificate")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "ServerCertificateName" serverCertificateName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetServerCertificateResult"
      ( \s h x ->
          GetServerCertificateResponse'
            Core.<$> (x Core..@ "ServerCertificate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetServerCertificate' request.
--
-- /See:/ 'mkGetServerCertificateResponse' smart constructor.
data GetServerCertificateResponse = GetServerCertificateResponse'
  { -- | A structure containing details about the server certificate.
    serverCertificate :: Types.ServerCertificate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetServerCertificateResponse' value with any optional fields omitted.
mkGetServerCertificateResponse ::
  -- | 'serverCertificate'
  Types.ServerCertificate ->
  -- | 'responseStatus'
  Core.Int ->
  GetServerCertificateResponse
mkGetServerCertificateResponse serverCertificate responseStatus =
  GetServerCertificateResponse' {serverCertificate, responseStatus}

-- | A structure containing details about the server certificate.
--
-- /Note:/ Consider using 'serverCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsServerCertificate :: Lens.Lens' GetServerCertificateResponse Types.ServerCertificate
gscrrsServerCertificate = Lens.field @"serverCertificate"
{-# DEPRECATED gscrrsServerCertificate "Use generic-lens or generic-optics with 'serverCertificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsResponseStatus :: Lens.Lens' GetServerCertificateResponse Core.Int
gscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
