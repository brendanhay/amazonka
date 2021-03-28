{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetServerCertificate (..)
    , mkGetServerCertificate
    -- ** Request lenses
    , gscServerCertificateName

    -- * Destructuring the response
    , GetServerCertificateResponse (..)
    , mkGetServerCertificateResponse
    -- ** Response lenses
    , gscrrsServerCertificate
    , gscrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetServerCertificate' smart constructor.
newtype GetServerCertificate = GetServerCertificate'
  { serverCertificateName :: Types.ServerCertificateName
    -- ^ The name of the server certificate you want to retrieve information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetServerCertificate' value with any optional fields omitted.
mkGetServerCertificate
    :: Types.ServerCertificateName -- ^ 'serverCertificateName'
    -> GetServerCertificate
mkGetServerCertificate serverCertificateName
  = GetServerCertificate'{serverCertificateName}

-- | The name of the server certificate you want to retrieve information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscServerCertificateName :: Lens.Lens' GetServerCertificate Types.ServerCertificateName
gscServerCertificateName = Lens.field @"serverCertificateName"
{-# INLINEABLE gscServerCertificateName #-}
{-# DEPRECATED serverCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead"  #-}

instance Core.ToQuery GetServerCertificate where
        toQuery GetServerCertificate{..}
          = Core.toQueryPair "Action" ("GetServerCertificate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "ServerCertificateName" serverCertificateName

instance Core.ToHeaders GetServerCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetServerCertificate where
        type Rs GetServerCertificate = GetServerCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetServerCertificateResult"
              (\ s h x ->
                 GetServerCertificateResponse' Core.<$>
                   (x Core..@ "ServerCertificate") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetServerCertificate' request. 
--
-- /See:/ 'mkGetServerCertificateResponse' smart constructor.
data GetServerCertificateResponse = GetServerCertificateResponse'
  { serverCertificate :: Types.ServerCertificate
    -- ^ A structure containing details about the server certificate.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetServerCertificateResponse' value with any optional fields omitted.
mkGetServerCertificateResponse
    :: Types.ServerCertificate -- ^ 'serverCertificate'
    -> Core.Int -- ^ 'responseStatus'
    -> GetServerCertificateResponse
mkGetServerCertificateResponse serverCertificate responseStatus
  = GetServerCertificateResponse'{serverCertificate, responseStatus}

-- | A structure containing details about the server certificate.
--
-- /Note:/ Consider using 'serverCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsServerCertificate :: Lens.Lens' GetServerCertificateResponse Types.ServerCertificate
gscrrsServerCertificate = Lens.field @"serverCertificate"
{-# INLINEABLE gscrrsServerCertificate #-}
{-# DEPRECATED serverCertificate "Use generic-lens or generic-optics with 'serverCertificate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsResponseStatus :: Lens.Lens' GetServerCertificateResponse Core.Int
gscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
