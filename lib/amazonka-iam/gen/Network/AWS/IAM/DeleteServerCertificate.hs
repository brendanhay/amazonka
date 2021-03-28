{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified server certificate.
--
-- For more information about working with server certificates, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic also includes a list of AWS services that can use the server certificates that you manage with IAM.
-- /Important:/ If you are using a server certificate with Elastic Load Balancing, deleting the certificate could have implications for your application. If Elastic Load Balancing doesn't detect the deletion of bound certificates, it may continue to use the certificates. This could cause Elastic Load Balancing to stop accepting traffic. We recommend that you remove the reference to the certificate from Elastic Load Balancing before using this command to delete the certificate. For more information, go to <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html DeleteLoadBalancerListeners> in the /Elastic Load Balancing API Reference/ .
module Network.AWS.IAM.DeleteServerCertificate
    (
    -- * Creating a request
      DeleteServerCertificate (..)
    , mkDeleteServerCertificate
    -- ** Request lenses
    , dscServerCertificateName

    -- * Destructuring the response
    , DeleteServerCertificateResponse (..)
    , mkDeleteServerCertificateResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteServerCertificate' smart constructor.
newtype DeleteServerCertificate = DeleteServerCertificate'
  { serverCertificateName :: Types.ServerCertificateNameType
    -- ^ The name of the server certificate you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServerCertificate' value with any optional fields omitted.
mkDeleteServerCertificate
    :: Types.ServerCertificateNameType -- ^ 'serverCertificateName'
    -> DeleteServerCertificate
mkDeleteServerCertificate serverCertificateName
  = DeleteServerCertificate'{serverCertificateName}

-- | The name of the server certificate you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscServerCertificateName :: Lens.Lens' DeleteServerCertificate Types.ServerCertificateNameType
dscServerCertificateName = Lens.field @"serverCertificateName"
{-# INLINEABLE dscServerCertificateName #-}
{-# DEPRECATED serverCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead"  #-}

instance Core.ToQuery DeleteServerCertificate where
        toQuery DeleteServerCertificate{..}
          = Core.toQueryPair "Action"
              ("DeleteServerCertificate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "ServerCertificateName" serverCertificateName

instance Core.ToHeaders DeleteServerCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteServerCertificate where
        type Rs DeleteServerCertificate = DeleteServerCertificateResponse
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
          = Response.receiveNull DeleteServerCertificateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteServerCertificateResponse' smart constructor.
data DeleteServerCertificateResponse = DeleteServerCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServerCertificateResponse' value with any optional fields omitted.
mkDeleteServerCertificateResponse
    :: DeleteServerCertificateResponse
mkDeleteServerCertificateResponse
  = DeleteServerCertificateResponse'
