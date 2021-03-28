{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'ClientCertificate' resource.
module Network.AWS.ApiGateway.DeleteClientCertificate
    (
    -- * Creating a request
      DeleteClientCertificate (..)
    , mkDeleteClientCertificate
    -- ** Request lenses
    , dccClientCertificateId

    -- * Destructuring the response
    , DeleteClientCertificateResponse (..)
    , mkDeleteClientCertificateResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete the 'ClientCertificate' resource.
--
-- /See:/ 'mkDeleteClientCertificate' smart constructor.
newtype DeleteClientCertificate = DeleteClientCertificate'
  { clientCertificateId :: Core.Text
    -- ^ [Required] The identifier of the 'ClientCertificate' resource to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClientCertificate' value with any optional fields omitted.
mkDeleteClientCertificate
    :: Core.Text -- ^ 'clientCertificateId'
    -> DeleteClientCertificate
mkDeleteClientCertificate clientCertificateId
  = DeleteClientCertificate'{clientCertificateId}

-- | [Required] The identifier of the 'ClientCertificate' resource to be deleted.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccClientCertificateId :: Lens.Lens' DeleteClientCertificate Core.Text
dccClientCertificateId = Lens.field @"clientCertificateId"
{-# INLINEABLE dccClientCertificateId #-}
{-# DEPRECATED clientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead"  #-}

instance Core.ToQuery DeleteClientCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteClientCertificate where
        toHeaders DeleteClientCertificate{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteClientCertificate where
        type Rs DeleteClientCertificate = DeleteClientCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/clientcertificates/" Core.<> Core.toText clientCertificateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteClientCertificateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClientCertificateResponse' smart constructor.
data DeleteClientCertificateResponse = DeleteClientCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClientCertificateResponse' value with any optional fields omitted.
mkDeleteClientCertificateResponse
    :: DeleteClientCertificateResponse
mkDeleteClientCertificateResponse
  = DeleteClientCertificateResponse'
