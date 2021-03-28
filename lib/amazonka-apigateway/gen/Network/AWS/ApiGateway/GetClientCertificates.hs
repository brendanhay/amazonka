{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetClientCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a collection of 'ClientCertificate' resources.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetClientCertificates
    (
    -- * Creating a request
      GetClientCertificates (..)
    , mkGetClientCertificates
    -- ** Request lenses
    , gccLimit
    , gccPosition

    -- * Destructuring the response
    , GetClientCertificatesResponse (..)
    , mkGetClientCertificatesResponse
    -- ** Response lenses
    , gccrrsItems
    , gccrrsPosition
    , gccrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about a collection of 'ClientCertificate' resources.
--
-- /See:/ 'mkGetClientCertificates' smart constructor.
data GetClientCertificates = GetClientCertificates'
  { limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetClientCertificates' value with any optional fields omitted.
mkGetClientCertificates
    :: GetClientCertificates
mkGetClientCertificates
  = GetClientCertificates'{limit = Core.Nothing,
                           position = Core.Nothing}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccLimit :: Lens.Lens' GetClientCertificates (Core.Maybe Core.Int)
gccLimit = Lens.field @"limit"
{-# INLINEABLE gccLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccPosition :: Lens.Lens' GetClientCertificates (Core.Maybe Core.Text)
gccPosition = Lens.field @"position"
{-# INLINEABLE gccPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetClientCertificates where
        toQuery GetClientCertificates{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetClientCertificates where
        toHeaders GetClientCertificates{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetClientCertificates where
        type Rs GetClientCertificates = GetClientCertificatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/clientcertificates",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetClientCertificatesResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetClientCertificates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Represents a collection of 'ClientCertificate' resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html Use Client-Side Certificate> 
--
-- /See:/ 'mkGetClientCertificatesResponse' smart constructor.
data GetClientCertificatesResponse = GetClientCertificatesResponse'
  { items :: Core.Maybe [Types.ClientCertificate]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetClientCertificatesResponse' value with any optional fields omitted.
mkGetClientCertificatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetClientCertificatesResponse
mkGetClientCertificatesResponse responseStatus
  = GetClientCertificatesResponse'{items = Core.Nothing,
                                   position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrrsItems :: Lens.Lens' GetClientCertificatesResponse (Core.Maybe [Types.ClientCertificate])
gccrrsItems = Lens.field @"items"
{-# INLINEABLE gccrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrrsPosition :: Lens.Lens' GetClientCertificatesResponse (Core.Maybe Core.Text)
gccrrsPosition = Lens.field @"position"
{-# INLINEABLE gccrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrrsResponseStatus :: Lens.Lens' GetClientCertificatesResponse Core.Int
gccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
