{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetNamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single query. Requires that you have access to the workgroup in which the query was saved.
module Network.AWS.Athena.GetNamedQuery
    (
    -- * Creating a request
      GetNamedQuery (..)
    , mkGetNamedQuery
    -- ** Request lenses
    , gnqNamedQueryId

    -- * Destructuring the response
    , GetNamedQueryResponse (..)
    , mkGetNamedQueryResponse
    -- ** Response lenses
    , gnqrrsNamedQuery
    , gnqrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetNamedQuery' smart constructor.
newtype GetNamedQuery = GetNamedQuery'
  { namedQueryId :: Types.NamedQueryId
    -- ^ The unique ID of the query. Use 'ListNamedQueries' to get query IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetNamedQuery' value with any optional fields omitted.
mkGetNamedQuery
    :: Types.NamedQueryId -- ^ 'namedQueryId'
    -> GetNamedQuery
mkGetNamedQuery namedQueryId = GetNamedQuery'{namedQueryId}

-- | The unique ID of the query. Use 'ListNamedQueries' to get query IDs.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnqNamedQueryId :: Lens.Lens' GetNamedQuery Types.NamedQueryId
gnqNamedQueryId = Lens.field @"namedQueryId"
{-# INLINEABLE gnqNamedQueryId #-}
{-# DEPRECATED namedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead"  #-}

instance Core.ToQuery GetNamedQuery where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetNamedQuery where
        toHeaders GetNamedQuery{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.GetNamedQuery") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetNamedQuery where
        toJSON GetNamedQuery{..}
          = Core.object
              (Core.catMaybes [Core.Just ("NamedQueryId" Core..= namedQueryId)])

instance Core.AWSRequest GetNamedQuery where
        type Rs GetNamedQuery = GetNamedQueryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetNamedQueryResponse' Core.<$>
                   (x Core..:? "NamedQuery") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetNamedQueryResponse' smart constructor.
data GetNamedQueryResponse = GetNamedQueryResponse'
  { namedQuery :: Core.Maybe Types.NamedQuery
    -- ^ Information about the query.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetNamedQueryResponse' value with any optional fields omitted.
mkGetNamedQueryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetNamedQueryResponse
mkGetNamedQueryResponse responseStatus
  = GetNamedQueryResponse'{namedQuery = Core.Nothing, responseStatus}

-- | Information about the query.
--
-- /Note:/ Consider using 'namedQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnqrrsNamedQuery :: Lens.Lens' GetNamedQueryResponse (Core.Maybe Types.NamedQuery)
gnqrrsNamedQuery = Lens.field @"namedQuery"
{-# INLINEABLE gnqrrsNamedQuery #-}
{-# DEPRECATED namedQuery "Use generic-lens or generic-optics with 'namedQuery' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnqrrsResponseStatus :: Lens.Lens' GetNamedQueryResponse Core.Int
gnqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gnqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
