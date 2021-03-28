{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetSdkTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetSdkTypes
    (
    -- * Creating a request
      GetSdkTypes (..)
    , mkGetSdkTypes
    -- ** Request lenses
    , gstLimit
    , gstPosition

    -- * Destructuring the response
    , GetSdkTypesResponse (..)
    , mkGetSdkTypesResponse
    -- ** Response lenses
    , gstrrsItems
    , gstrrsPosition
    , gstrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Get the 'SdkTypes' collection.
--
-- /See:/ 'mkGetSdkTypes' smart constructor.
data GetSdkTypes = GetSdkTypes'
  { limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSdkTypes' value with any optional fields omitted.
mkGetSdkTypes
    :: GetSdkTypes
mkGetSdkTypes
  = GetSdkTypes'{limit = Core.Nothing, position = Core.Nothing}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstLimit :: Lens.Lens' GetSdkTypes (Core.Maybe Core.Int)
gstLimit = Lens.field @"limit"
{-# INLINEABLE gstLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstPosition :: Lens.Lens' GetSdkTypes (Core.Maybe Core.Text)
gstPosition = Lens.field @"position"
{-# INLINEABLE gstPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetSdkTypes where
        toQuery GetSdkTypes{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetSdkTypes where
        toHeaders GetSdkTypes{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetSdkTypes where
        type Rs GetSdkTypes = GetSdkTypesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/sdktypes",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSdkTypesResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetSdkTypes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | The collection of 'SdkType' instances.
--
-- /See:/ 'mkGetSdkTypesResponse' smart constructor.
data GetSdkTypesResponse = GetSdkTypesResponse'
  { items :: Core.Maybe [Types.SdkType]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSdkTypesResponse' value with any optional fields omitted.
mkGetSdkTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSdkTypesResponse
mkGetSdkTypesResponse responseStatus
  = GetSdkTypesResponse'{items = Core.Nothing,
                         position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsItems :: Lens.Lens' GetSdkTypesResponse (Core.Maybe [Types.SdkType])
gstrrsItems = Lens.field @"items"
{-# INLINEABLE gstrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsPosition :: Lens.Lens' GetSdkTypesResponse (Core.Maybe Core.Text)
gstrrsPosition = Lens.field @"position"
{-# INLINEABLE gstrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsResponseStatus :: Lens.Lens' GetSdkTypesResponse Core.Int
gstrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gstrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
