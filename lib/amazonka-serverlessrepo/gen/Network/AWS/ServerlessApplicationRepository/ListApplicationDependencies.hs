{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of applications nested in the containing application.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies
    (
    -- * Creating a request
      ListApplicationDependencies (..)
    , mkListApplicationDependencies
    -- ** Request lenses
    , ladApplicationId
    , ladMaxItems
    , ladNextToken
    , ladSemanticVersion

    -- * Destructuring the response
    , ListApplicationDependenciesResponse (..)
    , mkListApplicationDependenciesResponse
    -- ** Response lenses
    , ladrrsDependencies
    , ladrrsNextToken
    , ladrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkListApplicationDependencies' smart constructor.
data ListApplicationDependencies = ListApplicationDependencies'
  { applicationId :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the application.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ The total number of items to return.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token to specify where to start paginating.
  , semanticVersion :: Core.Maybe Core.Text
    -- ^ The semantic version of the application to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApplicationDependencies' value with any optional fields omitted.
mkListApplicationDependencies
    :: Core.Text -- ^ 'applicationId'
    -> ListApplicationDependencies
mkListApplicationDependencies applicationId
  = ListApplicationDependencies'{applicationId,
                                 maxItems = Core.Nothing, nextToken = Core.Nothing,
                                 semanticVersion = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladApplicationId :: Lens.Lens' ListApplicationDependencies Core.Text
ladApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ladApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The total number of items to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladMaxItems :: Lens.Lens' ListApplicationDependencies (Core.Maybe Core.Natural)
ladMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ladMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | A token to specify where to start paginating.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladNextToken :: Lens.Lens' ListApplicationDependencies (Core.Maybe Core.Text)
ladNextToken = Lens.field @"nextToken"
{-# INLINEABLE ladNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The semantic version of the application to get.
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladSemanticVersion :: Lens.Lens' ListApplicationDependencies (Core.Maybe Core.Text)
ladSemanticVersion = Lens.field @"semanticVersion"
{-# INLINEABLE ladSemanticVersion #-}
{-# DEPRECATED semanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead"  #-}

instance Core.ToQuery ListApplicationDependencies where
        toQuery ListApplicationDependencies{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxItems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "semanticVersion")
                semanticVersion

instance Core.ToHeaders ListApplicationDependencies where
        toHeaders ListApplicationDependencies{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListApplicationDependencies where
        type Rs ListApplicationDependencies =
             ListApplicationDependenciesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/applications/" Core.<> Core.toText applicationId Core.<>
                             "/dependencies",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListApplicationDependenciesResponse' Core.<$>
                   (x Core..:? "dependencies") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListApplicationDependencies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dependencies" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListApplicationDependenciesResponse' smart constructor.
data ListApplicationDependenciesResponse = ListApplicationDependenciesResponse'
  { dependencies :: Core.Maybe [Types.ApplicationDependencySummary]
    -- ^ An array of application summaries nested in the application.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to request the next page of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApplicationDependenciesResponse' value with any optional fields omitted.
mkListApplicationDependenciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListApplicationDependenciesResponse
mkListApplicationDependenciesResponse responseStatus
  = ListApplicationDependenciesResponse'{dependencies = Core.Nothing,
                                         nextToken = Core.Nothing, responseStatus}

-- | An array of application summaries nested in the application.
--
-- /Note:/ Consider using 'dependencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrrsDependencies :: Lens.Lens' ListApplicationDependenciesResponse (Core.Maybe [Types.ApplicationDependencySummary])
ladrrsDependencies = Lens.field @"dependencies"
{-# INLINEABLE ladrrsDependencies #-}
{-# DEPRECATED dependencies "Use generic-lens or generic-optics with 'dependencies' instead"  #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrrsNextToken :: Lens.Lens' ListApplicationDependenciesResponse (Core.Maybe Core.Text)
ladrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ladrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrrsResponseStatus :: Lens.Lens' ListApplicationDependenciesResponse Core.Int
ladrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ladrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
