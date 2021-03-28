{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListCreatedArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the created artifacts attached to a given migration task in an update stream. This API has the following traits:
--
--
--     * Gets the list of the created artifacts while migration is taking place.
--
--
--     * Shows the artifacts created by the migration tool that was associated by the @AssociateCreatedArtifact@ API. 
--
--
--     * Lists created artifacts in a paginated interface. 
--
--
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListCreatedArtifacts
    (
    -- * Creating a request
      ListCreatedArtifacts (..)
    , mkListCreatedArtifacts
    -- ** Request lenses
    , lcaProgressUpdateStream
    , lcaMigrationTaskName
    , lcaMaxResults
    , lcaNextToken

    -- * Destructuring the response
    , ListCreatedArtifactsResponse (..)
    , mkListCreatedArtifactsResponse
    -- ** Response lenses
    , lcarrsCreatedArtifactList
    , lcarrsNextToken
    , lcarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCreatedArtifacts' smart constructor.
data ListCreatedArtifacts = ListCreatedArtifacts'
  { progressUpdateStream :: Types.ProgressUpdateStream
    -- ^ The name of the ProgressUpdateStream. 
  , migrationTaskName :: Types.MigrationTaskName
    -- ^ Unique identifier that references the migration task. /Do not store personal data in this field./ 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of results to be returned per page.
  , nextToken :: Core.Maybe Types.Token
    -- ^ If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCreatedArtifacts' value with any optional fields omitted.
mkListCreatedArtifacts
    :: Types.ProgressUpdateStream -- ^ 'progressUpdateStream'
    -> Types.MigrationTaskName -- ^ 'migrationTaskName'
    -> ListCreatedArtifacts
mkListCreatedArtifacts progressUpdateStream migrationTaskName
  = ListCreatedArtifacts'{progressUpdateStream, migrationTaskName,
                          maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the ProgressUpdateStream. 
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaProgressUpdateStream :: Lens.Lens' ListCreatedArtifacts Types.ProgressUpdateStream
lcaProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# INLINEABLE lcaProgressUpdateStream #-}
{-# DEPRECATED progressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead"  #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./ 
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaMigrationTaskName :: Lens.Lens' ListCreatedArtifacts Types.MigrationTaskName
lcaMigrationTaskName = Lens.field @"migrationTaskName"
{-# INLINEABLE lcaMigrationTaskName #-}
{-# DEPRECATED migrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead"  #-}

-- | Maximum number of results to be returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaMaxResults :: Lens.Lens' ListCreatedArtifacts (Core.Maybe Core.Natural)
lcaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaNextToken :: Lens.Lens' ListCreatedArtifacts (Core.Maybe Types.Token)
lcaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListCreatedArtifacts where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListCreatedArtifacts where
        toHeaders ListCreatedArtifacts{..}
          = Core.pure
              ("X-Amz-Target", "AWSMigrationHub.ListCreatedArtifacts")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListCreatedArtifacts where
        toJSON ListCreatedArtifacts{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
                  Core.Just ("MigrationTaskName" Core..= migrationTaskName),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListCreatedArtifacts where
        type Rs ListCreatedArtifacts = ListCreatedArtifactsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListCreatedArtifactsResponse' Core.<$>
                   (x Core..:? "CreatedArtifactList") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListCreatedArtifacts where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"createdArtifactList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListCreatedArtifactsResponse' smart constructor.
data ListCreatedArtifactsResponse = ListCreatedArtifactsResponse'
  { createdArtifactList :: Core.Maybe [Types.CreatedArtifact]
    -- ^ List of created artifacts up to the maximum number of results specified in the request.
  , nextToken :: Core.Maybe Types.Token
    -- ^ If there are more created artifacts than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCreatedArtifactsResponse' value with any optional fields omitted.
mkListCreatedArtifactsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCreatedArtifactsResponse
mkListCreatedArtifactsResponse responseStatus
  = ListCreatedArtifactsResponse'{createdArtifactList = Core.Nothing,
                                  nextToken = Core.Nothing, responseStatus}

-- | List of created artifacts up to the maximum number of results specified in the request.
--
-- /Note:/ Consider using 'createdArtifactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarrsCreatedArtifactList :: Lens.Lens' ListCreatedArtifactsResponse (Core.Maybe [Types.CreatedArtifact])
lcarrsCreatedArtifactList = Lens.field @"createdArtifactList"
{-# INLINEABLE lcarrsCreatedArtifactList #-}
{-# DEPRECATED createdArtifactList "Use generic-lens or generic-optics with 'createdArtifactList' instead"  #-}

-- | If there are more created artifacts than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarrsNextToken :: Lens.Lens' ListCreatedArtifactsResponse (Core.Maybe Types.Token)
lcarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarrsResponseStatus :: Lens.Lens' ListCreatedArtifactsResponse Core.Int
lcarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
