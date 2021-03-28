{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListBuilds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build IDs, with each build ID representing a single build.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuilds
    (
    -- * Creating a request
      ListBuilds (..)
    , mkListBuilds
    -- ** Request lenses
    , lbNextToken
    , lbSortOrder

    -- * Destructuring the response
    , ListBuildsResponse (..)
    , mkListBuildsResponse
    -- ** Response lenses
    , lbrrsIds
    , lbrrsNextToken
    , lbrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBuilds' smart constructor.
data ListBuilds = ListBuilds'
  { nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
  , sortOrder :: Core.Maybe Types.SortOrderType
    -- ^ The order to list build IDs. Valid values include:
--
--
--     * @ASCENDING@ : List the build IDs in ascending order by build ID.
--
--
--     * @DESCENDING@ : List the build IDs in descending order by build ID.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBuilds' value with any optional fields omitted.
mkListBuilds
    :: ListBuilds
mkListBuilds
  = ListBuilds'{nextToken = Core.Nothing, sortOrder = Core.Nothing}

-- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbNextToken :: Lens.Lens' ListBuilds (Core.Maybe Core.Text)
lbNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The order to list build IDs. Valid values include:
--
--
--     * @ASCENDING@ : List the build IDs in ascending order by build ID.
--
--
--     * @DESCENDING@ : List the build IDs in descending order by build ID.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbSortOrder :: Lens.Lens' ListBuilds (Core.Maybe Types.SortOrderType)
lbSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lbSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListBuilds where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListBuilds where
        toHeaders ListBuilds{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.ListBuilds")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListBuilds where
        toJSON ListBuilds{..}
          = Core.object
              (Core.catMaybes
                 [("nextToken" Core..=) Core.<$> nextToken,
                  ("sortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListBuilds where
        type Rs ListBuilds = ListBuildsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBuildsResponse' Core.<$>
                   (x Core..:? "ids") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListBuilds where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"ids" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
  { ids :: Core.Maybe (Core.NonEmpty Types.NonEmptyString)
    -- ^ A list of build IDs, with each build ID representing a single build.
  , nextToken :: Core.Maybe Core.Text
    -- ^ If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBuildsResponse' value with any optional fields omitted.
mkListBuildsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBuildsResponse
mkListBuildsResponse responseStatus
  = ListBuildsResponse'{ids = Core.Nothing, nextToken = Core.Nothing,
                        responseStatus}

-- | A list of build IDs, with each build ID representing a single build.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsIds :: Lens.Lens' ListBuildsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
lbrrsIds = Lens.field @"ids"
{-# INLINEABLE lbrrsIds #-}
{-# DEPRECATED ids "Use generic-lens or generic-optics with 'ids' instead"  #-}

-- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsNextToken :: Lens.Lens' ListBuildsResponse (Core.Maybe Core.Text)
lbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsResponseStatus :: Lens.Lens' ListBuildsResponse Core.Int
lbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
