{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListAppImageConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AppImageConfigs in your account and their properties. The list can be filtered by creation time or modified time, and whether the AppImageConfig name contains a specified string.
module Network.AWS.SageMaker.ListAppImageConfigs
  ( -- * Creating a request
    ListAppImageConfigs (..),
    mkListAppImageConfigs,

    -- ** Request lenses
    laicCreationTimeAfter,
    laicCreationTimeBefore,
    laicMaxResults,
    laicModifiedTimeAfter,
    laicModifiedTimeBefore,
    laicNameContains,
    laicNextToken,
    laicSortBy,
    laicSortOrder,

    -- * Destructuring the response
    ListAppImageConfigsResponse (..),
    mkListAppImageConfigsResponse,

    -- ** Response lenses
    laicrrsAppImageConfigs,
    laicrrsNextToken,
    laicrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListAppImageConfigs' smart constructor.
data ListAppImageConfigs = ListAppImageConfigs'
  { -- | A filter that returns only AppImageConfigs created on or after the specified time.
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only AppImageConfigs created on or before the specified time.
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of AppImageConfigs to return in the response. The default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only AppImageConfigs modified on or after the specified time.
    modifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only AppImageConfigs modified on or before the specified time.
    modifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only AppImageConfigs whose name contains the specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the previous call to @ListImages@ didn't return the full set of AppImageConfigs, the call returns a token for getting the next set of AppImageConfigs.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The property used to sort results. The default value is @CreationTime@ .
    sortBy :: Core.Maybe Types.AppImageConfigSortKey,
    -- | The sort order. The default value is @Descending@ .
    sortOrder :: Core.Maybe Types.SortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAppImageConfigs' value with any optional fields omitted.
mkListAppImageConfigs ::
  ListAppImageConfigs
mkListAppImageConfigs =
  ListAppImageConfigs'
    { creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      maxResults = Core.Nothing,
      modifiedTimeAfter = Core.Nothing,
      modifiedTimeBefore = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | A filter that returns only AppImageConfigs created on or after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicCreationTimeAfter :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.NominalDiffTime)
laicCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED laicCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only AppImageConfigs created on or before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicCreationTimeBefore :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.NominalDiffTime)
laicCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED laicCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of AppImageConfigs to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicMaxResults :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.Natural)
laicMaxResults = Lens.field @"maxResults"
{-# DEPRECATED laicMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A filter that returns only AppImageConfigs modified on or after the specified time.
--
-- /Note:/ Consider using 'modifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicModifiedTimeAfter :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.NominalDiffTime)
laicModifiedTimeAfter = Lens.field @"modifiedTimeAfter"
{-# DEPRECATED laicModifiedTimeAfter "Use generic-lens or generic-optics with 'modifiedTimeAfter' instead." #-}

-- | A filter that returns only AppImageConfigs modified on or before the specified time.
--
-- /Note:/ Consider using 'modifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicModifiedTimeBefore :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.NominalDiffTime)
laicModifiedTimeBefore = Lens.field @"modifiedTimeBefore"
{-# DEPRECATED laicModifiedTimeBefore "Use generic-lens or generic-optics with 'modifiedTimeBefore' instead." #-}

-- | A filter that returns only AppImageConfigs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicNameContains :: Lens.Lens' ListAppImageConfigs (Core.Maybe Types.NameContains)
laicNameContains = Lens.field @"nameContains"
{-# DEPRECATED laicNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the previous call to @ListImages@ didn't return the full set of AppImageConfigs, the call returns a token for getting the next set of AppImageConfigs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicNextToken :: Lens.Lens' ListAppImageConfigs (Core.Maybe Types.NextToken)
laicNextToken = Lens.field @"nextToken"
{-# DEPRECATED laicNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicSortBy :: Lens.Lens' ListAppImageConfigs (Core.Maybe Types.AppImageConfigSortKey)
laicSortBy = Lens.field @"sortBy"
{-# DEPRECATED laicSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicSortOrder :: Lens.Lens' ListAppImageConfigs (Core.Maybe Types.SortOrder)
laicSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED laicSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListAppImageConfigs where
  toJSON ListAppImageConfigs {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
            ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ModifiedTimeAfter" Core..=) Core.<$> modifiedTimeAfter,
            ("ModifiedTimeBefore" Core..=) Core.<$> modifiedTimeBefore,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListAppImageConfigs where
  type Rs ListAppImageConfigs = ListAppImageConfigsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListAppImageConfigs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppImageConfigsResponse'
            Core.<$> (x Core..:? "AppImageConfigs")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListAppImageConfigsResponse' smart constructor.
data ListAppImageConfigsResponse = ListAppImageConfigsResponse'
  { -- | A list of AppImageConfigs and their properties.
    appImageConfigs :: Core.Maybe [Types.AppImageConfigDetails],
    -- | A token for getting the next set of AppImageConfigs, if there are any.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAppImageConfigsResponse' value with any optional fields omitted.
mkListAppImageConfigsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAppImageConfigsResponse
mkListAppImageConfigsResponse responseStatus =
  ListAppImageConfigsResponse'
    { appImageConfigs = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of AppImageConfigs and their properties.
--
-- /Note:/ Consider using 'appImageConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicrrsAppImageConfigs :: Lens.Lens' ListAppImageConfigsResponse (Core.Maybe [Types.AppImageConfigDetails])
laicrrsAppImageConfigs = Lens.field @"appImageConfigs"
{-# DEPRECATED laicrrsAppImageConfigs "Use generic-lens or generic-optics with 'appImageConfigs' instead." #-}

-- | A token for getting the next set of AppImageConfigs, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicrrsNextToken :: Lens.Lens' ListAppImageConfigsResponse (Core.Maybe Types.NextToken)
laicrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED laicrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicrrsResponseStatus :: Lens.Lens' ListAppImageConfigsResponse Core.Int
laicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED laicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
