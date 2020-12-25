{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.ListCostCategoryDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name, ARN, @NumberOfRules@ and effective dates of all Cost Categories defined in the account. You have the option to use @EffectiveOn@ to return a list of Cost Categories that were active on a specific date. If there is no @EffectiveOn@ specified, you’ll see Cost Categories that are effective on the current date. If Cost Category is still effective, @EffectiveEnd@ is omitted in the response. @ListCostCategoryDefinitions@ supports pagination. The request can have a @MaxResults@ range up to 100.
module Network.AWS.CostExplorer.ListCostCategoryDefinitions
  ( -- * Creating a request
    ListCostCategoryDefinitions (..),
    mkListCostCategoryDefinitions,

    -- ** Request lenses
    lccdEffectiveOn,
    lccdMaxResults,
    lccdNextToken,

    -- * Destructuring the response
    ListCostCategoryDefinitionsResponse (..),
    mkListCostCategoryDefinitionsResponse,

    -- ** Response lenses
    lccdrrsCostCategoryReferences,
    lccdrrsNextToken,
    lccdrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCostCategoryDefinitions' smart constructor.
data ListCostCategoryDefinitions = ListCostCategoryDefinitions'
  { -- | The date when the Cost Category was effective.
    effectiveOn :: Core.Maybe Types.EffectiveOn,
    -- | The number of entries a paginated response contains.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
    nextToken :: Core.Maybe Types.NextPageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCostCategoryDefinitions' value with any optional fields omitted.
mkListCostCategoryDefinitions ::
  ListCostCategoryDefinitions
mkListCostCategoryDefinitions =
  ListCostCategoryDefinitions'
    { effectiveOn = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The date when the Cost Category was effective.
--
-- /Note:/ Consider using 'effectiveOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdEffectiveOn :: Lens.Lens' ListCostCategoryDefinitions (Core.Maybe Types.EffectiveOn)
lccdEffectiveOn = Lens.field @"effectiveOn"
{-# DEPRECATED lccdEffectiveOn "Use generic-lens or generic-optics with 'effectiveOn' instead." #-}

-- | The number of entries a paginated response contains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdMaxResults :: Lens.Lens' ListCostCategoryDefinitions (Core.Maybe Core.Natural)
lccdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lccdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdNextToken :: Lens.Lens' ListCostCategoryDefinitions (Core.Maybe Types.NextPageToken)
lccdNextToken = Lens.field @"nextToken"
{-# DEPRECATED lccdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListCostCategoryDefinitions where
  toJSON ListCostCategoryDefinitions {..} =
    Core.object
      ( Core.catMaybes
          [ ("EffectiveOn" Core..=) Core.<$> effectiveOn,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListCostCategoryDefinitions where
  type
    Rs ListCostCategoryDefinitions =
      ListCostCategoryDefinitionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSInsightsIndexService.ListCostCategoryDefinitions"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCostCategoryDefinitionsResponse'
            Core.<$> (x Core..:? "CostCategoryReferences")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListCostCategoryDefinitionsResponse' smart constructor.
data ListCostCategoryDefinitionsResponse = ListCostCategoryDefinitionsResponse'
  { -- | A reference to a Cost Category containing enough information to identify the Cost Category.
    costCategoryReferences :: Core.Maybe [Types.CostCategoryReference],
    -- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
    nextToken :: Core.Maybe Types.NextPageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCostCategoryDefinitionsResponse' value with any optional fields omitted.
mkListCostCategoryDefinitionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCostCategoryDefinitionsResponse
mkListCostCategoryDefinitionsResponse responseStatus =
  ListCostCategoryDefinitionsResponse'
    { costCategoryReferences =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A reference to a Cost Category containing enough information to identify the Cost Category.
--
-- /Note:/ Consider using 'costCategoryReferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdrrsCostCategoryReferences :: Lens.Lens' ListCostCategoryDefinitionsResponse (Core.Maybe [Types.CostCategoryReference])
lccdrrsCostCategoryReferences = Lens.field @"costCategoryReferences"
{-# DEPRECATED lccdrrsCostCategoryReferences "Use generic-lens or generic-optics with 'costCategoryReferences' instead." #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdrrsNextToken :: Lens.Lens' ListCostCategoryDefinitionsResponse (Core.Maybe Types.NextPageToken)
lccdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lccdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdrrsResponseStatus :: Lens.Lens' ListCostCategoryDefinitionsResponse Core.Int
lccdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lccdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
