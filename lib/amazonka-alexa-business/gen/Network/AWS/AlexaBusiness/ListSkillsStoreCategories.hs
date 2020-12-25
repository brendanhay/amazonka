{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListSkillsStoreCategories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all categories in the Alexa skill store.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkillsStoreCategories
  ( -- * Creating a request
    ListSkillsStoreCategories (..),
    mkListSkillsStoreCategories,

    -- ** Request lenses
    lsscMaxResults,
    lsscNextToken,

    -- * Destructuring the response
    ListSkillsStoreCategoriesResponse (..),
    mkListSkillsStoreCategoriesResponse,

    -- ** Response lenses
    lsscrrsCategoryList,
    lsscrrsNextToken,
    lsscrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSkillsStoreCategories' smart constructor.
data ListSkillsStoreCategories = ListSkillsStoreCategories'
  { -- | The maximum number of categories returned, per paginated calls.
    maxResults :: Core.Maybe Core.Natural,
    -- | The tokens used for pagination.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSkillsStoreCategories' value with any optional fields omitted.
mkListSkillsStoreCategories ::
  ListSkillsStoreCategories
mkListSkillsStoreCategories =
  ListSkillsStoreCategories'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of categories returned, per paginated calls.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscMaxResults :: Lens.Lens' ListSkillsStoreCategories (Core.Maybe Core.Natural)
lsscMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscNextToken :: Lens.Lens' ListSkillsStoreCategories (Core.Maybe Types.NextToken)
lsscNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListSkillsStoreCategories where
  toJSON ListSkillsStoreCategories {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListSkillsStoreCategories where
  type
    Rs ListSkillsStoreCategories =
      ListSkillsStoreCategoriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.ListSkillsStoreCategories")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSkillsStoreCategoriesResponse'
            Core.<$> (x Core..:? "CategoryList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSkillsStoreCategories where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"categoryList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSkillsStoreCategoriesResponse' smart constructor.
data ListSkillsStoreCategoriesResponse = ListSkillsStoreCategoriesResponse'
  { -- | The list of categories.
    categoryList :: Core.Maybe [Types.Category],
    -- | The tokens used for pagination.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSkillsStoreCategoriesResponse' value with any optional fields omitted.
mkListSkillsStoreCategoriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSkillsStoreCategoriesResponse
mkListSkillsStoreCategoriesResponse responseStatus =
  ListSkillsStoreCategoriesResponse'
    { categoryList = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of categories.
--
-- /Note:/ Consider using 'categoryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrrsCategoryList :: Lens.Lens' ListSkillsStoreCategoriesResponse (Core.Maybe [Types.Category])
lsscrrsCategoryList = Lens.field @"categoryList"
{-# DEPRECATED lsscrrsCategoryList "Use generic-lens or generic-optics with 'categoryList' instead." #-}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrrsNextToken :: Lens.Lens' ListSkillsStoreCategoriesResponse (Core.Maybe Types.NextToken)
lsscrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsscrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrrsResponseStatus :: Lens.Lens' ListSkillsStoreCategoriesResponse Core.Int
lsscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
