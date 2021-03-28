{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all skills in the Alexa skill store by category.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
    (
    -- * Creating a request
      ListSkillsStoreSkillsByCategory (..)
    , mkListSkillsStoreSkillsByCategory
    -- ** Request lenses
    , lsssbcCategoryId
    , lsssbcMaxResults
    , lsssbcNextToken

    -- * Destructuring the response
    , ListSkillsStoreSkillsByCategoryResponse (..)
    , mkListSkillsStoreSkillsByCategoryResponse
    -- ** Response lenses
    , lsssbcrrsNextToken
    , lsssbcrrsSkillsStoreSkills
    , lsssbcrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSkillsStoreSkillsByCategory' smart constructor.
data ListSkillsStoreSkillsByCategory = ListSkillsStoreSkillsByCategory'
  { categoryId :: Core.Natural
    -- ^ The category ID for which the skills are being retrieved from the skill store.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of skills returned per paginated calls.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The tokens used for pagination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSkillsStoreSkillsByCategory' value with any optional fields omitted.
mkListSkillsStoreSkillsByCategory
    :: Core.Natural -- ^ 'categoryId'
    -> ListSkillsStoreSkillsByCategory
mkListSkillsStoreSkillsByCategory categoryId
  = ListSkillsStoreSkillsByCategory'{categoryId,
                                     maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The category ID for which the skills are being retrieved from the skill store.
--
-- /Note:/ Consider using 'categoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcCategoryId :: Lens.Lens' ListSkillsStoreSkillsByCategory Core.Natural
lsssbcCategoryId = Lens.field @"categoryId"
{-# INLINEABLE lsssbcCategoryId #-}
{-# DEPRECATED categoryId "Use generic-lens or generic-optics with 'categoryId' instead"  #-}

-- | The maximum number of skills returned per paginated calls.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcMaxResults :: Lens.Lens' ListSkillsStoreSkillsByCategory (Core.Maybe Core.Natural)
lsssbcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsssbcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcNextToken :: Lens.Lens' ListSkillsStoreSkillsByCategory (Core.Maybe Types.NextToken)
lsssbcNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsssbcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSkillsStoreSkillsByCategory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSkillsStoreSkillsByCategory where
        toHeaders ListSkillsStoreSkillsByCategory{..}
          = Core.pure
              ("X-Amz-Target",
               "AlexaForBusiness.ListSkillsStoreSkillsByCategory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSkillsStoreSkillsByCategory where
        toJSON ListSkillsStoreSkillsByCategory{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CategoryId" Core..= categoryId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListSkillsStoreSkillsByCategory where
        type Rs ListSkillsStoreSkillsByCategory =
             ListSkillsStoreSkillsByCategoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSkillsStoreSkillsByCategoryResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SkillsStoreSkills"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSkillsStoreSkillsByCategory where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"skillsStoreSkills" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSkillsStoreSkillsByCategoryResponse' smart constructor.
data ListSkillsStoreSkillsByCategoryResponse = ListSkillsStoreSkillsByCategoryResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The tokens used for pagination.
  , skillsStoreSkills :: Core.Maybe [Types.SkillsStoreSkill]
    -- ^ The skill store skills.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSkillsStoreSkillsByCategoryResponse' value with any optional fields omitted.
mkListSkillsStoreSkillsByCategoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSkillsStoreSkillsByCategoryResponse
mkListSkillsStoreSkillsByCategoryResponse responseStatus
  = ListSkillsStoreSkillsByCategoryResponse'{nextToken =
                                               Core.Nothing,
                                             skillsStoreSkills = Core.Nothing, responseStatus}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcrrsNextToken :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse (Core.Maybe Types.NextToken)
lsssbcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsssbcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The skill store skills.
--
-- /Note:/ Consider using 'skillsStoreSkills' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcrrsSkillsStoreSkills :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse (Core.Maybe [Types.SkillsStoreSkill])
lsssbcrrsSkillsStoreSkills = Lens.field @"skillsStoreSkills"
{-# INLINEABLE lsssbcrrsSkillsStoreSkills #-}
{-# DEPRECATED skillsStoreSkills "Use generic-lens or generic-optics with 'skillsStoreSkills' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcrrsResponseStatus :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse Core.Int
lsssbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsssbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
