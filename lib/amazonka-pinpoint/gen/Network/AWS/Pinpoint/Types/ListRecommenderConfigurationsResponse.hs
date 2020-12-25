{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ListRecommenderConfigurationsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ListRecommenderConfigurationsResponse
  ( ListRecommenderConfigurationsResponse (..),

    -- * Smart constructor
    mkListRecommenderConfigurationsResponse,

    -- * Lenses
    lrcrItem,
    lrcrNextToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about all the recommender model configurations that are associated with your Amazon Pinpoint account.
--
-- /See:/ 'mkListRecommenderConfigurationsResponse' smart constructor.
data ListRecommenderConfigurationsResponse = ListRecommenderConfigurationsResponse'
  { -- | An array of responses, one for each recommender model configuration that's associated with your Amazon Pinpoint account.
    item :: [Types.RecommenderConfigurationResponse],
    -- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRecommenderConfigurationsResponse' value with any optional fields omitted.
mkListRecommenderConfigurationsResponse ::
  ListRecommenderConfigurationsResponse
mkListRecommenderConfigurationsResponse =
  ListRecommenderConfigurationsResponse'
    { item = Core.mempty,
      nextToken = Core.Nothing
    }

-- | An array of responses, one for each recommender model configuration that's associated with your Amazon Pinpoint account.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcrItem :: Lens.Lens' ListRecommenderConfigurationsResponse [Types.RecommenderConfigurationResponse]
lrcrItem = Lens.field @"item"
{-# DEPRECATED lrcrItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcrNextToken :: Lens.Lens' ListRecommenderConfigurationsResponse (Core.Maybe Core.Text)
lrcrNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListRecommenderConfigurationsResponse where
  parseJSON =
    Core.withObject "ListRecommenderConfigurationsResponse" Core.$
      \x ->
        ListRecommenderConfigurationsResponse'
          Core.<$> (x Core..:? "Item" Core..!= Core.mempty)
          Core.<*> (x Core..:? "NextToken")
