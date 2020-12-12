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
    lrcNextToken,
    lrcItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about all the recommender model configurations that are associated with your Amazon Pinpoint account.
--
-- /See:/ 'mkListRecommenderConfigurationsResponse' smart constructor.
data ListRecommenderConfigurationsResponse = ListRecommenderConfigurationsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    item ::
      [RecommenderConfigurationResponse]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRecommenderConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'item' - An array of responses, one for each recommender model configuration that's associated with your Amazon Pinpoint account.
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
mkListRecommenderConfigurationsResponse ::
  ListRecommenderConfigurationsResponse
mkListRecommenderConfigurationsResponse =
  ListRecommenderConfigurationsResponse'
    { nextToken = Lude.Nothing,
      item = Lude.mempty
    }

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcNextToken :: Lens.Lens' ListRecommenderConfigurationsResponse (Lude.Maybe Lude.Text)
lrcNextToken = Lens.lens (nextToken :: ListRecommenderConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRecommenderConfigurationsResponse)
{-# DEPRECATED lrcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of responses, one for each recommender model configuration that's associated with your Amazon Pinpoint account.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcItem :: Lens.Lens' ListRecommenderConfigurationsResponse [RecommenderConfigurationResponse]
lrcItem = Lens.lens (item :: ListRecommenderConfigurationsResponse -> [RecommenderConfigurationResponse]) (\s a -> s {item = a} :: ListRecommenderConfigurationsResponse)
{-# DEPRECATED lrcItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON ListRecommenderConfigurationsResponse where
  parseJSON =
    Lude.withObject
      "ListRecommenderConfigurationsResponse"
      ( \x ->
          ListRecommenderConfigurationsResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )
