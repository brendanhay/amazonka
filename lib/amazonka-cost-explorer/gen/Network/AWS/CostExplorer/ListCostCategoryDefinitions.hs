{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    lccdNextToken,
    lccdMaxResults,

    -- * Destructuring the response
    ListCostCategoryDefinitionsResponse (..),
    mkListCostCategoryDefinitionsResponse,

    -- ** Response lenses
    lccdrsCostCategoryReferences,
    lccdrsNextToken,
    lccdrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCostCategoryDefinitions' smart constructor.
data ListCostCategoryDefinitions = ListCostCategoryDefinitions'
  { effectiveOn ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCostCategoryDefinitions' with the minimum fields required to make a request.
--
-- * 'effectiveOn' - The date when the Cost Category was effective.
-- * 'maxResults' - The number of entries a paginated response contains.
-- * 'nextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
mkListCostCategoryDefinitions ::
  ListCostCategoryDefinitions
mkListCostCategoryDefinitions =
  ListCostCategoryDefinitions'
    { effectiveOn = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The date when the Cost Category was effective.
--
-- /Note:/ Consider using 'effectiveOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdEffectiveOn :: Lens.Lens' ListCostCategoryDefinitions (Lude.Maybe Lude.Text)
lccdEffectiveOn = Lens.lens (effectiveOn :: ListCostCategoryDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {effectiveOn = a} :: ListCostCategoryDefinitions)
{-# DEPRECATED lccdEffectiveOn "Use generic-lens or generic-optics with 'effectiveOn' instead." #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdNextToken :: Lens.Lens' ListCostCategoryDefinitions (Lude.Maybe Lude.Text)
lccdNextToken = Lens.lens (nextToken :: ListCostCategoryDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCostCategoryDefinitions)
{-# DEPRECATED lccdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of entries a paginated response contains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdMaxResults :: Lens.Lens' ListCostCategoryDefinitions (Lude.Maybe Lude.Natural)
lccdMaxResults = Lens.lens (maxResults :: ListCostCategoryDefinitions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCostCategoryDefinitions)
{-# DEPRECATED lccdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListCostCategoryDefinitions where
  type
    Rs ListCostCategoryDefinitions =
      ListCostCategoryDefinitionsResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCostCategoryDefinitionsResponse'
            Lude.<$> (x Lude..?> "CostCategoryReferences" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCostCategoryDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.ListCostCategoryDefinitions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCostCategoryDefinitions where
  toJSON ListCostCategoryDefinitions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EffectiveOn" Lude..=) Lude.<$> effectiveOn,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListCostCategoryDefinitions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCostCategoryDefinitions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCostCategoryDefinitionsResponse' smart constructor.
data ListCostCategoryDefinitionsResponse = ListCostCategoryDefinitionsResponse'
  { costCategoryReferences ::
      Lude.Maybe
        [CostCategoryReference],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCostCategoryDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'costCategoryReferences' - A reference to a Cost Category containing enough information to identify the Cost Category.
-- * 'nextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
-- * 'responseStatus' - The response status code.
mkListCostCategoryDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCostCategoryDefinitionsResponse
mkListCostCategoryDefinitionsResponse pResponseStatus_ =
  ListCostCategoryDefinitionsResponse'
    { costCategoryReferences =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A reference to a Cost Category containing enough information to identify the Cost Category.
--
-- /Note:/ Consider using 'costCategoryReferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdrsCostCategoryReferences :: Lens.Lens' ListCostCategoryDefinitionsResponse (Lude.Maybe [CostCategoryReference])
lccdrsCostCategoryReferences = Lens.lens (costCategoryReferences :: ListCostCategoryDefinitionsResponse -> Lude.Maybe [CostCategoryReference]) (\s a -> s {costCategoryReferences = a} :: ListCostCategoryDefinitionsResponse)
{-# DEPRECATED lccdrsCostCategoryReferences "Use generic-lens or generic-optics with 'costCategoryReferences' instead." #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdrsNextToken :: Lens.Lens' ListCostCategoryDefinitionsResponse (Lude.Maybe Lude.Text)
lccdrsNextToken = Lens.lens (nextToken :: ListCostCategoryDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCostCategoryDefinitionsResponse)
{-# DEPRECATED lccdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lccdrsResponseStatus :: Lens.Lens' ListCostCategoryDefinitionsResponse Lude.Int
lccdrsResponseStatus = Lens.lens (responseStatus :: ListCostCategoryDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCostCategoryDefinitionsResponse)
{-# DEPRECATED lccdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
