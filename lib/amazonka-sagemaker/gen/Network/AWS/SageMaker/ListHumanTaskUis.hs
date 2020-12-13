{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListHumanTaskUis
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the human task user interfaces in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListHumanTaskUis
  ( -- * Creating a request
    ListHumanTaskUis (..),
    mkListHumanTaskUis,

    -- ** Request lenses
    lhtuCreationTimeAfter,
    lhtuNextToken,
    lhtuSortOrder,
    lhtuCreationTimeBefore,
    lhtuMaxResults,

    -- * Destructuring the response
    ListHumanTaskUisResponse (..),
    mkListHumanTaskUisResponse,

    -- ** Response lenses
    lhtursHumanTaskUiSummaries,
    lhtursNextToken,
    lhtursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListHumanTaskUis' smart constructor.
data ListHumanTaskUis = ListHumanTaskUis'
  { -- | A filter that returns only human task user interfaces with a creation time greater than or equal to the specified timestamp.
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A token to resume pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only human task user interfaces that were created before the specified timestamp.
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHumanTaskUis' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only human task user interfaces with a creation time greater than or equal to the specified timestamp.
-- * 'nextToken' - A token to resume pagination.
-- * 'sortOrder' - An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
-- * 'creationTimeBefore' - A filter that returns only human task user interfaces that were created before the specified timestamp.
-- * 'maxResults' - The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
mkListHumanTaskUis ::
  ListHumanTaskUis
mkListHumanTaskUis =
  ListHumanTaskUis'
    { creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A filter that returns only human task user interfaces with a creation time greater than or equal to the specified timestamp.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuCreationTimeAfter :: Lens.Lens' ListHumanTaskUis (Lude.Maybe Lude.Timestamp)
lhtuCreationTimeAfter = Lens.lens (creationTimeAfter :: ListHumanTaskUis -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListHumanTaskUis)
{-# DEPRECATED lhtuCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuNextToken :: Lens.Lens' ListHumanTaskUis (Lude.Maybe Lude.Text)
lhtuNextToken = Lens.lens (nextToken :: ListHumanTaskUis -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHumanTaskUis)
{-# DEPRECATED lhtuNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuSortOrder :: Lens.Lens' ListHumanTaskUis (Lude.Maybe SortOrder)
lhtuSortOrder = Lens.lens (sortOrder :: ListHumanTaskUis -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListHumanTaskUis)
{-# DEPRECATED lhtuSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only human task user interfaces that were created before the specified timestamp.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuCreationTimeBefore :: Lens.Lens' ListHumanTaskUis (Lude.Maybe Lude.Timestamp)
lhtuCreationTimeBefore = Lens.lens (creationTimeBefore :: ListHumanTaskUis -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListHumanTaskUis)
{-# DEPRECATED lhtuCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuMaxResults :: Lens.Lens' ListHumanTaskUis (Lude.Maybe Lude.Natural)
lhtuMaxResults = Lens.lens (maxResults :: ListHumanTaskUis -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListHumanTaskUis)
{-# DEPRECATED lhtuMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListHumanTaskUis where
  page rq rs
    | Page.stop (rs Lens.^. lhtursNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lhtursHumanTaskUiSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lhtuNextToken Lens..~ rs Lens.^. lhtursNextToken

instance Lude.AWSRequest ListHumanTaskUis where
  type Rs ListHumanTaskUis = ListHumanTaskUisResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListHumanTaskUisResponse'
            Lude.<$> (x Lude..?> "HumanTaskUiSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListHumanTaskUis where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListHumanTaskUis" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListHumanTaskUis where
  toJSON ListHumanTaskUis' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListHumanTaskUis where
  toPath = Lude.const "/"

instance Lude.ToQuery ListHumanTaskUis where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListHumanTaskUisResponse' smart constructor.
data ListHumanTaskUisResponse = ListHumanTaskUisResponse'
  { -- | An array of objects describing the human task user interfaces.
    humanTaskUiSummaries :: [HumanTaskUiSummary],
    -- | A token to resume pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHumanTaskUisResponse' with the minimum fields required to make a request.
--
-- * 'humanTaskUiSummaries' - An array of objects describing the human task user interfaces.
-- * 'nextToken' - A token to resume pagination.
-- * 'responseStatus' - The response status code.
mkListHumanTaskUisResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListHumanTaskUisResponse
mkListHumanTaskUisResponse pResponseStatus_ =
  ListHumanTaskUisResponse'
    { humanTaskUiSummaries = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects describing the human task user interfaces.
--
-- /Note:/ Consider using 'humanTaskUiSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtursHumanTaskUiSummaries :: Lens.Lens' ListHumanTaskUisResponse [HumanTaskUiSummary]
lhtursHumanTaskUiSummaries = Lens.lens (humanTaskUiSummaries :: ListHumanTaskUisResponse -> [HumanTaskUiSummary]) (\s a -> s {humanTaskUiSummaries = a} :: ListHumanTaskUisResponse)
{-# DEPRECATED lhtursHumanTaskUiSummaries "Use generic-lens or generic-optics with 'humanTaskUiSummaries' instead." #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtursNextToken :: Lens.Lens' ListHumanTaskUisResponse (Lude.Maybe Lude.Text)
lhtursNextToken = Lens.lens (nextToken :: ListHumanTaskUisResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHumanTaskUisResponse)
{-# DEPRECATED lhtursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtursResponseStatus :: Lens.Lens' ListHumanTaskUisResponse Lude.Int
lhtursResponseStatus = Lens.lens (responseStatus :: ListHumanTaskUisResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHumanTaskUisResponse)
{-# DEPRECATED lhtursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
