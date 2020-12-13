{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListContributorInsights
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ContributorInsightsSummary for a table and all its global secondary indexes.
module Network.AWS.DynamoDB.ListContributorInsights
  ( -- * Creating a request
    ListContributorInsights (..),
    mkListContributorInsights,

    -- ** Request lenses
    lciNextToken,
    lciMaxResults,
    lciTableName,

    -- * Destructuring the response
    ListContributorInsightsResponse (..),
    mkListContributorInsightsResponse,

    -- ** Response lenses
    lcirsContributorInsightsSummaries,
    lcirsNextToken,
    lcirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListContributorInsights' smart constructor.
data ListContributorInsights = ListContributorInsights'
  { -- | A token to for the desired page, if there is one.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of results to return per page.
    maxResults :: Lude.Maybe Lude.Int,
    -- | The name of the table.
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListContributorInsights' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to for the desired page, if there is one.
-- * 'maxResults' - Maximum number of results to return per page.
-- * 'tableName' - The name of the table.
mkListContributorInsights ::
  ListContributorInsights
mkListContributorInsights =
  ListContributorInsights'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | A token to for the desired page, if there is one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciNextToken :: Lens.Lens' ListContributorInsights (Lude.Maybe Lude.Text)
lciNextToken = Lens.lens (nextToken :: ListContributorInsights -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListContributorInsights)
{-# DEPRECATED lciNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMaxResults :: Lens.Lens' ListContributorInsights (Lude.Maybe Lude.Int)
lciMaxResults = Lens.lens (maxResults :: ListContributorInsights -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListContributorInsights)
{-# DEPRECATED lciMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciTableName :: Lens.Lens' ListContributorInsights (Lude.Maybe Lude.Text)
lciTableName = Lens.lens (tableName :: ListContributorInsights -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: ListContributorInsights)
{-# DEPRECATED lciTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest ListContributorInsights where
  type Rs ListContributorInsights = ListContributorInsightsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListContributorInsightsResponse'
            Lude.<$> (x Lude..?> "ContributorInsightsSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListContributorInsights where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.ListContributorInsights" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListContributorInsights where
  toJSON ListContributorInsights' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("TableName" Lude..=) Lude.<$> tableName
          ]
      )

instance Lude.ToPath ListContributorInsights where
  toPath = Lude.const "/"

instance Lude.ToQuery ListContributorInsights where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListContributorInsightsResponse' smart constructor.
data ListContributorInsightsResponse = ListContributorInsightsResponse'
  { -- | A list of ContributorInsightsSummary.
    contributorInsightsSummaries :: Lude.Maybe [ContributorInsightsSummary],
    -- | A token to go to the next page if there is one.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListContributorInsightsResponse' with the minimum fields required to make a request.
--
-- * 'contributorInsightsSummaries' - A list of ContributorInsightsSummary.
-- * 'nextToken' - A token to go to the next page if there is one.
-- * 'responseStatus' - The response status code.
mkListContributorInsightsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListContributorInsightsResponse
mkListContributorInsightsResponse pResponseStatus_ =
  ListContributorInsightsResponse'
    { contributorInsightsSummaries =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of ContributorInsightsSummary.
--
-- /Note:/ Consider using 'contributorInsightsSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsContributorInsightsSummaries :: Lens.Lens' ListContributorInsightsResponse (Lude.Maybe [ContributorInsightsSummary])
lcirsContributorInsightsSummaries = Lens.lens (contributorInsightsSummaries :: ListContributorInsightsResponse -> Lude.Maybe [ContributorInsightsSummary]) (\s a -> s {contributorInsightsSummaries = a} :: ListContributorInsightsResponse)
{-# DEPRECATED lcirsContributorInsightsSummaries "Use generic-lens or generic-optics with 'contributorInsightsSummaries' instead." #-}

-- | A token to go to the next page if there is one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsNextToken :: Lens.Lens' ListContributorInsightsResponse (Lude.Maybe Lude.Text)
lcirsNextToken = Lens.lens (nextToken :: ListContributorInsightsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListContributorInsightsResponse)
{-# DEPRECATED lcirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsResponseStatus :: Lens.Lens' ListContributorInsightsResponse Lude.Int
lcirsResponseStatus = Lens.lens (responseStatus :: ListContributorInsightsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListContributorInsightsResponse)
{-# DEPRECATED lcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
