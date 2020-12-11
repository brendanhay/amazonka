{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeContributorInsights
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about contributor insights, for a given table or global secondary index.
module Network.AWS.DynamoDB.DescribeContributorInsights
  ( -- * Creating a request
    DescribeContributorInsights (..),
    mkDescribeContributorInsights,

    -- ** Request lenses
    dciIndexName,
    dciTableName,

    -- * Destructuring the response
    DescribeContributorInsightsResponse (..),
    mkDescribeContributorInsightsResponse,

    -- ** Response lenses
    dcirsContributorInsightsRuleList,
    dcirsFailureException,
    dcirsContributorInsightsStatus,
    dcirsLastUpdateDateTime,
    dcirsTableName,
    dcirsIndexName,
    dcirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeContributorInsights' smart constructor.
data DescribeContributorInsights = DescribeContributorInsights'
  { indexName ::
      Lude.Maybe Lude.Text,
    tableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeContributorInsights' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index to describe, if applicable.
-- * 'tableName' - The name of the table to describe.
mkDescribeContributorInsights ::
  -- | 'tableName'
  Lude.Text ->
  DescribeContributorInsights
mkDescribeContributorInsights pTableName_ =
  DescribeContributorInsights'
    { indexName = Lude.Nothing,
      tableName = pTableName_
    }

-- | The name of the global secondary index to describe, if applicable.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciIndexName :: Lens.Lens' DescribeContributorInsights (Lude.Maybe Lude.Text)
dciIndexName = Lens.lens (indexName :: DescribeContributorInsights -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: DescribeContributorInsights)
{-# DEPRECATED dciIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The name of the table to describe.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciTableName :: Lens.Lens' DescribeContributorInsights Lude.Text
dciTableName = Lens.lens (tableName :: DescribeContributorInsights -> Lude.Text) (\s a -> s {tableName = a} :: DescribeContributorInsights)
{-# DEPRECATED dciTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DescribeContributorInsights where
  type
    Rs DescribeContributorInsights =
      DescribeContributorInsightsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeContributorInsightsResponse'
            Lude.<$> (x Lude..?> "ContributorInsightsRuleList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "FailureException")
            Lude.<*> (x Lude..?> "ContributorInsightsStatus")
            Lude.<*> (x Lude..?> "LastUpdateDateTime")
            Lude.<*> (x Lude..?> "TableName")
            Lude.<*> (x Lude..?> "IndexName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeContributorInsights where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DynamoDB_20120810.DescribeContributorInsights" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeContributorInsights where
  toJSON DescribeContributorInsights' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IndexName" Lude..=) Lude.<$> indexName,
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath DescribeContributorInsights where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeContributorInsights where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeContributorInsightsResponse' smart constructor.
data DescribeContributorInsightsResponse = DescribeContributorInsightsResponse'
  { contributorInsightsRuleList ::
      Lude.Maybe
        [Lude.Text],
    failureException ::
      Lude.Maybe
        FailureException,
    contributorInsightsStatus ::
      Lude.Maybe
        ContributorInsightsStatus,
    lastUpdateDateTime ::
      Lude.Maybe
        Lude.Timestamp,
    tableName ::
      Lude.Maybe
        Lude.Text,
    indexName ::
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

-- | Creates a value of 'DescribeContributorInsightsResponse' with the minimum fields required to make a request.
--
-- * 'contributorInsightsRuleList' - List of names of the associated Alpine rules.
-- * 'contributorInsightsStatus' - Current Status contributor insights.
-- * 'failureException' - Returns information about the last failure that encountered.
--
-- The most common exceptions for a FAILED status are:
--
--     * LimitExceededException - Per-account Amazon CloudWatch Contributor Insights rule limit reached. Please disable Contributor Insights for other tables/indexes OR disable Contributor Insights rules before retrying.
--
--
--     * AccessDeniedException - Amazon CloudWatch Contributor Insights rules cannot be modified due to insufficient permissions.
--
--
--     * AccessDeniedException - Failed to create service-linked role for Contributor Insights due to insufficient permissions.
--
--
--     * InternalServerError - Failed to create Amazon CloudWatch Contributor Insights rules. Please retry request.
--
--
-- * 'indexName' - The name of the global secondary index being described.
-- * 'lastUpdateDateTime' - Timestamp of the last time the status was changed.
-- * 'responseStatus' - The response status code.
-- * 'tableName' - The name of the table being described.
mkDescribeContributorInsightsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeContributorInsightsResponse
mkDescribeContributorInsightsResponse pResponseStatus_ =
  DescribeContributorInsightsResponse'
    { contributorInsightsRuleList =
        Lude.Nothing,
      failureException = Lude.Nothing,
      contributorInsightsStatus = Lude.Nothing,
      lastUpdateDateTime = Lude.Nothing,
      tableName = Lude.Nothing,
      indexName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of names of the associated Alpine rules.
--
-- /Note:/ Consider using 'contributorInsightsRuleList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsContributorInsightsRuleList :: Lens.Lens' DescribeContributorInsightsResponse (Lude.Maybe [Lude.Text])
dcirsContributorInsightsRuleList = Lens.lens (contributorInsightsRuleList :: DescribeContributorInsightsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {contributorInsightsRuleList = a} :: DescribeContributorInsightsResponse)
{-# DEPRECATED dcirsContributorInsightsRuleList "Use generic-lens or generic-optics with 'contributorInsightsRuleList' instead." #-}

-- | Returns information about the last failure that encountered.
--
-- The most common exceptions for a FAILED status are:
--
--     * LimitExceededException - Per-account Amazon CloudWatch Contributor Insights rule limit reached. Please disable Contributor Insights for other tables/indexes OR disable Contributor Insights rules before retrying.
--
--
--     * AccessDeniedException - Amazon CloudWatch Contributor Insights rules cannot be modified due to insufficient permissions.
--
--
--     * AccessDeniedException - Failed to create service-linked role for Contributor Insights due to insufficient permissions.
--
--
--     * InternalServerError - Failed to create Amazon CloudWatch Contributor Insights rules. Please retry request.
--
--
--
-- /Note:/ Consider using 'failureException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsFailureException :: Lens.Lens' DescribeContributorInsightsResponse (Lude.Maybe FailureException)
dcirsFailureException = Lens.lens (failureException :: DescribeContributorInsightsResponse -> Lude.Maybe FailureException) (\s a -> s {failureException = a} :: DescribeContributorInsightsResponse)
{-# DEPRECATED dcirsFailureException "Use generic-lens or generic-optics with 'failureException' instead." #-}

-- | Current Status contributor insights.
--
-- /Note:/ Consider using 'contributorInsightsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsContributorInsightsStatus :: Lens.Lens' DescribeContributorInsightsResponse (Lude.Maybe ContributorInsightsStatus)
dcirsContributorInsightsStatus = Lens.lens (contributorInsightsStatus :: DescribeContributorInsightsResponse -> Lude.Maybe ContributorInsightsStatus) (\s a -> s {contributorInsightsStatus = a} :: DescribeContributorInsightsResponse)
{-# DEPRECATED dcirsContributorInsightsStatus "Use generic-lens or generic-optics with 'contributorInsightsStatus' instead." #-}

-- | Timestamp of the last time the status was changed.
--
-- /Note:/ Consider using 'lastUpdateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsLastUpdateDateTime :: Lens.Lens' DescribeContributorInsightsResponse (Lude.Maybe Lude.Timestamp)
dcirsLastUpdateDateTime = Lens.lens (lastUpdateDateTime :: DescribeContributorInsightsResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateDateTime = a} :: DescribeContributorInsightsResponse)
{-# DEPRECATED dcirsLastUpdateDateTime "Use generic-lens or generic-optics with 'lastUpdateDateTime' instead." #-}

-- | The name of the table being described.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsTableName :: Lens.Lens' DescribeContributorInsightsResponse (Lude.Maybe Lude.Text)
dcirsTableName = Lens.lens (tableName :: DescribeContributorInsightsResponse -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: DescribeContributorInsightsResponse)
{-# DEPRECATED dcirsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The name of the global secondary index being described.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsIndexName :: Lens.Lens' DescribeContributorInsightsResponse (Lude.Maybe Lude.Text)
dcirsIndexName = Lens.lens (indexName :: DescribeContributorInsightsResponse -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: DescribeContributorInsightsResponse)
{-# DEPRECATED dcirsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsResponseStatus :: Lens.Lens' DescribeContributorInsightsResponse Lude.Int
dcirsResponseStatus = Lens.lens (responseStatus :: DescribeContributorInsightsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeContributorInsightsResponse)
{-# DEPRECATED dcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
