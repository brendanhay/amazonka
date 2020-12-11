{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateContributorInsights
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status for contributor insights for a specific table or index.
module Network.AWS.DynamoDB.UpdateContributorInsights
  ( -- * Creating a request
    UpdateContributorInsights (..),
    mkUpdateContributorInsights,

    -- ** Request lenses
    uciIndexName,
    uciTableName,
    uciContributorInsightsAction,

    -- * Destructuring the response
    UpdateContributorInsightsResponse (..),
    mkUpdateContributorInsightsResponse,

    -- ** Response lenses
    ucirsContributorInsightsStatus,
    ucirsTableName,
    ucirsIndexName,
    ucirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateContributorInsights' smart constructor.
data UpdateContributorInsights = UpdateContributorInsights'
  { indexName ::
      Lude.Maybe Lude.Text,
    tableName :: Lude.Text,
    contributorInsightsAction ::
      ContributorInsightsAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContributorInsights' with the minimum fields required to make a request.
--
-- * 'contributorInsightsAction' - Represents the contributor insights action.
-- * 'indexName' - The global secondary index name, if applicable.
-- * 'tableName' - The name of the table.
mkUpdateContributorInsights ::
  -- | 'tableName'
  Lude.Text ->
  -- | 'contributorInsightsAction'
  ContributorInsightsAction ->
  UpdateContributorInsights
mkUpdateContributorInsights pTableName_ pContributorInsightsAction_ =
  UpdateContributorInsights'
    { indexName = Lude.Nothing,
      tableName = pTableName_,
      contributorInsightsAction = pContributorInsightsAction_
    }

-- | The global secondary index name, if applicable.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciIndexName :: Lens.Lens' UpdateContributorInsights (Lude.Maybe Lude.Text)
uciIndexName = Lens.lens (indexName :: UpdateContributorInsights -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: UpdateContributorInsights)
{-# DEPRECATED uciIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciTableName :: Lens.Lens' UpdateContributorInsights Lude.Text
uciTableName = Lens.lens (tableName :: UpdateContributorInsights -> Lude.Text) (\s a -> s {tableName = a} :: UpdateContributorInsights)
{-# DEPRECATED uciTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Represents the contributor insights action.
--
-- /Note:/ Consider using 'contributorInsightsAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciContributorInsightsAction :: Lens.Lens' UpdateContributorInsights ContributorInsightsAction
uciContributorInsightsAction = Lens.lens (contributorInsightsAction :: UpdateContributorInsights -> ContributorInsightsAction) (\s a -> s {contributorInsightsAction = a} :: UpdateContributorInsights)
{-# DEPRECATED uciContributorInsightsAction "Use generic-lens or generic-optics with 'contributorInsightsAction' instead." #-}

instance Lude.AWSRequest UpdateContributorInsights where
  type
    Rs UpdateContributorInsights =
      UpdateContributorInsightsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateContributorInsightsResponse'
            Lude.<$> (x Lude..?> "ContributorInsightsStatus")
            Lude.<*> (x Lude..?> "TableName")
            Lude.<*> (x Lude..?> "IndexName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateContributorInsights where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.UpdateContributorInsights" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateContributorInsights where
  toJSON UpdateContributorInsights' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IndexName" Lude..=) Lude.<$> indexName,
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just
              ("ContributorInsightsAction" Lude..= contributorInsightsAction)
          ]
      )

instance Lude.ToPath UpdateContributorInsights where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateContributorInsights where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateContributorInsightsResponse' smart constructor.
data UpdateContributorInsightsResponse = UpdateContributorInsightsResponse'
  { contributorInsightsStatus ::
      Lude.Maybe
        ContributorInsightsStatus,
    tableName ::
      Lude.Maybe Lude.Text,
    indexName ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateContributorInsightsResponse' with the minimum fields required to make a request.
--
-- * 'contributorInsightsStatus' - The status of contributor insights
-- * 'indexName' - The name of the global secondary index, if applicable.
-- * 'responseStatus' - The response status code.
-- * 'tableName' - The name of the table.
mkUpdateContributorInsightsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateContributorInsightsResponse
mkUpdateContributorInsightsResponse pResponseStatus_ =
  UpdateContributorInsightsResponse'
    { contributorInsightsStatus =
        Lude.Nothing,
      tableName = Lude.Nothing,
      indexName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of contributor insights
--
-- /Note:/ Consider using 'contributorInsightsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirsContributorInsightsStatus :: Lens.Lens' UpdateContributorInsightsResponse (Lude.Maybe ContributorInsightsStatus)
ucirsContributorInsightsStatus = Lens.lens (contributorInsightsStatus :: UpdateContributorInsightsResponse -> Lude.Maybe ContributorInsightsStatus) (\s a -> s {contributorInsightsStatus = a} :: UpdateContributorInsightsResponse)
{-# DEPRECATED ucirsContributorInsightsStatus "Use generic-lens or generic-optics with 'contributorInsightsStatus' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirsTableName :: Lens.Lens' UpdateContributorInsightsResponse (Lude.Maybe Lude.Text)
ucirsTableName = Lens.lens (tableName :: UpdateContributorInsightsResponse -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: UpdateContributorInsightsResponse)
{-# DEPRECATED ucirsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The name of the global secondary index, if applicable.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirsIndexName :: Lens.Lens' UpdateContributorInsightsResponse (Lude.Maybe Lude.Text)
ucirsIndexName = Lens.lens (indexName :: UpdateContributorInsightsResponse -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: UpdateContributorInsightsResponse)
{-# DEPRECATED ucirsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirsResponseStatus :: Lens.Lens' UpdateContributorInsightsResponse Lude.Int
ucirsResponseStatus = Lens.lens (responseStatus :: UpdateContributorInsightsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateContributorInsightsResponse)
{-# DEPRECATED ucirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
