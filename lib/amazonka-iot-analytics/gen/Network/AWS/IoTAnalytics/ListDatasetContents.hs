{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.ListDatasetContents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about data set contents that have been created.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatasetContents
  ( -- * Creating a request
    ListDatasetContents (..),
    mkListDatasetContents,

    -- ** Request lenses
    ldcNextToken,
    ldcScheduledBefore,
    ldcMaxResults,
    ldcScheduledOnOrAfter,
    ldcDatasetName,

    -- * Destructuring the response
    ListDatasetContentsResponse (..),
    mkListDatasetContentsResponse,

    -- ** Response lenses
    ldcrsDatasetContentSummaries,
    ldcrsNextToken,
    ldcrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDatasetContents' smart constructor.
data ListDatasetContents = ListDatasetContents'
  { nextToken ::
      Lude.Maybe Lude.Text,
    scheduledBefore :: Lude.Maybe Lude.Timestamp,
    maxResults :: Lude.Maybe Lude.Natural,
    scheduledOnOrAfter :: Lude.Maybe Lude.Timestamp,
    datasetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDatasetContents' with the minimum fields required to make a request.
--
-- * 'datasetName' - The name of the data set whose contents information you want to list.
-- * 'maxResults' - The maximum number of results to return in this request.
-- * 'nextToken' - The token for the next set of results.
-- * 'scheduledBefore' - A filter to limit results to those data set contents whose creation is scheduled before the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
-- * 'scheduledOnOrAfter' - A filter to limit results to those data set contents whose creation is scheduled on or after the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
mkListDatasetContents ::
  -- | 'datasetName'
  Lude.Text ->
  ListDatasetContents
mkListDatasetContents pDatasetName_ =
  ListDatasetContents'
    { nextToken = Lude.Nothing,
      scheduledBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      scheduledOnOrAfter = Lude.Nothing,
      datasetName = pDatasetName_
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcNextToken :: Lens.Lens' ListDatasetContents (Lude.Maybe Lude.Text)
ldcNextToken = Lens.lens (nextToken :: ListDatasetContents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDatasetContents)
{-# DEPRECATED ldcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A filter to limit results to those data set contents whose creation is scheduled before the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
--
-- /Note:/ Consider using 'scheduledBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcScheduledBefore :: Lens.Lens' ListDatasetContents (Lude.Maybe Lude.Timestamp)
ldcScheduledBefore = Lens.lens (scheduledBefore :: ListDatasetContents -> Lude.Maybe Lude.Timestamp) (\s a -> s {scheduledBefore = a} :: ListDatasetContents)
{-# DEPRECATED ldcScheduledBefore "Use generic-lens or generic-optics with 'scheduledBefore' instead." #-}

-- | The maximum number of results to return in this request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcMaxResults :: Lens.Lens' ListDatasetContents (Lude.Maybe Lude.Natural)
ldcMaxResults = Lens.lens (maxResults :: ListDatasetContents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDatasetContents)
{-# DEPRECATED ldcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A filter to limit results to those data set contents whose creation is scheduled on or after the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
--
-- /Note:/ Consider using 'scheduledOnOrAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcScheduledOnOrAfter :: Lens.Lens' ListDatasetContents (Lude.Maybe Lude.Timestamp)
ldcScheduledOnOrAfter = Lens.lens (scheduledOnOrAfter :: ListDatasetContents -> Lude.Maybe Lude.Timestamp) (\s a -> s {scheduledOnOrAfter = a} :: ListDatasetContents)
{-# DEPRECATED ldcScheduledOnOrAfter "Use generic-lens or generic-optics with 'scheduledOnOrAfter' instead." #-}

-- | The name of the data set whose contents information you want to list.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcDatasetName :: Lens.Lens' ListDatasetContents Lude.Text
ldcDatasetName = Lens.lens (datasetName :: ListDatasetContents -> Lude.Text) (\s a -> s {datasetName = a} :: ListDatasetContents)
{-# DEPRECATED ldcDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

instance Page.AWSPager ListDatasetContents where
  page rq rs
    | Page.stop (rs Lens.^. ldcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldcrsDatasetContentSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldcNextToken Lens..~ rs Lens.^. ldcrsNextToken

instance Lude.AWSRequest ListDatasetContents where
  type Rs ListDatasetContents = ListDatasetContentsResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDatasetContentsResponse'
            Lude.<$> (x Lude..?> "datasetContentSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDatasetContents where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDatasetContents where
  toPath ListDatasetContents' {..} =
    Lude.mconcat ["/datasets/", Lude.toBS datasetName, "/contents"]

instance Lude.ToQuery ListDatasetContents where
  toQuery ListDatasetContents' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "scheduledBefore" Lude.=: scheduledBefore,
        "maxResults" Lude.=: maxResults,
        "scheduledOnOrAfter" Lude.=: scheduledOnOrAfter
      ]

-- | /See:/ 'mkListDatasetContentsResponse' smart constructor.
data ListDatasetContentsResponse = ListDatasetContentsResponse'
  { datasetContentSummaries ::
      Lude.Maybe [DatasetContentSummary],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDatasetContentsResponse' with the minimum fields required to make a request.
--
-- * 'datasetContentSummaries' - Summary information about data set contents that have been created.
-- * 'nextToken' - The token to retrieve the next set of results, or @null@ if there are no more results.
-- * 'responseStatus' - The response status code.
mkListDatasetContentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDatasetContentsResponse
mkListDatasetContentsResponse pResponseStatus_ =
  ListDatasetContentsResponse'
    { datasetContentSummaries =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Summary information about data set contents that have been created.
--
-- /Note:/ Consider using 'datasetContentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsDatasetContentSummaries :: Lens.Lens' ListDatasetContentsResponse (Lude.Maybe [DatasetContentSummary])
ldcrsDatasetContentSummaries = Lens.lens (datasetContentSummaries :: ListDatasetContentsResponse -> Lude.Maybe [DatasetContentSummary]) (\s a -> s {datasetContentSummaries = a} :: ListDatasetContentsResponse)
{-# DEPRECATED ldcrsDatasetContentSummaries "Use generic-lens or generic-optics with 'datasetContentSummaries' instead." #-}

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsNextToken :: Lens.Lens' ListDatasetContentsResponse (Lude.Maybe Lude.Text)
ldcrsNextToken = Lens.lens (nextToken :: ListDatasetContentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDatasetContentsResponse)
{-# DEPRECATED ldcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsResponseStatus :: Lens.Lens' ListDatasetContentsResponse Lude.Int
ldcrsResponseStatus = Lens.lens (responseStatus :: ListDatasetContentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDatasetContentsResponse)
{-# DEPRECATED ldcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
