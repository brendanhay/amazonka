{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetAnomalies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all of the cost anomalies detected on your account, during the time period specified by the @DateInterval@ object.
module Network.AWS.CostExplorer.GetAnomalies
  ( -- * Creating a request
    GetAnomalies (..),
    mkGetAnomalies,

    -- ** Request lenses
    gaNextPageToken,
    gaTotalImpact,
    gaDateInterval,
    gaMaxResults,
    gaFeedback,
    gaMonitorARN,

    -- * Destructuring the response
    GetAnomaliesResponse (..),
    mkGetAnomaliesResponse,

    -- ** Response lenses
    garsNextPageToken,
    garsAnomalies,
    garsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAnomalies' smart constructor.
data GetAnomalies = GetAnomalies'
  { -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Filters anomaly results by the total impact field on the anomaly object. For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve anomalies, with an estimated dollar impact greater than 200.
    totalImpact :: Lude.Maybe TotalImpactFilter,
    -- | Assigns the start and end dates for retrieving cost anomalies. The returned anomaly object will have an @AnomalyEndDate@ in the specified time range.
    dateInterval :: AnomalyDateInterval,
    -- | The number of entries a paginated response contains.
    maxResults :: Lude.Maybe Lude.Int,
    -- | Filters anomaly results by the feedback field on the anomaly object.
    feedback :: Lude.Maybe AnomalyFeedbackType,
    -- | Retrieves all of the cost anomalies detected for a specific cost anomaly monitor Amazon Resource Name (ARN).
    monitorARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAnomalies' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'totalImpact' - Filters anomaly results by the total impact field on the anomaly object. For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve anomalies, with an estimated dollar impact greater than 200.
-- * 'dateInterval' - Assigns the start and end dates for retrieving cost anomalies. The returned anomaly object will have an @AnomalyEndDate@ in the specified time range.
-- * 'maxResults' - The number of entries a paginated response contains.
-- * 'feedback' - Filters anomaly results by the feedback field on the anomaly object.
-- * 'monitorARN' - Retrieves all of the cost anomalies detected for a specific cost anomaly monitor Amazon Resource Name (ARN).
mkGetAnomalies ::
  -- | 'dateInterval'
  AnomalyDateInterval ->
  GetAnomalies
mkGetAnomalies pDateInterval_ =
  GetAnomalies'
    { nextPageToken = Lude.Nothing,
      totalImpact = Lude.Nothing,
      dateInterval = pDateInterval_,
      maxResults = Lude.Nothing,
      feedback = Lude.Nothing,
      monitorARN = Lude.Nothing
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaNextPageToken :: Lens.Lens' GetAnomalies (Lude.Maybe Lude.Text)
gaNextPageToken = Lens.lens (nextPageToken :: GetAnomalies -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetAnomalies)
{-# DEPRECATED gaNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Filters anomaly results by the total impact field on the anomaly object. For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve anomalies, with an estimated dollar impact greater than 200.
--
-- /Note:/ Consider using 'totalImpact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaTotalImpact :: Lens.Lens' GetAnomalies (Lude.Maybe TotalImpactFilter)
gaTotalImpact = Lens.lens (totalImpact :: GetAnomalies -> Lude.Maybe TotalImpactFilter) (\s a -> s {totalImpact = a} :: GetAnomalies)
{-# DEPRECATED gaTotalImpact "Use generic-lens or generic-optics with 'totalImpact' instead." #-}

-- | Assigns the start and end dates for retrieving cost anomalies. The returned anomaly object will have an @AnomalyEndDate@ in the specified time range.
--
-- /Note:/ Consider using 'dateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaDateInterval :: Lens.Lens' GetAnomalies AnomalyDateInterval
gaDateInterval = Lens.lens (dateInterval :: GetAnomalies -> AnomalyDateInterval) (\s a -> s {dateInterval = a} :: GetAnomalies)
{-# DEPRECATED gaDateInterval "Use generic-lens or generic-optics with 'dateInterval' instead." #-}

-- | The number of entries a paginated response contains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaMaxResults :: Lens.Lens' GetAnomalies (Lude.Maybe Lude.Int)
gaMaxResults = Lens.lens (maxResults :: GetAnomalies -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetAnomalies)
{-# DEPRECATED gaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Filters anomaly results by the feedback field on the anomaly object.
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaFeedback :: Lens.Lens' GetAnomalies (Lude.Maybe AnomalyFeedbackType)
gaFeedback = Lens.lens (feedback :: GetAnomalies -> Lude.Maybe AnomalyFeedbackType) (\s a -> s {feedback = a} :: GetAnomalies)
{-# DEPRECATED gaFeedback "Use generic-lens or generic-optics with 'feedback' instead." #-}

-- | Retrieves all of the cost anomalies detected for a specific cost anomaly monitor Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'monitorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaMonitorARN :: Lens.Lens' GetAnomalies (Lude.Maybe Lude.Text)
gaMonitorARN = Lens.lens (monitorARN :: GetAnomalies -> Lude.Maybe Lude.Text) (\s a -> s {monitorARN = a} :: GetAnomalies)
{-# DEPRECATED gaMonitorARN "Use generic-lens or generic-optics with 'monitorARN' instead." #-}

instance Lude.AWSRequest GetAnomalies where
  type Rs GetAnomalies = GetAnomaliesResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAnomaliesResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "Anomalies" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAnomalies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSInsightsIndexService.GetAnomalies" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAnomalies where
  toJSON GetAnomalies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("TotalImpact" Lude..=) Lude.<$> totalImpact,
            Lude.Just ("DateInterval" Lude..= dateInterval),
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("Feedback" Lude..=) Lude.<$> feedback,
            ("MonitorArn" Lude..=) Lude.<$> monitorARN
          ]
      )

instance Lude.ToPath GetAnomalies where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAnomalies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAnomaliesResponse' smart constructor.
data GetAnomaliesResponse = GetAnomaliesResponse'
  { -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | A list of cost anomalies.
    anomalies :: [Anomaly],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAnomaliesResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'anomalies' - A list of cost anomalies.
-- * 'responseStatus' - The response status code.
mkGetAnomaliesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAnomaliesResponse
mkGetAnomaliesResponse pResponseStatus_ =
  GetAnomaliesResponse'
    { nextPageToken = Lude.Nothing,
      anomalies = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsNextPageToken :: Lens.Lens' GetAnomaliesResponse (Lude.Maybe Lude.Text)
garsNextPageToken = Lens.lens (nextPageToken :: GetAnomaliesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetAnomaliesResponse)
{-# DEPRECATED garsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | A list of cost anomalies.
--
-- /Note:/ Consider using 'anomalies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsAnomalies :: Lens.Lens' GetAnomaliesResponse [Anomaly]
garsAnomalies = Lens.lens (anomalies :: GetAnomaliesResponse -> [Anomaly]) (\s a -> s {anomalies = a} :: GetAnomaliesResponse)
{-# DEPRECATED garsAnomalies "Use generic-lens or generic-optics with 'anomalies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GetAnomaliesResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GetAnomaliesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAnomaliesResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
