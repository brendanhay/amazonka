{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetAnomalyMonitors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly monitor definitions for your account. You can filter using a list of cost anomaly monitor Amazon Resource Names (ARNs).
module Network.AWS.CostExplorer.GetAnomalyMonitors
  ( -- * Creating a request
    GetAnomalyMonitors (..),
    mkGetAnomalyMonitors,

    -- ** Request lenses
    gamNextPageToken,
    gamMonitorARNList,
    gamMaxResults,

    -- * Destructuring the response
    GetAnomalyMonitorsResponse (..),
    mkGetAnomalyMonitorsResponse,

    -- ** Response lenses
    gamrsNextPageToken,
    gamrsResponseStatus,
    gamrsAnomalyMonitors,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAnomalyMonitors' smart constructor.
data GetAnomalyMonitors = GetAnomalyMonitors'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    monitorARNList :: Lude.Maybe [Lude.Text],
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAnomalyMonitors' with the minimum fields required to make a request.
--
-- * 'maxResults' - The number of entries a paginated response contains.
-- * 'monitorARNList' - A list of cost anomaly monitor ARNs.
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
mkGetAnomalyMonitors ::
  GetAnomalyMonitors
mkGetAnomalyMonitors =
  GetAnomalyMonitors'
    { nextPageToken = Lude.Nothing,
      monitorARNList = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamNextPageToken :: Lens.Lens' GetAnomalyMonitors (Lude.Maybe Lude.Text)
gamNextPageToken = Lens.lens (nextPageToken :: GetAnomalyMonitors -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetAnomalyMonitors)
{-# DEPRECATED gamNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | A list of cost anomaly monitor ARNs.
--
-- /Note:/ Consider using 'monitorARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamMonitorARNList :: Lens.Lens' GetAnomalyMonitors (Lude.Maybe [Lude.Text])
gamMonitorARNList = Lens.lens (monitorARNList :: GetAnomalyMonitors -> Lude.Maybe [Lude.Text]) (\s a -> s {monitorARNList = a} :: GetAnomalyMonitors)
{-# DEPRECATED gamMonitorARNList "Use generic-lens or generic-optics with 'monitorARNList' instead." #-}

-- | The number of entries a paginated response contains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamMaxResults :: Lens.Lens' GetAnomalyMonitors (Lude.Maybe Lude.Int)
gamMaxResults = Lens.lens (maxResults :: GetAnomalyMonitors -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetAnomalyMonitors)
{-# DEPRECATED gamMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest GetAnomalyMonitors where
  type Rs GetAnomalyMonitors = GetAnomalyMonitorsResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAnomalyMonitorsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "AnomalyMonitors" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders GetAnomalyMonitors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSInsightsIndexService.GetAnomalyMonitors" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAnomalyMonitors where
  toJSON GetAnomalyMonitors' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("MonitorArnList" Lude..=) Lude.<$> monitorARNList,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetAnomalyMonitors where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAnomalyMonitors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAnomalyMonitorsResponse' smart constructor.
data GetAnomalyMonitorsResponse = GetAnomalyMonitorsResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    anomalyMonitors :: [AnomalyMonitor]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAnomalyMonitorsResponse' with the minimum fields required to make a request.
--
-- * 'anomalyMonitors' - A list of cost anomaly monitors that includes the detailed metadata for each monitor.
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'responseStatus' - The response status code.
mkGetAnomalyMonitorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAnomalyMonitorsResponse
mkGetAnomalyMonitorsResponse pResponseStatus_ =
  GetAnomalyMonitorsResponse'
    { nextPageToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      anomalyMonitors = Lude.mempty
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamrsNextPageToken :: Lens.Lens' GetAnomalyMonitorsResponse (Lude.Maybe Lude.Text)
gamrsNextPageToken = Lens.lens (nextPageToken :: GetAnomalyMonitorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetAnomalyMonitorsResponse)
{-# DEPRECATED gamrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamrsResponseStatus :: Lens.Lens' GetAnomalyMonitorsResponse Lude.Int
gamrsResponseStatus = Lens.lens (responseStatus :: GetAnomalyMonitorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAnomalyMonitorsResponse)
{-# DEPRECATED gamrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of cost anomaly monitors that includes the detailed metadata for each monitor.
--
-- /Note:/ Consider using 'anomalyMonitors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gamrsAnomalyMonitors :: Lens.Lens' GetAnomalyMonitorsResponse [AnomalyMonitor]
gamrsAnomalyMonitors = Lens.lens (anomalyMonitors :: GetAnomalyMonitorsResponse -> [AnomalyMonitor]) (\s a -> s {anomalyMonitors = a} :: GetAnomalyMonitorsResponse)
{-# DEPRECATED gamrsAnomalyMonitors "Use generic-lens or generic-optics with 'anomalyMonitors' instead." #-}
