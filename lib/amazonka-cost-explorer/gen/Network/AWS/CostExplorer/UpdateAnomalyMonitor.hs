{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.UpdateAnomalyMonitor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing cost anomaly monitor. The changes made are applied going forward, and does not change anomalies detected in the past.
module Network.AWS.CostExplorer.UpdateAnomalyMonitor
  ( -- * Creating a request
    UpdateAnomalyMonitor (..),
    mkUpdateAnomalyMonitor,

    -- ** Request lenses
    uamMonitorName,
    uamMonitorARN,

    -- * Destructuring the response
    UpdateAnomalyMonitorResponse (..),
    mkUpdateAnomalyMonitorResponse,

    -- ** Response lenses
    uamrsResponseStatus,
    uamrsMonitorARN,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAnomalyMonitor' smart constructor.
data UpdateAnomalyMonitor = UpdateAnomalyMonitor'
  { monitorName ::
      Lude.Maybe Lude.Text,
    monitorARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAnomalyMonitor' with the minimum fields required to make a request.
--
-- * 'monitorARN' - Cost anomaly monitor Amazon Resource Names (ARNs).
-- * 'monitorName' - The new name for the cost anomaly monitor.
mkUpdateAnomalyMonitor ::
  -- | 'monitorARN'
  Lude.Text ->
  UpdateAnomalyMonitor
mkUpdateAnomalyMonitor pMonitorARN_ =
  UpdateAnomalyMonitor'
    { monitorName = Lude.Nothing,
      monitorARN = pMonitorARN_
    }

-- | The new name for the cost anomaly monitor.
--
-- /Note:/ Consider using 'monitorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamMonitorName :: Lens.Lens' UpdateAnomalyMonitor (Lude.Maybe Lude.Text)
uamMonitorName = Lens.lens (monitorName :: UpdateAnomalyMonitor -> Lude.Maybe Lude.Text) (\s a -> s {monitorName = a} :: UpdateAnomalyMonitor)
{-# DEPRECATED uamMonitorName "Use generic-lens or generic-optics with 'monitorName' instead." #-}

-- | Cost anomaly monitor Amazon Resource Names (ARNs).
--
-- /Note:/ Consider using 'monitorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamMonitorARN :: Lens.Lens' UpdateAnomalyMonitor Lude.Text
uamMonitorARN = Lens.lens (monitorARN :: UpdateAnomalyMonitor -> Lude.Text) (\s a -> s {monitorARN = a} :: UpdateAnomalyMonitor)
{-# DEPRECATED uamMonitorARN "Use generic-lens or generic-optics with 'monitorARN' instead." #-}

instance Lude.AWSRequest UpdateAnomalyMonitor where
  type Rs UpdateAnomalyMonitor = UpdateAnomalyMonitorResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAnomalyMonitorResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "MonitorArn")
      )

instance Lude.ToHeaders UpdateAnomalyMonitor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.UpdateAnomalyMonitor" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAnomalyMonitor where
  toJSON UpdateAnomalyMonitor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MonitorName" Lude..=) Lude.<$> monitorName,
            Lude.Just ("MonitorArn" Lude..= monitorARN)
          ]
      )

instance Lude.ToPath UpdateAnomalyMonitor where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAnomalyMonitor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAnomalyMonitorResponse' smart constructor.
data UpdateAnomalyMonitorResponse = UpdateAnomalyMonitorResponse'
  { responseStatus ::
      Lude.Int,
    monitorARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAnomalyMonitorResponse' with the minimum fields required to make a request.
--
-- * 'monitorARN' - A cost anomaly monitor ARN.
-- * 'responseStatus' - The response status code.
mkUpdateAnomalyMonitorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'monitorARN'
  Lude.Text ->
  UpdateAnomalyMonitorResponse
mkUpdateAnomalyMonitorResponse pResponseStatus_ pMonitorARN_ =
  UpdateAnomalyMonitorResponse'
    { responseStatus = pResponseStatus_,
      monitorARN = pMonitorARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamrsResponseStatus :: Lens.Lens' UpdateAnomalyMonitorResponse Lude.Int
uamrsResponseStatus = Lens.lens (responseStatus :: UpdateAnomalyMonitorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAnomalyMonitorResponse)
{-# DEPRECATED uamrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A cost anomaly monitor ARN.
--
-- /Note:/ Consider using 'monitorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamrsMonitorARN :: Lens.Lens' UpdateAnomalyMonitorResponse Lude.Text
uamrsMonitorARN = Lens.lens (monitorARN :: UpdateAnomalyMonitorResponse -> Lude.Text) (\s a -> s {monitorARN = a} :: UpdateAnomalyMonitorResponse)
{-# DEPRECATED uamrsMonitorARN "Use generic-lens or generic-optics with 'monitorARN' instead." #-}
