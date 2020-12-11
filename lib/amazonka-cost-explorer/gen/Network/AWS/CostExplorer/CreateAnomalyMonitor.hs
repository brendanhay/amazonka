{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.CreateAnomalyMonitor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cost anomaly detection monitor with the requested type and monitor specification.
module Network.AWS.CostExplorer.CreateAnomalyMonitor
  ( -- * Creating a request
    CreateAnomalyMonitor (..),
    mkCreateAnomalyMonitor,

    -- ** Request lenses
    camAnomalyMonitor,

    -- * Destructuring the response
    CreateAnomalyMonitorResponse (..),
    mkCreateAnomalyMonitorResponse,

    -- ** Response lenses
    camrsResponseStatus,
    camrsMonitorARN,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAnomalyMonitor' smart constructor.
newtype CreateAnomalyMonitor = CreateAnomalyMonitor'
  { anomalyMonitor ::
      AnomalyMonitor
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAnomalyMonitor' with the minimum fields required to make a request.
--
-- * 'anomalyMonitor' - The cost anomaly detection monitor object that you want to create.
mkCreateAnomalyMonitor ::
  -- | 'anomalyMonitor'
  AnomalyMonitor ->
  CreateAnomalyMonitor
mkCreateAnomalyMonitor pAnomalyMonitor_ =
  CreateAnomalyMonitor' {anomalyMonitor = pAnomalyMonitor_}

-- | The cost anomaly detection monitor object that you want to create.
--
-- /Note:/ Consider using 'anomalyMonitor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camAnomalyMonitor :: Lens.Lens' CreateAnomalyMonitor AnomalyMonitor
camAnomalyMonitor = Lens.lens (anomalyMonitor :: CreateAnomalyMonitor -> AnomalyMonitor) (\s a -> s {anomalyMonitor = a} :: CreateAnomalyMonitor)
{-# DEPRECATED camAnomalyMonitor "Use generic-lens or generic-optics with 'anomalyMonitor' instead." #-}

instance Lude.AWSRequest CreateAnomalyMonitor where
  type Rs CreateAnomalyMonitor = CreateAnomalyMonitorResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAnomalyMonitorResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "MonitorArn")
      )

instance Lude.ToHeaders CreateAnomalyMonitor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.CreateAnomalyMonitor" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAnomalyMonitor where
  toJSON CreateAnomalyMonitor' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AnomalyMonitor" Lude..= anomalyMonitor)]
      )

instance Lude.ToPath CreateAnomalyMonitor where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAnomalyMonitor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAnomalyMonitorResponse' smart constructor.
data CreateAnomalyMonitorResponse = CreateAnomalyMonitorResponse'
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

-- | Creates a value of 'CreateAnomalyMonitorResponse' with the minimum fields required to make a request.
--
-- * 'monitorARN' - The unique identifier of your newly created cost anomaly detection monitor.
-- * 'responseStatus' - The response status code.
mkCreateAnomalyMonitorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'monitorARN'
  Lude.Text ->
  CreateAnomalyMonitorResponse
mkCreateAnomalyMonitorResponse pResponseStatus_ pMonitorARN_ =
  CreateAnomalyMonitorResponse'
    { responseStatus = pResponseStatus_,
      monitorARN = pMonitorARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camrsResponseStatus :: Lens.Lens' CreateAnomalyMonitorResponse Lude.Int
camrsResponseStatus = Lens.lens (responseStatus :: CreateAnomalyMonitorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAnomalyMonitorResponse)
{-# DEPRECATED camrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The unique identifier of your newly created cost anomaly detection monitor.
--
-- /Note:/ Consider using 'monitorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camrsMonitorARN :: Lens.Lens' CreateAnomalyMonitorResponse Lude.Text
camrsMonitorARN = Lens.lens (monitorARN :: CreateAnomalyMonitorResponse -> Lude.Text) (\s a -> s {monitorARN = a} :: CreateAnomalyMonitorResponse)
{-# DEPRECATED camrsMonitorARN "Use generic-lens or generic-optics with 'monitorARN' instead." #-}
