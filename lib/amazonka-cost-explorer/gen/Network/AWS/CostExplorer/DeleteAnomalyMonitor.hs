{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.DeleteAnomalyMonitor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cost anomaly monitor.
module Network.AWS.CostExplorer.DeleteAnomalyMonitor
  ( -- * Creating a request
    DeleteAnomalyMonitor (..),
    mkDeleteAnomalyMonitor,

    -- ** Request lenses
    damMonitorARN,

    -- * Destructuring the response
    DeleteAnomalyMonitorResponse (..),
    mkDeleteAnomalyMonitorResponse,

    -- ** Response lenses
    damrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAnomalyMonitor' smart constructor.
newtype DeleteAnomalyMonitor = DeleteAnomalyMonitor'
  { monitorARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAnomalyMonitor' with the minimum fields required to make a request.
--
-- * 'monitorARN' - The unique identifier of the cost anomaly monitor that you want to delete.
mkDeleteAnomalyMonitor ::
  -- | 'monitorARN'
  Lude.Text ->
  DeleteAnomalyMonitor
mkDeleteAnomalyMonitor pMonitorARN_ =
  DeleteAnomalyMonitor' {monitorARN = pMonitorARN_}

-- | The unique identifier of the cost anomaly monitor that you want to delete.
--
-- /Note:/ Consider using 'monitorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damMonitorARN :: Lens.Lens' DeleteAnomalyMonitor Lude.Text
damMonitorARN = Lens.lens (monitorARN :: DeleteAnomalyMonitor -> Lude.Text) (\s a -> s {monitorARN = a} :: DeleteAnomalyMonitor)
{-# DEPRECATED damMonitorARN "Use generic-lens or generic-optics with 'monitorARN' instead." #-}

instance Lude.AWSRequest DeleteAnomalyMonitor where
  type Rs DeleteAnomalyMonitor = DeleteAnomalyMonitorResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAnomalyMonitorResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAnomalyMonitor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.DeleteAnomalyMonitor" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAnomalyMonitor where
  toJSON DeleteAnomalyMonitor' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("MonitorArn" Lude..= monitorARN)])

instance Lude.ToPath DeleteAnomalyMonitor where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAnomalyMonitor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAnomalyMonitorResponse' smart constructor.
newtype DeleteAnomalyMonitorResponse = DeleteAnomalyMonitorResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAnomalyMonitorResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAnomalyMonitorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAnomalyMonitorResponse
mkDeleteAnomalyMonitorResponse pResponseStatus_ =
  DeleteAnomalyMonitorResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrsResponseStatus :: Lens.Lens' DeleteAnomalyMonitorResponse Lude.Int
damrsResponseStatus = Lens.lens (responseStatus :: DeleteAnomalyMonitorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAnomalyMonitorResponse)
{-# DEPRECATED damrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
