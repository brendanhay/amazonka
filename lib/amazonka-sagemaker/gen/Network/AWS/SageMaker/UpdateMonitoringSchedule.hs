{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created schedule.
module Network.AWS.SageMaker.UpdateMonitoringSchedule
  ( -- * Creating a request
    UpdateMonitoringSchedule (..),
    mkUpdateMonitoringSchedule,

    -- ** Request lenses
    umsMonitoringScheduleName,
    umsMonitoringScheduleConfig,

    -- * Destructuring the response
    UpdateMonitoringScheduleResponse (..),
    mkUpdateMonitoringScheduleResponse,

    -- ** Response lenses
    umsrsResponseStatus,
    umsrsMonitoringScheduleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateMonitoringSchedule' smart constructor.
data UpdateMonitoringSchedule = UpdateMonitoringSchedule'
  { monitoringScheduleName ::
      Lude.Text,
    monitoringScheduleConfig ::
      MonitoringScheduleConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMonitoringSchedule' with the minimum fields required to make a request.
--
-- * 'monitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and defines the monitoring job.
-- * 'monitoringScheduleName' - The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
mkUpdateMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Lude.Text ->
  -- | 'monitoringScheduleConfig'
  MonitoringScheduleConfig ->
  UpdateMonitoringSchedule
mkUpdateMonitoringSchedule
  pMonitoringScheduleName_
  pMonitoringScheduleConfig_ =
    UpdateMonitoringSchedule'
      { monitoringScheduleName =
          pMonitoringScheduleName_,
        monitoringScheduleConfig = pMonitoringScheduleConfig_
      }

-- | The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsMonitoringScheduleName :: Lens.Lens' UpdateMonitoringSchedule Lude.Text
umsMonitoringScheduleName = Lens.lens (monitoringScheduleName :: UpdateMonitoringSchedule -> Lude.Text) (\s a -> s {monitoringScheduleName = a} :: UpdateMonitoringSchedule)
{-# DEPRECATED umsMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

-- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
--
-- /Note:/ Consider using 'monitoringScheduleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsMonitoringScheduleConfig :: Lens.Lens' UpdateMonitoringSchedule MonitoringScheduleConfig
umsMonitoringScheduleConfig = Lens.lens (monitoringScheduleConfig :: UpdateMonitoringSchedule -> MonitoringScheduleConfig) (\s a -> s {monitoringScheduleConfig = a} :: UpdateMonitoringSchedule)
{-# DEPRECATED umsMonitoringScheduleConfig "Use generic-lens or generic-optics with 'monitoringScheduleConfig' instead." #-}

instance Lude.AWSRequest UpdateMonitoringSchedule where
  type Rs UpdateMonitoringSchedule = UpdateMonitoringScheduleResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMonitoringScheduleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "MonitoringScheduleArn")
      )

instance Lude.ToHeaders UpdateMonitoringSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateMonitoringSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMonitoringSchedule where
  toJSON UpdateMonitoringSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("MonitoringScheduleName" Lude..= monitoringScheduleName),
            Lude.Just
              ("MonitoringScheduleConfig" Lude..= monitoringScheduleConfig)
          ]
      )

instance Lude.ToPath UpdateMonitoringSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMonitoringSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMonitoringScheduleResponse' smart constructor.
data UpdateMonitoringScheduleResponse = UpdateMonitoringScheduleResponse'
  { responseStatus ::
      Lude.Int,
    monitoringScheduleARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMonitoringScheduleResponse' with the minimum fields required to make a request.
--
-- * 'monitoringScheduleARN' - The Amazon Resource Name (ARN) of the monitoring schedule.
-- * 'responseStatus' - The response status code.
mkUpdateMonitoringScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'monitoringScheduleARN'
  Lude.Text ->
  UpdateMonitoringScheduleResponse
mkUpdateMonitoringScheduleResponse
  pResponseStatus_
  pMonitoringScheduleARN_ =
    UpdateMonitoringScheduleResponse'
      { responseStatus =
          pResponseStatus_,
        monitoringScheduleARN = pMonitoringScheduleARN_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsrsResponseStatus :: Lens.Lens' UpdateMonitoringScheduleResponse Lude.Int
umsrsResponseStatus = Lens.lens (responseStatus :: UpdateMonitoringScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMonitoringScheduleResponse)
{-# DEPRECATED umsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsrsMonitoringScheduleARN :: Lens.Lens' UpdateMonitoringScheduleResponse Lude.Text
umsrsMonitoringScheduleARN = Lens.lens (monitoringScheduleARN :: UpdateMonitoringScheduleResponse -> Lude.Text) (\s a -> s {monitoringScheduleARN = a} :: UpdateMonitoringScheduleResponse)
{-# DEPRECATED umsrsMonitoringScheduleARN "Use generic-lens or generic-optics with 'monitoringScheduleARN' instead." #-}
