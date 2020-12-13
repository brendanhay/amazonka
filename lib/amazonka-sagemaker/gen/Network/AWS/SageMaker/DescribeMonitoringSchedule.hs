{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the schedule for a monitoring job.
module Network.AWS.SageMaker.DescribeMonitoringSchedule
  ( -- * Creating a request
    DescribeMonitoringSchedule (..),
    mkDescribeMonitoringSchedule,

    -- ** Request lenses
    dmsMonitoringScheduleName,

    -- * Destructuring the response
    DescribeMonitoringScheduleResponse (..),
    mkDescribeMonitoringScheduleResponse,

    -- ** Response lenses
    dmsrsCreationTime,
    dmsrsFailureReason,
    dmsrsMonitoringScheduleARN,
    dmsrsEndpointName,
    dmsrsLastModifiedTime,
    dmsrsMonitoringScheduleStatus,
    dmsrsLastMonitoringExecutionSummary,
    dmsrsMonitoringScheduleConfig,
    dmsrsMonitoringScheduleName,
    dmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeMonitoringSchedule' smart constructor.
newtype DescribeMonitoringSchedule = DescribeMonitoringSchedule'
  { -- | Name of a previously created monitoring schedule.
    monitoringScheduleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMonitoringSchedule' with the minimum fields required to make a request.
--
-- * 'monitoringScheduleName' - Name of a previously created monitoring schedule.
mkDescribeMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Lude.Text ->
  DescribeMonitoringSchedule
mkDescribeMonitoringSchedule pMonitoringScheduleName_ =
  DescribeMonitoringSchedule'
    { monitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | Name of a previously created monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsMonitoringScheduleName :: Lens.Lens' DescribeMonitoringSchedule Lude.Text
dmsMonitoringScheduleName = Lens.lens (monitoringScheduleName :: DescribeMonitoringSchedule -> Lude.Text) (\s a -> s {monitoringScheduleName = a} :: DescribeMonitoringSchedule)
{-# DEPRECATED dmsMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

instance Lude.AWSRequest DescribeMonitoringSchedule where
  type
    Rs DescribeMonitoringSchedule =
      DescribeMonitoringScheduleResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMonitoringScheduleResponse'
            Lude.<$> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..:> "MonitoringScheduleArn")
            Lude.<*> (x Lude..?> "EndpointName")
            Lude.<*> (x Lude..:> "LastModifiedTime")
            Lude.<*> (x Lude..:> "MonitoringScheduleStatus")
            Lude.<*> (x Lude..?> "LastMonitoringExecutionSummary")
            Lude.<*> (x Lude..:> "MonitoringScheduleConfig")
            Lude.<*> (x Lude..:> "MonitoringScheduleName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMonitoringSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeMonitoringSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMonitoringSchedule where
  toJSON DescribeMonitoringSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("MonitoringScheduleName" Lude..= monitoringScheduleName)
          ]
      )

instance Lude.ToPath DescribeMonitoringSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMonitoringSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMonitoringScheduleResponse' smart constructor.
data DescribeMonitoringScheduleResponse = DescribeMonitoringScheduleResponse'
  { -- | The time at which the monitoring job was created.
    creationTime :: Lude.Timestamp,
    -- | A string, up to one KB in size, that contains the reason a monitoring job failed, if it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleARN :: Lude.Text,
    -- | The name of the endpoint for the monitoring job.
    endpointName :: Lude.Maybe Lude.Text,
    -- | The time at which the monitoring job was last modified.
    lastModifiedTime :: Lude.Timestamp,
    -- | The status of an monitoring job.
    monitoringScheduleStatus :: ScheduleStatus,
    -- | Describes metadata on the last execution to run, if there was one.
    lastMonitoringExecutionSummary :: Lude.Maybe MonitoringExecutionSummary,
    -- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
    monitoringScheduleConfig :: MonitoringScheduleConfig,
    -- | Name of the monitoring schedule.
    monitoringScheduleName :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMonitoringScheduleResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time at which the monitoring job was created.
-- * 'failureReason' - A string, up to one KB in size, that contains the reason a monitoring job failed, if it failed.
-- * 'monitoringScheduleARN' - The Amazon Resource Name (ARN) of the monitoring schedule.
-- * 'endpointName' - The name of the endpoint for the monitoring job.
-- * 'lastModifiedTime' - The time at which the monitoring job was last modified.
-- * 'monitoringScheduleStatus' - The status of an monitoring job.
-- * 'lastMonitoringExecutionSummary' - Describes metadata on the last execution to run, if there was one.
-- * 'monitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and defines the monitoring job.
-- * 'monitoringScheduleName' - Name of the monitoring schedule.
-- * 'responseStatus' - The response status code.
mkDescribeMonitoringScheduleResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'monitoringScheduleARN'
  Lude.Text ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'monitoringScheduleStatus'
  ScheduleStatus ->
  -- | 'monitoringScheduleConfig'
  MonitoringScheduleConfig ->
  -- | 'monitoringScheduleName'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMonitoringScheduleResponse
mkDescribeMonitoringScheduleResponse
  pCreationTime_
  pMonitoringScheduleARN_
  pLastModifiedTime_
  pMonitoringScheduleStatus_
  pMonitoringScheduleConfig_
  pMonitoringScheduleName_
  pResponseStatus_ =
    DescribeMonitoringScheduleResponse'
      { creationTime =
          pCreationTime_,
        failureReason = Lude.Nothing,
        monitoringScheduleARN = pMonitoringScheduleARN_,
        endpointName = Lude.Nothing,
        lastModifiedTime = pLastModifiedTime_,
        monitoringScheduleStatus = pMonitoringScheduleStatus_,
        lastMonitoringExecutionSummary = Lude.Nothing,
        monitoringScheduleConfig = pMonitoringScheduleConfig_,
        monitoringScheduleName = pMonitoringScheduleName_,
        responseStatus = pResponseStatus_
      }

-- | The time at which the monitoring job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsCreationTime :: Lens.Lens' DescribeMonitoringScheduleResponse Lude.Timestamp
dmsrsCreationTime = Lens.lens (creationTime :: DescribeMonitoringScheduleResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A string, up to one KB in size, that contains the reason a monitoring job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsFailureReason :: Lens.Lens' DescribeMonitoringScheduleResponse (Lude.Maybe Lude.Text)
dmsrsFailureReason = Lens.lens (failureReason :: DescribeMonitoringScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsMonitoringScheduleARN :: Lens.Lens' DescribeMonitoringScheduleResponse Lude.Text
dmsrsMonitoringScheduleARN = Lens.lens (monitoringScheduleARN :: DescribeMonitoringScheduleResponse -> Lude.Text) (\s a -> s {monitoringScheduleARN = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsMonitoringScheduleARN "Use generic-lens or generic-optics with 'monitoringScheduleARN' instead." #-}

-- | The name of the endpoint for the monitoring job.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsEndpointName :: Lens.Lens' DescribeMonitoringScheduleResponse (Lude.Maybe Lude.Text)
dmsrsEndpointName = Lens.lens (endpointName :: DescribeMonitoringScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {endpointName = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The time at which the monitoring job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsLastModifiedTime :: Lens.Lens' DescribeMonitoringScheduleResponse Lude.Timestamp
dmsrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeMonitoringScheduleResponse -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status of an monitoring job.
--
-- /Note:/ Consider using 'monitoringScheduleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsMonitoringScheduleStatus :: Lens.Lens' DescribeMonitoringScheduleResponse ScheduleStatus
dmsrsMonitoringScheduleStatus = Lens.lens (monitoringScheduleStatus :: DescribeMonitoringScheduleResponse -> ScheduleStatus) (\s a -> s {monitoringScheduleStatus = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsMonitoringScheduleStatus "Use generic-lens or generic-optics with 'monitoringScheduleStatus' instead." #-}

-- | Describes metadata on the last execution to run, if there was one.
--
-- /Note:/ Consider using 'lastMonitoringExecutionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsLastMonitoringExecutionSummary :: Lens.Lens' DescribeMonitoringScheduleResponse (Lude.Maybe MonitoringExecutionSummary)
dmsrsLastMonitoringExecutionSummary = Lens.lens (lastMonitoringExecutionSummary :: DescribeMonitoringScheduleResponse -> Lude.Maybe MonitoringExecutionSummary) (\s a -> s {lastMonitoringExecutionSummary = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsLastMonitoringExecutionSummary "Use generic-lens or generic-optics with 'lastMonitoringExecutionSummary' instead." #-}

-- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
--
-- /Note:/ Consider using 'monitoringScheduleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsMonitoringScheduleConfig :: Lens.Lens' DescribeMonitoringScheduleResponse MonitoringScheduleConfig
dmsrsMonitoringScheduleConfig = Lens.lens (monitoringScheduleConfig :: DescribeMonitoringScheduleResponse -> MonitoringScheduleConfig) (\s a -> s {monitoringScheduleConfig = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsMonitoringScheduleConfig "Use generic-lens or generic-optics with 'monitoringScheduleConfig' instead." #-}

-- | Name of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsMonitoringScheduleName :: Lens.Lens' DescribeMonitoringScheduleResponse Lude.Text
dmsrsMonitoringScheduleName = Lens.lens (monitoringScheduleName :: DescribeMonitoringScheduleResponse -> Lude.Text) (\s a -> s {monitoringScheduleName = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsResponseStatus :: Lens.Lens' DescribeMonitoringScheduleResponse Lude.Int
dmsrsResponseStatus = Lens.lens (responseStatus :: DescribeMonitoringScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMonitoringScheduleResponse)
{-# DEPRECATED dmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
