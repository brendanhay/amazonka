{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a schedule that regularly starts Amazon SageMaker Processing Jobs to monitor the data captured for an Amazon SageMaker Endoint.
module Network.AWS.SageMaker.CreateMonitoringSchedule
  ( -- * Creating a request
    CreateMonitoringSchedule (..),
    mkCreateMonitoringSchedule,

    -- ** Request lenses
    cmsTags,
    cmsMonitoringScheduleName,
    cmsMonitoringScheduleConfig,

    -- * Destructuring the response
    CreateMonitoringScheduleResponse (..),
    mkCreateMonitoringScheduleResponse,

    -- ** Response lenses
    cmsrsResponseStatus,
    cmsrsMonitoringScheduleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateMonitoringSchedule' smart constructor.
data CreateMonitoringSchedule = CreateMonitoringSchedule'
  { tags ::
      Lude.Maybe [Tag],
    monitoringScheduleName :: Lude.Text,
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

-- | Creates a value of 'CreateMonitoringSchedule' with the minimum fields required to make a request.
--
-- * 'monitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and defines the monitoring job.
-- * 'monitoringScheduleName' - The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
-- * 'tags' - (Optional) An array of key-value pairs. For more information, see < https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
mkCreateMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Lude.Text ->
  -- | 'monitoringScheduleConfig'
  MonitoringScheduleConfig ->
  CreateMonitoringSchedule
mkCreateMonitoringSchedule
  pMonitoringScheduleName_
  pMonitoringScheduleConfig_ =
    CreateMonitoringSchedule'
      { tags = Lude.Nothing,
        monitoringScheduleName = pMonitoringScheduleName_,
        monitoringScheduleConfig = pMonitoringScheduleConfig_
      }

-- | (Optional) An array of key-value pairs. For more information, see < https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsTags :: Lens.Lens' CreateMonitoringSchedule (Lude.Maybe [Tag])
cmsTags = Lens.lens (tags :: CreateMonitoringSchedule -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateMonitoringSchedule)
{-# DEPRECATED cmsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsMonitoringScheduleName :: Lens.Lens' CreateMonitoringSchedule Lude.Text
cmsMonitoringScheduleName = Lens.lens (monitoringScheduleName :: CreateMonitoringSchedule -> Lude.Text) (\s a -> s {monitoringScheduleName = a} :: CreateMonitoringSchedule)
{-# DEPRECATED cmsMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

-- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
--
-- /Note:/ Consider using 'monitoringScheduleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsMonitoringScheduleConfig :: Lens.Lens' CreateMonitoringSchedule MonitoringScheduleConfig
cmsMonitoringScheduleConfig = Lens.lens (monitoringScheduleConfig :: CreateMonitoringSchedule -> MonitoringScheduleConfig) (\s a -> s {monitoringScheduleConfig = a} :: CreateMonitoringSchedule)
{-# DEPRECATED cmsMonitoringScheduleConfig "Use generic-lens or generic-optics with 'monitoringScheduleConfig' instead." #-}

instance Lude.AWSRequest CreateMonitoringSchedule where
  type Rs CreateMonitoringSchedule = CreateMonitoringScheduleResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMonitoringScheduleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "MonitoringScheduleArn")
      )

instance Lude.ToHeaders CreateMonitoringSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateMonitoringSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMonitoringSchedule where
  toJSON CreateMonitoringSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just
              ("MonitoringScheduleName" Lude..= monitoringScheduleName),
            Lude.Just
              ("MonitoringScheduleConfig" Lude..= monitoringScheduleConfig)
          ]
      )

instance Lude.ToPath CreateMonitoringSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateMonitoringSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateMonitoringScheduleResponse' smart constructor.
data CreateMonitoringScheduleResponse = CreateMonitoringScheduleResponse'
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

-- | Creates a value of 'CreateMonitoringScheduleResponse' with the minimum fields required to make a request.
--
-- * 'monitoringScheduleARN' - The Amazon Resource Name (ARN) of the monitoring schedule.
-- * 'responseStatus' - The response status code.
mkCreateMonitoringScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'monitoringScheduleARN'
  Lude.Text ->
  CreateMonitoringScheduleResponse
mkCreateMonitoringScheduleResponse
  pResponseStatus_
  pMonitoringScheduleARN_ =
    CreateMonitoringScheduleResponse'
      { responseStatus =
          pResponseStatus_,
        monitoringScheduleARN = pMonitoringScheduleARN_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsrsResponseStatus :: Lens.Lens' CreateMonitoringScheduleResponse Lude.Int
cmsrsResponseStatus = Lens.lens (responseStatus :: CreateMonitoringScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMonitoringScheduleResponse)
{-# DEPRECATED cmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsrsMonitoringScheduleARN :: Lens.Lens' CreateMonitoringScheduleResponse Lude.Text
cmsrsMonitoringScheduleARN = Lens.lens (monitoringScheduleARN :: CreateMonitoringScheduleResponse -> Lude.Text) (\s a -> s {monitoringScheduleARN = a} :: CreateMonitoringScheduleResponse)
{-# DEPRECATED cmsrsMonitoringScheduleARN "Use generic-lens or generic-optics with 'monitoringScheduleARN' instead." #-}
