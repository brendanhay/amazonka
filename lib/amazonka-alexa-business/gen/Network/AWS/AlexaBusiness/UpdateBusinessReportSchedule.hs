{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of the report delivery schedule with the specified schedule ARN.
module Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
  ( -- * Creating a request
    UpdateBusinessReportSchedule (..),
    mkUpdateBusinessReportSchedule,

    -- ** Request lenses
    ubrsS3KeyPrefix,
    ubrsFormat,
    ubrsRecurrence,
    ubrsScheduleName,
    ubrsS3BucketName,
    ubrsScheduleARN,

    -- * Destructuring the response
    UpdateBusinessReportScheduleResponse (..),
    mkUpdateBusinessReportScheduleResponse,

    -- ** Response lenses
    ubrsrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateBusinessReportSchedule' smart constructor.
data UpdateBusinessReportSchedule = UpdateBusinessReportSchedule'
  { s3KeyPrefix ::
      Lude.Maybe Lude.Text,
    format ::
      Lude.Maybe BusinessReportFormat,
    recurrence ::
      Lude.Maybe
        BusinessReportRecurrence,
    scheduleName ::
      Lude.Maybe Lude.Text,
    s3BucketName ::
      Lude.Maybe Lude.Text,
    scheduleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBusinessReportSchedule' with the minimum fields required to make a request.
--
-- * 'format' - The format of the generated report (individual CSV files or zipped files of individual files).
-- * 'recurrence' - The recurrence of the reports.
-- * 's3BucketName' - The S3 location of the output reports.
-- * 's3KeyPrefix' - The S3 key where the report is delivered.
-- * 'scheduleARN' - The ARN of the business report schedule.
-- * 'scheduleName' - The name identifier of the schedule.
mkUpdateBusinessReportSchedule ::
  -- | 'scheduleARN'
  Lude.Text ->
  UpdateBusinessReportSchedule
mkUpdateBusinessReportSchedule pScheduleARN_ =
  UpdateBusinessReportSchedule'
    { s3KeyPrefix = Lude.Nothing,
      format = Lude.Nothing,
      recurrence = Lude.Nothing,
      scheduleName = Lude.Nothing,
      s3BucketName = Lude.Nothing,
      scheduleARN = pScheduleARN_
    }

-- | The S3 key where the report is delivered.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsS3KeyPrefix :: Lens.Lens' UpdateBusinessReportSchedule (Lude.Maybe Lude.Text)
ubrsS3KeyPrefix = Lens.lens (s3KeyPrefix :: UpdateBusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: UpdateBusinessReportSchedule)
{-# DEPRECATED ubrsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The format of the generated report (individual CSV files or zipped files of individual files).
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsFormat :: Lens.Lens' UpdateBusinessReportSchedule (Lude.Maybe BusinessReportFormat)
ubrsFormat = Lens.lens (format :: UpdateBusinessReportSchedule -> Lude.Maybe BusinessReportFormat) (\s a -> s {format = a} :: UpdateBusinessReportSchedule)
{-# DEPRECATED ubrsFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The recurrence of the reports.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsRecurrence :: Lens.Lens' UpdateBusinessReportSchedule (Lude.Maybe BusinessReportRecurrence)
ubrsRecurrence = Lens.lens (recurrence :: UpdateBusinessReportSchedule -> Lude.Maybe BusinessReportRecurrence) (\s a -> s {recurrence = a} :: UpdateBusinessReportSchedule)
{-# DEPRECATED ubrsRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The name identifier of the schedule.
--
-- /Note:/ Consider using 'scheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsScheduleName :: Lens.Lens' UpdateBusinessReportSchedule (Lude.Maybe Lude.Text)
ubrsScheduleName = Lens.lens (scheduleName :: UpdateBusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleName = a} :: UpdateBusinessReportSchedule)
{-# DEPRECATED ubrsScheduleName "Use generic-lens or generic-optics with 'scheduleName' instead." #-}

-- | The S3 location of the output reports.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsS3BucketName :: Lens.Lens' UpdateBusinessReportSchedule (Lude.Maybe Lude.Text)
ubrsS3BucketName = Lens.lens (s3BucketName :: UpdateBusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: UpdateBusinessReportSchedule)
{-# DEPRECATED ubrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The ARN of the business report schedule.
--
-- /Note:/ Consider using 'scheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsScheduleARN :: Lens.Lens' UpdateBusinessReportSchedule Lude.Text
ubrsScheduleARN = Lens.lens (scheduleARN :: UpdateBusinessReportSchedule -> Lude.Text) (\s a -> s {scheduleARN = a} :: UpdateBusinessReportSchedule)
{-# DEPRECATED ubrsScheduleARN "Use generic-lens or generic-optics with 'scheduleARN' instead." #-}

instance Lude.AWSRequest UpdateBusinessReportSchedule where
  type
    Rs UpdateBusinessReportSchedule =
      UpdateBusinessReportScheduleResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateBusinessReportScheduleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBusinessReportSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.UpdateBusinessReportSchedule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateBusinessReportSchedule where
  toJSON UpdateBusinessReportSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3KeyPrefix" Lude..=) Lude.<$> s3KeyPrefix,
            ("Format" Lude..=) Lude.<$> format,
            ("Recurrence" Lude..=) Lude.<$> recurrence,
            ("ScheduleName" Lude..=) Lude.<$> scheduleName,
            ("S3BucketName" Lude..=) Lude.<$> s3BucketName,
            Lude.Just ("ScheduleArn" Lude..= scheduleARN)
          ]
      )

instance Lude.ToPath UpdateBusinessReportSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateBusinessReportSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateBusinessReportScheduleResponse' smart constructor.
newtype UpdateBusinessReportScheduleResponse = UpdateBusinessReportScheduleResponse'
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

-- | Creates a value of 'UpdateBusinessReportScheduleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateBusinessReportScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBusinessReportScheduleResponse
mkUpdateBusinessReportScheduleResponse pResponseStatus_ =
  UpdateBusinessReportScheduleResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsrsResponseStatus :: Lens.Lens' UpdateBusinessReportScheduleResponse Lude.Int
ubrsrsResponseStatus = Lens.lens (responseStatus :: UpdateBusinessReportScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBusinessReportScheduleResponse)
{-# DEPRECATED ubrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
