{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a recurring schedule for usage reports to deliver to the specified S3 location with a specified daily or weekly interval.
module Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
  ( -- * Creating a request
    CreateBusinessReportSchedule (..),
    mkCreateBusinessReportSchedule,

    -- ** Request lenses
    cbrsS3KeyPrefix,
    cbrsRecurrence,
    cbrsScheduleName,
    cbrsClientRequestToken,
    cbrsS3BucketName,
    cbrsTags,
    cbrsFormat,
    cbrsContentRange,

    -- * Destructuring the response
    CreateBusinessReportScheduleResponse (..),
    mkCreateBusinessReportScheduleResponse,

    -- ** Response lenses
    cbrsrsScheduleARN,
    cbrsrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateBusinessReportSchedule' smart constructor.
data CreateBusinessReportSchedule = CreateBusinessReportSchedule'
  { s3KeyPrefix ::
      Lude.Maybe Lude.Text,
    recurrence ::
      Lude.Maybe
        BusinessReportRecurrence,
    scheduleName ::
      Lude.Maybe Lude.Text,
    clientRequestToken ::
      Lude.Maybe Lude.Text,
    s3BucketName ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    format :: BusinessReportFormat,
    contentRange ::
      BusinessReportContentRange
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBusinessReportSchedule' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - The client request token.
-- * 'contentRange' - The content range of the reports.
-- * 'format' - The format of the generated report (individual CSV files or zipped files of individual files).
-- * 'recurrence' - The recurrence of the reports. If this isn't specified, the report will only be delivered one time when the API is called.
-- * 's3BucketName' - The S3 bucket name of the output reports. If this isn't specified, the report can be retrieved from a download link by calling ListBusinessReportSchedule.
-- * 's3KeyPrefix' - The S3 key where the report is delivered.
-- * 'scheduleName' - The name identifier of the schedule.
-- * 'tags' - The tags for the business report schedule.
mkCreateBusinessReportSchedule ::
  -- | 'format'
  BusinessReportFormat ->
  -- | 'contentRange'
  BusinessReportContentRange ->
  CreateBusinessReportSchedule
mkCreateBusinessReportSchedule pFormat_ pContentRange_ =
  CreateBusinessReportSchedule'
    { s3KeyPrefix = Lude.Nothing,
      recurrence = Lude.Nothing,
      scheduleName = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      s3BucketName = Lude.Nothing,
      tags = Lude.Nothing,
      format = pFormat_,
      contentRange = pContentRange_
    }

-- | The S3 key where the report is delivered.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsS3KeyPrefix :: Lens.Lens' CreateBusinessReportSchedule (Lude.Maybe Lude.Text)
cbrsS3KeyPrefix = Lens.lens (s3KeyPrefix :: CreateBusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: CreateBusinessReportSchedule)
{-# DEPRECATED cbrsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The recurrence of the reports. If this isn't specified, the report will only be delivered one time when the API is called.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsRecurrence :: Lens.Lens' CreateBusinessReportSchedule (Lude.Maybe BusinessReportRecurrence)
cbrsRecurrence = Lens.lens (recurrence :: CreateBusinessReportSchedule -> Lude.Maybe BusinessReportRecurrence) (\s a -> s {recurrence = a} :: CreateBusinessReportSchedule)
{-# DEPRECATED cbrsRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The name identifier of the schedule.
--
-- /Note:/ Consider using 'scheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsScheduleName :: Lens.Lens' CreateBusinessReportSchedule (Lude.Maybe Lude.Text)
cbrsScheduleName = Lens.lens (scheduleName :: CreateBusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleName = a} :: CreateBusinessReportSchedule)
{-# DEPRECATED cbrsScheduleName "Use generic-lens or generic-optics with 'scheduleName' instead." #-}

-- | The client request token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsClientRequestToken :: Lens.Lens' CreateBusinessReportSchedule (Lude.Maybe Lude.Text)
cbrsClientRequestToken = Lens.lens (clientRequestToken :: CreateBusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateBusinessReportSchedule)
{-# DEPRECATED cbrsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The S3 bucket name of the output reports. If this isn't specified, the report can be retrieved from a download link by calling ListBusinessReportSchedule.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsS3BucketName :: Lens.Lens' CreateBusinessReportSchedule (Lude.Maybe Lude.Text)
cbrsS3BucketName = Lens.lens (s3BucketName :: CreateBusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: CreateBusinessReportSchedule)
{-# DEPRECATED cbrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The tags for the business report schedule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsTags :: Lens.Lens' CreateBusinessReportSchedule (Lude.Maybe [Tag])
cbrsTags = Lens.lens (tags :: CreateBusinessReportSchedule -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateBusinessReportSchedule)
{-# DEPRECATED cbrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The format of the generated report (individual CSV files or zipped files of individual files).
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsFormat :: Lens.Lens' CreateBusinessReportSchedule BusinessReportFormat
cbrsFormat = Lens.lens (format :: CreateBusinessReportSchedule -> BusinessReportFormat) (\s a -> s {format = a} :: CreateBusinessReportSchedule)
{-# DEPRECATED cbrsFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The content range of the reports.
--
-- /Note:/ Consider using 'contentRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsContentRange :: Lens.Lens' CreateBusinessReportSchedule BusinessReportContentRange
cbrsContentRange = Lens.lens (contentRange :: CreateBusinessReportSchedule -> BusinessReportContentRange) (\s a -> s {contentRange = a} :: CreateBusinessReportSchedule)
{-# DEPRECATED cbrsContentRange "Use generic-lens or generic-optics with 'contentRange' instead." #-}

instance Lude.AWSRequest CreateBusinessReportSchedule where
  type
    Rs CreateBusinessReportSchedule =
      CreateBusinessReportScheduleResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBusinessReportScheduleResponse'
            Lude.<$> (x Lude..?> "ScheduleArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBusinessReportSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.CreateBusinessReportSchedule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBusinessReportSchedule where
  toJSON CreateBusinessReportSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3KeyPrefix" Lude..=) Lude.<$> s3KeyPrefix,
            ("Recurrence" Lude..=) Lude.<$> recurrence,
            ("ScheduleName" Lude..=) Lude.<$> scheduleName,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("S3BucketName" Lude..=) Lude.<$> s3BucketName,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Format" Lude..= format),
            Lude.Just ("ContentRange" Lude..= contentRange)
          ]
      )

instance Lude.ToPath CreateBusinessReportSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateBusinessReportSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBusinessReportScheduleResponse' smart constructor.
data CreateBusinessReportScheduleResponse = CreateBusinessReportScheduleResponse'
  { scheduleARN ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBusinessReportScheduleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'scheduleARN' - The ARN of the business report schedule.
mkCreateBusinessReportScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBusinessReportScheduleResponse
mkCreateBusinessReportScheduleResponse pResponseStatus_ =
  CreateBusinessReportScheduleResponse'
    { scheduleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the business report schedule.
--
-- /Note:/ Consider using 'scheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsrsScheduleARN :: Lens.Lens' CreateBusinessReportScheduleResponse (Lude.Maybe Lude.Text)
cbrsrsScheduleARN = Lens.lens (scheduleARN :: CreateBusinessReportScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduleARN = a} :: CreateBusinessReportScheduleResponse)
{-# DEPRECATED cbrsrsScheduleARN "Use generic-lens or generic-optics with 'scheduleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsrsResponseStatus :: Lens.Lens' CreateBusinessReportScheduleResponse Lude.Int
cbrsrsResponseStatus = Lens.lens (responseStatus :: CreateBusinessReportScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBusinessReportScheduleResponse)
{-# DEPRECATED cbrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
