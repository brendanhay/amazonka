{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a recurring schedule for usage reports to deliver to the
-- specified S3 location with a specified daily or weekly interval.
module Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
  ( -- * Creating a Request
    CreateBusinessReportSchedule (..),
    newCreateBusinessReportSchedule,

    -- * Request Lenses
    createBusinessReportSchedule_s3KeyPrefix,
    createBusinessReportSchedule_recurrence,
    createBusinessReportSchedule_tags,
    createBusinessReportSchedule_s3BucketName,
    createBusinessReportSchedule_clientRequestToken,
    createBusinessReportSchedule_scheduleName,
    createBusinessReportSchedule_format,
    createBusinessReportSchedule_contentRange,

    -- * Destructuring the Response
    CreateBusinessReportScheduleResponse (..),
    newCreateBusinessReportScheduleResponse,

    -- * Response Lenses
    createBusinessReportScheduleResponse_scheduleArn,
    createBusinessReportScheduleResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBusinessReportSchedule' smart constructor.
data CreateBusinessReportSchedule = CreateBusinessReportSchedule'
  { -- | The S3 key where the report is delivered.
    s3KeyPrefix :: Core.Maybe Core.Text,
    -- | The recurrence of the reports. If this isn\'t specified, the report will
    -- only be delivered one time when the API is called.
    recurrence :: Core.Maybe BusinessReportRecurrence,
    -- | The tags for the business report schedule.
    tags :: Core.Maybe [Tag],
    -- | The S3 bucket name of the output reports. If this isn\'t specified, the
    -- report can be retrieved from a download link by calling
    -- ListBusinessReportSchedule.
    s3BucketName :: Core.Maybe Core.Text,
    -- | The client request token.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The name identifier of the schedule.
    scheduleName :: Core.Maybe Core.Text,
    -- | The format of the generated report (individual CSV files or zipped files
    -- of individual files).
    format :: BusinessReportFormat,
    -- | The content range of the reports.
    contentRange :: BusinessReportContentRange
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBusinessReportSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyPrefix', 'createBusinessReportSchedule_s3KeyPrefix' - The S3 key where the report is delivered.
--
-- 'recurrence', 'createBusinessReportSchedule_recurrence' - The recurrence of the reports. If this isn\'t specified, the report will
-- only be delivered one time when the API is called.
--
-- 'tags', 'createBusinessReportSchedule_tags' - The tags for the business report schedule.
--
-- 's3BucketName', 'createBusinessReportSchedule_s3BucketName' - The S3 bucket name of the output reports. If this isn\'t specified, the
-- report can be retrieved from a download link by calling
-- ListBusinessReportSchedule.
--
-- 'clientRequestToken', 'createBusinessReportSchedule_clientRequestToken' - The client request token.
--
-- 'scheduleName', 'createBusinessReportSchedule_scheduleName' - The name identifier of the schedule.
--
-- 'format', 'createBusinessReportSchedule_format' - The format of the generated report (individual CSV files or zipped files
-- of individual files).
--
-- 'contentRange', 'createBusinessReportSchedule_contentRange' - The content range of the reports.
newCreateBusinessReportSchedule ::
  -- | 'format'
  BusinessReportFormat ->
  -- | 'contentRange'
  BusinessReportContentRange ->
  CreateBusinessReportSchedule
newCreateBusinessReportSchedule
  pFormat_
  pContentRange_ =
    CreateBusinessReportSchedule'
      { s3KeyPrefix =
          Core.Nothing,
        recurrence = Core.Nothing,
        tags = Core.Nothing,
        s3BucketName = Core.Nothing,
        clientRequestToken = Core.Nothing,
        scheduleName = Core.Nothing,
        format = pFormat_,
        contentRange = pContentRange_
      }

-- | The S3 key where the report is delivered.
createBusinessReportSchedule_s3KeyPrefix :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe Core.Text)
createBusinessReportSchedule_s3KeyPrefix = Lens.lens (\CreateBusinessReportSchedule' {s3KeyPrefix} -> s3KeyPrefix) (\s@CreateBusinessReportSchedule' {} a -> s {s3KeyPrefix = a} :: CreateBusinessReportSchedule)

-- | The recurrence of the reports. If this isn\'t specified, the report will
-- only be delivered one time when the API is called.
createBusinessReportSchedule_recurrence :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe BusinessReportRecurrence)
createBusinessReportSchedule_recurrence = Lens.lens (\CreateBusinessReportSchedule' {recurrence} -> recurrence) (\s@CreateBusinessReportSchedule' {} a -> s {recurrence = a} :: CreateBusinessReportSchedule)

-- | The tags for the business report schedule.
createBusinessReportSchedule_tags :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe [Tag])
createBusinessReportSchedule_tags = Lens.lens (\CreateBusinessReportSchedule' {tags} -> tags) (\s@CreateBusinessReportSchedule' {} a -> s {tags = a} :: CreateBusinessReportSchedule) Core.. Lens.mapping Lens._Coerce

-- | The S3 bucket name of the output reports. If this isn\'t specified, the
-- report can be retrieved from a download link by calling
-- ListBusinessReportSchedule.
createBusinessReportSchedule_s3BucketName :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe Core.Text)
createBusinessReportSchedule_s3BucketName = Lens.lens (\CreateBusinessReportSchedule' {s3BucketName} -> s3BucketName) (\s@CreateBusinessReportSchedule' {} a -> s {s3BucketName = a} :: CreateBusinessReportSchedule)

-- | The client request token.
createBusinessReportSchedule_clientRequestToken :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe Core.Text)
createBusinessReportSchedule_clientRequestToken = Lens.lens (\CreateBusinessReportSchedule' {clientRequestToken} -> clientRequestToken) (\s@CreateBusinessReportSchedule' {} a -> s {clientRequestToken = a} :: CreateBusinessReportSchedule)

-- | The name identifier of the schedule.
createBusinessReportSchedule_scheduleName :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe Core.Text)
createBusinessReportSchedule_scheduleName = Lens.lens (\CreateBusinessReportSchedule' {scheduleName} -> scheduleName) (\s@CreateBusinessReportSchedule' {} a -> s {scheduleName = a} :: CreateBusinessReportSchedule)

-- | The format of the generated report (individual CSV files or zipped files
-- of individual files).
createBusinessReportSchedule_format :: Lens.Lens' CreateBusinessReportSchedule BusinessReportFormat
createBusinessReportSchedule_format = Lens.lens (\CreateBusinessReportSchedule' {format} -> format) (\s@CreateBusinessReportSchedule' {} a -> s {format = a} :: CreateBusinessReportSchedule)

-- | The content range of the reports.
createBusinessReportSchedule_contentRange :: Lens.Lens' CreateBusinessReportSchedule BusinessReportContentRange
createBusinessReportSchedule_contentRange = Lens.lens (\CreateBusinessReportSchedule' {contentRange} -> contentRange) (\s@CreateBusinessReportSchedule' {} a -> s {contentRange = a} :: CreateBusinessReportSchedule)

instance Core.AWSRequest CreateBusinessReportSchedule where
  type
    AWSResponse CreateBusinessReportSchedule =
      CreateBusinessReportScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBusinessReportScheduleResponse'
            Core.<$> (x Core..?> "ScheduleArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateBusinessReportSchedule

instance Core.NFData CreateBusinessReportSchedule

instance Core.ToHeaders CreateBusinessReportSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.CreateBusinessReportSchedule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateBusinessReportSchedule where
  toJSON CreateBusinessReportSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3KeyPrefix" Core..=) Core.<$> s3KeyPrefix,
            ("Recurrence" Core..=) Core.<$> recurrence,
            ("Tags" Core..=) Core.<$> tags,
            ("S3BucketName" Core..=) Core.<$> s3BucketName,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("ScheduleName" Core..=) Core.<$> scheduleName,
            Core.Just ("Format" Core..= format),
            Core.Just ("ContentRange" Core..= contentRange)
          ]
      )

instance Core.ToPath CreateBusinessReportSchedule where
  toPath = Core.const "/"

instance Core.ToQuery CreateBusinessReportSchedule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateBusinessReportScheduleResponse' smart constructor.
data CreateBusinessReportScheduleResponse = CreateBusinessReportScheduleResponse'
  { -- | The ARN of the business report schedule.
    scheduleArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBusinessReportScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleArn', 'createBusinessReportScheduleResponse_scheduleArn' - The ARN of the business report schedule.
--
-- 'httpStatus', 'createBusinessReportScheduleResponse_httpStatus' - The response's http status code.
newCreateBusinessReportScheduleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateBusinessReportScheduleResponse
newCreateBusinessReportScheduleResponse pHttpStatus_ =
  CreateBusinessReportScheduleResponse'
    { scheduleArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the business report schedule.
createBusinessReportScheduleResponse_scheduleArn :: Lens.Lens' CreateBusinessReportScheduleResponse (Core.Maybe Core.Text)
createBusinessReportScheduleResponse_scheduleArn = Lens.lens (\CreateBusinessReportScheduleResponse' {scheduleArn} -> scheduleArn) (\s@CreateBusinessReportScheduleResponse' {} a -> s {scheduleArn = a} :: CreateBusinessReportScheduleResponse)

-- | The response's http status code.
createBusinessReportScheduleResponse_httpStatus :: Lens.Lens' CreateBusinessReportScheduleResponse Core.Int
createBusinessReportScheduleResponse_httpStatus = Lens.lens (\CreateBusinessReportScheduleResponse' {httpStatus} -> httpStatus) (\s@CreateBusinessReportScheduleResponse' {} a -> s {httpStatus = a} :: CreateBusinessReportScheduleResponse)

instance
  Core.NFData
    CreateBusinessReportScheduleResponse
