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
-- Module      : Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of the report delivery schedule with the
-- specified schedule ARN.
module Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
  ( -- * Creating a Request
    UpdateBusinessReportSchedule (..),
    newUpdateBusinessReportSchedule,

    -- * Request Lenses
    updateBusinessReportSchedule_format,
    updateBusinessReportSchedule_s3KeyPrefix,
    updateBusinessReportSchedule_recurrence,
    updateBusinessReportSchedule_s3BucketName,
    updateBusinessReportSchedule_scheduleName,
    updateBusinessReportSchedule_scheduleArn,

    -- * Destructuring the Response
    UpdateBusinessReportScheduleResponse (..),
    newUpdateBusinessReportScheduleResponse,

    -- * Response Lenses
    updateBusinessReportScheduleResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateBusinessReportSchedule' smart constructor.
data UpdateBusinessReportSchedule = UpdateBusinessReportSchedule'
  { -- | The format of the generated report (individual CSV files or zipped files
    -- of individual files).
    format :: Core.Maybe BusinessReportFormat,
    -- | The S3 key where the report is delivered.
    s3KeyPrefix :: Core.Maybe Core.Text,
    -- | The recurrence of the reports.
    recurrence :: Core.Maybe BusinessReportRecurrence,
    -- | The S3 location of the output reports.
    s3BucketName :: Core.Maybe Core.Text,
    -- | The name identifier of the schedule.
    scheduleName :: Core.Maybe Core.Text,
    -- | The ARN of the business report schedule.
    scheduleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBusinessReportSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'updateBusinessReportSchedule_format' - The format of the generated report (individual CSV files or zipped files
-- of individual files).
--
-- 's3KeyPrefix', 'updateBusinessReportSchedule_s3KeyPrefix' - The S3 key where the report is delivered.
--
-- 'recurrence', 'updateBusinessReportSchedule_recurrence' - The recurrence of the reports.
--
-- 's3BucketName', 'updateBusinessReportSchedule_s3BucketName' - The S3 location of the output reports.
--
-- 'scheduleName', 'updateBusinessReportSchedule_scheduleName' - The name identifier of the schedule.
--
-- 'scheduleArn', 'updateBusinessReportSchedule_scheduleArn' - The ARN of the business report schedule.
newUpdateBusinessReportSchedule ::
  -- | 'scheduleArn'
  Core.Text ->
  UpdateBusinessReportSchedule
newUpdateBusinessReportSchedule pScheduleArn_ =
  UpdateBusinessReportSchedule'
    { format =
        Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      recurrence = Core.Nothing,
      s3BucketName = Core.Nothing,
      scheduleName = Core.Nothing,
      scheduleArn = pScheduleArn_
    }

-- | The format of the generated report (individual CSV files or zipped files
-- of individual files).
updateBusinessReportSchedule_format :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe BusinessReportFormat)
updateBusinessReportSchedule_format = Lens.lens (\UpdateBusinessReportSchedule' {format} -> format) (\s@UpdateBusinessReportSchedule' {} a -> s {format = a} :: UpdateBusinessReportSchedule)

-- | The S3 key where the report is delivered.
updateBusinessReportSchedule_s3KeyPrefix :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe Core.Text)
updateBusinessReportSchedule_s3KeyPrefix = Lens.lens (\UpdateBusinessReportSchedule' {s3KeyPrefix} -> s3KeyPrefix) (\s@UpdateBusinessReportSchedule' {} a -> s {s3KeyPrefix = a} :: UpdateBusinessReportSchedule)

-- | The recurrence of the reports.
updateBusinessReportSchedule_recurrence :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe BusinessReportRecurrence)
updateBusinessReportSchedule_recurrence = Lens.lens (\UpdateBusinessReportSchedule' {recurrence} -> recurrence) (\s@UpdateBusinessReportSchedule' {} a -> s {recurrence = a} :: UpdateBusinessReportSchedule)

-- | The S3 location of the output reports.
updateBusinessReportSchedule_s3BucketName :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe Core.Text)
updateBusinessReportSchedule_s3BucketName = Lens.lens (\UpdateBusinessReportSchedule' {s3BucketName} -> s3BucketName) (\s@UpdateBusinessReportSchedule' {} a -> s {s3BucketName = a} :: UpdateBusinessReportSchedule)

-- | The name identifier of the schedule.
updateBusinessReportSchedule_scheduleName :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe Core.Text)
updateBusinessReportSchedule_scheduleName = Lens.lens (\UpdateBusinessReportSchedule' {scheduleName} -> scheduleName) (\s@UpdateBusinessReportSchedule' {} a -> s {scheduleName = a} :: UpdateBusinessReportSchedule)

-- | The ARN of the business report schedule.
updateBusinessReportSchedule_scheduleArn :: Lens.Lens' UpdateBusinessReportSchedule Core.Text
updateBusinessReportSchedule_scheduleArn = Lens.lens (\UpdateBusinessReportSchedule' {scheduleArn} -> scheduleArn) (\s@UpdateBusinessReportSchedule' {} a -> s {scheduleArn = a} :: UpdateBusinessReportSchedule)

instance Core.AWSRequest UpdateBusinessReportSchedule where
  type
    AWSResponse UpdateBusinessReportSchedule =
      UpdateBusinessReportScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateBusinessReportScheduleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateBusinessReportSchedule

instance Core.NFData UpdateBusinessReportSchedule

instance Core.ToHeaders UpdateBusinessReportSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateBusinessReportSchedule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateBusinessReportSchedule where
  toJSON UpdateBusinessReportSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Format" Core..=) Core.<$> format,
            ("S3KeyPrefix" Core..=) Core.<$> s3KeyPrefix,
            ("Recurrence" Core..=) Core.<$> recurrence,
            ("S3BucketName" Core..=) Core.<$> s3BucketName,
            ("ScheduleName" Core..=) Core.<$> scheduleName,
            Core.Just ("ScheduleArn" Core..= scheduleArn)
          ]
      )

instance Core.ToPath UpdateBusinessReportSchedule where
  toPath = Core.const "/"

instance Core.ToQuery UpdateBusinessReportSchedule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateBusinessReportScheduleResponse' smart constructor.
data UpdateBusinessReportScheduleResponse = UpdateBusinessReportScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBusinessReportScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateBusinessReportScheduleResponse_httpStatus' - The response's http status code.
newUpdateBusinessReportScheduleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateBusinessReportScheduleResponse
newUpdateBusinessReportScheduleResponse pHttpStatus_ =
  UpdateBusinessReportScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateBusinessReportScheduleResponse_httpStatus :: Lens.Lens' UpdateBusinessReportScheduleResponse Core.Int
updateBusinessReportScheduleResponse_httpStatus = Lens.lens (\UpdateBusinessReportScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateBusinessReportScheduleResponse' {} a -> s {httpStatus = a} :: UpdateBusinessReportScheduleResponse)

instance
  Core.NFData
    UpdateBusinessReportScheduleResponse
