{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateBusinessReportSchedule' smart constructor.
data UpdateBusinessReportSchedule = UpdateBusinessReportSchedule'
  { -- | The format of the generated report (individual CSV files or zipped files
    -- of individual files).
    format :: Prelude.Maybe BusinessReportFormat,
    -- | The S3 key where the report is delivered.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The recurrence of the reports.
    recurrence :: Prelude.Maybe BusinessReportRecurrence,
    -- | The S3 location of the output reports.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The name identifier of the schedule.
    scheduleName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the business report schedule.
    scheduleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateBusinessReportSchedule
newUpdateBusinessReportSchedule pScheduleArn_ =
  UpdateBusinessReportSchedule'
    { format =
        Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      recurrence = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      scheduleName = Prelude.Nothing,
      scheduleArn = pScheduleArn_
    }

-- | The format of the generated report (individual CSV files or zipped files
-- of individual files).
updateBusinessReportSchedule_format :: Lens.Lens' UpdateBusinessReportSchedule (Prelude.Maybe BusinessReportFormat)
updateBusinessReportSchedule_format = Lens.lens (\UpdateBusinessReportSchedule' {format} -> format) (\s@UpdateBusinessReportSchedule' {} a -> s {format = a} :: UpdateBusinessReportSchedule)

-- | The S3 key where the report is delivered.
updateBusinessReportSchedule_s3KeyPrefix :: Lens.Lens' UpdateBusinessReportSchedule (Prelude.Maybe Prelude.Text)
updateBusinessReportSchedule_s3KeyPrefix = Lens.lens (\UpdateBusinessReportSchedule' {s3KeyPrefix} -> s3KeyPrefix) (\s@UpdateBusinessReportSchedule' {} a -> s {s3KeyPrefix = a} :: UpdateBusinessReportSchedule)

-- | The recurrence of the reports.
updateBusinessReportSchedule_recurrence :: Lens.Lens' UpdateBusinessReportSchedule (Prelude.Maybe BusinessReportRecurrence)
updateBusinessReportSchedule_recurrence = Lens.lens (\UpdateBusinessReportSchedule' {recurrence} -> recurrence) (\s@UpdateBusinessReportSchedule' {} a -> s {recurrence = a} :: UpdateBusinessReportSchedule)

-- | The S3 location of the output reports.
updateBusinessReportSchedule_s3BucketName :: Lens.Lens' UpdateBusinessReportSchedule (Prelude.Maybe Prelude.Text)
updateBusinessReportSchedule_s3BucketName = Lens.lens (\UpdateBusinessReportSchedule' {s3BucketName} -> s3BucketName) (\s@UpdateBusinessReportSchedule' {} a -> s {s3BucketName = a} :: UpdateBusinessReportSchedule)

-- | The name identifier of the schedule.
updateBusinessReportSchedule_scheduleName :: Lens.Lens' UpdateBusinessReportSchedule (Prelude.Maybe Prelude.Text)
updateBusinessReportSchedule_scheduleName = Lens.lens (\UpdateBusinessReportSchedule' {scheduleName} -> scheduleName) (\s@UpdateBusinessReportSchedule' {} a -> s {scheduleName = a} :: UpdateBusinessReportSchedule)

-- | The ARN of the business report schedule.
updateBusinessReportSchedule_scheduleArn :: Lens.Lens' UpdateBusinessReportSchedule Prelude.Text
updateBusinessReportSchedule_scheduleArn = Lens.lens (\UpdateBusinessReportSchedule' {scheduleArn} -> scheduleArn) (\s@UpdateBusinessReportSchedule' {} a -> s {scheduleArn = a} :: UpdateBusinessReportSchedule)

instance
  Prelude.AWSRequest
    UpdateBusinessReportSchedule
  where
  type
    Rs UpdateBusinessReportSchedule =
      UpdateBusinessReportScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateBusinessReportScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateBusinessReportSchedule

instance Prelude.NFData UpdateBusinessReportSchedule

instance
  Prelude.ToHeaders
    UpdateBusinessReportSchedule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.UpdateBusinessReportSchedule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateBusinessReportSchedule where
  toJSON UpdateBusinessReportSchedule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Format" Prelude..=) Prelude.<$> format,
            ("S3KeyPrefix" Prelude..=) Prelude.<$> s3KeyPrefix,
            ("Recurrence" Prelude..=) Prelude.<$> recurrence,
            ("S3BucketName" Prelude..=) Prelude.<$> s3BucketName,
            ("ScheduleName" Prelude..=) Prelude.<$> scheduleName,
            Prelude.Just ("ScheduleArn" Prelude..= scheduleArn)
          ]
      )

instance Prelude.ToPath UpdateBusinessReportSchedule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateBusinessReportSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBusinessReportScheduleResponse' smart constructor.
data UpdateBusinessReportScheduleResponse = UpdateBusinessReportScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateBusinessReportScheduleResponse
newUpdateBusinessReportScheduleResponse pHttpStatus_ =
  UpdateBusinessReportScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateBusinessReportScheduleResponse_httpStatus :: Lens.Lens' UpdateBusinessReportScheduleResponse Prelude.Int
updateBusinessReportScheduleResponse_httpStatus = Lens.lens (\UpdateBusinessReportScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateBusinessReportScheduleResponse' {} a -> s {httpStatus = a} :: UpdateBusinessReportScheduleResponse)

instance
  Prelude.NFData
    UpdateBusinessReportScheduleResponse
