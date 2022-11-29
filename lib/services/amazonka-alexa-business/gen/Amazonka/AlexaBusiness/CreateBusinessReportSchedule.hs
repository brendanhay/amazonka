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
-- Module      : Amazonka.AlexaBusiness.CreateBusinessReportSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a recurring schedule for usage reports to deliver to the
-- specified S3 location with a specified daily or weekly interval.
module Amazonka.AlexaBusiness.CreateBusinessReportSchedule
  ( -- * Creating a Request
    CreateBusinessReportSchedule (..),
    newCreateBusinessReportSchedule,

    -- * Request Lenses
    createBusinessReportSchedule_tags,
    createBusinessReportSchedule_s3KeyPrefix,
    createBusinessReportSchedule_clientRequestToken,
    createBusinessReportSchedule_s3BucketName,
    createBusinessReportSchedule_recurrence,
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBusinessReportSchedule' smart constructor.
data CreateBusinessReportSchedule = CreateBusinessReportSchedule'
  { -- | The tags for the business report schedule.
    tags :: Prelude.Maybe [Tag],
    -- | The S3 key where the report is delivered.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The client request token.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket name of the output reports. If this isn\'t specified, the
    -- report can be retrieved from a download link by calling
    -- ListBusinessReportSchedule.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The recurrence of the reports. If this isn\'t specified, the report will
    -- only be delivered one time when the API is called.
    recurrence :: Prelude.Maybe BusinessReportRecurrence,
    -- | The name identifier of the schedule.
    scheduleName :: Prelude.Maybe Prelude.Text,
    -- | The format of the generated report (individual CSV files or zipped files
    -- of individual files).
    format :: BusinessReportFormat,
    -- | The content range of the reports.
    contentRange :: BusinessReportContentRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBusinessReportSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createBusinessReportSchedule_tags' - The tags for the business report schedule.
--
-- 's3KeyPrefix', 'createBusinessReportSchedule_s3KeyPrefix' - The S3 key where the report is delivered.
--
-- 'clientRequestToken', 'createBusinessReportSchedule_clientRequestToken' - The client request token.
--
-- 's3BucketName', 'createBusinessReportSchedule_s3BucketName' - The S3 bucket name of the output reports. If this isn\'t specified, the
-- report can be retrieved from a download link by calling
-- ListBusinessReportSchedule.
--
-- 'recurrence', 'createBusinessReportSchedule_recurrence' - The recurrence of the reports. If this isn\'t specified, the report will
-- only be delivered one time when the API is called.
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
      { tags =
          Prelude.Nothing,
        s3KeyPrefix = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        s3BucketName = Prelude.Nothing,
        recurrence = Prelude.Nothing,
        scheduleName = Prelude.Nothing,
        format = pFormat_,
        contentRange = pContentRange_
      }

-- | The tags for the business report schedule.
createBusinessReportSchedule_tags :: Lens.Lens' CreateBusinessReportSchedule (Prelude.Maybe [Tag])
createBusinessReportSchedule_tags = Lens.lens (\CreateBusinessReportSchedule' {tags} -> tags) (\s@CreateBusinessReportSchedule' {} a -> s {tags = a} :: CreateBusinessReportSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The S3 key where the report is delivered.
createBusinessReportSchedule_s3KeyPrefix :: Lens.Lens' CreateBusinessReportSchedule (Prelude.Maybe Prelude.Text)
createBusinessReportSchedule_s3KeyPrefix = Lens.lens (\CreateBusinessReportSchedule' {s3KeyPrefix} -> s3KeyPrefix) (\s@CreateBusinessReportSchedule' {} a -> s {s3KeyPrefix = a} :: CreateBusinessReportSchedule)

-- | The client request token.
createBusinessReportSchedule_clientRequestToken :: Lens.Lens' CreateBusinessReportSchedule (Prelude.Maybe Prelude.Text)
createBusinessReportSchedule_clientRequestToken = Lens.lens (\CreateBusinessReportSchedule' {clientRequestToken} -> clientRequestToken) (\s@CreateBusinessReportSchedule' {} a -> s {clientRequestToken = a} :: CreateBusinessReportSchedule)

-- | The S3 bucket name of the output reports. If this isn\'t specified, the
-- report can be retrieved from a download link by calling
-- ListBusinessReportSchedule.
createBusinessReportSchedule_s3BucketName :: Lens.Lens' CreateBusinessReportSchedule (Prelude.Maybe Prelude.Text)
createBusinessReportSchedule_s3BucketName = Lens.lens (\CreateBusinessReportSchedule' {s3BucketName} -> s3BucketName) (\s@CreateBusinessReportSchedule' {} a -> s {s3BucketName = a} :: CreateBusinessReportSchedule)

-- | The recurrence of the reports. If this isn\'t specified, the report will
-- only be delivered one time when the API is called.
createBusinessReportSchedule_recurrence :: Lens.Lens' CreateBusinessReportSchedule (Prelude.Maybe BusinessReportRecurrence)
createBusinessReportSchedule_recurrence = Lens.lens (\CreateBusinessReportSchedule' {recurrence} -> recurrence) (\s@CreateBusinessReportSchedule' {} a -> s {recurrence = a} :: CreateBusinessReportSchedule)

-- | The name identifier of the schedule.
createBusinessReportSchedule_scheduleName :: Lens.Lens' CreateBusinessReportSchedule (Prelude.Maybe Prelude.Text)
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBusinessReportScheduleResponse'
            Prelude.<$> (x Core..?> "ScheduleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateBusinessReportSchedule
  where
  hashWithSalt _salt CreateBusinessReportSchedule' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` recurrence
      `Prelude.hashWithSalt` scheduleName
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` contentRange

instance Prelude.NFData CreateBusinessReportSchedule where
  rnf CreateBusinessReportSchedule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf recurrence
      `Prelude.seq` Prelude.rnf scheduleName
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf contentRange

instance Core.ToHeaders CreateBusinessReportSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.CreateBusinessReportSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBusinessReportSchedule where
  toJSON CreateBusinessReportSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("S3KeyPrefix" Core..=) Prelude.<$> s3KeyPrefix,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("S3BucketName" Core..=) Prelude.<$> s3BucketName,
            ("Recurrence" Core..=) Prelude.<$> recurrence,
            ("ScheduleName" Core..=) Prelude.<$> scheduleName,
            Prelude.Just ("Format" Core..= format),
            Prelude.Just ("ContentRange" Core..= contentRange)
          ]
      )

instance Core.ToPath CreateBusinessReportSchedule where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateBusinessReportSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBusinessReportScheduleResponse' smart constructor.
data CreateBusinessReportScheduleResponse = CreateBusinessReportScheduleResponse'
  { -- | The ARN of the business report schedule.
    scheduleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateBusinessReportScheduleResponse
newCreateBusinessReportScheduleResponse pHttpStatus_ =
  CreateBusinessReportScheduleResponse'
    { scheduleArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the business report schedule.
createBusinessReportScheduleResponse_scheduleArn :: Lens.Lens' CreateBusinessReportScheduleResponse (Prelude.Maybe Prelude.Text)
createBusinessReportScheduleResponse_scheduleArn = Lens.lens (\CreateBusinessReportScheduleResponse' {scheduleArn} -> scheduleArn) (\s@CreateBusinessReportScheduleResponse' {} a -> s {scheduleArn = a} :: CreateBusinessReportScheduleResponse)

-- | The response's http status code.
createBusinessReportScheduleResponse_httpStatus :: Lens.Lens' CreateBusinessReportScheduleResponse Prelude.Int
createBusinessReportScheduleResponse_httpStatus = Lens.lens (\CreateBusinessReportScheduleResponse' {httpStatus} -> httpStatus) (\s@CreateBusinessReportScheduleResponse' {} a -> s {httpStatus = a} :: CreateBusinessReportScheduleResponse)

instance
  Prelude.NFData
    CreateBusinessReportScheduleResponse
  where
  rnf CreateBusinessReportScheduleResponse' {..} =
    Prelude.rnf scheduleArn
      `Prelude.seq` Prelude.rnf httpStatus
