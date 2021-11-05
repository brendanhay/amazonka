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
-- Module      : Amazonka.Backup.UpdateReportPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing report plan identified by its @ReportPlanName@ with
-- the input document in JSON format.
module Amazonka.Backup.UpdateReportPlan
  ( -- * Creating a Request
    UpdateReportPlan (..),
    newUpdateReportPlan,

    -- * Request Lenses
    updateReportPlan_idempotencyToken,
    updateReportPlan_reportSetting,
    updateReportPlan_reportPlanDescription,
    updateReportPlan_reportDeliveryChannel,
    updateReportPlan_reportPlanName,

    -- * Destructuring the Response
    UpdateReportPlanResponse (..),
    newUpdateReportPlanResponse,

    -- * Response Lenses
    updateReportPlanResponse_creationTime,
    updateReportPlanResponse_reportPlanName,
    updateReportPlanResponse_reportPlanArn,
    updateReportPlanResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateReportPlan' smart constructor.
data UpdateReportPlan = UpdateReportPlan'
  { -- | A customer-chosen string that you can use to distinguish between
    -- otherwise identical calls to @UpdateReportPlanInput@. Retrying a
    -- successful request with the same idempotency token results in a success
    -- message with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Identifies the report template for the report. Reports are built using a
    -- report template. The report templates are:
    --
    -- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
    --
    -- If the report template is @RESOURCE_COMPLIANCE_REPORT@ or
    -- @CONTROL_COMPLIANCE_REPORT@, this API resource also describes the report
    -- coverage by Amazon Web Services Regions and frameworks.
    reportSetting :: Prelude.Maybe ReportSetting,
    -- | An optional description of the report plan with a maximum 1,024
    -- characters.
    reportPlanDescription :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains information about where to deliver your
    -- reports, specifically your Amazon S3 bucket name, S3 key prefix, and the
    -- formats of your reports.
    reportDeliveryChannel :: Prelude.Maybe ReportDeliveryChannel,
    -- | The unique name of the report plan. This name is between 1 and 256
    -- characters, starting with a letter, and consisting of letters (a-z,
    -- A-Z), numbers (0-9), and underscores (_).
    reportPlanName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReportPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'updateReportPlan_idempotencyToken' - A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @UpdateReportPlanInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
--
-- 'reportSetting', 'updateReportPlan_reportSetting' - Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
--
-- If the report template is @RESOURCE_COMPLIANCE_REPORT@ or
-- @CONTROL_COMPLIANCE_REPORT@, this API resource also describes the report
-- coverage by Amazon Web Services Regions and frameworks.
--
-- 'reportPlanDescription', 'updateReportPlan_reportPlanDescription' - An optional description of the report plan with a maximum 1,024
-- characters.
--
-- 'reportDeliveryChannel', 'updateReportPlan_reportDeliveryChannel' - A structure that contains information about where to deliver your
-- reports, specifically your Amazon S3 bucket name, S3 key prefix, and the
-- formats of your reports.
--
-- 'reportPlanName', 'updateReportPlan_reportPlanName' - The unique name of the report plan. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
newUpdateReportPlan ::
  -- | 'reportPlanName'
  Prelude.Text ->
  UpdateReportPlan
newUpdateReportPlan pReportPlanName_ =
  UpdateReportPlan'
    { idempotencyToken =
        Prelude.Nothing,
      reportSetting = Prelude.Nothing,
      reportPlanDescription = Prelude.Nothing,
      reportDeliveryChannel = Prelude.Nothing,
      reportPlanName = pReportPlanName_
    }

-- | A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @UpdateReportPlanInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
updateReportPlan_idempotencyToken :: Lens.Lens' UpdateReportPlan (Prelude.Maybe Prelude.Text)
updateReportPlan_idempotencyToken = Lens.lens (\UpdateReportPlan' {idempotencyToken} -> idempotencyToken) (\s@UpdateReportPlan' {} a -> s {idempotencyToken = a} :: UpdateReportPlan)

-- | Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
--
-- If the report template is @RESOURCE_COMPLIANCE_REPORT@ or
-- @CONTROL_COMPLIANCE_REPORT@, this API resource also describes the report
-- coverage by Amazon Web Services Regions and frameworks.
updateReportPlan_reportSetting :: Lens.Lens' UpdateReportPlan (Prelude.Maybe ReportSetting)
updateReportPlan_reportSetting = Lens.lens (\UpdateReportPlan' {reportSetting} -> reportSetting) (\s@UpdateReportPlan' {} a -> s {reportSetting = a} :: UpdateReportPlan)

-- | An optional description of the report plan with a maximum 1,024
-- characters.
updateReportPlan_reportPlanDescription :: Lens.Lens' UpdateReportPlan (Prelude.Maybe Prelude.Text)
updateReportPlan_reportPlanDescription = Lens.lens (\UpdateReportPlan' {reportPlanDescription} -> reportPlanDescription) (\s@UpdateReportPlan' {} a -> s {reportPlanDescription = a} :: UpdateReportPlan)

-- | A structure that contains information about where to deliver your
-- reports, specifically your Amazon S3 bucket name, S3 key prefix, and the
-- formats of your reports.
updateReportPlan_reportDeliveryChannel :: Lens.Lens' UpdateReportPlan (Prelude.Maybe ReportDeliveryChannel)
updateReportPlan_reportDeliveryChannel = Lens.lens (\UpdateReportPlan' {reportDeliveryChannel} -> reportDeliveryChannel) (\s@UpdateReportPlan' {} a -> s {reportDeliveryChannel = a} :: UpdateReportPlan)

-- | The unique name of the report plan. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
updateReportPlan_reportPlanName :: Lens.Lens' UpdateReportPlan Prelude.Text
updateReportPlan_reportPlanName = Lens.lens (\UpdateReportPlan' {reportPlanName} -> reportPlanName) (\s@UpdateReportPlan' {} a -> s {reportPlanName = a} :: UpdateReportPlan)

instance Core.AWSRequest UpdateReportPlan where
  type
    AWSResponse UpdateReportPlan =
      UpdateReportPlanResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateReportPlanResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "ReportPlanName")
            Prelude.<*> (x Core..?> "ReportPlanArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateReportPlan

instance Prelude.NFData UpdateReportPlan

instance Core.ToHeaders UpdateReportPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateReportPlan where
  toJSON UpdateReportPlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Core..=)
              Prelude.<$> idempotencyToken,
            ("ReportSetting" Core..=) Prelude.<$> reportSetting,
            ("ReportPlanDescription" Core..=)
              Prelude.<$> reportPlanDescription,
            ("ReportDeliveryChannel" Core..=)
              Prelude.<$> reportDeliveryChannel
          ]
      )

instance Core.ToPath UpdateReportPlan where
  toPath UpdateReportPlan' {..} =
    Prelude.mconcat
      ["/audit/report-plans/", Core.toBS reportPlanName]

instance Core.ToQuery UpdateReportPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateReportPlanResponse' smart constructor.
data UpdateReportPlanResponse = UpdateReportPlanResponse'
  { -- | The date and time that a report plan is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationTime@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The unique name of the report plan.
    reportPlanName :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    reportPlanArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReportPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'updateReportPlanResponse_creationTime' - The date and time that a report plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'reportPlanName', 'updateReportPlanResponse_reportPlanName' - The unique name of the report plan.
--
-- 'reportPlanArn', 'updateReportPlanResponse_reportPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'httpStatus', 'updateReportPlanResponse_httpStatus' - The response's http status code.
newUpdateReportPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateReportPlanResponse
newUpdateReportPlanResponse pHttpStatus_ =
  UpdateReportPlanResponse'
    { creationTime =
        Prelude.Nothing,
      reportPlanName = Prelude.Nothing,
      reportPlanArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that a report plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
updateReportPlanResponse_creationTime :: Lens.Lens' UpdateReportPlanResponse (Prelude.Maybe Prelude.UTCTime)
updateReportPlanResponse_creationTime = Lens.lens (\UpdateReportPlanResponse' {creationTime} -> creationTime) (\s@UpdateReportPlanResponse' {} a -> s {creationTime = a} :: UpdateReportPlanResponse) Prelude.. Lens.mapping Core._Time

-- | The unique name of the report plan.
updateReportPlanResponse_reportPlanName :: Lens.Lens' UpdateReportPlanResponse (Prelude.Maybe Prelude.Text)
updateReportPlanResponse_reportPlanName = Lens.lens (\UpdateReportPlanResponse' {reportPlanName} -> reportPlanName) (\s@UpdateReportPlanResponse' {} a -> s {reportPlanName = a} :: UpdateReportPlanResponse)

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
updateReportPlanResponse_reportPlanArn :: Lens.Lens' UpdateReportPlanResponse (Prelude.Maybe Prelude.Text)
updateReportPlanResponse_reportPlanArn = Lens.lens (\UpdateReportPlanResponse' {reportPlanArn} -> reportPlanArn) (\s@UpdateReportPlanResponse' {} a -> s {reportPlanArn = a} :: UpdateReportPlanResponse)

-- | The response's http status code.
updateReportPlanResponse_httpStatus :: Lens.Lens' UpdateReportPlanResponse Prelude.Int
updateReportPlanResponse_httpStatus = Lens.lens (\UpdateReportPlanResponse' {httpStatus} -> httpStatus) (\s@UpdateReportPlanResponse' {} a -> s {httpStatus = a} :: UpdateReportPlanResponse)

instance Prelude.NFData UpdateReportPlanResponse
