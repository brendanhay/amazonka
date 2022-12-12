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
-- Module      : Amazonka.Backup.CreateReportPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a report plan. A report plan is a document that contains
-- information about the contents of the report and where Backup will
-- deliver it.
--
-- If you call @CreateReportPlan@ with a plan that already exists, you
-- receive an @AlreadyExistsException@ exception.
module Amazonka.Backup.CreateReportPlan
  ( -- * Creating a Request
    CreateReportPlan (..),
    newCreateReportPlan,

    -- * Request Lenses
    createReportPlan_idempotencyToken,
    createReportPlan_reportPlanDescription,
    createReportPlan_reportPlanTags,
    createReportPlan_reportPlanName,
    createReportPlan_reportDeliveryChannel,
    createReportPlan_reportSetting,

    -- * Destructuring the Response
    CreateReportPlanResponse (..),
    newCreateReportPlanResponse,

    -- * Response Lenses
    createReportPlanResponse_creationTime,
    createReportPlanResponse_reportPlanArn,
    createReportPlanResponse_reportPlanName,
    createReportPlanResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateReportPlan' smart constructor.
data CreateReportPlan = CreateReportPlan'
  { -- | A customer-chosen string that you can use to distinguish between
    -- otherwise identical calls to @CreateReportPlanInput@. Retrying a
    -- successful request with the same idempotency token results in a success
    -- message with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | An optional description of the report plan with a maximum of 1,024
    -- characters.
    reportPlanDescription :: Prelude.Maybe Prelude.Text,
    -- | Metadata that you can assign to help organize the report plans that you
    -- create. Each tag is a key-value pair.
    reportPlanTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name of the report plan. The name must be between 1 and 256
    -- characters, starting with a letter, and consisting of letters (a-z,
    -- A-Z), numbers (0-9), and underscores (_).
    reportPlanName :: Prelude.Text,
    -- | A structure that contains information about where and how to deliver
    -- your reports, specifically your Amazon S3 bucket name, S3 key prefix,
    -- and the formats of your reports.
    reportDeliveryChannel :: ReportDeliveryChannel,
    -- | Identifies the report template for the report. Reports are built using a
    -- report template. The report templates are:
    --
    -- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
    --
    -- If the report template is @RESOURCE_COMPLIANCE_REPORT@ or
    -- @CONTROL_COMPLIANCE_REPORT@, this API resource also describes the report
    -- coverage by Amazon Web Services Regions and frameworks.
    reportSetting :: ReportSetting
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReportPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'createReportPlan_idempotencyToken' - A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @CreateReportPlanInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
--
-- 'reportPlanDescription', 'createReportPlan_reportPlanDescription' - An optional description of the report plan with a maximum of 1,024
-- characters.
--
-- 'reportPlanTags', 'createReportPlan_reportPlanTags' - Metadata that you can assign to help organize the report plans that you
-- create. Each tag is a key-value pair.
--
-- 'reportPlanName', 'createReportPlan_reportPlanName' - The unique name of the report plan. The name must be between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
--
-- 'reportDeliveryChannel', 'createReportPlan_reportDeliveryChannel' - A structure that contains information about where and how to deliver
-- your reports, specifically your Amazon S3 bucket name, S3 key prefix,
-- and the formats of your reports.
--
-- 'reportSetting', 'createReportPlan_reportSetting' - Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
--
-- If the report template is @RESOURCE_COMPLIANCE_REPORT@ or
-- @CONTROL_COMPLIANCE_REPORT@, this API resource also describes the report
-- coverage by Amazon Web Services Regions and frameworks.
newCreateReportPlan ::
  -- | 'reportPlanName'
  Prelude.Text ->
  -- | 'reportDeliveryChannel'
  ReportDeliveryChannel ->
  -- | 'reportSetting'
  ReportSetting ->
  CreateReportPlan
newCreateReportPlan
  pReportPlanName_
  pReportDeliveryChannel_
  pReportSetting_ =
    CreateReportPlan'
      { idempotencyToken =
          Prelude.Nothing,
        reportPlanDescription = Prelude.Nothing,
        reportPlanTags = Prelude.Nothing,
        reportPlanName = pReportPlanName_,
        reportDeliveryChannel = pReportDeliveryChannel_,
        reportSetting = pReportSetting_
      }

-- | A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @CreateReportPlanInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
createReportPlan_idempotencyToken :: Lens.Lens' CreateReportPlan (Prelude.Maybe Prelude.Text)
createReportPlan_idempotencyToken = Lens.lens (\CreateReportPlan' {idempotencyToken} -> idempotencyToken) (\s@CreateReportPlan' {} a -> s {idempotencyToken = a} :: CreateReportPlan)

-- | An optional description of the report plan with a maximum of 1,024
-- characters.
createReportPlan_reportPlanDescription :: Lens.Lens' CreateReportPlan (Prelude.Maybe Prelude.Text)
createReportPlan_reportPlanDescription = Lens.lens (\CreateReportPlan' {reportPlanDescription} -> reportPlanDescription) (\s@CreateReportPlan' {} a -> s {reportPlanDescription = a} :: CreateReportPlan)

-- | Metadata that you can assign to help organize the report plans that you
-- create. Each tag is a key-value pair.
createReportPlan_reportPlanTags :: Lens.Lens' CreateReportPlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createReportPlan_reportPlanTags = Lens.lens (\CreateReportPlan' {reportPlanTags} -> reportPlanTags) (\s@CreateReportPlan' {} a -> s {reportPlanTags = a} :: CreateReportPlan) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the report plan. The name must be between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
createReportPlan_reportPlanName :: Lens.Lens' CreateReportPlan Prelude.Text
createReportPlan_reportPlanName = Lens.lens (\CreateReportPlan' {reportPlanName} -> reportPlanName) (\s@CreateReportPlan' {} a -> s {reportPlanName = a} :: CreateReportPlan)

-- | A structure that contains information about where and how to deliver
-- your reports, specifically your Amazon S3 bucket name, S3 key prefix,
-- and the formats of your reports.
createReportPlan_reportDeliveryChannel :: Lens.Lens' CreateReportPlan ReportDeliveryChannel
createReportPlan_reportDeliveryChannel = Lens.lens (\CreateReportPlan' {reportDeliveryChannel} -> reportDeliveryChannel) (\s@CreateReportPlan' {} a -> s {reportDeliveryChannel = a} :: CreateReportPlan)

-- | Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
--
-- If the report template is @RESOURCE_COMPLIANCE_REPORT@ or
-- @CONTROL_COMPLIANCE_REPORT@, this API resource also describes the report
-- coverage by Amazon Web Services Regions and frameworks.
createReportPlan_reportSetting :: Lens.Lens' CreateReportPlan ReportSetting
createReportPlan_reportSetting = Lens.lens (\CreateReportPlan' {reportSetting} -> reportSetting) (\s@CreateReportPlan' {} a -> s {reportSetting = a} :: CreateReportPlan)

instance Core.AWSRequest CreateReportPlan where
  type
    AWSResponse CreateReportPlan =
      CreateReportPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReportPlanResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "ReportPlanArn")
            Prelude.<*> (x Data..?> "ReportPlanName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReportPlan where
  hashWithSalt _salt CreateReportPlan' {..} =
    _salt `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` reportPlanDescription
      `Prelude.hashWithSalt` reportPlanTags
      `Prelude.hashWithSalt` reportPlanName
      `Prelude.hashWithSalt` reportDeliveryChannel
      `Prelude.hashWithSalt` reportSetting

instance Prelude.NFData CreateReportPlan where
  rnf CreateReportPlan' {..} =
    Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf reportPlanDescription
      `Prelude.seq` Prelude.rnf reportPlanTags
      `Prelude.seq` Prelude.rnf reportPlanName
      `Prelude.seq` Prelude.rnf reportDeliveryChannel
      `Prelude.seq` Prelude.rnf reportSetting

instance Data.ToHeaders CreateReportPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateReportPlan where
  toJSON CreateReportPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("ReportPlanDescription" Data..=)
              Prelude.<$> reportPlanDescription,
            ("ReportPlanTags" Data..=)
              Prelude.<$> reportPlanTags,
            Prelude.Just
              ("ReportPlanName" Data..= reportPlanName),
            Prelude.Just
              ( "ReportDeliveryChannel"
                  Data..= reportDeliveryChannel
              ),
            Prelude.Just
              ("ReportSetting" Data..= reportSetting)
          ]
      )

instance Data.ToPath CreateReportPlan where
  toPath = Prelude.const "/audit/report-plans"

instance Data.ToQuery CreateReportPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateReportPlanResponse' smart constructor.
data CreateReportPlanResponse = CreateReportPlanResponse'
  { -- | The date and time a backup vault is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationTime@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    reportPlanArn :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the report plan.
    reportPlanName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReportPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'createReportPlanResponse_creationTime' - The date and time a backup vault is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'reportPlanArn', 'createReportPlanResponse_reportPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'reportPlanName', 'createReportPlanResponse_reportPlanName' - The unique name of the report plan.
--
-- 'httpStatus', 'createReportPlanResponse_httpStatus' - The response's http status code.
newCreateReportPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateReportPlanResponse
newCreateReportPlanResponse pHttpStatus_ =
  CreateReportPlanResponse'
    { creationTime =
        Prelude.Nothing,
      reportPlanArn = Prelude.Nothing,
      reportPlanName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time a backup vault is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
createReportPlanResponse_creationTime :: Lens.Lens' CreateReportPlanResponse (Prelude.Maybe Prelude.UTCTime)
createReportPlanResponse_creationTime = Lens.lens (\CreateReportPlanResponse' {creationTime} -> creationTime) (\s@CreateReportPlanResponse' {} a -> s {creationTime = a} :: CreateReportPlanResponse) Prelude.. Lens.mapping Data._Time

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
createReportPlanResponse_reportPlanArn :: Lens.Lens' CreateReportPlanResponse (Prelude.Maybe Prelude.Text)
createReportPlanResponse_reportPlanArn = Lens.lens (\CreateReportPlanResponse' {reportPlanArn} -> reportPlanArn) (\s@CreateReportPlanResponse' {} a -> s {reportPlanArn = a} :: CreateReportPlanResponse)

-- | The unique name of the report plan.
createReportPlanResponse_reportPlanName :: Lens.Lens' CreateReportPlanResponse (Prelude.Maybe Prelude.Text)
createReportPlanResponse_reportPlanName = Lens.lens (\CreateReportPlanResponse' {reportPlanName} -> reportPlanName) (\s@CreateReportPlanResponse' {} a -> s {reportPlanName = a} :: CreateReportPlanResponse)

-- | The response's http status code.
createReportPlanResponse_httpStatus :: Lens.Lens' CreateReportPlanResponse Prelude.Int
createReportPlanResponse_httpStatus = Lens.lens (\CreateReportPlanResponse' {httpStatus} -> httpStatus) (\s@CreateReportPlanResponse' {} a -> s {httpStatus = a} :: CreateReportPlanResponse)

instance Prelude.NFData CreateReportPlanResponse where
  rnf CreateReportPlanResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf reportPlanArn
      `Prelude.seq` Prelude.rnf reportPlanName
      `Prelude.seq` Prelude.rnf httpStatus
