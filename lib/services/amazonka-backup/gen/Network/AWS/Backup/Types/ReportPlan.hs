{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.Types.ReportPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.ReportPlan where

import Amazonka.Backup.Types.ReportDeliveryChannel
import Amazonka.Backup.Types.ReportSetting
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a report plan.
--
-- /See:/ 'newReportPlan' smart constructor.
data ReportPlan = ReportPlan'
  { -- | The date and time that a report plan is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationTime@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The unique name of the report plan. This name is between 1 and 256
    -- characters starting with a letter, and consisting of letters (a-z, A-Z),
    -- numbers (0-9), and underscores (_).
    reportPlanName :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    reportPlanArn :: Prelude.Maybe Prelude.Text,
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
    -- | The date and time that a report job associated with this report plan
    -- last attempted to run, in Unix format and Coordinated Universal Time
    -- (UTC). The value of @LastAttemptedExecutionTime@ is accurate to
    -- milliseconds. For example, the value 1516925490.087 represents Friday,
    -- January 26, 2018 12:11:30.087 AM.
    lastAttemptedExecutionTime :: Prelude.Maybe Core.POSIX,
    -- | The deployment status of a report plan. The statuses are:
    --
    -- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED@
    deploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a report job associated with this report plan
    -- last successfully ran, in Unix format and Coordinated Universal Time
    -- (UTC). The value of @LastSuccessfulExecutionTime@ is accurate to
    -- milliseconds. For example, the value 1516925490.087 represents Friday,
    -- January 26, 2018 12:11:30.087 AM.
    lastSuccessfulExecutionTime :: Prelude.Maybe Core.POSIX,
    -- | Contains information about where and how to deliver your reports,
    -- specifically your Amazon S3 bucket name, S3 key prefix, and the formats
    -- of your reports.
    reportDeliveryChannel :: Prelude.Maybe ReportDeliveryChannel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'reportPlan_creationTime' - The date and time that a report plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'reportPlanName', 'reportPlan_reportPlanName' - The unique name of the report plan. This name is between 1 and 256
-- characters starting with a letter, and consisting of letters (a-z, A-Z),
-- numbers (0-9), and underscores (_).
--
-- 'reportPlanArn', 'reportPlan_reportPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'reportSetting', 'reportPlan_reportSetting' - Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
--
-- If the report template is @RESOURCE_COMPLIANCE_REPORT@ or
-- @CONTROL_COMPLIANCE_REPORT@, this API resource also describes the report
-- coverage by Amazon Web Services Regions and frameworks.
--
-- 'reportPlanDescription', 'reportPlan_reportPlanDescription' - An optional description of the report plan with a maximum 1,024
-- characters.
--
-- 'lastAttemptedExecutionTime', 'reportPlan_lastAttemptedExecutionTime' - The date and time that a report job associated with this report plan
-- last attempted to run, in Unix format and Coordinated Universal Time
-- (UTC). The value of @LastAttemptedExecutionTime@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
--
-- 'deploymentStatus', 'reportPlan_deploymentStatus' - The deployment status of a report plan. The statuses are:
--
-- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED@
--
-- 'lastSuccessfulExecutionTime', 'reportPlan_lastSuccessfulExecutionTime' - The date and time that a report job associated with this report plan
-- last successfully ran, in Unix format and Coordinated Universal Time
-- (UTC). The value of @LastSuccessfulExecutionTime@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
--
-- 'reportDeliveryChannel', 'reportPlan_reportDeliveryChannel' - Contains information about where and how to deliver your reports,
-- specifically your Amazon S3 bucket name, S3 key prefix, and the formats
-- of your reports.
newReportPlan ::
  ReportPlan
newReportPlan =
  ReportPlan'
    { creationTime = Prelude.Nothing,
      reportPlanName = Prelude.Nothing,
      reportPlanArn = Prelude.Nothing,
      reportSetting = Prelude.Nothing,
      reportPlanDescription = Prelude.Nothing,
      lastAttemptedExecutionTime = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      lastSuccessfulExecutionTime = Prelude.Nothing,
      reportDeliveryChannel = Prelude.Nothing
    }

-- | The date and time that a report plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
reportPlan_creationTime :: Lens.Lens' ReportPlan (Prelude.Maybe Prelude.UTCTime)
reportPlan_creationTime = Lens.lens (\ReportPlan' {creationTime} -> creationTime) (\s@ReportPlan' {} a -> s {creationTime = a} :: ReportPlan) Prelude.. Lens.mapping Core._Time

-- | The unique name of the report plan. This name is between 1 and 256
-- characters starting with a letter, and consisting of letters (a-z, A-Z),
-- numbers (0-9), and underscores (_).
reportPlan_reportPlanName :: Lens.Lens' ReportPlan (Prelude.Maybe Prelude.Text)
reportPlan_reportPlanName = Lens.lens (\ReportPlan' {reportPlanName} -> reportPlanName) (\s@ReportPlan' {} a -> s {reportPlanName = a} :: ReportPlan)

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
reportPlan_reportPlanArn :: Lens.Lens' ReportPlan (Prelude.Maybe Prelude.Text)
reportPlan_reportPlanArn = Lens.lens (\ReportPlan' {reportPlanArn} -> reportPlanArn) (\s@ReportPlan' {} a -> s {reportPlanArn = a} :: ReportPlan)

-- | Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
--
-- If the report template is @RESOURCE_COMPLIANCE_REPORT@ or
-- @CONTROL_COMPLIANCE_REPORT@, this API resource also describes the report
-- coverage by Amazon Web Services Regions and frameworks.
reportPlan_reportSetting :: Lens.Lens' ReportPlan (Prelude.Maybe ReportSetting)
reportPlan_reportSetting = Lens.lens (\ReportPlan' {reportSetting} -> reportSetting) (\s@ReportPlan' {} a -> s {reportSetting = a} :: ReportPlan)

-- | An optional description of the report plan with a maximum 1,024
-- characters.
reportPlan_reportPlanDescription :: Lens.Lens' ReportPlan (Prelude.Maybe Prelude.Text)
reportPlan_reportPlanDescription = Lens.lens (\ReportPlan' {reportPlanDescription} -> reportPlanDescription) (\s@ReportPlan' {} a -> s {reportPlanDescription = a} :: ReportPlan)

-- | The date and time that a report job associated with this report plan
-- last attempted to run, in Unix format and Coordinated Universal Time
-- (UTC). The value of @LastAttemptedExecutionTime@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
reportPlan_lastAttemptedExecutionTime :: Lens.Lens' ReportPlan (Prelude.Maybe Prelude.UTCTime)
reportPlan_lastAttemptedExecutionTime = Lens.lens (\ReportPlan' {lastAttemptedExecutionTime} -> lastAttemptedExecutionTime) (\s@ReportPlan' {} a -> s {lastAttemptedExecutionTime = a} :: ReportPlan) Prelude.. Lens.mapping Core._Time

-- | The deployment status of a report plan. The statuses are:
--
-- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED@
reportPlan_deploymentStatus :: Lens.Lens' ReportPlan (Prelude.Maybe Prelude.Text)
reportPlan_deploymentStatus = Lens.lens (\ReportPlan' {deploymentStatus} -> deploymentStatus) (\s@ReportPlan' {} a -> s {deploymentStatus = a} :: ReportPlan)

-- | The date and time that a report job associated with this report plan
-- last successfully ran, in Unix format and Coordinated Universal Time
-- (UTC). The value of @LastSuccessfulExecutionTime@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
reportPlan_lastSuccessfulExecutionTime :: Lens.Lens' ReportPlan (Prelude.Maybe Prelude.UTCTime)
reportPlan_lastSuccessfulExecutionTime = Lens.lens (\ReportPlan' {lastSuccessfulExecutionTime} -> lastSuccessfulExecutionTime) (\s@ReportPlan' {} a -> s {lastSuccessfulExecutionTime = a} :: ReportPlan) Prelude.. Lens.mapping Core._Time

-- | Contains information about where and how to deliver your reports,
-- specifically your Amazon S3 bucket name, S3 key prefix, and the formats
-- of your reports.
reportPlan_reportDeliveryChannel :: Lens.Lens' ReportPlan (Prelude.Maybe ReportDeliveryChannel)
reportPlan_reportDeliveryChannel = Lens.lens (\ReportPlan' {reportDeliveryChannel} -> reportDeliveryChannel) (\s@ReportPlan' {} a -> s {reportDeliveryChannel = a} :: ReportPlan)

instance Core.FromJSON ReportPlan where
  parseJSON =
    Core.withObject
      "ReportPlan"
      ( \x ->
          ReportPlan'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "ReportPlanName")
            Prelude.<*> (x Core..:? "ReportPlanArn")
            Prelude.<*> (x Core..:? "ReportSetting")
            Prelude.<*> (x Core..:? "ReportPlanDescription")
            Prelude.<*> (x Core..:? "LastAttemptedExecutionTime")
            Prelude.<*> (x Core..:? "DeploymentStatus")
            Prelude.<*> (x Core..:? "LastSuccessfulExecutionTime")
            Prelude.<*> (x Core..:? "ReportDeliveryChannel")
      )

instance Prelude.Hashable ReportPlan

instance Prelude.NFData ReportPlan
