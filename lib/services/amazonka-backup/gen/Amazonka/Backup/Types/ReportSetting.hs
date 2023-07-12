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
-- Module      : Amazonka.Backup.Types.ReportSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.ReportSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a report setting.
--
-- /See:/ 'newReportSetting' smart constructor.
data ReportSetting = ReportSetting'
  { -- | These are the accounts to be included in the report.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARNs) of the frameworks a report covers.
    frameworkArns :: Prelude.Maybe [Prelude.Text],
    -- | The number of frameworks a report covers.
    numberOfFrameworks :: Prelude.Maybe Prelude.Int,
    -- | These are the Organizational Units to be included in the report.
    organizationUnits :: Prelude.Maybe [Prelude.Text],
    -- | These are the Regions to be included in the report.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | Identifies the report template for the report. Reports are built using a
    -- report template. The report templates are:
    --
    -- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
    reportTemplate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accounts', 'reportSetting_accounts' - These are the accounts to be included in the report.
--
-- 'frameworkArns', 'reportSetting_frameworkArns' - The Amazon Resource Names (ARNs) of the frameworks a report covers.
--
-- 'numberOfFrameworks', 'reportSetting_numberOfFrameworks' - The number of frameworks a report covers.
--
-- 'organizationUnits', 'reportSetting_organizationUnits' - These are the Organizational Units to be included in the report.
--
-- 'regions', 'reportSetting_regions' - These are the Regions to be included in the report.
--
-- 'reportTemplate', 'reportSetting_reportTemplate' - Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
newReportSetting ::
  -- | 'reportTemplate'
  Prelude.Text ->
  ReportSetting
newReportSetting pReportTemplate_ =
  ReportSetting'
    { accounts = Prelude.Nothing,
      frameworkArns = Prelude.Nothing,
      numberOfFrameworks = Prelude.Nothing,
      organizationUnits = Prelude.Nothing,
      regions = Prelude.Nothing,
      reportTemplate = pReportTemplate_
    }

-- | These are the accounts to be included in the report.
reportSetting_accounts :: Lens.Lens' ReportSetting (Prelude.Maybe [Prelude.Text])
reportSetting_accounts = Lens.lens (\ReportSetting' {accounts} -> accounts) (\s@ReportSetting' {} a -> s {accounts = a} :: ReportSetting) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the frameworks a report covers.
reportSetting_frameworkArns :: Lens.Lens' ReportSetting (Prelude.Maybe [Prelude.Text])
reportSetting_frameworkArns = Lens.lens (\ReportSetting' {frameworkArns} -> frameworkArns) (\s@ReportSetting' {} a -> s {frameworkArns = a} :: ReportSetting) Prelude.. Lens.mapping Lens.coerced

-- | The number of frameworks a report covers.
reportSetting_numberOfFrameworks :: Lens.Lens' ReportSetting (Prelude.Maybe Prelude.Int)
reportSetting_numberOfFrameworks = Lens.lens (\ReportSetting' {numberOfFrameworks} -> numberOfFrameworks) (\s@ReportSetting' {} a -> s {numberOfFrameworks = a} :: ReportSetting)

-- | These are the Organizational Units to be included in the report.
reportSetting_organizationUnits :: Lens.Lens' ReportSetting (Prelude.Maybe [Prelude.Text])
reportSetting_organizationUnits = Lens.lens (\ReportSetting' {organizationUnits} -> organizationUnits) (\s@ReportSetting' {} a -> s {organizationUnits = a} :: ReportSetting) Prelude.. Lens.mapping Lens.coerced

-- | These are the Regions to be included in the report.
reportSetting_regions :: Lens.Lens' ReportSetting (Prelude.Maybe [Prelude.Text])
reportSetting_regions = Lens.lens (\ReportSetting' {regions} -> regions) (\s@ReportSetting' {} a -> s {regions = a} :: ReportSetting) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
reportSetting_reportTemplate :: Lens.Lens' ReportSetting Prelude.Text
reportSetting_reportTemplate = Lens.lens (\ReportSetting' {reportTemplate} -> reportTemplate) (\s@ReportSetting' {} a -> s {reportTemplate = a} :: ReportSetting)

instance Data.FromJSON ReportSetting where
  parseJSON =
    Data.withObject
      "ReportSetting"
      ( \x ->
          ReportSetting'
            Prelude.<$> (x Data..:? "Accounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FrameworkArns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NumberOfFrameworks")
            Prelude.<*> ( x
                            Data..:? "OrganizationUnits"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Regions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "ReportTemplate")
      )

instance Prelude.Hashable ReportSetting where
  hashWithSalt _salt ReportSetting' {..} =
    _salt
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` frameworkArns
      `Prelude.hashWithSalt` numberOfFrameworks
      `Prelude.hashWithSalt` organizationUnits
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` reportTemplate

instance Prelude.NFData ReportSetting where
  rnf ReportSetting' {..} =
    Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf frameworkArns
      `Prelude.seq` Prelude.rnf numberOfFrameworks
      `Prelude.seq` Prelude.rnf organizationUnits
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf reportTemplate

instance Data.ToJSON ReportSetting where
  toJSON ReportSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Accounts" Data..=) Prelude.<$> accounts,
            ("FrameworkArns" Data..=) Prelude.<$> frameworkArns,
            ("NumberOfFrameworks" Data..=)
              Prelude.<$> numberOfFrameworks,
            ("OrganizationUnits" Data..=)
              Prelude.<$> organizationUnits,
            ("Regions" Data..=) Prelude.<$> regions,
            Prelude.Just
              ("ReportTemplate" Data..= reportTemplate)
          ]
      )
