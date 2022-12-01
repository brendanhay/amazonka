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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.ReportSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a report setting.
--
-- /See:/ 'newReportSetting' smart constructor.
data ReportSetting = ReportSetting'
  { -- | The Amazon Resource Names (ARNs) of the frameworks a report covers.
    frameworkArns :: Prelude.Maybe [Prelude.Text],
    -- | The number of frameworks a report covers.
    numberOfFrameworks :: Prelude.Maybe Prelude.Int,
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
-- 'frameworkArns', 'reportSetting_frameworkArns' - The Amazon Resource Names (ARNs) of the frameworks a report covers.
--
-- 'numberOfFrameworks', 'reportSetting_numberOfFrameworks' - The number of frameworks a report covers.
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
    { frameworkArns = Prelude.Nothing,
      numberOfFrameworks = Prelude.Nothing,
      reportTemplate = pReportTemplate_
    }

-- | The Amazon Resource Names (ARNs) of the frameworks a report covers.
reportSetting_frameworkArns :: Lens.Lens' ReportSetting (Prelude.Maybe [Prelude.Text])
reportSetting_frameworkArns = Lens.lens (\ReportSetting' {frameworkArns} -> frameworkArns) (\s@ReportSetting' {} a -> s {frameworkArns = a} :: ReportSetting) Prelude.. Lens.mapping Lens.coerced

-- | The number of frameworks a report covers.
reportSetting_numberOfFrameworks :: Lens.Lens' ReportSetting (Prelude.Maybe Prelude.Int)
reportSetting_numberOfFrameworks = Lens.lens (\ReportSetting' {numberOfFrameworks} -> numberOfFrameworks) (\s@ReportSetting' {} a -> s {numberOfFrameworks = a} :: ReportSetting)

-- | Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
reportSetting_reportTemplate :: Lens.Lens' ReportSetting Prelude.Text
reportSetting_reportTemplate = Lens.lens (\ReportSetting' {reportTemplate} -> reportTemplate) (\s@ReportSetting' {} a -> s {reportTemplate = a} :: ReportSetting)

instance Core.FromJSON ReportSetting where
  parseJSON =
    Core.withObject
      "ReportSetting"
      ( \x ->
          ReportSetting'
            Prelude.<$> (x Core..:? "FrameworkArns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "NumberOfFrameworks")
            Prelude.<*> (x Core..: "ReportTemplate")
      )

instance Prelude.Hashable ReportSetting where
  hashWithSalt _salt ReportSetting' {..} =
    _salt `Prelude.hashWithSalt` frameworkArns
      `Prelude.hashWithSalt` numberOfFrameworks
      `Prelude.hashWithSalt` reportTemplate

instance Prelude.NFData ReportSetting where
  rnf ReportSetting' {..} =
    Prelude.rnf frameworkArns
      `Prelude.seq` Prelude.rnf numberOfFrameworks
      `Prelude.seq` Prelude.rnf reportTemplate

instance Core.ToJSON ReportSetting where
  toJSON ReportSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FrameworkArns" Core..=) Prelude.<$> frameworkArns,
            ("NumberOfFrameworks" Core..=)
              Prelude.<$> numberOfFrameworks,
            Prelude.Just
              ("ReportTemplate" Core..= reportTemplate)
          ]
      )
