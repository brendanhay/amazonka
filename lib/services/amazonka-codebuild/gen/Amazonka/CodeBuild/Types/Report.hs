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
-- Module      : Amazonka.CodeBuild.Types.Report
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.Report where

import Amazonka.CodeBuild.Types.CodeCoverageReportSummary
import Amazonka.CodeBuild.Types.ReportExportConfig
import Amazonka.CodeBuild.Types.ReportStatusType
import Amazonka.CodeBuild.Types.ReportType
import Amazonka.CodeBuild.Types.TestReportSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the results from running a series of test cases during
-- the run of a build project. The test cases are specified in the
-- buildspec for the build project using one or more paths to the test case
-- files. You can specify any type of tests you want, such as unit tests,
-- integration tests, and functional tests.
--
-- /See:/ 'newReport' smart constructor.
data Report = Report'
  { -- | The ARN of the report run.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A @CodeCoverageReportSummary@ object that contains a code coverage
    -- summary for this report.
    codeCoverageSummary :: Prelude.Maybe CodeCoverageReportSummary,
    -- | The date and time this report run occurred.
    created :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the build run that generated this report.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The date and time a report expires. A report expires 30 days after it is
    -- created. An expired report is not available to view in CodeBuild.
    expired :: Prelude.Maybe Data.POSIX,
    -- | Information about where the raw data used to generate this report was
    -- exported.
    exportConfig :: Prelude.Maybe ReportExportConfig,
    -- | The name of the report that was run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the report group associated with this report.
    reportGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The status of this report.
    status :: Prelude.Maybe ReportStatusType,
    -- | A @TestReportSummary@ object that contains information about this test
    -- report.
    testSummary :: Prelude.Maybe TestReportSummary,
    -- | A boolean that specifies if this report run is truncated. The list of
    -- test cases is truncated after the maximum number of test cases is
    -- reached.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | The type of the report that was run.
    --
    -- [CODE_COVERAGE]
    --     A code coverage report.
    --
    -- [TEST]
    --     A test report.
    type' :: Prelude.Maybe ReportType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Report' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'report_arn' - The ARN of the report run.
--
-- 'codeCoverageSummary', 'report_codeCoverageSummary' - A @CodeCoverageReportSummary@ object that contains a code coverage
-- summary for this report.
--
-- 'created', 'report_created' - The date and time this report run occurred.
--
-- 'executionId', 'report_executionId' - The ARN of the build run that generated this report.
--
-- 'expired', 'report_expired' - The date and time a report expires. A report expires 30 days after it is
-- created. An expired report is not available to view in CodeBuild.
--
-- 'exportConfig', 'report_exportConfig' - Information about where the raw data used to generate this report was
-- exported.
--
-- 'name', 'report_name' - The name of the report that was run.
--
-- 'reportGroupArn', 'report_reportGroupArn' - The ARN of the report group associated with this report.
--
-- 'status', 'report_status' - The status of this report.
--
-- 'testSummary', 'report_testSummary' - A @TestReportSummary@ object that contains information about this test
-- report.
--
-- 'truncated', 'report_truncated' - A boolean that specifies if this report run is truncated. The list of
-- test cases is truncated after the maximum number of test cases is
-- reached.
--
-- 'type'', 'report_type' - The type of the report that was run.
--
-- [CODE_COVERAGE]
--     A code coverage report.
--
-- [TEST]
--     A test report.
newReport ::
  Report
newReport =
  Report'
    { arn = Prelude.Nothing,
      codeCoverageSummary = Prelude.Nothing,
      created = Prelude.Nothing,
      executionId = Prelude.Nothing,
      expired = Prelude.Nothing,
      exportConfig = Prelude.Nothing,
      name = Prelude.Nothing,
      reportGroupArn = Prelude.Nothing,
      status = Prelude.Nothing,
      testSummary = Prelude.Nothing,
      truncated = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ARN of the report run.
report_arn :: Lens.Lens' Report (Prelude.Maybe Prelude.Text)
report_arn = Lens.lens (\Report' {arn} -> arn) (\s@Report' {} a -> s {arn = a} :: Report)

-- | A @CodeCoverageReportSummary@ object that contains a code coverage
-- summary for this report.
report_codeCoverageSummary :: Lens.Lens' Report (Prelude.Maybe CodeCoverageReportSummary)
report_codeCoverageSummary = Lens.lens (\Report' {codeCoverageSummary} -> codeCoverageSummary) (\s@Report' {} a -> s {codeCoverageSummary = a} :: Report)

-- | The date and time this report run occurred.
report_created :: Lens.Lens' Report (Prelude.Maybe Prelude.UTCTime)
report_created = Lens.lens (\Report' {created} -> created) (\s@Report' {} a -> s {created = a} :: Report) Prelude.. Lens.mapping Data._Time

-- | The ARN of the build run that generated this report.
report_executionId :: Lens.Lens' Report (Prelude.Maybe Prelude.Text)
report_executionId = Lens.lens (\Report' {executionId} -> executionId) (\s@Report' {} a -> s {executionId = a} :: Report)

-- | The date and time a report expires. A report expires 30 days after it is
-- created. An expired report is not available to view in CodeBuild.
report_expired :: Lens.Lens' Report (Prelude.Maybe Prelude.UTCTime)
report_expired = Lens.lens (\Report' {expired} -> expired) (\s@Report' {} a -> s {expired = a} :: Report) Prelude.. Lens.mapping Data._Time

-- | Information about where the raw data used to generate this report was
-- exported.
report_exportConfig :: Lens.Lens' Report (Prelude.Maybe ReportExportConfig)
report_exportConfig = Lens.lens (\Report' {exportConfig} -> exportConfig) (\s@Report' {} a -> s {exportConfig = a} :: Report)

-- | The name of the report that was run.
report_name :: Lens.Lens' Report (Prelude.Maybe Prelude.Text)
report_name = Lens.lens (\Report' {name} -> name) (\s@Report' {} a -> s {name = a} :: Report)

-- | The ARN of the report group associated with this report.
report_reportGroupArn :: Lens.Lens' Report (Prelude.Maybe Prelude.Text)
report_reportGroupArn = Lens.lens (\Report' {reportGroupArn} -> reportGroupArn) (\s@Report' {} a -> s {reportGroupArn = a} :: Report)

-- | The status of this report.
report_status :: Lens.Lens' Report (Prelude.Maybe ReportStatusType)
report_status = Lens.lens (\Report' {status} -> status) (\s@Report' {} a -> s {status = a} :: Report)

-- | A @TestReportSummary@ object that contains information about this test
-- report.
report_testSummary :: Lens.Lens' Report (Prelude.Maybe TestReportSummary)
report_testSummary = Lens.lens (\Report' {testSummary} -> testSummary) (\s@Report' {} a -> s {testSummary = a} :: Report)

-- | A boolean that specifies if this report run is truncated. The list of
-- test cases is truncated after the maximum number of test cases is
-- reached.
report_truncated :: Lens.Lens' Report (Prelude.Maybe Prelude.Bool)
report_truncated = Lens.lens (\Report' {truncated} -> truncated) (\s@Report' {} a -> s {truncated = a} :: Report)

-- | The type of the report that was run.
--
-- [CODE_COVERAGE]
--     A code coverage report.
--
-- [TEST]
--     A test report.
report_type :: Lens.Lens' Report (Prelude.Maybe ReportType)
report_type = Lens.lens (\Report' {type'} -> type') (\s@Report' {} a -> s {type' = a} :: Report)

instance Data.FromJSON Report where
  parseJSON =
    Data.withObject
      "Report"
      ( \x ->
          Report'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "codeCoverageSummary")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "executionId")
            Prelude.<*> (x Data..:? "expired")
            Prelude.<*> (x Data..:? "exportConfig")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "reportGroupArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "testSummary")
            Prelude.<*> (x Data..:? "truncated")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable Report where
  hashWithSalt _salt Report' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` codeCoverageSummary
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` expired
      `Prelude.hashWithSalt` exportConfig
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reportGroupArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` testSummary
      `Prelude.hashWithSalt` truncated
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Report where
  rnf Report' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf codeCoverageSummary
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf expired
      `Prelude.seq` Prelude.rnf exportConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reportGroupArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf testSummary
      `Prelude.seq` Prelude.rnf truncated
      `Prelude.seq` Prelude.rnf type'
