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
-- Module      : Network.AWS.CodeBuild.Types.Report
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Report where

import Network.AWS.CodeBuild.Types.CodeCoverageReportSummary
import Network.AWS.CodeBuild.Types.ReportExportConfig
import Network.AWS.CodeBuild.Types.ReportStatusType
import Network.AWS.CodeBuild.Types.ReportType
import Network.AWS.CodeBuild.Types.TestReportSummary
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the results from running a series of test cases during
-- the run of a build project. The test cases are specified in the
-- buildspec for the build project using one or more paths to the test case
-- files. You can specify any type of tests you want, such as unit tests,
-- integration tests, and functional tests.
--
-- /See:/ 'newReport' smart constructor.
data Report = Report'
  { -- | A @CodeCoverageReportSummary@ object that contains a code coverage
    -- summary for this report.
    codeCoverageSummary :: Core.Maybe CodeCoverageReportSummary,
    -- | The ARN of the report group associated with this report.
    reportGroupArn :: Core.Maybe Core.Text,
    -- | The status of this report.
    status :: Core.Maybe ReportStatusType,
    -- | Information about where the raw data used to generate this report was
    -- exported.
    exportConfig :: Core.Maybe ReportExportConfig,
    -- | The ARN of the report run.
    arn :: Core.Maybe Core.Text,
    -- | A @TestReportSummary@ object that contains information about this test
    -- report.
    testSummary :: Core.Maybe TestReportSummary,
    -- | The name of the report that was run.
    name :: Core.Maybe Core.Text,
    -- | The date and time a report expires. A report expires 30 days after it is
    -- created. An expired report is not available to view in CodeBuild.
    expired :: Core.Maybe Core.POSIX,
    -- | The ARN of the build run that generated this report.
    executionId :: Core.Maybe Core.Text,
    -- | The date and time this report run occurred.
    created :: Core.Maybe Core.POSIX,
    -- | The type of the report that was run.
    --
    -- [CODE_COVERAGE]
    --     A code coverage report.
    --
    -- [TEST]
    --     A test report.
    type' :: Core.Maybe ReportType,
    -- | A boolean that specifies if this report run is truncated. The list of
    -- test cases is truncated after the maximum number of test cases is
    -- reached.
    truncated :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Report' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeCoverageSummary', 'report_codeCoverageSummary' - A @CodeCoverageReportSummary@ object that contains a code coverage
-- summary for this report.
--
-- 'reportGroupArn', 'report_reportGroupArn' - The ARN of the report group associated with this report.
--
-- 'status', 'report_status' - The status of this report.
--
-- 'exportConfig', 'report_exportConfig' - Information about where the raw data used to generate this report was
-- exported.
--
-- 'arn', 'report_arn' - The ARN of the report run.
--
-- 'testSummary', 'report_testSummary' - A @TestReportSummary@ object that contains information about this test
-- report.
--
-- 'name', 'report_name' - The name of the report that was run.
--
-- 'expired', 'report_expired' - The date and time a report expires. A report expires 30 days after it is
-- created. An expired report is not available to view in CodeBuild.
--
-- 'executionId', 'report_executionId' - The ARN of the build run that generated this report.
--
-- 'created', 'report_created' - The date and time this report run occurred.
--
-- 'type'', 'report_type' - The type of the report that was run.
--
-- [CODE_COVERAGE]
--     A code coverage report.
--
-- [TEST]
--     A test report.
--
-- 'truncated', 'report_truncated' - A boolean that specifies if this report run is truncated. The list of
-- test cases is truncated after the maximum number of test cases is
-- reached.
newReport ::
  Report
newReport =
  Report'
    { codeCoverageSummary = Core.Nothing,
      reportGroupArn = Core.Nothing,
      status = Core.Nothing,
      exportConfig = Core.Nothing,
      arn = Core.Nothing,
      testSummary = Core.Nothing,
      name = Core.Nothing,
      expired = Core.Nothing,
      executionId = Core.Nothing,
      created = Core.Nothing,
      type' = Core.Nothing,
      truncated = Core.Nothing
    }

-- | A @CodeCoverageReportSummary@ object that contains a code coverage
-- summary for this report.
report_codeCoverageSummary :: Lens.Lens' Report (Core.Maybe CodeCoverageReportSummary)
report_codeCoverageSummary = Lens.lens (\Report' {codeCoverageSummary} -> codeCoverageSummary) (\s@Report' {} a -> s {codeCoverageSummary = a} :: Report)

-- | The ARN of the report group associated with this report.
report_reportGroupArn :: Lens.Lens' Report (Core.Maybe Core.Text)
report_reportGroupArn = Lens.lens (\Report' {reportGroupArn} -> reportGroupArn) (\s@Report' {} a -> s {reportGroupArn = a} :: Report)

-- | The status of this report.
report_status :: Lens.Lens' Report (Core.Maybe ReportStatusType)
report_status = Lens.lens (\Report' {status} -> status) (\s@Report' {} a -> s {status = a} :: Report)

-- | Information about where the raw data used to generate this report was
-- exported.
report_exportConfig :: Lens.Lens' Report (Core.Maybe ReportExportConfig)
report_exportConfig = Lens.lens (\Report' {exportConfig} -> exportConfig) (\s@Report' {} a -> s {exportConfig = a} :: Report)

-- | The ARN of the report run.
report_arn :: Lens.Lens' Report (Core.Maybe Core.Text)
report_arn = Lens.lens (\Report' {arn} -> arn) (\s@Report' {} a -> s {arn = a} :: Report)

-- | A @TestReportSummary@ object that contains information about this test
-- report.
report_testSummary :: Lens.Lens' Report (Core.Maybe TestReportSummary)
report_testSummary = Lens.lens (\Report' {testSummary} -> testSummary) (\s@Report' {} a -> s {testSummary = a} :: Report)

-- | The name of the report that was run.
report_name :: Lens.Lens' Report (Core.Maybe Core.Text)
report_name = Lens.lens (\Report' {name} -> name) (\s@Report' {} a -> s {name = a} :: Report)

-- | The date and time a report expires. A report expires 30 days after it is
-- created. An expired report is not available to view in CodeBuild.
report_expired :: Lens.Lens' Report (Core.Maybe Core.UTCTime)
report_expired = Lens.lens (\Report' {expired} -> expired) (\s@Report' {} a -> s {expired = a} :: Report) Core.. Lens.mapping Core._Time

-- | The ARN of the build run that generated this report.
report_executionId :: Lens.Lens' Report (Core.Maybe Core.Text)
report_executionId = Lens.lens (\Report' {executionId} -> executionId) (\s@Report' {} a -> s {executionId = a} :: Report)

-- | The date and time this report run occurred.
report_created :: Lens.Lens' Report (Core.Maybe Core.UTCTime)
report_created = Lens.lens (\Report' {created} -> created) (\s@Report' {} a -> s {created = a} :: Report) Core.. Lens.mapping Core._Time

-- | The type of the report that was run.
--
-- [CODE_COVERAGE]
--     A code coverage report.
--
-- [TEST]
--     A test report.
report_type :: Lens.Lens' Report (Core.Maybe ReportType)
report_type = Lens.lens (\Report' {type'} -> type') (\s@Report' {} a -> s {type' = a} :: Report)

-- | A boolean that specifies if this report run is truncated. The list of
-- test cases is truncated after the maximum number of test cases is
-- reached.
report_truncated :: Lens.Lens' Report (Core.Maybe Core.Bool)
report_truncated = Lens.lens (\Report' {truncated} -> truncated) (\s@Report' {} a -> s {truncated = a} :: Report)

instance Core.FromJSON Report where
  parseJSON =
    Core.withObject
      "Report"
      ( \x ->
          Report'
            Core.<$> (x Core..:? "codeCoverageSummary")
            Core.<*> (x Core..:? "reportGroupArn")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "exportConfig")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "testSummary")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "expired")
            Core.<*> (x Core..:? "executionId")
            Core.<*> (x Core..:? "created")
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..:? "truncated")
      )

instance Core.Hashable Report

instance Core.NFData Report
