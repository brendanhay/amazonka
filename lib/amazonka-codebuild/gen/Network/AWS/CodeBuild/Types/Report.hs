{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Report
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Report
  ( Report (..),

    -- * Smart constructor
    mkReport,

    -- * Lenses
    rArn,
    rCodeCoverageSummary,
    rCreated,
    rExecutionId,
    rExpired,
    rExportConfig,
    rName,
    rReportGroupArn,
    rStatus,
    rTestSummary,
    rTruncated,
    rType,
  )
where

import qualified Network.AWS.CodeBuild.Types.Arn as Types
import qualified Network.AWS.CodeBuild.Types.CodeCoverageReportSummary as Types
import qualified Network.AWS.CodeBuild.Types.ReportExportConfig as Types
import qualified Network.AWS.CodeBuild.Types.ReportGroupArn as Types
import qualified Network.AWS.CodeBuild.Types.ReportStatusType as Types
import qualified Network.AWS.CodeBuild.Types.ReportType as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.CodeBuild.Types.TestReportSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the results from running a series of test cases during the run of a build project. The test cases are specified in the buildspec for the build project using one or more paths to the test case files. You can specify any type of tests you want, such as unit tests, integration tests, and functional tests.
--
-- /See:/ 'mkReport' smart constructor.
data Report = Report'
  { -- | The ARN of the report run.
    arn :: Core.Maybe Types.Arn,
    -- | A @CodeCoverageReportSummary@ object that contains a code coverage summary for this report.
    codeCoverageSummary :: Core.Maybe Types.CodeCoverageReportSummary,
    -- | The date and time this report run occurred.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the build run that generated this report.
    executionId :: Core.Maybe Types.String,
    -- | The date and time a report expires. A report expires 30 days after it is created. An expired report is not available to view in CodeBuild.
    expired :: Core.Maybe Core.NominalDiffTime,
    -- | Information about where the raw data used to generate this report was exported.
    exportConfig :: Core.Maybe Types.ReportExportConfig,
    -- | The name of the report that was run.
    name :: Core.Maybe Types.String,
    -- | The ARN of the report group associated with this report.
    reportGroupArn :: Core.Maybe Types.ReportGroupArn,
    -- | The status of this report.
    status :: Core.Maybe Types.ReportStatusType,
    -- | A @TestReportSummary@ object that contains information about this test report.
    testSummary :: Core.Maybe Types.TestReportSummary,
    -- | A boolean that specifies if this report run is truncated. The list of test cases is truncated after the maximum number of test cases is reached.
    truncated :: Core.Maybe Core.Bool,
    -- | The type of the report that was run.
    --
    --
    --     * CODE_COVERAGE
    --
    --     * A code coverage report.
    --
    --
    --     * TEST
    --
    --     * A test report.
    type' :: Core.Maybe Types.ReportType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Report' value with any optional fields omitted.
mkReport ::
  Report
mkReport =
  Report'
    { arn = Core.Nothing,
      codeCoverageSummary = Core.Nothing,
      created = Core.Nothing,
      executionId = Core.Nothing,
      expired = Core.Nothing,
      exportConfig = Core.Nothing,
      name = Core.Nothing,
      reportGroupArn = Core.Nothing,
      status = Core.Nothing,
      testSummary = Core.Nothing,
      truncated = Core.Nothing,
      type' = Core.Nothing
    }

-- | The ARN of the report run.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Report (Core.Maybe Types.Arn)
rArn = Lens.field @"arn"
{-# DEPRECATED rArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A @CodeCoverageReportSummary@ object that contains a code coverage summary for this report.
--
-- /Note:/ Consider using 'codeCoverageSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCodeCoverageSummary :: Lens.Lens' Report (Core.Maybe Types.CodeCoverageReportSummary)
rCodeCoverageSummary = Lens.field @"codeCoverageSummary"
{-# DEPRECATED rCodeCoverageSummary "Use generic-lens or generic-optics with 'codeCoverageSummary' instead." #-}

-- | The date and time this report run occurred.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreated :: Lens.Lens' Report (Core.Maybe Core.NominalDiffTime)
rCreated = Lens.field @"created"
{-# DEPRECATED rCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The ARN of the build run that generated this report.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rExecutionId :: Lens.Lens' Report (Core.Maybe Types.String)
rExecutionId = Lens.field @"executionId"
{-# DEPRECATED rExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | The date and time a report expires. A report expires 30 days after it is created. An expired report is not available to view in CodeBuild.
--
-- /Note:/ Consider using 'expired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rExpired :: Lens.Lens' Report (Core.Maybe Core.NominalDiffTime)
rExpired = Lens.field @"expired"
{-# DEPRECATED rExpired "Use generic-lens or generic-optics with 'expired' instead." #-}

-- | Information about where the raw data used to generate this report was exported.
--
-- /Note:/ Consider using 'exportConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rExportConfig :: Lens.Lens' Report (Core.Maybe Types.ReportExportConfig)
rExportConfig = Lens.field @"exportConfig"
{-# DEPRECATED rExportConfig "Use generic-lens or generic-optics with 'exportConfig' instead." #-}

-- | The name of the report that was run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Report (Core.Maybe Types.String)
rName = Lens.field @"name"
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the report group associated with this report.
--
-- /Note:/ Consider using 'reportGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReportGroupArn :: Lens.Lens' Report (Core.Maybe Types.ReportGroupArn)
rReportGroupArn = Lens.field @"reportGroupArn"
{-# DEPRECATED rReportGroupArn "Use generic-lens or generic-optics with 'reportGroupArn' instead." #-}

-- | The status of this report.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStatus :: Lens.Lens' Report (Core.Maybe Types.ReportStatusType)
rStatus = Lens.field @"status"
{-# DEPRECATED rStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A @TestReportSummary@ object that contains information about this test report.
--
-- /Note:/ Consider using 'testSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTestSummary :: Lens.Lens' Report (Core.Maybe Types.TestReportSummary)
rTestSummary = Lens.field @"testSummary"
{-# DEPRECATED rTestSummary "Use generic-lens or generic-optics with 'testSummary' instead." #-}

-- | A boolean that specifies if this report run is truncated. The list of test cases is truncated after the maximum number of test cases is reached.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTruncated :: Lens.Lens' Report (Core.Maybe Core.Bool)
rTruncated = Lens.field @"truncated"
{-# DEPRECATED rTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | The type of the report that was run.
--
--
--     * CODE_COVERAGE
--
--     * A code coverage report.
--
--
--     * TEST
--
--     * A test report.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rType :: Lens.Lens' Report (Core.Maybe Types.ReportType)
rType = Lens.field @"type'"
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Report where
  parseJSON =
    Core.withObject "Report" Core.$
      \x ->
        Report'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "codeCoverageSummary")
          Core.<*> (x Core..:? "created")
          Core.<*> (x Core..:? "executionId")
          Core.<*> (x Core..:? "expired")
          Core.<*> (x Core..:? "exportConfig")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "reportGroupArn")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "testSummary")
          Core.<*> (x Core..:? "truncated")
          Core.<*> (x Core..:? "type")
