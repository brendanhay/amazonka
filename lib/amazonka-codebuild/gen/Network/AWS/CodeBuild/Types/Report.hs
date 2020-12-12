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
    rReportGroupARN,
    rStatus,
    rExpired,
    rExecutionId,
    rTruncated,
    rArn,
    rCreated,
    rName,
    rCodeCoverageSummary,
    rTestSummary,
    rType,
    rExportConfig,
  )
where

import Network.AWS.CodeBuild.Types.CodeCoverageReportSummary
import Network.AWS.CodeBuild.Types.ReportExportConfig
import Network.AWS.CodeBuild.Types.ReportStatusType
import Network.AWS.CodeBuild.Types.ReportType
import Network.AWS.CodeBuild.Types.TestReportSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the results from running a series of test cases during the run of a build project. The test cases are specified in the buildspec for the build project using one or more paths to the test case files. You can specify any type of tests you want, such as unit tests, integration tests, and functional tests.
--
-- /See:/ 'mkReport' smart constructor.
data Report = Report'
  { reportGroupARN :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe ReportStatusType,
    expired :: Lude.Maybe Lude.Timestamp,
    executionId :: Lude.Maybe Lude.Text,
    truncated :: Lude.Maybe Lude.Bool,
    arn :: Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    codeCoverageSummary :: Lude.Maybe CodeCoverageReportSummary,
    testSummary :: Lude.Maybe TestReportSummary,
    type' :: Lude.Maybe ReportType,
    exportConfig :: Lude.Maybe ReportExportConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Report' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the report run.
-- * 'codeCoverageSummary' - A @CodeCoverageReportSummary@ object that contains a code coverage summary for this report.
-- * 'created' - The date and time this report run occurred.
-- * 'executionId' - The ARN of the build run that generated this report.
-- * 'expired' - The date and time a report expires. A report expires 30 days after it is created. An expired report is not available to view in CodeBuild.
-- * 'exportConfig' - Information about where the raw data used to generate this report was exported.
-- * 'name' - The name of the report that was run.
-- * 'reportGroupARN' - The ARN of the report group associated with this report.
-- * 'status' - The status of this report.
-- * 'testSummary' - A @TestReportSummary@ object that contains information about this test report.
-- * 'truncated' - A boolean that specifies if this report run is truncated. The list of test cases is truncated after the maximum number of test cases is reached.
-- * 'type'' - The type of the report that was run.
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
mkReport ::
  Report
mkReport =
  Report'
    { reportGroupARN = Lude.Nothing,
      status = Lude.Nothing,
      expired = Lude.Nothing,
      executionId = Lude.Nothing,
      truncated = Lude.Nothing,
      arn = Lude.Nothing,
      created = Lude.Nothing,
      name = Lude.Nothing,
      codeCoverageSummary = Lude.Nothing,
      testSummary = Lude.Nothing,
      type' = Lude.Nothing,
      exportConfig = Lude.Nothing
    }

-- | The ARN of the report group associated with this report.
--
-- /Note:/ Consider using 'reportGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReportGroupARN :: Lens.Lens' Report (Lude.Maybe Lude.Text)
rReportGroupARN = Lens.lens (reportGroupARN :: Report -> Lude.Maybe Lude.Text) (\s a -> s {reportGroupARN = a} :: Report)
{-# DEPRECATED rReportGroupARN "Use generic-lens or generic-optics with 'reportGroupARN' instead." #-}

-- | The status of this report.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStatus :: Lens.Lens' Report (Lude.Maybe ReportStatusType)
rStatus = Lens.lens (status :: Report -> Lude.Maybe ReportStatusType) (\s a -> s {status = a} :: Report)
{-# DEPRECATED rStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time a report expires. A report expires 30 days after it is created. An expired report is not available to view in CodeBuild.
--
-- /Note:/ Consider using 'expired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rExpired :: Lens.Lens' Report (Lude.Maybe Lude.Timestamp)
rExpired = Lens.lens (expired :: Report -> Lude.Maybe Lude.Timestamp) (\s a -> s {expired = a} :: Report)
{-# DEPRECATED rExpired "Use generic-lens or generic-optics with 'expired' instead." #-}

-- | The ARN of the build run that generated this report.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rExecutionId :: Lens.Lens' Report (Lude.Maybe Lude.Text)
rExecutionId = Lens.lens (executionId :: Report -> Lude.Maybe Lude.Text) (\s a -> s {executionId = a} :: Report)
{-# DEPRECATED rExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | A boolean that specifies if this report run is truncated. The list of test cases is truncated after the maximum number of test cases is reached.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTruncated :: Lens.Lens' Report (Lude.Maybe Lude.Bool)
rTruncated = Lens.lens (truncated :: Report -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: Report)
{-# DEPRECATED rTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | The ARN of the report run.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Report (Lude.Maybe Lude.Text)
rArn = Lens.lens (arn :: Report -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Report)
{-# DEPRECATED rArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time this report run occurred.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreated :: Lens.Lens' Report (Lude.Maybe Lude.Timestamp)
rCreated = Lens.lens (created :: Report -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: Report)
{-# DEPRECATED rCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The name of the report that was run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Report (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: Report -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Report)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A @CodeCoverageReportSummary@ object that contains a code coverage summary for this report.
--
-- /Note:/ Consider using 'codeCoverageSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCodeCoverageSummary :: Lens.Lens' Report (Lude.Maybe CodeCoverageReportSummary)
rCodeCoverageSummary = Lens.lens (codeCoverageSummary :: Report -> Lude.Maybe CodeCoverageReportSummary) (\s a -> s {codeCoverageSummary = a} :: Report)
{-# DEPRECATED rCodeCoverageSummary "Use generic-lens or generic-optics with 'codeCoverageSummary' instead." #-}

-- | A @TestReportSummary@ object that contains information about this test report.
--
-- /Note:/ Consider using 'testSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTestSummary :: Lens.Lens' Report (Lude.Maybe TestReportSummary)
rTestSummary = Lens.lens (testSummary :: Report -> Lude.Maybe TestReportSummary) (\s a -> s {testSummary = a} :: Report)
{-# DEPRECATED rTestSummary "Use generic-lens or generic-optics with 'testSummary' instead." #-}

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
rType :: Lens.Lens' Report (Lude.Maybe ReportType)
rType = Lens.lens (type' :: Report -> Lude.Maybe ReportType) (\s a -> s {type' = a} :: Report)
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Information about where the raw data used to generate this report was exported.
--
-- /Note:/ Consider using 'exportConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rExportConfig :: Lens.Lens' Report (Lude.Maybe ReportExportConfig)
rExportConfig = Lens.lens (exportConfig :: Report -> Lude.Maybe ReportExportConfig) (\s a -> s {exportConfig = a} :: Report)
{-# DEPRECATED rExportConfig "Use generic-lens or generic-optics with 'exportConfig' instead." #-}

instance Lude.FromJSON Report where
  parseJSON =
    Lude.withObject
      "Report"
      ( \x ->
          Report'
            Lude.<$> (x Lude..:? "reportGroupArn")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "expired")
            Lude.<*> (x Lude..:? "executionId")
            Lude.<*> (x Lude..:? "truncated")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "codeCoverageSummary")
            Lude.<*> (x Lude..:? "testSummary")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "exportConfig")
      )
