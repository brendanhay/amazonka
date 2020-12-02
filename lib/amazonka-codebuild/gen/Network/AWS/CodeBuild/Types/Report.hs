{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Report
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the results from running a series of test cases during the run of a build project. The test cases are specified in the buildspec for the build project using one or more paths to the test case files. You can specify any type of tests you want, such as unit tests, integration tests, and functional tests.
--
--
--
-- /See:/ 'report' smart constructor.
data Report = Report'
  { _rReportGroupARN :: !(Maybe Text),
    _rStatus :: !(Maybe ReportStatusType),
    _rExpired :: !(Maybe POSIX),
    _rExecutionId :: !(Maybe Text),
    _rTruncated :: !(Maybe Bool),
    _rArn :: !(Maybe Text),
    _rCreated :: !(Maybe POSIX),
    _rName :: !(Maybe Text),
    _rCodeCoverageSummary :: !(Maybe CodeCoverageReportSummary),
    _rTestSummary :: !(Maybe TestReportSummary),
    _rType :: !(Maybe ReportType),
    _rExportConfig :: !(Maybe ReportExportConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Report' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rReportGroupARN' - The ARN of the report group associated with this report.
--
-- * 'rStatus' - The status of this report.
--
-- * 'rExpired' - The date and time a report expires. A report expires 30 days after it is created. An expired report is not available to view in CodeBuild.
--
-- * 'rExecutionId' - The ARN of the build run that generated this report.
--
-- * 'rTruncated' - A boolean that specifies if this report run is truncated. The list of test cases is truncated after the maximum number of test cases is reached.
--
-- * 'rArn' - The ARN of the report run.
--
-- * 'rCreated' - The date and time this report run occurred.
--
-- * 'rName' - The name of the report that was run.
--
-- * 'rCodeCoverageSummary' - A @CodeCoverageReportSummary@ object that contains a code coverage summary for this report.
--
-- * 'rTestSummary' - A @TestReportSummary@ object that contains information about this test report.
--
-- * 'rType' - The type of the report that was run.     * CODE_COVERAGE    * A code coverage report.     * TEST    * A test report.
--
-- * 'rExportConfig' - Information about where the raw data used to generate this report was exported.
report ::
  Report
report =
  Report'
    { _rReportGroupARN = Nothing,
      _rStatus = Nothing,
      _rExpired = Nothing,
      _rExecutionId = Nothing,
      _rTruncated = Nothing,
      _rArn = Nothing,
      _rCreated = Nothing,
      _rName = Nothing,
      _rCodeCoverageSummary = Nothing,
      _rTestSummary = Nothing,
      _rType = Nothing,
      _rExportConfig = Nothing
    }

-- | The ARN of the report group associated with this report.
rReportGroupARN :: Lens' Report (Maybe Text)
rReportGroupARN = lens _rReportGroupARN (\s a -> s {_rReportGroupARN = a})

-- | The status of this report.
rStatus :: Lens' Report (Maybe ReportStatusType)
rStatus = lens _rStatus (\s a -> s {_rStatus = a})

-- | The date and time a report expires. A report expires 30 days after it is created. An expired report is not available to view in CodeBuild.
rExpired :: Lens' Report (Maybe UTCTime)
rExpired = lens _rExpired (\s a -> s {_rExpired = a}) . mapping _Time

-- | The ARN of the build run that generated this report.
rExecutionId :: Lens' Report (Maybe Text)
rExecutionId = lens _rExecutionId (\s a -> s {_rExecutionId = a})

-- | A boolean that specifies if this report run is truncated. The list of test cases is truncated after the maximum number of test cases is reached.
rTruncated :: Lens' Report (Maybe Bool)
rTruncated = lens _rTruncated (\s a -> s {_rTruncated = a})

-- | The ARN of the report run.
rArn :: Lens' Report (Maybe Text)
rArn = lens _rArn (\s a -> s {_rArn = a})

-- | The date and time this report run occurred.
rCreated :: Lens' Report (Maybe UTCTime)
rCreated = lens _rCreated (\s a -> s {_rCreated = a}) . mapping _Time

-- | The name of the report that was run.
rName :: Lens' Report (Maybe Text)
rName = lens _rName (\s a -> s {_rName = a})

-- | A @CodeCoverageReportSummary@ object that contains a code coverage summary for this report.
rCodeCoverageSummary :: Lens' Report (Maybe CodeCoverageReportSummary)
rCodeCoverageSummary = lens _rCodeCoverageSummary (\s a -> s {_rCodeCoverageSummary = a})

-- | A @TestReportSummary@ object that contains information about this test report.
rTestSummary :: Lens' Report (Maybe TestReportSummary)
rTestSummary = lens _rTestSummary (\s a -> s {_rTestSummary = a})

-- | The type of the report that was run.     * CODE_COVERAGE    * A code coverage report.     * TEST    * A test report.
rType :: Lens' Report (Maybe ReportType)
rType = lens _rType (\s a -> s {_rType = a})

-- | Information about where the raw data used to generate this report was exported.
rExportConfig :: Lens' Report (Maybe ReportExportConfig)
rExportConfig = lens _rExportConfig (\s a -> s {_rExportConfig = a})

instance FromJSON Report where
  parseJSON =
    withObject
      "Report"
      ( \x ->
          Report'
            <$> (x .:? "reportGroupArn")
            <*> (x .:? "status")
            <*> (x .:? "expired")
            <*> (x .:? "executionId")
            <*> (x .:? "truncated")
            <*> (x .:? "arn")
            <*> (x .:? "created")
            <*> (x .:? "name")
            <*> (x .:? "codeCoverageSummary")
            <*> (x .:? "testSummary")
            <*> (x .:? "type")
            <*> (x .:? "exportConfig")
      )

instance Hashable Report

instance NFData Report
