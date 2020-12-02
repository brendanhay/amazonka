{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.TestCase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.TestCase where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a test case created using a framework such as NUnit or Cucumber. A test case might be a unit test or a configuration test.
--
--
--
-- /See:/ 'testCase' smart constructor.
data TestCase = TestCase'
  { _tcDurationInNanoSeconds ::
      !(Maybe Integer),
    _tcStatus :: !(Maybe Text),
    _tcExpired :: !(Maybe POSIX),
    _tcPrefix :: !(Maybe Text),
    _tcName :: !(Maybe Text),
    _tcTestRawDataPath :: !(Maybe Text),
    _tcMessage :: !(Maybe Text),
    _tcReportARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestCase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcDurationInNanoSeconds' - The number of nanoseconds it took to run this test case.
--
-- * 'tcStatus' - The status returned by the test case after it was run. Valid statuses are @SUCCEEDED@ , @FAILED@ , @ERROR@ , @SKIPPED@ , and @UNKNOWN@ .
--
-- * 'tcExpired' - The date and time a test case expires. A test case expires 30 days after it is created. An expired test case is not available to view in CodeBuild.
--
-- * 'tcPrefix' - A string that is applied to a series of related test cases. CodeBuild generates the prefix. The prefix depends on the framework used to generate the tests.
--
-- * 'tcName' - The name of the test case.
--
-- * 'tcTestRawDataPath' - The path to the raw data file that contains the test result.
--
-- * 'tcMessage' - A message associated with a test case. For example, an error message or stack trace.
--
-- * 'tcReportARN' - The ARN of the report to which the test case belongs.
testCase ::
  TestCase
testCase =
  TestCase'
    { _tcDurationInNanoSeconds = Nothing,
      _tcStatus = Nothing,
      _tcExpired = Nothing,
      _tcPrefix = Nothing,
      _tcName = Nothing,
      _tcTestRawDataPath = Nothing,
      _tcMessage = Nothing,
      _tcReportARN = Nothing
    }

-- | The number of nanoseconds it took to run this test case.
tcDurationInNanoSeconds :: Lens' TestCase (Maybe Integer)
tcDurationInNanoSeconds = lens _tcDurationInNanoSeconds (\s a -> s {_tcDurationInNanoSeconds = a})

-- | The status returned by the test case after it was run. Valid statuses are @SUCCEEDED@ , @FAILED@ , @ERROR@ , @SKIPPED@ , and @UNKNOWN@ .
tcStatus :: Lens' TestCase (Maybe Text)
tcStatus = lens _tcStatus (\s a -> s {_tcStatus = a})

-- | The date and time a test case expires. A test case expires 30 days after it is created. An expired test case is not available to view in CodeBuild.
tcExpired :: Lens' TestCase (Maybe UTCTime)
tcExpired = lens _tcExpired (\s a -> s {_tcExpired = a}) . mapping _Time

-- | A string that is applied to a series of related test cases. CodeBuild generates the prefix. The prefix depends on the framework used to generate the tests.
tcPrefix :: Lens' TestCase (Maybe Text)
tcPrefix = lens _tcPrefix (\s a -> s {_tcPrefix = a})

-- | The name of the test case.
tcName :: Lens' TestCase (Maybe Text)
tcName = lens _tcName (\s a -> s {_tcName = a})

-- | The path to the raw data file that contains the test result.
tcTestRawDataPath :: Lens' TestCase (Maybe Text)
tcTestRawDataPath = lens _tcTestRawDataPath (\s a -> s {_tcTestRawDataPath = a})

-- | A message associated with a test case. For example, an error message or stack trace.
tcMessage :: Lens' TestCase (Maybe Text)
tcMessage = lens _tcMessage (\s a -> s {_tcMessage = a})

-- | The ARN of the report to which the test case belongs.
tcReportARN :: Lens' TestCase (Maybe Text)
tcReportARN = lens _tcReportARN (\s a -> s {_tcReportARN = a})

instance FromJSON TestCase where
  parseJSON =
    withObject
      "TestCase"
      ( \x ->
          TestCase'
            <$> (x .:? "durationInNanoSeconds")
            <*> (x .:? "status")
            <*> (x .:? "expired")
            <*> (x .:? "prefix")
            <*> (x .:? "name")
            <*> (x .:? "testRawDataPath")
            <*> (x .:? "message")
            <*> (x .:? "reportArn")
      )

instance Hashable TestCase

instance NFData TestCase
