{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.TestReportSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.TestReportSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a test report.
--
--
--
-- /See:/ 'testReportSummary' smart constructor.
data TestReportSummary = TestReportSummary'
  { _trsTotal :: !Int,
    _trsStatusCounts :: !(Map Text (Int)),
    _trsDurationInNanoSeconds :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestReportSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trsTotal' - The number of test cases in this @TestReportSummary@ . The total includes truncated test cases.
--
-- * 'trsStatusCounts' - A map that contains the number of each type of status returned by the test results in this @TestReportSummary@ .
--
-- * 'trsDurationInNanoSeconds' - The number of nanoseconds it took to run all of the test cases in this report.
testReportSummary ::
  -- | 'trsTotal'
  Int ->
  -- | 'trsDurationInNanoSeconds'
  Integer ->
  TestReportSummary
testReportSummary pTotal_ pDurationInNanoSeconds_ =
  TestReportSummary'
    { _trsTotal = pTotal_,
      _trsStatusCounts = mempty,
      _trsDurationInNanoSeconds = pDurationInNanoSeconds_
    }

-- | The number of test cases in this @TestReportSummary@ . The total includes truncated test cases.
trsTotal :: Lens' TestReportSummary Int
trsTotal = lens _trsTotal (\s a -> s {_trsTotal = a})

-- | A map that contains the number of each type of status returned by the test results in this @TestReportSummary@ .
trsStatusCounts :: Lens' TestReportSummary (HashMap Text (Int))
trsStatusCounts = lens _trsStatusCounts (\s a -> s {_trsStatusCounts = a}) . _Map

-- | The number of nanoseconds it took to run all of the test cases in this report.
trsDurationInNanoSeconds :: Lens' TestReportSummary Integer
trsDurationInNanoSeconds = lens _trsDurationInNanoSeconds (\s a -> s {_trsDurationInNanoSeconds = a})

instance FromJSON TestReportSummary where
  parseJSON =
    withObject
      "TestReportSummary"
      ( \x ->
          TestReportSummary'
            <$> (x .: "total")
            <*> (x .:? "statusCounts" .!= mempty)
            <*> (x .: "durationInNanoSeconds")
      )

instance Hashable TestReportSummary

instance NFData TestReportSummary
