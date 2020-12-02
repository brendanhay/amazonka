{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CodeCoverageReportSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CodeCoverageReportSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a summary of a code coverage report.
--
--
-- Line coverage measures how many statements your tests cover. A statement is a single instruction, not including comments, conditionals, etc.
--
-- Branch coverage determines if your tests cover every possible branch of a control structure, such as an @if@ or @case@ statement.
--
--
-- /See:/ 'codeCoverageReportSummary' smart constructor.
data CodeCoverageReportSummary = CodeCoverageReportSummary'
  { _ccrsBranchesMissed ::
      !(Maybe Nat),
    _ccrsLinesMissed :: !(Maybe Nat),
    _ccrsBranchesCovered :: !(Maybe Nat),
    _ccrsLinesCovered :: !(Maybe Nat),
    _ccrsBranchCoveragePercentage ::
      !(Maybe Double),
    _ccrsLineCoveragePercentage ::
      !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeCoverageReportSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsBranchesMissed' - The number of conditional branches that are not covered by your tests.
--
-- * 'ccrsLinesMissed' - The number of lines that are not covered by your tests.
--
-- * 'ccrsBranchesCovered' - The number of conditional branches that are covered by your tests.
--
-- * 'ccrsLinesCovered' - The number of lines that are covered by your tests.
--
-- * 'ccrsBranchCoveragePercentage' - The percentage of branches that are covered by your tests.
--
-- * 'ccrsLineCoveragePercentage' - The percentage of lines that are covered by your tests.
codeCoverageReportSummary ::
  CodeCoverageReportSummary
codeCoverageReportSummary =
  CodeCoverageReportSummary'
    { _ccrsBranchesMissed = Nothing,
      _ccrsLinesMissed = Nothing,
      _ccrsBranchesCovered = Nothing,
      _ccrsLinesCovered = Nothing,
      _ccrsBranchCoveragePercentage = Nothing,
      _ccrsLineCoveragePercentage = Nothing
    }

-- | The number of conditional branches that are not covered by your tests.
ccrsBranchesMissed :: Lens' CodeCoverageReportSummary (Maybe Natural)
ccrsBranchesMissed = lens _ccrsBranchesMissed (\s a -> s {_ccrsBranchesMissed = a}) . mapping _Nat

-- | The number of lines that are not covered by your tests.
ccrsLinesMissed :: Lens' CodeCoverageReportSummary (Maybe Natural)
ccrsLinesMissed = lens _ccrsLinesMissed (\s a -> s {_ccrsLinesMissed = a}) . mapping _Nat

-- | The number of conditional branches that are covered by your tests.
ccrsBranchesCovered :: Lens' CodeCoverageReportSummary (Maybe Natural)
ccrsBranchesCovered = lens _ccrsBranchesCovered (\s a -> s {_ccrsBranchesCovered = a}) . mapping _Nat

-- | The number of lines that are covered by your tests.
ccrsLinesCovered :: Lens' CodeCoverageReportSummary (Maybe Natural)
ccrsLinesCovered = lens _ccrsLinesCovered (\s a -> s {_ccrsLinesCovered = a}) . mapping _Nat

-- | The percentage of branches that are covered by your tests.
ccrsBranchCoveragePercentage :: Lens' CodeCoverageReportSummary (Maybe Double)
ccrsBranchCoveragePercentage = lens _ccrsBranchCoveragePercentage (\s a -> s {_ccrsBranchCoveragePercentage = a})

-- | The percentage of lines that are covered by your tests.
ccrsLineCoveragePercentage :: Lens' CodeCoverageReportSummary (Maybe Double)
ccrsLineCoveragePercentage = lens _ccrsLineCoveragePercentage (\s a -> s {_ccrsLineCoveragePercentage = a})

instance FromJSON CodeCoverageReportSummary where
  parseJSON =
    withObject
      "CodeCoverageReportSummary"
      ( \x ->
          CodeCoverageReportSummary'
            <$> (x .:? "branchesMissed")
            <*> (x .:? "linesMissed")
            <*> (x .:? "branchesCovered")
            <*> (x .:? "linesCovered")
            <*> (x .:? "branchCoveragePercentage")
            <*> (x .:? "lineCoveragePercentage")
      )

instance Hashable CodeCoverageReportSummary

instance NFData CodeCoverageReportSummary
