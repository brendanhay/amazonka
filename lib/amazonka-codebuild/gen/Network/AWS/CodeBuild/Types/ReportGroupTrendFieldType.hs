{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupTrendFieldType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupTrendFieldType where

import Network.AWS.Prelude

data ReportGroupTrendFieldType
  = BranchCoverage
  | BranchesCovered
  | BranchesMissed
  | Duration
  | LineCoverage
  | LinesCovered
  | LinesMissed
  | PassRate
  | Total
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ReportGroupTrendFieldType where
  parser =
    takeLowerText >>= \case
      "branch_coverage" -> pure BranchCoverage
      "branches_covered" -> pure BranchesCovered
      "branches_missed" -> pure BranchesMissed
      "duration" -> pure Duration
      "line_coverage" -> pure LineCoverage
      "lines_covered" -> pure LinesCovered
      "lines_missed" -> pure LinesMissed
      "pass_rate" -> pure PassRate
      "total" -> pure Total
      e ->
        fromTextError $
          "Failure parsing ReportGroupTrendFieldType from value: '" <> e
            <> "'. Accepted values: branch_coverage, branches_covered, branches_missed, duration, line_coverage, lines_covered, lines_missed, pass_rate, total"

instance ToText ReportGroupTrendFieldType where
  toText = \case
    BranchCoverage -> "BRANCH_COVERAGE"
    BranchesCovered -> "BRANCHES_COVERED"
    BranchesMissed -> "BRANCHES_MISSED"
    Duration -> "DURATION"
    LineCoverage -> "LINE_COVERAGE"
    LinesCovered -> "LINES_COVERED"
    LinesMissed -> "LINES_MISSED"
    PassRate -> "PASS_RATE"
    Total -> "TOTAL"

instance Hashable ReportGroupTrendFieldType

instance NFData ReportGroupTrendFieldType

instance ToByteString ReportGroupTrendFieldType

instance ToQuery ReportGroupTrendFieldType

instance ToHeader ReportGroupTrendFieldType

instance ToJSON ReportGroupTrendFieldType where
  toJSON = toJSONText
