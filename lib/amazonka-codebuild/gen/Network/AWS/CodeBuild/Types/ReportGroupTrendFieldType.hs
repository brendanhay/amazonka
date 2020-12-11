-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupTrendFieldType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupTrendFieldType
  ( ReportGroupTrendFieldType
      ( ReportGroupTrendFieldType',
        BranchCoverage,
        BranchesCovered,
        BranchesMissed,
        Duration,
        LineCoverage,
        LinesCovered,
        LinesMissed,
        PassRate,
        Total
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReportGroupTrendFieldType = ReportGroupTrendFieldType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern BranchCoverage :: ReportGroupTrendFieldType
pattern BranchCoverage = ReportGroupTrendFieldType' "BRANCH_COVERAGE"

pattern BranchesCovered :: ReportGroupTrendFieldType
pattern BranchesCovered = ReportGroupTrendFieldType' "BRANCHES_COVERED"

pattern BranchesMissed :: ReportGroupTrendFieldType
pattern BranchesMissed = ReportGroupTrendFieldType' "BRANCHES_MISSED"

pattern Duration :: ReportGroupTrendFieldType
pattern Duration = ReportGroupTrendFieldType' "DURATION"

pattern LineCoverage :: ReportGroupTrendFieldType
pattern LineCoverage = ReportGroupTrendFieldType' "LINE_COVERAGE"

pattern LinesCovered :: ReportGroupTrendFieldType
pattern LinesCovered = ReportGroupTrendFieldType' "LINES_COVERED"

pattern LinesMissed :: ReportGroupTrendFieldType
pattern LinesMissed = ReportGroupTrendFieldType' "LINES_MISSED"

pattern PassRate :: ReportGroupTrendFieldType
pattern PassRate = ReportGroupTrendFieldType' "PASS_RATE"

pattern Total :: ReportGroupTrendFieldType
pattern Total = ReportGroupTrendFieldType' "TOTAL"

{-# COMPLETE
  BranchCoverage,
  BranchesCovered,
  BranchesMissed,
  Duration,
  LineCoverage,
  LinesCovered,
  LinesMissed,
  PassRate,
  Total,
  ReportGroupTrendFieldType'
  #-}
