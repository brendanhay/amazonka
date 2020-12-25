{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ReportGroupTrendFieldTypePassRate,
        ReportGroupTrendFieldTypeDuration,
        ReportGroupTrendFieldTypeTotal,
        ReportGroupTrendFieldTypeLineCoverage,
        ReportGroupTrendFieldTypeLinesCovered,
        ReportGroupTrendFieldTypeLinesMissed,
        ReportGroupTrendFieldTypeBranchCoverage,
        ReportGroupTrendFieldTypeBranchesCovered,
        ReportGroupTrendFieldTypeBranchesMissed,
        fromReportGroupTrendFieldType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ReportGroupTrendFieldType = ReportGroupTrendFieldType'
  { fromReportGroupTrendFieldType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ReportGroupTrendFieldTypePassRate :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldTypePassRate = ReportGroupTrendFieldType' "PASS_RATE"

pattern ReportGroupTrendFieldTypeDuration :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldTypeDuration = ReportGroupTrendFieldType' "DURATION"

pattern ReportGroupTrendFieldTypeTotal :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldTypeTotal = ReportGroupTrendFieldType' "TOTAL"

pattern ReportGroupTrendFieldTypeLineCoverage :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldTypeLineCoverage = ReportGroupTrendFieldType' "LINE_COVERAGE"

pattern ReportGroupTrendFieldTypeLinesCovered :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldTypeLinesCovered = ReportGroupTrendFieldType' "LINES_COVERED"

pattern ReportGroupTrendFieldTypeLinesMissed :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldTypeLinesMissed = ReportGroupTrendFieldType' "LINES_MISSED"

pattern ReportGroupTrendFieldTypeBranchCoverage :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldTypeBranchCoverage = ReportGroupTrendFieldType' "BRANCH_COVERAGE"

pattern ReportGroupTrendFieldTypeBranchesCovered :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldTypeBranchesCovered = ReportGroupTrendFieldType' "BRANCHES_COVERED"

pattern ReportGroupTrendFieldTypeBranchesMissed :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldTypeBranchesMissed = ReportGroupTrendFieldType' "BRANCHES_MISSED"

{-# COMPLETE
  ReportGroupTrendFieldTypePassRate,
  ReportGroupTrendFieldTypeDuration,
  ReportGroupTrendFieldTypeTotal,
  ReportGroupTrendFieldTypeLineCoverage,
  ReportGroupTrendFieldTypeLinesCovered,
  ReportGroupTrendFieldTypeLinesMissed,
  ReportGroupTrendFieldTypeBranchCoverage,
  ReportGroupTrendFieldTypeBranchesCovered,
  ReportGroupTrendFieldTypeBranchesMissed,
  ReportGroupTrendFieldType'
  #-}
