{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupTrendFieldType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupTrendFieldType
  ( ReportGroupTrendFieldType
      ( ..,
        ReportGroupTrendFieldType_BRANCHES_COVERED,
        ReportGroupTrendFieldType_BRANCHES_MISSED,
        ReportGroupTrendFieldType_BRANCH_COVERAGE,
        ReportGroupTrendFieldType_DURATION,
        ReportGroupTrendFieldType_LINES_COVERED,
        ReportGroupTrendFieldType_LINES_MISSED,
        ReportGroupTrendFieldType_LINE_COVERAGE,
        ReportGroupTrendFieldType_PASS_RATE,
        ReportGroupTrendFieldType_TOTAL
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ReportGroupTrendFieldType = ReportGroupTrendFieldType'
  { fromReportGroupTrendFieldType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ReportGroupTrendFieldType_BRANCHES_COVERED :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldType_BRANCHES_COVERED = ReportGroupTrendFieldType' "BRANCHES_COVERED"

pattern ReportGroupTrendFieldType_BRANCHES_MISSED :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldType_BRANCHES_MISSED = ReportGroupTrendFieldType' "BRANCHES_MISSED"

pattern ReportGroupTrendFieldType_BRANCH_COVERAGE :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldType_BRANCH_COVERAGE = ReportGroupTrendFieldType' "BRANCH_COVERAGE"

pattern ReportGroupTrendFieldType_DURATION :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldType_DURATION = ReportGroupTrendFieldType' "DURATION"

pattern ReportGroupTrendFieldType_LINES_COVERED :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldType_LINES_COVERED = ReportGroupTrendFieldType' "LINES_COVERED"

pattern ReportGroupTrendFieldType_LINES_MISSED :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldType_LINES_MISSED = ReportGroupTrendFieldType' "LINES_MISSED"

pattern ReportGroupTrendFieldType_LINE_COVERAGE :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldType_LINE_COVERAGE = ReportGroupTrendFieldType' "LINE_COVERAGE"

pattern ReportGroupTrendFieldType_PASS_RATE :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldType_PASS_RATE = ReportGroupTrendFieldType' "PASS_RATE"

pattern ReportGroupTrendFieldType_TOTAL :: ReportGroupTrendFieldType
pattern ReportGroupTrendFieldType_TOTAL = ReportGroupTrendFieldType' "TOTAL"

{-# COMPLETE
  ReportGroupTrendFieldType_BRANCHES_COVERED,
  ReportGroupTrendFieldType_BRANCHES_MISSED,
  ReportGroupTrendFieldType_BRANCH_COVERAGE,
  ReportGroupTrendFieldType_DURATION,
  ReportGroupTrendFieldType_LINES_COVERED,
  ReportGroupTrendFieldType_LINES_MISSED,
  ReportGroupTrendFieldType_LINE_COVERAGE,
  ReportGroupTrendFieldType_PASS_RATE,
  ReportGroupTrendFieldType_TOTAL,
  ReportGroupTrendFieldType'
  #-}
