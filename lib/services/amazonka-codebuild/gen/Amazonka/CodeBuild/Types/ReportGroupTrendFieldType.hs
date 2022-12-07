{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeBuild.Types.ReportGroupTrendFieldType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ReportGroupTrendFieldType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReportGroupTrendFieldType = ReportGroupTrendFieldType'
  { fromReportGroupTrendFieldType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
