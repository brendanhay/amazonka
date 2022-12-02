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
-- Module      : Amazonka.CodeBuild.Types.ReportCodeCoverageSortByType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ReportCodeCoverageSortByType
  ( ReportCodeCoverageSortByType
      ( ..,
        ReportCodeCoverageSortByType_FILE_PATH,
        ReportCodeCoverageSortByType_LINE_COVERAGE_PERCENTAGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReportCodeCoverageSortByType = ReportCodeCoverageSortByType'
  { fromReportCodeCoverageSortByType ::
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

pattern ReportCodeCoverageSortByType_FILE_PATH :: ReportCodeCoverageSortByType
pattern ReportCodeCoverageSortByType_FILE_PATH = ReportCodeCoverageSortByType' "FILE_PATH"

pattern ReportCodeCoverageSortByType_LINE_COVERAGE_PERCENTAGE :: ReportCodeCoverageSortByType
pattern ReportCodeCoverageSortByType_LINE_COVERAGE_PERCENTAGE = ReportCodeCoverageSortByType' "LINE_COVERAGE_PERCENTAGE"

{-# COMPLETE
  ReportCodeCoverageSortByType_FILE_PATH,
  ReportCodeCoverageSortByType_LINE_COVERAGE_PERCENTAGE,
  ReportCodeCoverageSortByType'
  #-}
