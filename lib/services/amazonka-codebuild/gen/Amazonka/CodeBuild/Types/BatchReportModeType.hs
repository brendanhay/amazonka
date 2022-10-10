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
-- Module      : Amazonka.CodeBuild.Types.BatchReportModeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BatchReportModeType
  ( BatchReportModeType
      ( ..,
        BatchReportModeType_REPORT_AGGREGATED_BATCH,
        BatchReportModeType_REPORT_INDIVIDUAL_BUILDS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype BatchReportModeType = BatchReportModeType'
  { fromBatchReportModeType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern BatchReportModeType_REPORT_AGGREGATED_BATCH :: BatchReportModeType
pattern BatchReportModeType_REPORT_AGGREGATED_BATCH = BatchReportModeType' "REPORT_AGGREGATED_BATCH"

pattern BatchReportModeType_REPORT_INDIVIDUAL_BUILDS :: BatchReportModeType
pattern BatchReportModeType_REPORT_INDIVIDUAL_BUILDS = BatchReportModeType' "REPORT_INDIVIDUAL_BUILDS"

{-# COMPLETE
  BatchReportModeType_REPORT_AGGREGATED_BATCH,
  BatchReportModeType_REPORT_INDIVIDUAL_BUILDS,
  BatchReportModeType'
  #-}
