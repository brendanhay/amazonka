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
-- Module      : Amazonka.MigrationHubStrategy.Types.RuntimeAnalysisStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.RuntimeAnalysisStatus
  ( RuntimeAnalysisStatus
      ( ..,
        RuntimeAnalysisStatus_ANALYSIS_FAILED,
        RuntimeAnalysisStatus_ANALYSIS_STARTED,
        RuntimeAnalysisStatus_ANALYSIS_SUCCESS,
        RuntimeAnalysisStatus_ANALYSIS_TO_BE_SCHEDULED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RuntimeAnalysisStatus = RuntimeAnalysisStatus'
  { fromRuntimeAnalysisStatus ::
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

pattern RuntimeAnalysisStatus_ANALYSIS_FAILED :: RuntimeAnalysisStatus
pattern RuntimeAnalysisStatus_ANALYSIS_FAILED = RuntimeAnalysisStatus' "ANALYSIS_FAILED"

pattern RuntimeAnalysisStatus_ANALYSIS_STARTED :: RuntimeAnalysisStatus
pattern RuntimeAnalysisStatus_ANALYSIS_STARTED = RuntimeAnalysisStatus' "ANALYSIS_STARTED"

pattern RuntimeAnalysisStatus_ANALYSIS_SUCCESS :: RuntimeAnalysisStatus
pattern RuntimeAnalysisStatus_ANALYSIS_SUCCESS = RuntimeAnalysisStatus' "ANALYSIS_SUCCESS"

pattern RuntimeAnalysisStatus_ANALYSIS_TO_BE_SCHEDULED :: RuntimeAnalysisStatus
pattern RuntimeAnalysisStatus_ANALYSIS_TO_BE_SCHEDULED = RuntimeAnalysisStatus' "ANALYSIS_TO_BE_SCHEDULED"

{-# COMPLETE
  RuntimeAnalysisStatus_ANALYSIS_FAILED,
  RuntimeAnalysisStatus_ANALYSIS_STARTED,
  RuntimeAnalysisStatus_ANALYSIS_SUCCESS,
  RuntimeAnalysisStatus_ANALYSIS_TO_BE_SCHEDULED,
  RuntimeAnalysisStatus'
  #-}
