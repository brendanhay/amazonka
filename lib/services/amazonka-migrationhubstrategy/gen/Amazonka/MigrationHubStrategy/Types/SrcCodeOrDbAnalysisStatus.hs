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
-- Module      : Amazonka.MigrationHubStrategy.Types.SrcCodeOrDbAnalysisStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.SrcCodeOrDbAnalysisStatus
  ( SrcCodeOrDbAnalysisStatus
      ( ..,
        SrcCodeOrDbAnalysisStatus_ANALYSIS_FAILED,
        SrcCodeOrDbAnalysisStatus_ANALYSIS_PARTIAL_SUCCESS,
        SrcCodeOrDbAnalysisStatus_ANALYSIS_STARTED,
        SrcCodeOrDbAnalysisStatus_ANALYSIS_SUCCESS,
        SrcCodeOrDbAnalysisStatus_ANALYSIS_TO_BE_SCHEDULED,
        SrcCodeOrDbAnalysisStatus_CONFIGURED,
        SrcCodeOrDbAnalysisStatus_UNCONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SrcCodeOrDbAnalysisStatus = SrcCodeOrDbAnalysisStatus'
  { fromSrcCodeOrDbAnalysisStatus ::
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

pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_FAILED :: SrcCodeOrDbAnalysisStatus
pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_FAILED = SrcCodeOrDbAnalysisStatus' "ANALYSIS_FAILED"

pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_PARTIAL_SUCCESS :: SrcCodeOrDbAnalysisStatus
pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_PARTIAL_SUCCESS = SrcCodeOrDbAnalysisStatus' "ANALYSIS_PARTIAL_SUCCESS"

pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_STARTED :: SrcCodeOrDbAnalysisStatus
pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_STARTED = SrcCodeOrDbAnalysisStatus' "ANALYSIS_STARTED"

pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_SUCCESS :: SrcCodeOrDbAnalysisStatus
pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_SUCCESS = SrcCodeOrDbAnalysisStatus' "ANALYSIS_SUCCESS"

pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_TO_BE_SCHEDULED :: SrcCodeOrDbAnalysisStatus
pattern SrcCodeOrDbAnalysisStatus_ANALYSIS_TO_BE_SCHEDULED = SrcCodeOrDbAnalysisStatus' "ANALYSIS_TO_BE_SCHEDULED"

pattern SrcCodeOrDbAnalysisStatus_CONFIGURED :: SrcCodeOrDbAnalysisStatus
pattern SrcCodeOrDbAnalysisStatus_CONFIGURED = SrcCodeOrDbAnalysisStatus' "CONFIGURED"

pattern SrcCodeOrDbAnalysisStatus_UNCONFIGURED :: SrcCodeOrDbAnalysisStatus
pattern SrcCodeOrDbAnalysisStatus_UNCONFIGURED = SrcCodeOrDbAnalysisStatus' "UNCONFIGURED"

{-# COMPLETE
  SrcCodeOrDbAnalysisStatus_ANALYSIS_FAILED,
  SrcCodeOrDbAnalysisStatus_ANALYSIS_PARTIAL_SUCCESS,
  SrcCodeOrDbAnalysisStatus_ANALYSIS_STARTED,
  SrcCodeOrDbAnalysisStatus_ANALYSIS_SUCCESS,
  SrcCodeOrDbAnalysisStatus_ANALYSIS_TO_BE_SCHEDULED,
  SrcCodeOrDbAnalysisStatus_CONFIGURED,
  SrcCodeOrDbAnalysisStatus_UNCONFIGURED,
  SrcCodeOrDbAnalysisStatus'
  #-}
