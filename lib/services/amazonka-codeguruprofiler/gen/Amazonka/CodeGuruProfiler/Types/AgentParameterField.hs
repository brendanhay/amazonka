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
-- Module      : Amazonka.CodeGuruProfiler.Types.AgentParameterField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.AgentParameterField
  ( AgentParameterField
      ( ..,
        AgentParameterField_MaxStackDepth,
        AgentParameterField_MemoryUsageLimitPercent,
        AgentParameterField_MinimumTimeForReportingInMilliseconds,
        AgentParameterField_ReportingIntervalInMilliseconds,
        AgentParameterField_SamplingIntervalInMilliseconds
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AgentParameterField = AgentParameterField'
  { fromAgentParameterField ::
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

pattern AgentParameterField_MaxStackDepth :: AgentParameterField
pattern AgentParameterField_MaxStackDepth = AgentParameterField' "MaxStackDepth"

pattern AgentParameterField_MemoryUsageLimitPercent :: AgentParameterField
pattern AgentParameterField_MemoryUsageLimitPercent = AgentParameterField' "MemoryUsageLimitPercent"

pattern AgentParameterField_MinimumTimeForReportingInMilliseconds :: AgentParameterField
pattern AgentParameterField_MinimumTimeForReportingInMilliseconds = AgentParameterField' "MinimumTimeForReportingInMilliseconds"

pattern AgentParameterField_ReportingIntervalInMilliseconds :: AgentParameterField
pattern AgentParameterField_ReportingIntervalInMilliseconds = AgentParameterField' "ReportingIntervalInMilliseconds"

pattern AgentParameterField_SamplingIntervalInMilliseconds :: AgentParameterField
pattern AgentParameterField_SamplingIntervalInMilliseconds = AgentParameterField' "SamplingIntervalInMilliseconds"

{-# COMPLETE
  AgentParameterField_MaxStackDepth,
  AgentParameterField_MemoryUsageLimitPercent,
  AgentParameterField_MinimumTimeForReportingInMilliseconds,
  AgentParameterField_ReportingIntervalInMilliseconds,
  AgentParameterField_SamplingIntervalInMilliseconds,
  AgentParameterField'
  #-}
