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
-- Module      : Amazonka.MigrationHubStrategy.Types.AnalysisType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AnalysisType
  ( AnalysisType
      ( ..,
        AnalysisType_BINARY_ANALYSIS,
        AnalysisType_DATABASE_ANALYSIS,
        AnalysisType_RUNTIME_ANALYSIS,
        AnalysisType_SOURCE_CODE_ANALYSIS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnalysisType = AnalysisType'
  { fromAnalysisType ::
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

pattern AnalysisType_BINARY_ANALYSIS :: AnalysisType
pattern AnalysisType_BINARY_ANALYSIS = AnalysisType' "BINARY_ANALYSIS"

pattern AnalysisType_DATABASE_ANALYSIS :: AnalysisType
pattern AnalysisType_DATABASE_ANALYSIS = AnalysisType' "DATABASE_ANALYSIS"

pattern AnalysisType_RUNTIME_ANALYSIS :: AnalysisType
pattern AnalysisType_RUNTIME_ANALYSIS = AnalysisType' "RUNTIME_ANALYSIS"

pattern AnalysisType_SOURCE_CODE_ANALYSIS :: AnalysisType
pattern AnalysisType_SOURCE_CODE_ANALYSIS = AnalysisType' "SOURCE_CODE_ANALYSIS"

{-# COMPLETE
  AnalysisType_BINARY_ANALYSIS,
  AnalysisType_DATABASE_ANALYSIS,
  AnalysisType_RUNTIME_ANALYSIS,
  AnalysisType_SOURCE_CODE_ANALYSIS,
  AnalysisType'
  #-}
