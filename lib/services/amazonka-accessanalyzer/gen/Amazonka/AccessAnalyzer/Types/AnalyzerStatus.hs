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
-- Module      : Amazonka.AccessAnalyzer.Types.AnalyzerStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AnalyzerStatus
  ( AnalyzerStatus
      ( ..,
        AnalyzerStatus_ACTIVE,
        AnalyzerStatus_CREATING,
        AnalyzerStatus_DISABLED,
        AnalyzerStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnalyzerStatus = AnalyzerStatus'
  { fromAnalyzerStatus ::
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

pattern AnalyzerStatus_ACTIVE :: AnalyzerStatus
pattern AnalyzerStatus_ACTIVE = AnalyzerStatus' "ACTIVE"

pattern AnalyzerStatus_CREATING :: AnalyzerStatus
pattern AnalyzerStatus_CREATING = AnalyzerStatus' "CREATING"

pattern AnalyzerStatus_DISABLED :: AnalyzerStatus
pattern AnalyzerStatus_DISABLED = AnalyzerStatus' "DISABLED"

pattern AnalyzerStatus_FAILED :: AnalyzerStatus
pattern AnalyzerStatus_FAILED = AnalyzerStatus' "FAILED"

{-# COMPLETE
  AnalyzerStatus_ACTIVE,
  AnalyzerStatus_CREATING,
  AnalyzerStatus_DISABLED,
  AnalyzerStatus_FAILED,
  AnalyzerStatus'
  #-}
