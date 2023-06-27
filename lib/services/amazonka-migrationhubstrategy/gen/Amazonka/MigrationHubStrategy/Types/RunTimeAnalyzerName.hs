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
-- Module      : Amazonka.MigrationHubStrategy.Types.RunTimeAnalyzerName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.RunTimeAnalyzerName
  ( RunTimeAnalyzerName
      ( ..,
        RunTimeAnalyzerName_A2C_ANALYZER,
        RunTimeAnalyzerName_DATABASE_ANALYZER,
        RunTimeAnalyzerName_EMP_PA_ANALYZER,
        RunTimeAnalyzerName_REHOST_ANALYZER,
        RunTimeAnalyzerName_SCT_ANALYZER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RunTimeAnalyzerName = RunTimeAnalyzerName'
  { fromRunTimeAnalyzerName ::
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

pattern RunTimeAnalyzerName_A2C_ANALYZER :: RunTimeAnalyzerName
pattern RunTimeAnalyzerName_A2C_ANALYZER = RunTimeAnalyzerName' "A2C_ANALYZER"

pattern RunTimeAnalyzerName_DATABASE_ANALYZER :: RunTimeAnalyzerName
pattern RunTimeAnalyzerName_DATABASE_ANALYZER = RunTimeAnalyzerName' "DATABASE_ANALYZER"

pattern RunTimeAnalyzerName_EMP_PA_ANALYZER :: RunTimeAnalyzerName
pattern RunTimeAnalyzerName_EMP_PA_ANALYZER = RunTimeAnalyzerName' "EMP_PA_ANALYZER"

pattern RunTimeAnalyzerName_REHOST_ANALYZER :: RunTimeAnalyzerName
pattern RunTimeAnalyzerName_REHOST_ANALYZER = RunTimeAnalyzerName' "REHOST_ANALYZER"

pattern RunTimeAnalyzerName_SCT_ANALYZER :: RunTimeAnalyzerName
pattern RunTimeAnalyzerName_SCT_ANALYZER = RunTimeAnalyzerName' "SCT_ANALYZER"

{-# COMPLETE
  RunTimeAnalyzerName_A2C_ANALYZER,
  RunTimeAnalyzerName_DATABASE_ANALYZER,
  RunTimeAnalyzerName_EMP_PA_ANALYZER,
  RunTimeAnalyzerName_REHOST_ANALYZER,
  RunTimeAnalyzerName_SCT_ANALYZER,
  RunTimeAnalyzerName'
  #-}
