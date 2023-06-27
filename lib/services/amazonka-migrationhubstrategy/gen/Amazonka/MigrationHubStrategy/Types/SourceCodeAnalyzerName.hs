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
-- Module      : Amazonka.MigrationHubStrategy.Types.SourceCodeAnalyzerName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.SourceCodeAnalyzerName
  ( SourceCodeAnalyzerName
      ( ..,
        SourceCodeAnalyzerName_BYTECODE_ANALYZER,
        SourceCodeAnalyzerName_CSHARP_ANALYZER,
        SourceCodeAnalyzerName_JAVA_ANALYZER,
        SourceCodeAnalyzerName_PORTING_ASSISTANT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceCodeAnalyzerName = SourceCodeAnalyzerName'
  { fromSourceCodeAnalyzerName ::
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

pattern SourceCodeAnalyzerName_BYTECODE_ANALYZER :: SourceCodeAnalyzerName
pattern SourceCodeAnalyzerName_BYTECODE_ANALYZER = SourceCodeAnalyzerName' "BYTECODE_ANALYZER"

pattern SourceCodeAnalyzerName_CSHARP_ANALYZER :: SourceCodeAnalyzerName
pattern SourceCodeAnalyzerName_CSHARP_ANALYZER = SourceCodeAnalyzerName' "CSHARP_ANALYZER"

pattern SourceCodeAnalyzerName_JAVA_ANALYZER :: SourceCodeAnalyzerName
pattern SourceCodeAnalyzerName_JAVA_ANALYZER = SourceCodeAnalyzerName' "JAVA_ANALYZER"

pattern SourceCodeAnalyzerName_PORTING_ASSISTANT :: SourceCodeAnalyzerName
pattern SourceCodeAnalyzerName_PORTING_ASSISTANT = SourceCodeAnalyzerName' "PORTING_ASSISTANT"

{-# COMPLETE
  SourceCodeAnalyzerName_BYTECODE_ANALYZER,
  SourceCodeAnalyzerName_CSHARP_ANALYZER,
  SourceCodeAnalyzerName_JAVA_ANALYZER,
  SourceCodeAnalyzerName_PORTING_ASSISTANT,
  SourceCodeAnalyzerName'
  #-}
