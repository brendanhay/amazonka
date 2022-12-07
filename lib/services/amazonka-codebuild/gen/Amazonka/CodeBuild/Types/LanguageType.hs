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
-- Module      : Amazonka.CodeBuild.Types.LanguageType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.LanguageType
  ( LanguageType
      ( ..,
        LanguageType_ANDROID,
        LanguageType_BASE,
        LanguageType_DOCKER,
        LanguageType_DOTNET,
        LanguageType_GOLANG,
        LanguageType_JAVA,
        LanguageType_NODE_JS,
        LanguageType_PHP,
        LanguageType_PYTHON,
        LanguageType_RUBY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LanguageType = LanguageType'
  { fromLanguageType ::
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

pattern LanguageType_ANDROID :: LanguageType
pattern LanguageType_ANDROID = LanguageType' "ANDROID"

pattern LanguageType_BASE :: LanguageType
pattern LanguageType_BASE = LanguageType' "BASE"

pattern LanguageType_DOCKER :: LanguageType
pattern LanguageType_DOCKER = LanguageType' "DOCKER"

pattern LanguageType_DOTNET :: LanguageType
pattern LanguageType_DOTNET = LanguageType' "DOTNET"

pattern LanguageType_GOLANG :: LanguageType
pattern LanguageType_GOLANG = LanguageType' "GOLANG"

pattern LanguageType_JAVA :: LanguageType
pattern LanguageType_JAVA = LanguageType' "JAVA"

pattern LanguageType_NODE_JS :: LanguageType
pattern LanguageType_NODE_JS = LanguageType' "NODE_JS"

pattern LanguageType_PHP :: LanguageType
pattern LanguageType_PHP = LanguageType' "PHP"

pattern LanguageType_PYTHON :: LanguageType
pattern LanguageType_PYTHON = LanguageType' "PYTHON"

pattern LanguageType_RUBY :: LanguageType
pattern LanguageType_RUBY = LanguageType' "RUBY"

{-# COMPLETE
  LanguageType_ANDROID,
  LanguageType_BASE,
  LanguageType_DOCKER,
  LanguageType_DOTNET,
  LanguageType_GOLANG,
  LanguageType_JAVA,
  LanguageType_NODE_JS,
  LanguageType_PHP,
  LanguageType_PYTHON,
  LanguageType_RUBY,
  LanguageType'
  #-}
