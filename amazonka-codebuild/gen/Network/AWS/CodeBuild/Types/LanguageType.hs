{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LanguageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LanguageType
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

import qualified Network.AWS.Prelude as Prelude

newtype LanguageType = LanguageType'
  { fromLanguageType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
