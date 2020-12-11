-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LanguageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LanguageType
  ( LanguageType
      ( LanguageType',
        Android,
        Base,
        Docker,
        Dotnet,
        Golang,
        Java,
        NodeJs,
        PHP,
        Python,
        Ruby
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LanguageType = LanguageType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Android :: LanguageType
pattern Android = LanguageType' "ANDROID"

pattern Base :: LanguageType
pattern Base = LanguageType' "BASE"

pattern Docker :: LanguageType
pattern Docker = LanguageType' "DOCKER"

pattern Dotnet :: LanguageType
pattern Dotnet = LanguageType' "DOTNET"

pattern Golang :: LanguageType
pattern Golang = LanguageType' "GOLANG"

pattern Java :: LanguageType
pattern Java = LanguageType' "JAVA"

pattern NodeJs :: LanguageType
pattern NodeJs = LanguageType' "NODE_JS"

pattern PHP :: LanguageType
pattern PHP = LanguageType' "PHP"

pattern Python :: LanguageType
pattern Python = LanguageType' "PYTHON"

pattern Ruby :: LanguageType
pattern Ruby = LanguageType' "RUBY"

{-# COMPLETE
  Android,
  Base,
  Docker,
  Dotnet,
  Golang,
  Java,
  NodeJs,
  PHP,
  Python,
  Ruby,
  LanguageType'
  #-}
