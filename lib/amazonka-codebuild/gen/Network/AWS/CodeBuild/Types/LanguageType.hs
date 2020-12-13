{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Java,
        Python,
        NodeJs,
        Ruby,
        Golang,
        Docker,
        Android,
        Dotnet,
        Base,
        PHP
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

pattern Java :: LanguageType
pattern Java = LanguageType' "JAVA"

pattern Python :: LanguageType
pattern Python = LanguageType' "PYTHON"

pattern NodeJs :: LanguageType
pattern NodeJs = LanguageType' "NODE_JS"

pattern Ruby :: LanguageType
pattern Ruby = LanguageType' "RUBY"

pattern Golang :: LanguageType
pattern Golang = LanguageType' "GOLANG"

pattern Docker :: LanguageType
pattern Docker = LanguageType' "DOCKER"

pattern Android :: LanguageType
pattern Android = LanguageType' "ANDROID"

pattern Dotnet :: LanguageType
pattern Dotnet = LanguageType' "DOTNET"

pattern Base :: LanguageType
pattern Base = LanguageType' "BASE"

pattern PHP :: LanguageType
pattern PHP = LanguageType' "PHP"

{-# COMPLETE
  Java,
  Python,
  NodeJs,
  Ruby,
  Golang,
  Docker,
  Android,
  Dotnet,
  Base,
  PHP,
  LanguageType'
  #-}
