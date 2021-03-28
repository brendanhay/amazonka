{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LanguageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.LanguageType
  ( LanguageType
    ( LanguageType'
    , LanguageTypeJava
    , LanguageTypePython
    , LanguageTypeNodeJs
    , LanguageTypeRuby
    , LanguageTypeGolang
    , LanguageTypeDocker
    , LanguageTypeAndroid
    , LanguageTypeDotnet
    , LanguageTypeBase
    , LanguageTypePhp
    , fromLanguageType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LanguageType = LanguageType'{fromLanguageType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern LanguageTypeJava :: LanguageType
pattern LanguageTypeJava = LanguageType' "JAVA"

pattern LanguageTypePython :: LanguageType
pattern LanguageTypePython = LanguageType' "PYTHON"

pattern LanguageTypeNodeJs :: LanguageType
pattern LanguageTypeNodeJs = LanguageType' "NODE_JS"

pattern LanguageTypeRuby :: LanguageType
pattern LanguageTypeRuby = LanguageType' "RUBY"

pattern LanguageTypeGolang :: LanguageType
pattern LanguageTypeGolang = LanguageType' "GOLANG"

pattern LanguageTypeDocker :: LanguageType
pattern LanguageTypeDocker = LanguageType' "DOCKER"

pattern LanguageTypeAndroid :: LanguageType
pattern LanguageTypeAndroid = LanguageType' "ANDROID"

pattern LanguageTypeDotnet :: LanguageType
pattern LanguageTypeDotnet = LanguageType' "DOTNET"

pattern LanguageTypeBase :: LanguageType
pattern LanguageTypeBase = LanguageType' "BASE"

pattern LanguageTypePhp :: LanguageType
pattern LanguageTypePhp = LanguageType' "PHP"

{-# COMPLETE 
  LanguageTypeJava,

  LanguageTypePython,

  LanguageTypeNodeJs,

  LanguageTypeRuby,

  LanguageTypeGolang,

  LanguageTypeDocker,

  LanguageTypeAndroid,

  LanguageTypeDotnet,

  LanguageTypeBase,

  LanguageTypePhp,
  LanguageType'
  #-}
