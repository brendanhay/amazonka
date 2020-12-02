{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LanguageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LanguageType where

import Network.AWS.Prelude

data LanguageType
  = Android
  | Base
  | Docker
  | Dotnet
  | Golang
  | Java
  | NodeJs
  | PHP
  | Python
  | Ruby
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText LanguageType where
  parser =
    takeLowerText >>= \case
      "android" -> pure Android
      "base" -> pure Base
      "docker" -> pure Docker
      "dotnet" -> pure Dotnet
      "golang" -> pure Golang
      "java" -> pure Java
      "node_js" -> pure NodeJs
      "php" -> pure PHP
      "python" -> pure Python
      "ruby" -> pure Ruby
      e ->
        fromTextError $
          "Failure parsing LanguageType from value: '" <> e
            <> "'. Accepted values: android, base, docker, dotnet, golang, java, node_js, php, python, ruby"

instance ToText LanguageType where
  toText = \case
    Android -> "ANDROID"
    Base -> "BASE"
    Docker -> "DOCKER"
    Dotnet -> "DOTNET"
    Golang -> "GOLANG"
    Java -> "JAVA"
    NodeJs -> "NODE_JS"
    PHP -> "PHP"
    Python -> "PYTHON"
    Ruby -> "RUBY"

instance Hashable LanguageType

instance NFData LanguageType

instance ToByteString LanguageType

instance ToQuery LanguageType

instance ToHeader LanguageType

instance FromJSON LanguageType where
  parseJSON = parseJSONText "LanguageType"
