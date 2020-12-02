{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Runtime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Runtime where

import Network.AWS.Prelude

data Runtime
  = DOTNETCORE1_0
  | DOTNETCORE2_0
  | DOTNETCORE2_1
  | DOTNETCORE3_1
  | GO1_x
  | JAVA11
  | JAVA8
  | JAVA8_AL2
  | NODEJS10_x
  | NODEJS12_x
  | NODEJS4_3
  | NODEJS4_3Edge
  | NODEJS6_10
  | NODEJS8_10
  | Nodejs
  | PYTHON2_7
  | PYTHON3_6
  | PYTHON3_7
  | PYTHON3_8
  | Provided
  | Provided_AL2
  | RUBY2_5
  | RUBY2_7
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

instance FromText Runtime where
  parser =
    takeLowerText >>= \case
      "dotnetcore1.0" -> pure DOTNETCORE1_0
      "dotnetcore2.0" -> pure DOTNETCORE2_0
      "dotnetcore2.1" -> pure DOTNETCORE2_1
      "dotnetcore3.1" -> pure DOTNETCORE3_1
      "go1.x" -> pure GO1_x
      "java11" -> pure JAVA11
      "java8" -> pure JAVA8
      "java8.al2" -> pure JAVA8_AL2
      "nodejs10.x" -> pure NODEJS10_x
      "nodejs12.x" -> pure NODEJS12_x
      "nodejs4.3" -> pure NODEJS4_3
      "nodejs4.3-edge" -> pure NODEJS4_3Edge
      "nodejs6.10" -> pure NODEJS6_10
      "nodejs8.10" -> pure NODEJS8_10
      "nodejs" -> pure Nodejs
      "python2.7" -> pure PYTHON2_7
      "python3.6" -> pure PYTHON3_6
      "python3.7" -> pure PYTHON3_7
      "python3.8" -> pure PYTHON3_8
      "provided" -> pure Provided
      "provided.al2" -> pure Provided_AL2
      "ruby2.5" -> pure RUBY2_5
      "ruby2.7" -> pure RUBY2_7
      e ->
        fromTextError $
          "Failure parsing Runtime from value: '" <> e
            <> "'. Accepted values: dotnetcore1.0, dotnetcore2.0, dotnetcore2.1, dotnetcore3.1, go1.x, java11, java8, java8.al2, nodejs10.x, nodejs12.x, nodejs4.3, nodejs4.3-edge, nodejs6.10, nodejs8.10, nodejs, python2.7, python3.6, python3.7, python3.8, provided, provided.al2, ruby2.5, ruby2.7"

instance ToText Runtime where
  toText = \case
    DOTNETCORE1_0 -> "dotnetcore1.0"
    DOTNETCORE2_0 -> "dotnetcore2.0"
    DOTNETCORE2_1 -> "dotnetcore2.1"
    DOTNETCORE3_1 -> "dotnetcore3.1"
    GO1_x -> "go1.x"
    JAVA11 -> "java11"
    JAVA8 -> "java8"
    JAVA8_AL2 -> "java8.al2"
    NODEJS10_x -> "nodejs10.x"
    NODEJS12_x -> "nodejs12.x"
    NODEJS4_3 -> "nodejs4.3"
    NODEJS4_3Edge -> "nodejs4.3-edge"
    NODEJS6_10 -> "nodejs6.10"
    NODEJS8_10 -> "nodejs8.10"
    Nodejs -> "nodejs"
    PYTHON2_7 -> "python2.7"
    PYTHON3_6 -> "python3.6"
    PYTHON3_7 -> "python3.7"
    PYTHON3_8 -> "python3.8"
    Provided -> "provided"
    Provided_AL2 -> "provided.al2"
    RUBY2_5 -> "ruby2.5"
    RUBY2_7 -> "ruby2.7"

instance Hashable Runtime

instance NFData Runtime

instance ToByteString Runtime

instance ToQuery Runtime

instance ToHeader Runtime

instance ToJSON Runtime where
  toJSON = toJSONText

instance FromJSON Runtime where
  parseJSON = parseJSONText "Runtime"
