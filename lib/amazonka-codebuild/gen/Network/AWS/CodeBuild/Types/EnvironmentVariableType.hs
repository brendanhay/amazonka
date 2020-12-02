{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentVariableType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentVariableType where

import Network.AWS.Prelude

data EnvironmentVariableType
  = EVTParameterStore
  | EVTPlaintext
  | EVTSecretsManager
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

instance FromText EnvironmentVariableType where
  parser =
    takeLowerText >>= \case
      "parameter_store" -> pure EVTParameterStore
      "plaintext" -> pure EVTPlaintext
      "secrets_manager" -> pure EVTSecretsManager
      e ->
        fromTextError $
          "Failure parsing EnvironmentVariableType from value: '" <> e
            <> "'. Accepted values: parameter_store, plaintext, secrets_manager"

instance ToText EnvironmentVariableType where
  toText = \case
    EVTParameterStore -> "PARAMETER_STORE"
    EVTPlaintext -> "PLAINTEXT"
    EVTSecretsManager -> "SECRETS_MANAGER"

instance Hashable EnvironmentVariableType

instance NFData EnvironmentVariableType

instance ToByteString EnvironmentVariableType

instance ToQuery EnvironmentVariableType

instance ToHeader EnvironmentVariableType

instance ToJSON EnvironmentVariableType where
  toJSON = toJSONText

instance FromJSON EnvironmentVariableType where
  parseJSON = parseJSONText "EnvironmentVariableType"
