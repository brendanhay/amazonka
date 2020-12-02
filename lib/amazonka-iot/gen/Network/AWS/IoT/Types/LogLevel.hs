{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogLevel where

import Network.AWS.Prelude

data LogLevel
  = Debug
  | Disabled
  | Error'
  | Info
  | Warn
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

instance FromText LogLevel where
  parser =
    takeLowerText >>= \case
      "debug" -> pure Debug
      "disabled" -> pure Disabled
      "error" -> pure Error'
      "info" -> pure Info
      "warn" -> pure Warn
      e ->
        fromTextError $
          "Failure parsing LogLevel from value: '" <> e
            <> "'. Accepted values: debug, disabled, error, info, warn"

instance ToText LogLevel where
  toText = \case
    Debug -> "DEBUG"
    Disabled -> "DISABLED"
    Error' -> "ERROR"
    Info -> "INFO"
    Warn -> "WARN"

instance Hashable LogLevel

instance NFData LogLevel

instance ToByteString LogLevel

instance ToQuery LogLevel

instance ToHeader LogLevel

instance ToJSON LogLevel where
  toJSON = toJSONText

instance FromJSON LogLevel where
  parseJSON = parseJSONText "LogLevel"
