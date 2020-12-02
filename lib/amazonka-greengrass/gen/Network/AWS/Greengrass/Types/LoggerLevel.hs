{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LoggerLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerLevel where

import Network.AWS.Prelude

data LoggerLevel
  = Debug
  | Error'
  | Fatal
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

instance FromText LoggerLevel where
  parser =
    takeLowerText >>= \case
      "debug" -> pure Debug
      "error" -> pure Error'
      "fatal" -> pure Fatal
      "info" -> pure Info
      "warn" -> pure Warn
      e ->
        fromTextError $
          "Failure parsing LoggerLevel from value: '" <> e
            <> "'. Accepted values: debug, error, fatal, info, warn"

instance ToText LoggerLevel where
  toText = \case
    Debug -> "DEBUG"
    Error' -> "ERROR"
    Fatal -> "FATAL"
    Info -> "INFO"
    Warn -> "WARN"

instance Hashable LoggerLevel

instance NFData LoggerLevel

instance ToByteString LoggerLevel

instance ToQuery LoggerLevel

instance ToHeader LoggerLevel

instance ToJSON LoggerLevel where
  toJSON = toJSONText

instance FromJSON LoggerLevel where
  parseJSON = parseJSONText "LoggerLevel"
