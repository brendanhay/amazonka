{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.LogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.LogLevel where

import Network.AWS.Prelude

-- | The log level the user wants for their channel.
data LogLevel
  = LLDebug
  | LLDisabled
  | LLError'
  | LLInfo
  | LLWarning
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
      "debug" -> pure LLDebug
      "disabled" -> pure LLDisabled
      "error" -> pure LLError'
      "info" -> pure LLInfo
      "warning" -> pure LLWarning
      e ->
        fromTextError $
          "Failure parsing LogLevel from value: '" <> e
            <> "'. Accepted values: debug, disabled, error, info, warning"

instance ToText LogLevel where
  toText = \case
    LLDebug -> "DEBUG"
    LLDisabled -> "DISABLED"
    LLError' -> "ERROR"
    LLInfo -> "INFO"
    LLWarning -> "WARNING"

instance Hashable LogLevel

instance NFData LogLevel

instance ToByteString LogLevel

instance ToQuery LogLevel

instance ToHeader LogLevel

instance ToJSON LogLevel where
  toJSON = toJSONText

instance FromJSON LogLevel where
  parseJSON = parseJSONText "LogLevel"
