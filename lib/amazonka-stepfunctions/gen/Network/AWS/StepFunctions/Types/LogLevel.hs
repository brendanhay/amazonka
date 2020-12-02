{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LogLevel where

import Network.AWS.Prelude

data LogLevel
  = All
  | Error'
  | Fatal
  | Off
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
      "all" -> pure All
      "error" -> pure Error'
      "fatal" -> pure Fatal
      "off" -> pure Off
      e ->
        fromTextError $
          "Failure parsing LogLevel from value: '" <> e
            <> "'. Accepted values: all, error, fatal, off"

instance ToText LogLevel where
  toText = \case
    All -> "ALL"
    Error' -> "ERROR"
    Fatal -> "FATAL"
    Off -> "OFF"

instance Hashable LogLevel

instance NFData LogLevel

instance ToByteString LogLevel

instance ToQuery LogLevel

instance ToHeader LogLevel

instance ToJSON LogLevel where
  toJSON = toJSONText

instance FromJSON LogLevel where
  parseJSON = parseJSONText "LogLevel"
