{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.UpdateAgentLogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.UpdateAgentLogLevel where

import Network.AWS.Prelude

-- | The minimum level of log statements that should be logged by the OTA Agent during an update.
data UpdateAgentLogLevel
  = UALLDebug
  | UALLError'
  | UALLFatal
  | UALLInfo
  | UALLNone
  | UALLTrace
  | UALLVerbose
  | UALLWarn
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

instance FromText UpdateAgentLogLevel where
  parser =
    takeLowerText >>= \case
      "debug" -> pure UALLDebug
      "error" -> pure UALLError'
      "fatal" -> pure UALLFatal
      "info" -> pure UALLInfo
      "none" -> pure UALLNone
      "trace" -> pure UALLTrace
      "verbose" -> pure UALLVerbose
      "warn" -> pure UALLWarn
      e ->
        fromTextError $
          "Failure parsing UpdateAgentLogLevel from value: '" <> e
            <> "'. Accepted values: debug, error, fatal, info, none, trace, verbose, warn"

instance ToText UpdateAgentLogLevel where
  toText = \case
    UALLDebug -> "DEBUG"
    UALLError' -> "ERROR"
    UALLFatal -> "FATAL"
    UALLInfo -> "INFO"
    UALLNone -> "NONE"
    UALLTrace -> "TRACE"
    UALLVerbose -> "VERBOSE"
    UALLWarn -> "WARN"

instance Hashable UpdateAgentLogLevel

instance NFData UpdateAgentLogLevel

instance ToByteString UpdateAgentLogLevel

instance ToQuery UpdateAgentLogLevel

instance ToHeader UpdateAgentLogLevel

instance ToJSON UpdateAgentLogLevel where
  toJSON = toJSONText
