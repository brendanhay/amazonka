{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EventSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EventSeverity where

import Network.AWS.Prelude

data EventSeverity
  = LevelDebug
  | LevelError'
  | LevelFatal
  | LevelInfo
  | LevelTrace
  | LevelWarn
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

instance FromText EventSeverity where
  parser =
    takeLowerText >>= \case
      "debug" -> pure LevelDebug
      "error" -> pure LevelError'
      "fatal" -> pure LevelFatal
      "info" -> pure LevelInfo
      "trace" -> pure LevelTrace
      "warn" -> pure LevelWarn
      e ->
        fromTextError $
          "Failure parsing EventSeverity from value: '" <> e
            <> "'. Accepted values: debug, error, fatal, info, trace, warn"

instance ToText EventSeverity where
  toText = \case
    LevelDebug -> "DEBUG"
    LevelError' -> "ERROR"
    LevelFatal -> "FATAL"
    LevelInfo -> "INFO"
    LevelTrace -> "TRACE"
    LevelWarn -> "WARN"

instance Hashable EventSeverity

instance NFData EventSeverity

instance ToByteString EventSeverity

instance ToQuery EventSeverity

instance ToHeader EventSeverity

instance FromXML EventSeverity where
  parseXML = parseXMLText "EventSeverity"
