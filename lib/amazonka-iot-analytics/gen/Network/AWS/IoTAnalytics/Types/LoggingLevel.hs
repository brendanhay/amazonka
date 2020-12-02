{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LoggingLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LoggingLevel where

import Network.AWS.Prelude

data LoggingLevel = Error'
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

instance FromText LoggingLevel where
  parser =
    takeLowerText >>= \case
      "error" -> pure Error'
      e ->
        fromTextError $
          "Failure parsing LoggingLevel from value: '" <> e
            <> "'. Accepted values: error"

instance ToText LoggingLevel where
  toText = \case
    Error' -> "ERROR"

instance Hashable LoggingLevel

instance NFData LoggingLevel

instance ToByteString LoggingLevel

instance ToQuery LoggingLevel

instance ToHeader LoggingLevel

instance ToJSON LoggingLevel where
  toJSON = toJSONText

instance FromJSON LoggingLevel where
  parseJSON = parseJSONText "LoggingLevel"
