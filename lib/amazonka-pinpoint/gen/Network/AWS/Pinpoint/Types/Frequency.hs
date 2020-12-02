{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Frequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Frequency where

import Network.AWS.Prelude

data Frequency
  = Daily
  | Event
  | Hourly
  | Monthly
  | Once
  | Weekly
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

instance FromText Frequency where
  parser =
    takeLowerText >>= \case
      "daily" -> pure Daily
      "event" -> pure Event
      "hourly" -> pure Hourly
      "monthly" -> pure Monthly
      "once" -> pure Once
      "weekly" -> pure Weekly
      e ->
        fromTextError $
          "Failure parsing Frequency from value: '" <> e
            <> "'. Accepted values: daily, event, hourly, monthly, once, weekly"

instance ToText Frequency where
  toText = \case
    Daily -> "DAILY"
    Event -> "EVENT"
    Hourly -> "HOURLY"
    Monthly -> "MONTHLY"
    Once -> "ONCE"
    Weekly -> "WEEKLY"

instance Hashable Frequency

instance NFData Frequency

instance ToByteString Frequency

instance ToQuery Frequency

instance ToHeader Frequency

instance ToJSON Frequency where
  toJSON = toJSONText

instance FromJSON Frequency where
  parseJSON = parseJSONText "Frequency"
