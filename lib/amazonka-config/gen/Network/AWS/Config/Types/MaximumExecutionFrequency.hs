{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MaximumExecutionFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MaximumExecutionFrequency where

import Network.AWS.Prelude

data MaximumExecutionFrequency
  = OneHour
  | SixHours
  | ThreeHours
  | TwelveHours
  | TwentyFourHours
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

instance FromText MaximumExecutionFrequency where
  parser =
    takeLowerText >>= \case
      "one_hour" -> pure OneHour
      "six_hours" -> pure SixHours
      "three_hours" -> pure ThreeHours
      "twelve_hours" -> pure TwelveHours
      "twentyfour_hours" -> pure TwentyFourHours
      e ->
        fromTextError $
          "Failure parsing MaximumExecutionFrequency from value: '" <> e
            <> "'. Accepted values: one_hour, six_hours, three_hours, twelve_hours, twentyfour_hours"

instance ToText MaximumExecutionFrequency where
  toText = \case
    OneHour -> "One_Hour"
    SixHours -> "Six_Hours"
    ThreeHours -> "Three_Hours"
    TwelveHours -> "Twelve_Hours"
    TwentyFourHours -> "TwentyFour_Hours"

instance Hashable MaximumExecutionFrequency

instance NFData MaximumExecutionFrequency

instance ToByteString MaximumExecutionFrequency

instance ToQuery MaximumExecutionFrequency

instance ToHeader MaximumExecutionFrequency

instance ToJSON MaximumExecutionFrequency where
  toJSON = toJSONText

instance FromJSON MaximumExecutionFrequency where
  parseJSON = parseJSONText "MaximumExecutionFrequency"
