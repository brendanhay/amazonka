{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DayOfWeek
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DayOfWeek where

import Network.AWS.Prelude

data DayOfWeek
  = Fri
  | Mon
  | Sat
  | Sun
  | Thu
  | Tue
  | Wed
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

instance FromText DayOfWeek where
  parser =
    takeLowerText >>= \case
      "fri" -> pure Fri
      "mon" -> pure Mon
      "sat" -> pure Sat
      "sun" -> pure Sun
      "thu" -> pure Thu
      "tue" -> pure Tue
      "wed" -> pure Wed
      e ->
        fromTextError $
          "Failure parsing DayOfWeek from value: '" <> e
            <> "'. Accepted values: fri, mon, sat, sun, thu, tue, wed"

instance ToText DayOfWeek where
  toText = \case
    Fri -> "FRI"
    Mon -> "MON"
    Sat -> "SAT"
    Sun -> "SUN"
    Thu -> "THU"
    Tue -> "TUE"
    Wed -> "WED"

instance Hashable DayOfWeek

instance NFData DayOfWeek

instance ToByteString DayOfWeek

instance ToQuery DayOfWeek

instance ToHeader DayOfWeek

instance ToJSON DayOfWeek where
  toJSON = toJSONText

instance FromJSON DayOfWeek where
  parseJSON = parseJSONText "DayOfWeek"
