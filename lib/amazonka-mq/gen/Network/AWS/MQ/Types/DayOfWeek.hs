{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.DayOfWeek
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.DayOfWeek where

import Network.AWS.Prelude

data DayOfWeek
  = Friday
  | Monday
  | Saturday
  | Sunday
  | Thursday
  | Tuesday
  | Wednesday
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
      "friday" -> pure Friday
      "monday" -> pure Monday
      "saturday" -> pure Saturday
      "sunday" -> pure Sunday
      "thursday" -> pure Thursday
      "tuesday" -> pure Tuesday
      "wednesday" -> pure Wednesday
      e ->
        fromTextError $
          "Failure parsing DayOfWeek from value: '" <> e
            <> "'. Accepted values: friday, monday, saturday, sunday, thursday, tuesday, wednesday"

instance ToText DayOfWeek where
  toText = \case
    Friday -> "FRIDAY"
    Monday -> "MONDAY"
    Saturday -> "SATURDAY"
    Sunday -> "SUNDAY"
    Thursday -> "THURSDAY"
    Tuesday -> "TUESDAY"
    Wednesday -> "WEDNESDAY"

instance Hashable DayOfWeek

instance NFData DayOfWeek

instance ToByteString DayOfWeek

instance ToQuery DayOfWeek

instance ToHeader DayOfWeek

instance ToJSON DayOfWeek where
  toJSON = toJSONText

instance FromJSON DayOfWeek where
  parseJSON = parseJSONText "DayOfWeek"
