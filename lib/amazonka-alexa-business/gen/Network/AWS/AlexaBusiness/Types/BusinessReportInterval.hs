{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportInterval where

import Network.AWS.Prelude

data BusinessReportInterval
  = OneDay
  | OneWeek
  | ThirtyDays
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

instance FromText BusinessReportInterval where
  parser =
    takeLowerText >>= \case
      "one_day" -> pure OneDay
      "one_week" -> pure OneWeek
      "thirty_days" -> pure ThirtyDays
      e ->
        fromTextError $
          "Failure parsing BusinessReportInterval from value: '" <> e
            <> "'. Accepted values: one_day, one_week, thirty_days"

instance ToText BusinessReportInterval where
  toText = \case
    OneDay -> "ONE_DAY"
    OneWeek -> "ONE_WEEK"
    ThirtyDays -> "THIRTY_DAYS"

instance Hashable BusinessReportInterval

instance NFData BusinessReportInterval

instance ToByteString BusinessReportInterval

instance ToQuery BusinessReportInterval

instance ToHeader BusinessReportInterval

instance ToJSON BusinessReportInterval where
  toJSON = toJSONText

instance FromJSON BusinessReportInterval where
  parseJSON = parseJSONText "BusinessReportInterval"
