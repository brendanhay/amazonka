{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.QuotaPeriodType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.QuotaPeriodType where

import Network.AWS.Prelude

data QuotaPeriodType
  = Day
  | Month
  | Week
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

instance FromText QuotaPeriodType where
  parser =
    takeLowerText >>= \case
      "day" -> pure Day
      "month" -> pure Month
      "week" -> pure Week
      e ->
        fromTextError $
          "Failure parsing QuotaPeriodType from value: '" <> e
            <> "'. Accepted values: day, month, week"

instance ToText QuotaPeriodType where
  toText = \case
    Day -> "DAY"
    Month -> "MONTH"
    Week -> "WEEK"

instance Hashable QuotaPeriodType

instance NFData QuotaPeriodType

instance ToByteString QuotaPeriodType

instance ToQuery QuotaPeriodType

instance ToHeader QuotaPeriodType

instance ToJSON QuotaPeriodType where
  toJSON = toJSONText

instance FromJSON QuotaPeriodType where
  parseJSON = parseJSONText "QuotaPeriodType"
