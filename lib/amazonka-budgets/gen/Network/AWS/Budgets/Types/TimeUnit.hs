{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.TimeUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.TimeUnit where

import Network.AWS.Prelude

-- | The time unit of the budget, such as MONTHLY or QUARTERLY.
data TimeUnit
  = Annually
  | Daily
  | Monthly
  | Quarterly
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

instance FromText TimeUnit where
  parser =
    takeLowerText >>= \case
      "annually" -> pure Annually
      "daily" -> pure Daily
      "monthly" -> pure Monthly
      "quarterly" -> pure Quarterly
      e ->
        fromTextError $
          "Failure parsing TimeUnit from value: '" <> e
            <> "'. Accepted values: annually, daily, monthly, quarterly"

instance ToText TimeUnit where
  toText = \case
    Annually -> "ANNUALLY"
    Daily -> "DAILY"
    Monthly -> "MONTHLY"
    Quarterly -> "QUARTERLY"

instance Hashable TimeUnit

instance NFData TimeUnit

instance ToByteString TimeUnit

instance ToQuery TimeUnit

instance ToHeader TimeUnit

instance ToJSON TimeUnit where
  toJSON = toJSONText

instance FromJSON TimeUnit where
  parseJSON = parseJSONText "TimeUnit"
