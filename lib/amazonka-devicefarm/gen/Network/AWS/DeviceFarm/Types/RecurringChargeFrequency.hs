{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RecurringChargeFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.RecurringChargeFrequency where

import Network.AWS.Prelude

data RecurringChargeFrequency = Monthly
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

instance FromText RecurringChargeFrequency where
  parser =
    takeLowerText >>= \case
      "monthly" -> pure Monthly
      e ->
        fromTextError $
          "Failure parsing RecurringChargeFrequency from value: '" <> e
            <> "'. Accepted values: monthly"

instance ToText RecurringChargeFrequency where
  toText = \case
    Monthly -> "MONTHLY"

instance Hashable RecurringChargeFrequency

instance NFData RecurringChargeFrequency

instance ToByteString RecurringChargeFrequency

instance ToQuery RecurringChargeFrequency

instance ToHeader RecurringChargeFrequency

instance FromJSON RecurringChargeFrequency where
  parseJSON = parseJSONText "RecurringChargeFrequency"
