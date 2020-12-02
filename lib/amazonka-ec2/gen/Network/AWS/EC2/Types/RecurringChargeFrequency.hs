{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RecurringChargeFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RecurringChargeFrequency where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RecurringChargeFrequency = Hourly
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
      "hourly" -> pure Hourly
      e ->
        fromTextError $
          "Failure parsing RecurringChargeFrequency from value: '" <> e
            <> "'. Accepted values: hourly"

instance ToText RecurringChargeFrequency where
  toText = \case
    Hourly -> "Hourly"

instance Hashable RecurringChargeFrequency

instance NFData RecurringChargeFrequency

instance ToByteString RecurringChargeFrequency

instance ToQuery RecurringChargeFrequency

instance ToHeader RecurringChargeFrequency

instance FromXML RecurringChargeFrequency where
  parseXML = parseXMLText "RecurringChargeFrequency"
