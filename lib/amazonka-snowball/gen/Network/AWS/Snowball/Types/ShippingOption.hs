{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ShippingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ShippingOption where

import Network.AWS.Prelude

data ShippingOption
  = SOExpress
  | SONextDay
  | SOSecondDay
  | SOStandard
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

instance FromText ShippingOption where
  parser =
    takeLowerText >>= \case
      "express" -> pure SOExpress
      "next_day" -> pure SONextDay
      "second_day" -> pure SOSecondDay
      "standard" -> pure SOStandard
      e ->
        fromTextError $
          "Failure parsing ShippingOption from value: '" <> e
            <> "'. Accepted values: express, next_day, second_day, standard"

instance ToText ShippingOption where
  toText = \case
    SOExpress -> "EXPRESS"
    SONextDay -> "NEXT_DAY"
    SOSecondDay -> "SECOND_DAY"
    SOStandard -> "STANDARD"

instance Hashable ShippingOption

instance NFData ShippingOption

instance ToByteString ShippingOption

instance ToQuery ShippingOption

instance ToHeader ShippingOption

instance ToJSON ShippingOption where
  toJSON = toJSONText

instance FromJSON ShippingOption where
  parseJSON = parseJSONText "ShippingOption"
