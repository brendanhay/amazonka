{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.TemperatureUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.TemperatureUnit where

import Network.AWS.Prelude

data TemperatureUnit
  = Celsius
  | Fahrenheit
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

instance FromText TemperatureUnit where
  parser =
    takeLowerText >>= \case
      "celsius" -> pure Celsius
      "fahrenheit" -> pure Fahrenheit
      e ->
        fromTextError $
          "Failure parsing TemperatureUnit from value: '" <> e
            <> "'. Accepted values: celsius, fahrenheit"

instance ToText TemperatureUnit where
  toText = \case
    Celsius -> "CELSIUS"
    Fahrenheit -> "FAHRENHEIT"

instance Hashable TemperatureUnit

instance NFData TemperatureUnit

instance ToByteString TemperatureUnit

instance ToQuery TemperatureUnit

instance ToHeader TemperatureUnit

instance ToJSON TemperatureUnit where
  toJSON = toJSONText

instance FromJSON TemperatureUnit where
  parseJSON = parseJSONText "TemperatureUnit"
