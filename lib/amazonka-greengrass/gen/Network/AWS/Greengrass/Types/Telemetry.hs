{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Telemetry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Telemetry where

import Network.AWS.Prelude

data Telemetry
  = ON
  | Off
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

instance FromText Telemetry where
  parser =
    takeLowerText >>= \case
      "on" -> pure ON
      "off" -> pure Off
      e ->
        fromTextError $
          "Failure parsing Telemetry from value: '" <> e
            <> "'. Accepted values: on, off"

instance ToText Telemetry where
  toText = \case
    ON -> "On"
    Off -> "Off"

instance Hashable Telemetry

instance NFData Telemetry

instance ToByteString Telemetry

instance ToQuery Telemetry

instance ToHeader Telemetry

instance ToJSON Telemetry where
  toJSON = toJSONText

instance FromJSON Telemetry where
  parseJSON = parseJSONText "Telemetry"
