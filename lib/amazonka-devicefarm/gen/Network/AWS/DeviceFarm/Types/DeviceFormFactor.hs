{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceFormFactor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceFormFactor where

import Network.AWS.Prelude

data DeviceFormFactor
  = Phone
  | Tablet
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

instance FromText DeviceFormFactor where
  parser =
    takeLowerText >>= \case
      "phone" -> pure Phone
      "tablet" -> pure Tablet
      e ->
        fromTextError $
          "Failure parsing DeviceFormFactor from value: '" <> e
            <> "'. Accepted values: phone, tablet"

instance ToText DeviceFormFactor where
  toText = \case
    Phone -> "PHONE"
    Tablet -> "TABLET"

instance Hashable DeviceFormFactor

instance NFData DeviceFormFactor

instance ToByteString DeviceFormFactor

instance ToQuery DeviceFormFactor

instance ToHeader DeviceFormFactor

instance FromJSON DeviceFormFactor where
  parseJSON = parseJSONText "DeviceFormFactor"
