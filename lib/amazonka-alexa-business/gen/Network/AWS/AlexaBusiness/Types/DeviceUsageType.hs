{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceUsageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceUsageType where

import Network.AWS.Prelude

data DeviceUsageType = Voice
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

instance FromText DeviceUsageType where
  parser =
    takeLowerText >>= \case
      "voice" -> pure Voice
      e ->
        fromTextError $
          "Failure parsing DeviceUsageType from value: '" <> e
            <> "'. Accepted values: voice"

instance ToText DeviceUsageType where
  toText = \case
    Voice -> "VOICE"

instance Hashable DeviceUsageType

instance NFData DeviceUsageType

instance ToByteString DeviceUsageType

instance ToQuery DeviceUsageType

instance ToHeader DeviceUsageType

instance ToJSON DeviceUsageType where
  toJSON = toJSONText
