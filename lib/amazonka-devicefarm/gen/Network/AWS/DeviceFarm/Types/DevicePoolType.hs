{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DevicePoolType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DevicePoolType where

import Network.AWS.Prelude

data DevicePoolType
  = DPTCurated
  | DPTPrivate
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

instance FromText DevicePoolType where
  parser =
    takeLowerText >>= \case
      "curated" -> pure DPTCurated
      "private" -> pure DPTPrivate
      e ->
        fromTextError $
          "Failure parsing DevicePoolType from value: '" <> e
            <> "'. Accepted values: curated, private"

instance ToText DevicePoolType where
  toText = \case
    DPTCurated -> "CURATED"
    DPTPrivate -> "PRIVATE"

instance Hashable DevicePoolType

instance NFData DevicePoolType

instance ToByteString DevicePoolType

instance ToQuery DevicePoolType

instance ToHeader DevicePoolType

instance ToJSON DevicePoolType where
  toJSON = toJSONText

instance FromJSON DevicePoolType where
  parseJSON = parseJSONText "DevicePoolType"
