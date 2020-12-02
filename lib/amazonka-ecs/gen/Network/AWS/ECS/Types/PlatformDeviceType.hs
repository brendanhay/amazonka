{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlatformDeviceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlatformDeviceType where

import Network.AWS.Prelude

data PlatformDeviceType = Gpu
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

instance FromText PlatformDeviceType where
  parser =
    takeLowerText >>= \case
      "gpu" -> pure Gpu
      e ->
        fromTextError $
          "Failure parsing PlatformDeviceType from value: '" <> e
            <> "'. Accepted values: gpu"

instance ToText PlatformDeviceType where
  toText = \case
    Gpu -> "GPU"

instance Hashable PlatformDeviceType

instance NFData PlatformDeviceType

instance ToByteString PlatformDeviceType

instance ToQuery PlatformDeviceType

instance ToHeader PlatformDeviceType

instance ToJSON PlatformDeviceType where
  toJSON = toJSONText
