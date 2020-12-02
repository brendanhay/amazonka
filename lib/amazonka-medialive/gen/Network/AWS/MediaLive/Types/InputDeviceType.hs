{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceType where

import Network.AWS.Prelude

-- | The type of the input device. For an AWS Elemental Link device that outputs resolutions up to 1080, choose "HD".
data InputDeviceType = HD
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

instance FromText InputDeviceType where
  parser =
    takeLowerText >>= \case
      "hd" -> pure HD
      e ->
        fromTextError $
          "Failure parsing InputDeviceType from value: '" <> e
            <> "'. Accepted values: hd"

instance ToText InputDeviceType where
  toText = \case
    HD -> "HD"

instance Hashable InputDeviceType

instance NFData InputDeviceType

instance ToByteString InputDeviceType

instance ToQuery InputDeviceType

instance ToHeader InputDeviceType

instance FromJSON InputDeviceType where
  parseJSON = parseJSONText "InputDeviceType"
