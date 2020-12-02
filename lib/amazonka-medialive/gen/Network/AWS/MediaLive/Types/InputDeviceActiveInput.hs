{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceActiveInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceActiveInput where

import Network.AWS.Prelude

-- | The source at the input device that is currently active.
data InputDeviceActiveInput
  = Hdmi
  | Sdi
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

instance FromText InputDeviceActiveInput where
  parser =
    takeLowerText >>= \case
      "hdmi" -> pure Hdmi
      "sdi" -> pure Sdi
      e ->
        fromTextError $
          "Failure parsing InputDeviceActiveInput from value: '" <> e
            <> "'. Accepted values: hdmi, sdi"

instance ToText InputDeviceActiveInput where
  toText = \case
    Hdmi -> "HDMI"
    Sdi -> "SDI"

instance Hashable InputDeviceActiveInput

instance NFData InputDeviceActiveInput

instance ToByteString InputDeviceActiveInput

instance ToQuery InputDeviceActiveInput

instance ToHeader InputDeviceActiveInput

instance FromJSON InputDeviceActiveInput where
  parseJSON = parseJSONText "InputDeviceActiveInput"
