{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceConfiguredInput where

import Network.AWS.Prelude

-- | The source to activate (use) from the input device.
data InputDeviceConfiguredInput
  = IDCIAuto
  | IDCIHdmi
  | IDCISdi
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

instance FromText InputDeviceConfiguredInput where
  parser =
    takeLowerText >>= \case
      "auto" -> pure IDCIAuto
      "hdmi" -> pure IDCIHdmi
      "sdi" -> pure IDCISdi
      e ->
        fromTextError $
          "Failure parsing InputDeviceConfiguredInput from value: '" <> e
            <> "'. Accepted values: auto, hdmi, sdi"

instance ToText InputDeviceConfiguredInput where
  toText = \case
    IDCIAuto -> "AUTO"
    IDCIHdmi -> "HDMI"
    IDCISdi -> "SDI"

instance Hashable InputDeviceConfiguredInput

instance NFData InputDeviceConfiguredInput

instance ToByteString InputDeviceConfiguredInput

instance ToQuery InputDeviceConfiguredInput

instance ToHeader InputDeviceConfiguredInput

instance ToJSON InputDeviceConfiguredInput where
  toJSON = toJSONText

instance FromJSON InputDeviceConfiguredInput where
  parseJSON = parseJSONText "InputDeviceConfiguredInput"
