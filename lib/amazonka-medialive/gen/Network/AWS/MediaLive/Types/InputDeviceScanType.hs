{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceScanType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceScanType where

import Network.AWS.Prelude

-- | The scan type of the video source.
data InputDeviceScanType
  = IDSTInterlaced
  | IDSTProgressive
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

instance FromText InputDeviceScanType where
  parser =
    takeLowerText >>= \case
      "interlaced" -> pure IDSTInterlaced
      "progressive" -> pure IDSTProgressive
      e ->
        fromTextError $
          "Failure parsing InputDeviceScanType from value: '" <> e
            <> "'. Accepted values: interlaced, progressive"

instance ToText InputDeviceScanType where
  toText = \case
    IDSTInterlaced -> "INTERLACED"
    IDSTProgressive -> "PROGRESSIVE"

instance Hashable InputDeviceScanType

instance NFData InputDeviceScanType

instance ToByteString InputDeviceScanType

instance ToQuery InputDeviceScanType

instance ToHeader InputDeviceScanType

instance FromJSON InputDeviceScanType where
  parseJSON = parseJSONText "InputDeviceScanType"
