{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CdiInputResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CdiInputResolution where

import Network.AWS.Prelude

-- | Maximum CDI input resolution; SD is 480i and 576i up to 30 frames-per-second (fps), HD is 720p up to 60 fps / 1080i up to 30 fps, FHD is 1080p up to 60 fps, UHD is 2160p up to 60 fps
data CdiInputResolution
  = CIRFhd
  | CIRHD
  | CIRSD
  | CIRUhd
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

instance FromText CdiInputResolution where
  parser =
    takeLowerText >>= \case
      "fhd" -> pure CIRFhd
      "hd" -> pure CIRHD
      "sd" -> pure CIRSD
      "uhd" -> pure CIRUhd
      e ->
        fromTextError $
          "Failure parsing CdiInputResolution from value: '" <> e
            <> "'. Accepted values: fhd, hd, sd, uhd"

instance ToText CdiInputResolution where
  toText = \case
    CIRFhd -> "FHD"
    CIRHD -> "HD"
    CIRSD -> "SD"
    CIRUhd -> "UHD"

instance Hashable CdiInputResolution

instance NFData CdiInputResolution

instance ToByteString CdiInputResolution

instance ToQuery CdiInputResolution

instance ToHeader CdiInputResolution

instance ToJSON CdiInputResolution where
  toJSON = toJSONText

instance FromJSON CdiInputResolution where
  parseJSON = parseJSONText "CdiInputResolution"
