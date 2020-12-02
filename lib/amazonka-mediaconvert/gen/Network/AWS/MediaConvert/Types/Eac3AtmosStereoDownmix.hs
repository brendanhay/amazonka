{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix where

import Network.AWS.Prelude

-- | Choose how the service does stereo downmixing.
data Eac3AtmosStereoDownmix
  = DPL2
  | NotIndicated
  | Stereo
  | Surround
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

instance FromText Eac3AtmosStereoDownmix where
  parser =
    takeLowerText >>= \case
      "dpl2" -> pure DPL2
      "not_indicated" -> pure NotIndicated
      "stereo" -> pure Stereo
      "surround" -> pure Surround
      e ->
        fromTextError $
          "Failure parsing Eac3AtmosStereoDownmix from value: '" <> e
            <> "'. Accepted values: dpl2, not_indicated, stereo, surround"

instance ToText Eac3AtmosStereoDownmix where
  toText = \case
    DPL2 -> "DPL2"
    NotIndicated -> "NOT_INDICATED"
    Stereo -> "STEREO"
    Surround -> "SURROUND"

instance Hashable Eac3AtmosStereoDownmix

instance NFData Eac3AtmosStereoDownmix

instance ToByteString Eac3AtmosStereoDownmix

instance ToQuery Eac3AtmosStereoDownmix

instance ToHeader Eac3AtmosStereoDownmix

instance ToJSON Eac3AtmosStereoDownmix where
  toJSON = toJSONText

instance FromJSON Eac3AtmosStereoDownmix where
  parseJSON = parseJSONText "Eac3AtmosStereoDownmix"
