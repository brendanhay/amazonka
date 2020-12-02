{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.WavFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.WavFormat where

import Network.AWS.Prelude

-- | The service defaults to using RIFF for WAV outputs. If your output audio is likely to exceed 4 GB in file size, or if you otherwise need the extended support of the RF64 format, set your output WAV file format to RF64.
data WavFormat
  = RF64
  | Riff
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

instance FromText WavFormat where
  parser =
    takeLowerText >>= \case
      "rf64" -> pure RF64
      "riff" -> pure Riff
      e ->
        fromTextError $
          "Failure parsing WavFormat from value: '" <> e
            <> "'. Accepted values: rf64, riff"

instance ToText WavFormat where
  toText = \case
    RF64 -> "RF64"
    Riff -> "RIFF"

instance Hashable WavFormat

instance NFData WavFormat

instance ToByteString WavFormat

instance ToQuery WavFormat

instance ToHeader WavFormat

instance ToJSON WavFormat where
  toJSON = toJSONText

instance FromJSON WavFormat where
  parseJSON = parseJSONText "WavFormat"
