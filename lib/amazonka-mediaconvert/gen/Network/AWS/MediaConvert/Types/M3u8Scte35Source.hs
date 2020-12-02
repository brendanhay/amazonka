{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8Scte35Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8Scte35Source where

import Network.AWS.Prelude

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE) if you don't want manifest conditioning. Choose Passthrough (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest conditioning. In both cases, also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml).
data M3u8Scte35Source
  = MNone
  | MPassthrough
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

instance FromText M3u8Scte35Source where
  parser =
    takeLowerText >>= \case
      "none" -> pure MNone
      "passthrough" -> pure MPassthrough
      e ->
        fromTextError $
          "Failure parsing M3u8Scte35Source from value: '" <> e
            <> "'. Accepted values: none, passthrough"

instance ToText M3u8Scte35Source where
  toText = \case
    MNone -> "NONE"
    MPassthrough -> "PASSTHROUGH"

instance Hashable M3u8Scte35Source

instance NFData M3u8Scte35Source

instance ToByteString M3u8Scte35Source

instance ToQuery M3u8Scte35Source

instance ToHeader M3u8Scte35Source

instance ToJSON M3u8Scte35Source where
  toJSON = toJSONText

instance FromJSON M3u8Scte35Source where
  parseJSON = parseJSONText "M3u8Scte35Source"
