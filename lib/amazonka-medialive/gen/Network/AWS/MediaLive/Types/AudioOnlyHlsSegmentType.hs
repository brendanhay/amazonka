{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType where

import Network.AWS.Prelude

-- | Audio Only Hls Segment Type
data AudioOnlyHlsSegmentType
  = Aac
  | FMP4
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

instance FromText AudioOnlyHlsSegmentType where
  parser =
    takeLowerText >>= \case
      "aac" -> pure Aac
      "fmp4" -> pure FMP4
      e ->
        fromTextError $
          "Failure parsing AudioOnlyHlsSegmentType from value: '" <> e
            <> "'. Accepted values: aac, fmp4"

instance ToText AudioOnlyHlsSegmentType where
  toText = \case
    Aac -> "AAC"
    FMP4 -> "FMP4"

instance Hashable AudioOnlyHlsSegmentType

instance NFData AudioOnlyHlsSegmentType

instance ToByteString AudioOnlyHlsSegmentType

instance ToQuery AudioOnlyHlsSegmentType

instance ToHeader AudioOnlyHlsSegmentType

instance ToJSON AudioOnlyHlsSegmentType where
  toJSON = toJSONText

instance FromJSON AudioOnlyHlsSegmentType where
  parseJSON = parseJSONText "AudioOnlyHlsSegmentType"
