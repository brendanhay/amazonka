{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsOutputSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsOutputSelection where

import Network.AWS.Prelude

-- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
data HlsOutputSelection
  = ManifestsAndSegments
  | SegmentsOnly
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

instance FromText HlsOutputSelection where
  parser =
    takeLowerText >>= \case
      "manifests_and_segments" -> pure ManifestsAndSegments
      "segments_only" -> pure SegmentsOnly
      e ->
        fromTextError $
          "Failure parsing HlsOutputSelection from value: '" <> e
            <> "'. Accepted values: manifests_and_segments, segments_only"

instance ToText HlsOutputSelection where
  toText = \case
    ManifestsAndSegments -> "MANIFESTS_AND_SEGMENTS"
    SegmentsOnly -> "SEGMENTS_ONLY"

instance Hashable HlsOutputSelection

instance NFData HlsOutputSelection

instance ToByteString HlsOutputSelection

instance ToQuery HlsOutputSelection

instance ToHeader HlsOutputSelection

instance ToJSON HlsOutputSelection where
  toJSON = toJSONText

instance FromJSON HlsOutputSelection where
  parseJSON = parseJSONText "HlsOutputSelection"
