{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame where

import Network.AWS.Prelude

-- | Hls Timed Metadata Id3 Frame
data HlsTimedMetadataId3Frame
  = HTMIFNone
  | HTMIFPriv
  | HTMIFTdrl
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

instance FromText HlsTimedMetadataId3Frame where
  parser =
    takeLowerText >>= \case
      "none" -> pure HTMIFNone
      "priv" -> pure HTMIFPriv
      "tdrl" -> pure HTMIFTdrl
      e ->
        fromTextError $
          "Failure parsing HlsTimedMetadataId3Frame from value: '" <> e
            <> "'. Accepted values: none, priv, tdrl"

instance ToText HlsTimedMetadataId3Frame where
  toText = \case
    HTMIFNone -> "NONE"
    HTMIFPriv -> "PRIV"
    HTMIFTdrl -> "TDRL"

instance Hashable HlsTimedMetadataId3Frame

instance NFData HlsTimedMetadataId3Frame

instance ToByteString HlsTimedMetadataId3Frame

instance ToQuery HlsTimedMetadataId3Frame

instance ToHeader HlsTimedMetadataId3Frame

instance ToJSON HlsTimedMetadataId3Frame where
  toJSON = toJSONText

instance FromJSON HlsTimedMetadataId3Frame where
  parseJSON = parseJSONText "HlsTimedMetadataId3Frame"
