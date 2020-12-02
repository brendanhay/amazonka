{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame where

import Network.AWS.Prelude

-- | Udp Timed Metadata Id3 Frame
data UdpTimedMetadataId3Frame
  = UTMIFNone
  | UTMIFPriv
  | UTMIFTdrl
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

instance FromText UdpTimedMetadataId3Frame where
  parser =
    takeLowerText >>= \case
      "none" -> pure UTMIFNone
      "priv" -> pure UTMIFPriv
      "tdrl" -> pure UTMIFTdrl
      e ->
        fromTextError $
          "Failure parsing UdpTimedMetadataId3Frame from value: '" <> e
            <> "'. Accepted values: none, priv, tdrl"

instance ToText UdpTimedMetadataId3Frame where
  toText = \case
    UTMIFNone -> "NONE"
    UTMIFPriv -> "PRIV"
    UTMIFTdrl -> "TDRL"

instance Hashable UdpTimedMetadataId3Frame

instance NFData UdpTimedMetadataId3Frame

instance ToByteString UdpTimedMetadataId3Frame

instance ToQuery UdpTimedMetadataId3Frame

instance ToHeader UdpTimedMetadataId3Frame

instance ToJSON UdpTimedMetadataId3Frame where
  toJSON = toJSONText

instance FromJSON UdpTimedMetadataId3Frame where
  parseJSON = parseJSONText "UdpTimedMetadataId3Frame"
