{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsManifestCompression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsManifestCompression where

import Network.AWS.Prelude

-- | Hls Manifest Compression
data HlsManifestCompression
  = HMCGzip
  | HMCNone
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

instance FromText HlsManifestCompression where
  parser =
    takeLowerText >>= \case
      "gzip" -> pure HMCGzip
      "none" -> pure HMCNone
      e ->
        fromTextError $
          "Failure parsing HlsManifestCompression from value: '" <> e
            <> "'. Accepted values: gzip, none"

instance ToText HlsManifestCompression where
  toText = \case
    HMCGzip -> "GZIP"
    HMCNone -> "NONE"

instance Hashable HlsManifestCompression

instance NFData HlsManifestCompression

instance ToByteString HlsManifestCompression

instance ToQuery HlsManifestCompression

instance ToHeader HlsManifestCompression

instance ToJSON HlsManifestCompression where
  toJSON = toJSONText

instance FromJSON HlsManifestCompression where
  parseJSON = parseJSONText "HlsManifestCompression"
