{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafManifestCompression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafManifestCompression where

import Network.AWS.Prelude

-- | When set to GZIP, compresses HLS playlist.
data CmafManifestCompression
  = CMCGzip
  | CMCNone
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

instance FromText CmafManifestCompression where
  parser =
    takeLowerText >>= \case
      "gzip" -> pure CMCGzip
      "none" -> pure CMCNone
      e ->
        fromTextError $
          "Failure parsing CmafManifestCompression from value: '" <> e
            <> "'. Accepted values: gzip, none"

instance ToText CmafManifestCompression where
  toText = \case
    CMCGzip -> "GZIP"
    CMCNone -> "NONE"

instance Hashable CmafManifestCompression

instance NFData CmafManifestCompression

instance ToByteString CmafManifestCompression

instance ToQuery CmafManifestCompression

instance ToHeader CmafManifestCompression

instance ToJSON CmafManifestCompression where
  toJSON = toJSONText

instance FromJSON CmafManifestCompression where
  parseJSON = parseJSONText "CmafManifestCompression"
