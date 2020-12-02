{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest where

import Network.AWS.Prelude

-- | When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
data HlsIFrameOnlyManifest
  = HIFOMExclude
  | HIFOMInclude
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

instance FromText HlsIFrameOnlyManifest where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure HIFOMExclude
      "include" -> pure HIFOMInclude
      e ->
        fromTextError $
          "Failure parsing HlsIFrameOnlyManifest from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText HlsIFrameOnlyManifest where
  toText = \case
    HIFOMExclude -> "EXCLUDE"
    HIFOMInclude -> "INCLUDE"

instance Hashable HlsIFrameOnlyManifest

instance NFData HlsIFrameOnlyManifest

instance ToByteString HlsIFrameOnlyManifest

instance ToQuery HlsIFrameOnlyManifest

instance ToHeader HlsIFrameOnlyManifest

instance ToJSON HlsIFrameOnlyManifest where
  toJSON = toJSONText

instance FromJSON HlsIFrameOnlyManifest where
  parseJSON = parseJSONText "HlsIFrameOnlyManifest"
