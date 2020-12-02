{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsH265PackagingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsH265PackagingType where

import Network.AWS.Prelude

-- | Hls H265 Packaging Type
data HlsH265PackagingType
  = HEV1
  | HVC1
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

instance FromText HlsH265PackagingType where
  parser =
    takeLowerText >>= \case
      "hev1" -> pure HEV1
      "hvc1" -> pure HVC1
      e ->
        fromTextError $
          "Failure parsing HlsH265PackagingType from value: '" <> e
            <> "'. Accepted values: hev1, hvc1"

instance ToText HlsH265PackagingType where
  toText = \case
    HEV1 -> "HEV1"
    HVC1 -> "HVC1"

instance Hashable HlsH265PackagingType

instance NFData HlsH265PackagingType

instance ToByteString HlsH265PackagingType

instance ToQuery HlsH265PackagingType

instance ToHeader HlsH265PackagingType

instance ToJSON HlsH265PackagingType where
  toJSON = toJSONText

instance FromJSON HlsH265PackagingType where
  parseJSON = parseJSONText "HlsH265PackagingType"
