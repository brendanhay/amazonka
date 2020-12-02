{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsCodecSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsCodecSpecification where

import Network.AWS.Prelude

-- | Hls Codec Specification
data HlsCodecSpecification
  = Rfc4281
  | Rfc6381
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

instance FromText HlsCodecSpecification where
  parser =
    takeLowerText >>= \case
      "rfc_4281" -> pure Rfc4281
      "rfc_6381" -> pure Rfc6381
      e ->
        fromTextError $
          "Failure parsing HlsCodecSpecification from value: '" <> e
            <> "'. Accepted values: rfc_4281, rfc_6381"

instance ToText HlsCodecSpecification where
  toText = \case
    Rfc4281 -> "RFC_4281"
    Rfc6381 -> "RFC_6381"

instance Hashable HlsCodecSpecification

instance NFData HlsCodecSpecification

instance ToByteString HlsCodecSpecification

instance ToQuery HlsCodecSpecification

instance ToHeader HlsCodecSpecification

instance ToJSON HlsCodecSpecification where
  toJSON = toJSONText

instance FromJSON HlsCodecSpecification where
  parseJSON = parseJSONText "HlsCodecSpecification"
