{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsCodecSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsCodecSpecification where

import Network.AWS.Prelude

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
data HlsCodecSpecification
  = HCSRfc4281
  | HCSRfc6381
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
      "rfc_4281" -> pure HCSRfc4281
      "rfc_6381" -> pure HCSRfc6381
      e ->
        fromTextError $
          "Failure parsing HlsCodecSpecification from value: '" <> e
            <> "'. Accepted values: rfc_4281, rfc_6381"

instance ToText HlsCodecSpecification where
  toText = \case
    HCSRfc4281 -> "RFC_4281"
    HCSRfc6381 -> "RFC_6381"

instance Hashable HlsCodecSpecification

instance NFData HlsCodecSpecification

instance ToByteString HlsCodecSpecification

instance ToQuery HlsCodecSpecification

instance ToHeader HlsCodecSpecification

instance ToJSON HlsCodecSpecification where
  toJSON = toJSONText

instance FromJSON HlsCodecSpecification where
  parseJSON = parseJSONText "HlsCodecSpecification"
