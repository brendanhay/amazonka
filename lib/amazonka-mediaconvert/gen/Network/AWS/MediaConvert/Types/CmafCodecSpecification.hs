{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafCodecSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafCodecSpecification where

import Network.AWS.Prelude

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
data CmafCodecSpecification
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

instance FromText CmafCodecSpecification where
  parser =
    takeLowerText >>= \case
      "rfc_4281" -> pure Rfc4281
      "rfc_6381" -> pure Rfc6381
      e ->
        fromTextError $
          "Failure parsing CmafCodecSpecification from value: '" <> e
            <> "'. Accepted values: rfc_4281, rfc_6381"

instance ToText CmafCodecSpecification where
  toText = \case
    Rfc4281 -> "RFC_4281"
    Rfc6381 -> "RFC_6381"

instance Hashable CmafCodecSpecification

instance NFData CmafCodecSpecification

instance ToByteString CmafCodecSpecification

instance ToQuery CmafCodecSpecification

instance ToHeader CmafCodecSpecification

instance ToJSON CmafCodecSpecification where
  toJSON = toJSONText

instance FromJSON CmafCodecSpecification where
  parseJSON = parseJSONText "CmafCodecSpecification"
