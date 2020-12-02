{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAdMarkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAdMarkers where

import Network.AWS.Prelude

data HlsAdMarkers
  = Elemental
  | ElementalSCTE35
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

instance FromText HlsAdMarkers where
  parser =
    takeLowerText >>= \case
      "elemental" -> pure Elemental
      "elemental_scte35" -> pure ElementalSCTE35
      e ->
        fromTextError $
          "Failure parsing HlsAdMarkers from value: '" <> e
            <> "'. Accepted values: elemental, elemental_scte35"

instance ToText HlsAdMarkers where
  toText = \case
    Elemental -> "ELEMENTAL"
    ElementalSCTE35 -> "ELEMENTAL_SCTE35"

instance Hashable HlsAdMarkers

instance NFData HlsAdMarkers

instance ToByteString HlsAdMarkers

instance ToQuery HlsAdMarkers

instance ToHeader HlsAdMarkers

instance ToJSON HlsAdMarkers where
  toJSON = toJSONText

instance FromJSON HlsAdMarkers where
  parseJSON = parseJSONText "HlsAdMarkers"
