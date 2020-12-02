{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelSetFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ThreatIntelSetFormat where

import Network.AWS.Prelude

data ThreatIntelSetFormat
  = TISFAlienVault
  | TISFFireEye
  | TISFOtxCSV
  | TISFProofPoint
  | TISFStix
  | TISFTxt
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

instance FromText ThreatIntelSetFormat where
  parser =
    takeLowerText >>= \case
      "alien_vault" -> pure TISFAlienVault
      "fire_eye" -> pure TISFFireEye
      "otx_csv" -> pure TISFOtxCSV
      "proof_point" -> pure TISFProofPoint
      "stix" -> pure TISFStix
      "txt" -> pure TISFTxt
      e ->
        fromTextError $
          "Failure parsing ThreatIntelSetFormat from value: '" <> e
            <> "'. Accepted values: alien_vault, fire_eye, otx_csv, proof_point, stix, txt"

instance ToText ThreatIntelSetFormat where
  toText = \case
    TISFAlienVault -> "ALIEN_VAULT"
    TISFFireEye -> "FIRE_EYE"
    TISFOtxCSV -> "OTX_CSV"
    TISFProofPoint -> "PROOF_POINT"
    TISFStix -> "STIX"
    TISFTxt -> "TXT"

instance Hashable ThreatIntelSetFormat

instance NFData ThreatIntelSetFormat

instance ToByteString ThreatIntelSetFormat

instance ToQuery ThreatIntelSetFormat

instance ToHeader ThreatIntelSetFormat

instance ToJSON ThreatIntelSetFormat where
  toJSON = toJSONText

instance FromJSON ThreatIntelSetFormat where
  parseJSON = parseJSONText "ThreatIntelSetFormat"
