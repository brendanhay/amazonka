{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IPSetFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.IPSetFormat where

import Network.AWS.Prelude

data IPSetFormat
  = AlienVault
  | FireEye
  | OtxCSV
  | ProofPoint
  | Stix
  | Txt
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

instance FromText IPSetFormat where
  parser =
    takeLowerText >>= \case
      "alien_vault" -> pure AlienVault
      "fire_eye" -> pure FireEye
      "otx_csv" -> pure OtxCSV
      "proof_point" -> pure ProofPoint
      "stix" -> pure Stix
      "txt" -> pure Txt
      e ->
        fromTextError $
          "Failure parsing IPSetFormat from value: '" <> e
            <> "'. Accepted values: alien_vault, fire_eye, otx_csv, proof_point, stix, txt"

instance ToText IPSetFormat where
  toText = \case
    AlienVault -> "ALIEN_VAULT"
    FireEye -> "FIRE_EYE"
    OtxCSV -> "OTX_CSV"
    ProofPoint -> "PROOF_POINT"
    Stix -> "STIX"
    Txt -> "TXT"

instance Hashable IPSetFormat

instance NFData IPSetFormat

instance ToByteString IPSetFormat

instance ToQuery IPSetFormat

instance ToHeader IPSetFormat

instance ToJSON IPSetFormat where
  toJSON = toJSONText

instance FromJSON IPSetFormat where
  parseJSON = parseJSONText "IPSetFormat"
