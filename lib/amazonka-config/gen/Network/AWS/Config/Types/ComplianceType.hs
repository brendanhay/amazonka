{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceType where

import Network.AWS.Prelude

data ComplianceType
  = Compliant
  | InsufficientData
  | NonCompliant
  | NotApplicable
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

instance FromText ComplianceType where
  parser =
    takeLowerText >>= \case
      "compliant" -> pure Compliant
      "insufficient_data" -> pure InsufficientData
      "non_compliant" -> pure NonCompliant
      "not_applicable" -> pure NotApplicable
      e ->
        fromTextError $
          "Failure parsing ComplianceType from value: '" <> e
            <> "'. Accepted values: compliant, insufficient_data, non_compliant, not_applicable"

instance ToText ComplianceType where
  toText = \case
    Compliant -> "COMPLIANT"
    InsufficientData -> "INSUFFICIENT_DATA"
    NonCompliant -> "NON_COMPLIANT"
    NotApplicable -> "NOT_APPLICABLE"

instance Hashable ComplianceType

instance NFData ComplianceType

instance ToByteString ComplianceType

instance ToQuery ComplianceType

instance ToHeader ComplianceType

instance ToJSON ComplianceType where
  toJSON = toJSONText

instance FromJSON ComplianceType where
  parseJSON = parseJSONText "ComplianceType"
