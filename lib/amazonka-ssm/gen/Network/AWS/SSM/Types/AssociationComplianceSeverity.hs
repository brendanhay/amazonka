{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationComplianceSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationComplianceSeverity where

import Network.AWS.Prelude

data AssociationComplianceSeverity
  = ACSCritical
  | ACSHigh
  | ACSLow
  | ACSMedium
  | ACSUnspecified
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

instance FromText AssociationComplianceSeverity where
  parser =
    takeLowerText >>= \case
      "critical" -> pure ACSCritical
      "high" -> pure ACSHigh
      "low" -> pure ACSLow
      "medium" -> pure ACSMedium
      "unspecified" -> pure ACSUnspecified
      e ->
        fromTextError $
          "Failure parsing AssociationComplianceSeverity from value: '" <> e
            <> "'. Accepted values: critical, high, low, medium, unspecified"

instance ToText AssociationComplianceSeverity where
  toText = \case
    ACSCritical -> "CRITICAL"
    ACSHigh -> "HIGH"
    ACSLow -> "LOW"
    ACSMedium -> "MEDIUM"
    ACSUnspecified -> "UNSPECIFIED"

instance Hashable AssociationComplianceSeverity

instance NFData AssociationComplianceSeverity

instance ToByteString AssociationComplianceSeverity

instance ToQuery AssociationComplianceSeverity

instance ToHeader AssociationComplianceSeverity

instance ToJSON AssociationComplianceSeverity where
  toJSON = toJSONText

instance FromJSON AssociationComplianceSeverity where
  parseJSON = parseJSONText "AssociationComplianceSeverity"
