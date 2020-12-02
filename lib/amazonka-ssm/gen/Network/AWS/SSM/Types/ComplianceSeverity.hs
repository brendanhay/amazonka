{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceSeverity where

import Network.AWS.Prelude

data ComplianceSeverity
  = CSCritical
  | CSHigh
  | CSInformational
  | CSLow
  | CSMedium
  | CSUnspecified
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

instance FromText ComplianceSeverity where
  parser =
    takeLowerText >>= \case
      "critical" -> pure CSCritical
      "high" -> pure CSHigh
      "informational" -> pure CSInformational
      "low" -> pure CSLow
      "medium" -> pure CSMedium
      "unspecified" -> pure CSUnspecified
      e ->
        fromTextError $
          "Failure parsing ComplianceSeverity from value: '" <> e
            <> "'. Accepted values: critical, high, informational, low, medium, unspecified"

instance ToText ComplianceSeverity where
  toText = \case
    CSCritical -> "CRITICAL"
    CSHigh -> "HIGH"
    CSInformational -> "INFORMATIONAL"
    CSLow -> "LOW"
    CSMedium -> "MEDIUM"
    CSUnspecified -> "UNSPECIFIED"

instance Hashable ComplianceSeverity

instance NFData ComplianceSeverity

instance ToByteString ComplianceSeverity

instance ToQuery ComplianceSeverity

instance ToHeader ComplianceSeverity

instance ToJSON ComplianceSeverity where
  toJSON = toJSONText

instance FromJSON ComplianceSeverity where
  parseJSON = parseJSONText "ComplianceSeverity"
