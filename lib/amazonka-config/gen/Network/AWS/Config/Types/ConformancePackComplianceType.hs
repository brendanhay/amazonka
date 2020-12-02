{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackComplianceType where

import Network.AWS.Prelude

data ConformancePackComplianceType
  = CPCTCompliant
  | CPCTNonCompliant
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

instance FromText ConformancePackComplianceType where
  parser =
    takeLowerText >>= \case
      "compliant" -> pure CPCTCompliant
      "non_compliant" -> pure CPCTNonCompliant
      e ->
        fromTextError $
          "Failure parsing ConformancePackComplianceType from value: '" <> e
            <> "'. Accepted values: compliant, non_compliant"

instance ToText ConformancePackComplianceType where
  toText = \case
    CPCTCompliant -> "COMPLIANT"
    CPCTNonCompliant -> "NON_COMPLIANT"

instance Hashable ConformancePackComplianceType

instance NFData ConformancePackComplianceType

instance ToByteString ConformancePackComplianceType

instance ToQuery ConformancePackComplianceType

instance ToHeader ConformancePackComplianceType

instance ToJSON ConformancePackComplianceType where
  toJSON = toJSONText

instance FromJSON ConformancePackComplianceType where
  parseJSON = parseJSONText "ConformancePackComplianceType"
