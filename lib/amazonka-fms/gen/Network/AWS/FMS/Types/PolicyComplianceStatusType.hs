{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PolicyComplianceStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicyComplianceStatusType where

import Network.AWS.Prelude

data PolicyComplianceStatusType
  = Compliant
  | NonCompliant
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

instance FromText PolicyComplianceStatusType where
  parser =
    takeLowerText >>= \case
      "compliant" -> pure Compliant
      "non_compliant" -> pure NonCompliant
      e ->
        fromTextError $
          "Failure parsing PolicyComplianceStatusType from value: '" <> e
            <> "'. Accepted values: compliant, non_compliant"

instance ToText PolicyComplianceStatusType where
  toText = \case
    Compliant -> "COMPLIANT"
    NonCompliant -> "NON_COMPLIANT"

instance Hashable PolicyComplianceStatusType

instance NFData PolicyComplianceStatusType

instance ToByteString PolicyComplianceStatusType

instance ToQuery PolicyComplianceStatusType

instance ToHeader PolicyComplianceStatusType

instance FromJSON PolicyComplianceStatusType where
  parseJSON = parseJSONText "PolicyComplianceStatusType"
