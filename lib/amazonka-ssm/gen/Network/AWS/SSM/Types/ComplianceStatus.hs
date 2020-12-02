{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceStatus where

import Network.AWS.Prelude

data ComplianceStatus
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

instance FromText ComplianceStatus where
  parser =
    takeLowerText >>= \case
      "compliant" -> pure Compliant
      "non_compliant" -> pure NonCompliant
      e ->
        fromTextError $
          "Failure parsing ComplianceStatus from value: '" <> e
            <> "'. Accepted values: compliant, non_compliant"

instance ToText ComplianceStatus where
  toText = \case
    Compliant -> "COMPLIANT"
    NonCompliant -> "NON_COMPLIANT"

instance Hashable ComplianceStatus

instance NFData ComplianceStatus

instance ToByteString ComplianceStatus

instance ToQuery ComplianceStatus

instance ToHeader ComplianceStatus

instance ToJSON ComplianceStatus where
  toJSON = toJSONText

instance FromJSON ComplianceStatus where
  parseJSON = parseJSONText "ComplianceStatus"
