{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RenewalEligibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RenewalEligibility where

import Network.AWS.Prelude

data RenewalEligibility
  = Eligible
  | Ineligible
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

instance FromText RenewalEligibility where
  parser =
    takeLowerText >>= \case
      "eligible" -> pure Eligible
      "ineligible" -> pure Ineligible
      e ->
        fromTextError $
          "Failure parsing RenewalEligibility from value: '" <> e
            <> "'. Accepted values: eligible, ineligible"

instance ToText RenewalEligibility where
  toText = \case
    Eligible -> "ELIGIBLE"
    Ineligible -> "INELIGIBLE"

instance Hashable RenewalEligibility

instance NFData RenewalEligibility

instance ToByteString RenewalEligibility

instance ToQuery RenewalEligibility

instance ToHeader RenewalEligibility

instance FromJSON RenewalEligibility where
  parseJSON = parseJSONText "RenewalEligibility"
