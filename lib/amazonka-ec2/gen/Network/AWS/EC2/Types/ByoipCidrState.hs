{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ByoipCidrState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ByoipCidrState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ByoipCidrState
  = Advertised
  | Deprovisioned
  | FailedDeprovision
  | FailedProvision
  | PendingDeprovision
  | PendingProvision
  | Provisioned
  | ProvisionedNotPubliclyAdvertisable
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

instance FromText ByoipCidrState where
  parser =
    takeLowerText >>= \case
      "advertised" -> pure Advertised
      "deprovisioned" -> pure Deprovisioned
      "failed-deprovision" -> pure FailedDeprovision
      "failed-provision" -> pure FailedProvision
      "pending-deprovision" -> pure PendingDeprovision
      "pending-provision" -> pure PendingProvision
      "provisioned" -> pure Provisioned
      "provisioned-not-publicly-advertisable" -> pure ProvisionedNotPubliclyAdvertisable
      e ->
        fromTextError $
          "Failure parsing ByoipCidrState from value: '" <> e
            <> "'. Accepted values: advertised, deprovisioned, failed-deprovision, failed-provision, pending-deprovision, pending-provision, provisioned, provisioned-not-publicly-advertisable"

instance ToText ByoipCidrState where
  toText = \case
    Advertised -> "advertised"
    Deprovisioned -> "deprovisioned"
    FailedDeprovision -> "failed-deprovision"
    FailedProvision -> "failed-provision"
    PendingDeprovision -> "pending-deprovision"
    PendingProvision -> "pending-provision"
    Provisioned -> "provisioned"
    ProvisionedNotPubliclyAdvertisable -> "provisioned-not-publicly-advertisable"

instance Hashable ByoipCidrState

instance NFData ByoipCidrState

instance ToByteString ByoipCidrState

instance ToQuery ByoipCidrState

instance ToHeader ByoipCidrState

instance FromXML ByoipCidrState where
  parseXML = parseXMLText "ByoipCidrState"
