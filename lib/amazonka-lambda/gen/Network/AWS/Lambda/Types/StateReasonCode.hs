{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.StateReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.StateReasonCode where

import Network.AWS.Prelude

data StateReasonCode
  = Creating
  | EniLimitExceeded
  | Idle
  | InsufficientRolePermissions
  | InternalError
  | InvalidConfiguration
  | InvalidSecurityGroup
  | InvalidSubnet
  | Restoring
  | SubnetOutOfIPAddresses
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

instance FromText StateReasonCode where
  parser =
    takeLowerText >>= \case
      "creating" -> pure Creating
      "enilimitexceeded" -> pure EniLimitExceeded
      "idle" -> pure Idle
      "insufficientrolepermissions" -> pure InsufficientRolePermissions
      "internalerror" -> pure InternalError
      "invalidconfiguration" -> pure InvalidConfiguration
      "invalidsecuritygroup" -> pure InvalidSecurityGroup
      "invalidsubnet" -> pure InvalidSubnet
      "restoring" -> pure Restoring
      "subnetoutofipaddresses" -> pure SubnetOutOfIPAddresses
      e ->
        fromTextError $
          "Failure parsing StateReasonCode from value: '" <> e
            <> "'. Accepted values: creating, enilimitexceeded, idle, insufficientrolepermissions, internalerror, invalidconfiguration, invalidsecuritygroup, invalidsubnet, restoring, subnetoutofipaddresses"

instance ToText StateReasonCode where
  toText = \case
    Creating -> "Creating"
    EniLimitExceeded -> "EniLimitExceeded"
    Idle -> "Idle"
    InsufficientRolePermissions -> "InsufficientRolePermissions"
    InternalError -> "InternalError"
    InvalidConfiguration -> "InvalidConfiguration"
    InvalidSecurityGroup -> "InvalidSecurityGroup"
    InvalidSubnet -> "InvalidSubnet"
    Restoring -> "Restoring"
    SubnetOutOfIPAddresses -> "SubnetOutOfIPAddresses"

instance Hashable StateReasonCode

instance NFData StateReasonCode

instance ToByteString StateReasonCode

instance ToQuery StateReasonCode

instance ToHeader StateReasonCode

instance FromJSON StateReasonCode where
  parseJSON = parseJSONText "StateReasonCode"
