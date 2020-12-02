{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LastUpdateStatusReasonCode where

import Network.AWS.Prelude

data LastUpdateStatusReasonCode
  = LUSRCEniLimitExceeded
  | LUSRCInsufficientRolePermissions
  | LUSRCInternalError
  | LUSRCInvalidConfiguration
  | LUSRCInvalidSecurityGroup
  | LUSRCInvalidSubnet
  | LUSRCSubnetOutOfIPAddresses
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

instance FromText LastUpdateStatusReasonCode where
  parser =
    takeLowerText >>= \case
      "enilimitexceeded" -> pure LUSRCEniLimitExceeded
      "insufficientrolepermissions" -> pure LUSRCInsufficientRolePermissions
      "internalerror" -> pure LUSRCInternalError
      "invalidconfiguration" -> pure LUSRCInvalidConfiguration
      "invalidsecuritygroup" -> pure LUSRCInvalidSecurityGroup
      "invalidsubnet" -> pure LUSRCInvalidSubnet
      "subnetoutofipaddresses" -> pure LUSRCSubnetOutOfIPAddresses
      e ->
        fromTextError $
          "Failure parsing LastUpdateStatusReasonCode from value: '" <> e
            <> "'. Accepted values: enilimitexceeded, insufficientrolepermissions, internalerror, invalidconfiguration, invalidsecuritygroup, invalidsubnet, subnetoutofipaddresses"

instance ToText LastUpdateStatusReasonCode where
  toText = \case
    LUSRCEniLimitExceeded -> "EniLimitExceeded"
    LUSRCInsufficientRolePermissions -> "InsufficientRolePermissions"
    LUSRCInternalError -> "InternalError"
    LUSRCInvalidConfiguration -> "InvalidConfiguration"
    LUSRCInvalidSecurityGroup -> "InvalidSecurityGroup"
    LUSRCInvalidSubnet -> "InvalidSubnet"
    LUSRCSubnetOutOfIPAddresses -> "SubnetOutOfIPAddresses"

instance Hashable LastUpdateStatusReasonCode

instance NFData LastUpdateStatusReasonCode

instance ToByteString LastUpdateStatusReasonCode

instance ToQuery LastUpdateStatusReasonCode

instance ToHeader LastUpdateStatusReasonCode

instance FromJSON LastUpdateStatusReasonCode where
  parseJSON = parseJSONText "LastUpdateStatusReasonCode"
