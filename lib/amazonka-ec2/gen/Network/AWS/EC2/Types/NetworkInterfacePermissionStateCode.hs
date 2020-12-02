{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NetworkInterfacePermissionStateCode
  = NIPSCGranted
  | NIPSCPending
  | NIPSCRevoked
  | NIPSCRevoking
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

instance FromText NetworkInterfacePermissionStateCode where
  parser =
    takeLowerText >>= \case
      "granted" -> pure NIPSCGranted
      "pending" -> pure NIPSCPending
      "revoked" -> pure NIPSCRevoked
      "revoking" -> pure NIPSCRevoking
      e ->
        fromTextError $
          "Failure parsing NetworkInterfacePermissionStateCode from value: '" <> e
            <> "'. Accepted values: granted, pending, revoked, revoking"

instance ToText NetworkInterfacePermissionStateCode where
  toText = \case
    NIPSCGranted -> "granted"
    NIPSCPending -> "pending"
    NIPSCRevoked -> "revoked"
    NIPSCRevoking -> "revoking"

instance Hashable NetworkInterfacePermissionStateCode

instance NFData NetworkInterfacePermissionStateCode

instance ToByteString NetworkInterfacePermissionStateCode

instance ToQuery NetworkInterfacePermissionStateCode

instance ToHeader NetworkInterfacePermissionStateCode

instance FromXML NetworkInterfacePermissionStateCode where
  parseXML = parseXMLText "NetworkInterfacePermissionStateCode"
