{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ResourceAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ResourceAttributeType where

import Network.AWS.Prelude

data ResourceAttributeType
  = BiosId
  | Fqdn
  | IPV4Address
  | IPV6Address
  | MACAddress
  | MotherboardSerialNumber
  | VMManagedObjectReference
  | VMManagerId
  | VMName
  | VMPath
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

instance FromText ResourceAttributeType where
  parser =
    takeLowerText >>= \case
      "bios_id" -> pure BiosId
      "fqdn" -> pure Fqdn
      "ipv4_address" -> pure IPV4Address
      "ipv6_address" -> pure IPV6Address
      "mac_address" -> pure MACAddress
      "motherboard_serial_number" -> pure MotherboardSerialNumber
      "vm_managed_object_reference" -> pure VMManagedObjectReference
      "vm_manager_id" -> pure VMManagerId
      "vm_name" -> pure VMName
      "vm_path" -> pure VMPath
      e ->
        fromTextError $
          "Failure parsing ResourceAttributeType from value: '" <> e
            <> "'. Accepted values: bios_id, fqdn, ipv4_address, ipv6_address, mac_address, motherboard_serial_number, vm_managed_object_reference, vm_manager_id, vm_name, vm_path"

instance ToText ResourceAttributeType where
  toText = \case
    BiosId -> "BIOS_ID"
    Fqdn -> "FQDN"
    IPV4Address -> "IPV4_ADDRESS"
    IPV6Address -> "IPV6_ADDRESS"
    MACAddress -> "MAC_ADDRESS"
    MotherboardSerialNumber -> "MOTHERBOARD_SERIAL_NUMBER"
    VMManagedObjectReference -> "VM_MANAGED_OBJECT_REFERENCE"
    VMManagerId -> "VM_MANAGER_ID"
    VMName -> "VM_NAME"
    VMPath -> "VM_PATH"

instance Hashable ResourceAttributeType

instance NFData ResourceAttributeType

instance ToByteString ResourceAttributeType

instance ToQuery ResourceAttributeType

instance ToHeader ResourceAttributeType

instance ToJSON ResourceAttributeType where
  toJSON = toJSONText

instance FromJSON ResourceAttributeType where
  parseJSON = parseJSONText "ResourceAttributeType"
