{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ResourceAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ResourceAttributeType
  ( ResourceAttributeType
      ( ResourceAttributeType',
        IPV4Address,
        IPV6Address,
        MACAddress,
        Fqdn,
        VMManagerId,
        VMManagedObjectReference,
        VMName,
        VMPath,
        BiosId,
        MotherboardSerialNumber
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceAttributeType = ResourceAttributeType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern IPV4Address :: ResourceAttributeType
pattern IPV4Address = ResourceAttributeType' "IPV4_ADDRESS"

pattern IPV6Address :: ResourceAttributeType
pattern IPV6Address = ResourceAttributeType' "IPV6_ADDRESS"

pattern MACAddress :: ResourceAttributeType
pattern MACAddress = ResourceAttributeType' "MAC_ADDRESS"

pattern Fqdn :: ResourceAttributeType
pattern Fqdn = ResourceAttributeType' "FQDN"

pattern VMManagerId :: ResourceAttributeType
pattern VMManagerId = ResourceAttributeType' "VM_MANAGER_ID"

pattern VMManagedObjectReference :: ResourceAttributeType
pattern VMManagedObjectReference = ResourceAttributeType' "VM_MANAGED_OBJECT_REFERENCE"

pattern VMName :: ResourceAttributeType
pattern VMName = ResourceAttributeType' "VM_NAME"

pattern VMPath :: ResourceAttributeType
pattern VMPath = ResourceAttributeType' "VM_PATH"

pattern BiosId :: ResourceAttributeType
pattern BiosId = ResourceAttributeType' "BIOS_ID"

pattern MotherboardSerialNumber :: ResourceAttributeType
pattern MotherboardSerialNumber = ResourceAttributeType' "MOTHERBOARD_SERIAL_NUMBER"

{-# COMPLETE
  IPV4Address,
  IPV6Address,
  MACAddress,
  Fqdn,
  VMManagerId,
  VMManagedObjectReference,
  VMName,
  VMPath,
  BiosId,
  MotherboardSerialNumber,
  ResourceAttributeType'
  #-}
