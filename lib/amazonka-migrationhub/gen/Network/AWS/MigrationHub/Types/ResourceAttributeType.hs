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
        ResourceAttributeTypeIPV4Address,
        ResourceAttributeTypeIPV6Address,
        ResourceAttributeTypeMacAddress,
        ResourceAttributeTypeFqdn,
        ResourceAttributeTypeVmManagerId,
        ResourceAttributeTypeVmManagedObjectReference,
        ResourceAttributeTypeVmName,
        ResourceAttributeTypeVmPath,
        ResourceAttributeTypeBiosId,
        ResourceAttributeTypeMotherboardSerialNumber,
        fromResourceAttributeType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ResourceAttributeType = ResourceAttributeType'
  { fromResourceAttributeType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ResourceAttributeTypeIPV4Address :: ResourceAttributeType
pattern ResourceAttributeTypeIPV4Address = ResourceAttributeType' "IPV4_ADDRESS"

pattern ResourceAttributeTypeIPV6Address :: ResourceAttributeType
pattern ResourceAttributeTypeIPV6Address = ResourceAttributeType' "IPV6_ADDRESS"

pattern ResourceAttributeTypeMacAddress :: ResourceAttributeType
pattern ResourceAttributeTypeMacAddress = ResourceAttributeType' "MAC_ADDRESS"

pattern ResourceAttributeTypeFqdn :: ResourceAttributeType
pattern ResourceAttributeTypeFqdn = ResourceAttributeType' "FQDN"

pattern ResourceAttributeTypeVmManagerId :: ResourceAttributeType
pattern ResourceAttributeTypeVmManagerId = ResourceAttributeType' "VM_MANAGER_ID"

pattern ResourceAttributeTypeVmManagedObjectReference :: ResourceAttributeType
pattern ResourceAttributeTypeVmManagedObjectReference = ResourceAttributeType' "VM_MANAGED_OBJECT_REFERENCE"

pattern ResourceAttributeTypeVmName :: ResourceAttributeType
pattern ResourceAttributeTypeVmName = ResourceAttributeType' "VM_NAME"

pattern ResourceAttributeTypeVmPath :: ResourceAttributeType
pattern ResourceAttributeTypeVmPath = ResourceAttributeType' "VM_PATH"

pattern ResourceAttributeTypeBiosId :: ResourceAttributeType
pattern ResourceAttributeTypeBiosId = ResourceAttributeType' "BIOS_ID"

pattern ResourceAttributeTypeMotherboardSerialNumber :: ResourceAttributeType
pattern ResourceAttributeTypeMotherboardSerialNumber = ResourceAttributeType' "MOTHERBOARD_SERIAL_NUMBER"

{-# COMPLETE
  ResourceAttributeTypeIPV4Address,
  ResourceAttributeTypeIPV6Address,
  ResourceAttributeTypeMacAddress,
  ResourceAttributeTypeFqdn,
  ResourceAttributeTypeVmManagerId,
  ResourceAttributeTypeVmManagedObjectReference,
  ResourceAttributeTypeVmName,
  ResourceAttributeTypeVmPath,
  ResourceAttributeTypeBiosId,
  ResourceAttributeTypeMotherboardSerialNumber,
  ResourceAttributeType'
  #-}
