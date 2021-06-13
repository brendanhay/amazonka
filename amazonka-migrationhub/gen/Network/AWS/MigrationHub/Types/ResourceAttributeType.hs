{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ResourceAttributeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ResourceAttributeType
  ( ResourceAttributeType
      ( ..,
        ResourceAttributeType_BIOS_ID,
        ResourceAttributeType_FQDN,
        ResourceAttributeType_IPV4_ADDRESS,
        ResourceAttributeType_IPV6_ADDRESS,
        ResourceAttributeType_MAC_ADDRESS,
        ResourceAttributeType_MOTHERBOARD_SERIAL_NUMBER,
        ResourceAttributeType_VM_MANAGED_OBJECT_REFERENCE,
        ResourceAttributeType_VM_MANAGER_ID,
        ResourceAttributeType_VM_NAME,
        ResourceAttributeType_VM_PATH
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ResourceAttributeType = ResourceAttributeType'
  { fromResourceAttributeType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ResourceAttributeType_BIOS_ID :: ResourceAttributeType
pattern ResourceAttributeType_BIOS_ID = ResourceAttributeType' "BIOS_ID"

pattern ResourceAttributeType_FQDN :: ResourceAttributeType
pattern ResourceAttributeType_FQDN = ResourceAttributeType' "FQDN"

pattern ResourceAttributeType_IPV4_ADDRESS :: ResourceAttributeType
pattern ResourceAttributeType_IPV4_ADDRESS = ResourceAttributeType' "IPV4_ADDRESS"

pattern ResourceAttributeType_IPV6_ADDRESS :: ResourceAttributeType
pattern ResourceAttributeType_IPV6_ADDRESS = ResourceAttributeType' "IPV6_ADDRESS"

pattern ResourceAttributeType_MAC_ADDRESS :: ResourceAttributeType
pattern ResourceAttributeType_MAC_ADDRESS = ResourceAttributeType' "MAC_ADDRESS"

pattern ResourceAttributeType_MOTHERBOARD_SERIAL_NUMBER :: ResourceAttributeType
pattern ResourceAttributeType_MOTHERBOARD_SERIAL_NUMBER = ResourceAttributeType' "MOTHERBOARD_SERIAL_NUMBER"

pattern ResourceAttributeType_VM_MANAGED_OBJECT_REFERENCE :: ResourceAttributeType
pattern ResourceAttributeType_VM_MANAGED_OBJECT_REFERENCE = ResourceAttributeType' "VM_MANAGED_OBJECT_REFERENCE"

pattern ResourceAttributeType_VM_MANAGER_ID :: ResourceAttributeType
pattern ResourceAttributeType_VM_MANAGER_ID = ResourceAttributeType' "VM_MANAGER_ID"

pattern ResourceAttributeType_VM_NAME :: ResourceAttributeType
pattern ResourceAttributeType_VM_NAME = ResourceAttributeType' "VM_NAME"

pattern ResourceAttributeType_VM_PATH :: ResourceAttributeType
pattern ResourceAttributeType_VM_PATH = ResourceAttributeType' "VM_PATH"

{-# COMPLETE
  ResourceAttributeType_BIOS_ID,
  ResourceAttributeType_FQDN,
  ResourceAttributeType_IPV4_ADDRESS,
  ResourceAttributeType_IPV6_ADDRESS,
  ResourceAttributeType_MAC_ADDRESS,
  ResourceAttributeType_MOTHERBOARD_SERIAL_NUMBER,
  ResourceAttributeType_VM_MANAGED_OBJECT_REFERENCE,
  ResourceAttributeType_VM_MANAGER_ID,
  ResourceAttributeType_VM_NAME,
  ResourceAttributeType_VM_PATH,
  ResourceAttributeType'
  #-}
