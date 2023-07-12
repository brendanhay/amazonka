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
-- Module      : Amazonka.SecurityLake.Types.OcsfEventClass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.OcsfEventClass
  ( OcsfEventClass
      ( ..,
        OcsfEventClass_ACCESS_ACTIVITY,
        OcsfEventClass_ACCOUNT_CHANGE,
        OcsfEventClass_AUTHENTICATION,
        OcsfEventClass_AUTHORIZATION,
        OcsfEventClass_CLOUD_API,
        OcsfEventClass_CLOUD_STORAGE,
        OcsfEventClass_CONFIG_STATE,
        OcsfEventClass_CONTAINER_LIFECYCLE,
        OcsfEventClass_DATABASE_LIFECYCLE,
        OcsfEventClass_DHCP_ACTIVITY,
        OcsfEventClass_DNS_ACTIVITY,
        OcsfEventClass_ENTITY_MANAGEMENT_AUDIT,
        OcsfEventClass_FILE_ACTIVITY,
        OcsfEventClass_FTP_ACTIVITY,
        OcsfEventClass_HTTP_ACTIVITY,
        OcsfEventClass_INVENTORY_INFO,
        OcsfEventClass_KERNEL_ACTIVITY,
        OcsfEventClass_KERNEL_EXTENSION,
        OcsfEventClass_MEMORY_ACTIVITY,
        OcsfEventClass_MODULE_ACTIVITY,
        OcsfEventClass_NETWORK_ACTIVITY,
        OcsfEventClass_PROCESS_ACTIVITY,
        OcsfEventClass_RDP_ACTIVITY,
        OcsfEventClass_REGISTRY_KEY_ACTIVITY,
        OcsfEventClass_REGISTRY_VALUE_ACTIVITY,
        OcsfEventClass_RESOURCE_ACTIVITY,
        OcsfEventClass_RFB_ACTIVITY,
        OcsfEventClass_SCHEDULED_JOB_ACTIVITY,
        OcsfEventClass_SECURITY_FINDING,
        OcsfEventClass_SMB_ACTIVITY,
        OcsfEventClass_SMTP_ACTIVITY,
        OcsfEventClass_SSH_ACTIVITY,
        OcsfEventClass_VIRTUAL_MACHINE_ACTIVITY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OcsfEventClass = OcsfEventClass'
  { fromOcsfEventClass ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern OcsfEventClass_ACCESS_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_ACCESS_ACTIVITY = OcsfEventClass' "ACCESS_ACTIVITY"

pattern OcsfEventClass_ACCOUNT_CHANGE :: OcsfEventClass
pattern OcsfEventClass_ACCOUNT_CHANGE = OcsfEventClass' "ACCOUNT_CHANGE"

pattern OcsfEventClass_AUTHENTICATION :: OcsfEventClass
pattern OcsfEventClass_AUTHENTICATION = OcsfEventClass' "AUTHENTICATION"

pattern OcsfEventClass_AUTHORIZATION :: OcsfEventClass
pattern OcsfEventClass_AUTHORIZATION = OcsfEventClass' "AUTHORIZATION"

pattern OcsfEventClass_CLOUD_API :: OcsfEventClass
pattern OcsfEventClass_CLOUD_API = OcsfEventClass' "CLOUD_API"

pattern OcsfEventClass_CLOUD_STORAGE :: OcsfEventClass
pattern OcsfEventClass_CLOUD_STORAGE = OcsfEventClass' "CLOUD_STORAGE"

pattern OcsfEventClass_CONFIG_STATE :: OcsfEventClass
pattern OcsfEventClass_CONFIG_STATE = OcsfEventClass' "CONFIG_STATE"

pattern OcsfEventClass_CONTAINER_LIFECYCLE :: OcsfEventClass
pattern OcsfEventClass_CONTAINER_LIFECYCLE = OcsfEventClass' "CONTAINER_LIFECYCLE"

pattern OcsfEventClass_DATABASE_LIFECYCLE :: OcsfEventClass
pattern OcsfEventClass_DATABASE_LIFECYCLE = OcsfEventClass' "DATABASE_LIFECYCLE"

pattern OcsfEventClass_DHCP_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_DHCP_ACTIVITY = OcsfEventClass' "DHCP_ACTIVITY"

pattern OcsfEventClass_DNS_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_DNS_ACTIVITY = OcsfEventClass' "DNS_ACTIVITY"

pattern OcsfEventClass_ENTITY_MANAGEMENT_AUDIT :: OcsfEventClass
pattern OcsfEventClass_ENTITY_MANAGEMENT_AUDIT = OcsfEventClass' "ENTITY_MANAGEMENT_AUDIT"

pattern OcsfEventClass_FILE_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_FILE_ACTIVITY = OcsfEventClass' "FILE_ACTIVITY"

pattern OcsfEventClass_FTP_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_FTP_ACTIVITY = OcsfEventClass' "FTP_ACTIVITY"

pattern OcsfEventClass_HTTP_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_HTTP_ACTIVITY = OcsfEventClass' "HTTP_ACTIVITY"

pattern OcsfEventClass_INVENTORY_INFO :: OcsfEventClass
pattern OcsfEventClass_INVENTORY_INFO = OcsfEventClass' "INVENTORY_INFO"

pattern OcsfEventClass_KERNEL_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_KERNEL_ACTIVITY = OcsfEventClass' "KERNEL_ACTIVITY"

pattern OcsfEventClass_KERNEL_EXTENSION :: OcsfEventClass
pattern OcsfEventClass_KERNEL_EXTENSION = OcsfEventClass' "KERNEL_EXTENSION"

pattern OcsfEventClass_MEMORY_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_MEMORY_ACTIVITY = OcsfEventClass' "MEMORY_ACTIVITY"

pattern OcsfEventClass_MODULE_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_MODULE_ACTIVITY = OcsfEventClass' "MODULE_ACTIVITY"

pattern OcsfEventClass_NETWORK_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_NETWORK_ACTIVITY = OcsfEventClass' "NETWORK_ACTIVITY"

pattern OcsfEventClass_PROCESS_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_PROCESS_ACTIVITY = OcsfEventClass' "PROCESS_ACTIVITY"

pattern OcsfEventClass_RDP_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_RDP_ACTIVITY = OcsfEventClass' "RDP_ACTIVITY"

pattern OcsfEventClass_REGISTRY_KEY_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_REGISTRY_KEY_ACTIVITY = OcsfEventClass' "REGISTRY_KEY_ACTIVITY"

pattern OcsfEventClass_REGISTRY_VALUE_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_REGISTRY_VALUE_ACTIVITY = OcsfEventClass' "REGISTRY_VALUE_ACTIVITY"

pattern OcsfEventClass_RESOURCE_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_RESOURCE_ACTIVITY = OcsfEventClass' "RESOURCE_ACTIVITY"

pattern OcsfEventClass_RFB_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_RFB_ACTIVITY = OcsfEventClass' "RFB_ACTIVITY"

pattern OcsfEventClass_SCHEDULED_JOB_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_SCHEDULED_JOB_ACTIVITY = OcsfEventClass' "SCHEDULED_JOB_ACTIVITY"

pattern OcsfEventClass_SECURITY_FINDING :: OcsfEventClass
pattern OcsfEventClass_SECURITY_FINDING = OcsfEventClass' "SECURITY_FINDING"

pattern OcsfEventClass_SMB_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_SMB_ACTIVITY = OcsfEventClass' "SMB_ACTIVITY"

pattern OcsfEventClass_SMTP_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_SMTP_ACTIVITY = OcsfEventClass' "SMTP_ACTIVITY"

pattern OcsfEventClass_SSH_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_SSH_ACTIVITY = OcsfEventClass' "SSH_ACTIVITY"

pattern OcsfEventClass_VIRTUAL_MACHINE_ACTIVITY :: OcsfEventClass
pattern OcsfEventClass_VIRTUAL_MACHINE_ACTIVITY = OcsfEventClass' "VIRTUAL_MACHINE_ACTIVITY"

{-# COMPLETE
  OcsfEventClass_ACCESS_ACTIVITY,
  OcsfEventClass_ACCOUNT_CHANGE,
  OcsfEventClass_AUTHENTICATION,
  OcsfEventClass_AUTHORIZATION,
  OcsfEventClass_CLOUD_API,
  OcsfEventClass_CLOUD_STORAGE,
  OcsfEventClass_CONFIG_STATE,
  OcsfEventClass_CONTAINER_LIFECYCLE,
  OcsfEventClass_DATABASE_LIFECYCLE,
  OcsfEventClass_DHCP_ACTIVITY,
  OcsfEventClass_DNS_ACTIVITY,
  OcsfEventClass_ENTITY_MANAGEMENT_AUDIT,
  OcsfEventClass_FILE_ACTIVITY,
  OcsfEventClass_FTP_ACTIVITY,
  OcsfEventClass_HTTP_ACTIVITY,
  OcsfEventClass_INVENTORY_INFO,
  OcsfEventClass_KERNEL_ACTIVITY,
  OcsfEventClass_KERNEL_EXTENSION,
  OcsfEventClass_MEMORY_ACTIVITY,
  OcsfEventClass_MODULE_ACTIVITY,
  OcsfEventClass_NETWORK_ACTIVITY,
  OcsfEventClass_PROCESS_ACTIVITY,
  OcsfEventClass_RDP_ACTIVITY,
  OcsfEventClass_REGISTRY_KEY_ACTIVITY,
  OcsfEventClass_REGISTRY_VALUE_ACTIVITY,
  OcsfEventClass_RESOURCE_ACTIVITY,
  OcsfEventClass_RFB_ACTIVITY,
  OcsfEventClass_SCHEDULED_JOB_ACTIVITY,
  OcsfEventClass_SECURITY_FINDING,
  OcsfEventClass_SMB_ACTIVITY,
  OcsfEventClass_SMTP_ACTIVITY,
  OcsfEventClass_SSH_ACTIVITY,
  OcsfEventClass_VIRTUAL_MACHINE_ACTIVITY,
  OcsfEventClass'
  #-}
