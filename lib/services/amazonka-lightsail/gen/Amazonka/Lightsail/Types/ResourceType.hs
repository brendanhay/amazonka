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
-- Module      : Amazonka.Lightsail.Types.ResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_Alarm,
        ResourceType_Bucket,
        ResourceType_Certificate,
        ResourceType_CloudFormationStackRecord,
        ResourceType_ContactMethod,
        ResourceType_ContainerService,
        ResourceType_Disk,
        ResourceType_DiskSnapshot,
        ResourceType_Distribution,
        ResourceType_Domain,
        ResourceType_ExportSnapshotRecord,
        ResourceType_Instance,
        ResourceType_InstanceSnapshot,
        ResourceType_KeyPair,
        ResourceType_LoadBalancer,
        ResourceType_LoadBalancerTlsCertificate,
        ResourceType_PeeredVpc,
        ResourceType_RelationalDatabase,
        ResourceType_RelationalDatabaseSnapshot,
        ResourceType_StaticIp
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
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

pattern ResourceType_Alarm :: ResourceType
pattern ResourceType_Alarm = ResourceType' "Alarm"

pattern ResourceType_Bucket :: ResourceType
pattern ResourceType_Bucket = ResourceType' "Bucket"

pattern ResourceType_Certificate :: ResourceType
pattern ResourceType_Certificate = ResourceType' "Certificate"

pattern ResourceType_CloudFormationStackRecord :: ResourceType
pattern ResourceType_CloudFormationStackRecord = ResourceType' "CloudFormationStackRecord"

pattern ResourceType_ContactMethod :: ResourceType
pattern ResourceType_ContactMethod = ResourceType' "ContactMethod"

pattern ResourceType_ContainerService :: ResourceType
pattern ResourceType_ContainerService = ResourceType' "ContainerService"

pattern ResourceType_Disk :: ResourceType
pattern ResourceType_Disk = ResourceType' "Disk"

pattern ResourceType_DiskSnapshot :: ResourceType
pattern ResourceType_DiskSnapshot = ResourceType' "DiskSnapshot"

pattern ResourceType_Distribution :: ResourceType
pattern ResourceType_Distribution = ResourceType' "Distribution"

pattern ResourceType_Domain :: ResourceType
pattern ResourceType_Domain = ResourceType' "Domain"

pattern ResourceType_ExportSnapshotRecord :: ResourceType
pattern ResourceType_ExportSnapshotRecord = ResourceType' "ExportSnapshotRecord"

pattern ResourceType_Instance :: ResourceType
pattern ResourceType_Instance = ResourceType' "Instance"

pattern ResourceType_InstanceSnapshot :: ResourceType
pattern ResourceType_InstanceSnapshot = ResourceType' "InstanceSnapshot"

pattern ResourceType_KeyPair :: ResourceType
pattern ResourceType_KeyPair = ResourceType' "KeyPair"

pattern ResourceType_LoadBalancer :: ResourceType
pattern ResourceType_LoadBalancer = ResourceType' "LoadBalancer"

pattern ResourceType_LoadBalancerTlsCertificate :: ResourceType
pattern ResourceType_LoadBalancerTlsCertificate = ResourceType' "LoadBalancerTlsCertificate"

pattern ResourceType_PeeredVpc :: ResourceType
pattern ResourceType_PeeredVpc = ResourceType' "PeeredVpc"

pattern ResourceType_RelationalDatabase :: ResourceType
pattern ResourceType_RelationalDatabase = ResourceType' "RelationalDatabase"

pattern ResourceType_RelationalDatabaseSnapshot :: ResourceType
pattern ResourceType_RelationalDatabaseSnapshot = ResourceType' "RelationalDatabaseSnapshot"

pattern ResourceType_StaticIp :: ResourceType
pattern ResourceType_StaticIp = ResourceType' "StaticIp"

{-# COMPLETE
  ResourceType_Alarm,
  ResourceType_Bucket,
  ResourceType_Certificate,
  ResourceType_CloudFormationStackRecord,
  ResourceType_ContactMethod,
  ResourceType_ContainerService,
  ResourceType_Disk,
  ResourceType_DiskSnapshot,
  ResourceType_Distribution,
  ResourceType_Domain,
  ResourceType_ExportSnapshotRecord,
  ResourceType_Instance,
  ResourceType_InstanceSnapshot,
  ResourceType_KeyPair,
  ResourceType_LoadBalancer,
  ResourceType_LoadBalancerTlsCertificate,
  ResourceType_PeeredVpc,
  ResourceType_RelationalDatabase,
  ResourceType_RelationalDatabaseSnapshot,
  ResourceType_StaticIp,
  ResourceType'
  #-}
