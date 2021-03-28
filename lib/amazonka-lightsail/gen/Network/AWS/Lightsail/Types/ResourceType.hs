{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ResourceType
  ( ResourceType
    ( ResourceType'
    , ResourceTypeContainerService
    , ResourceTypeInstance
    , ResourceTypeStaticIp
    , ResourceTypeKeyPair
    , ResourceTypeInstanceSnapshot
    , ResourceTypeDomain
    , ResourceTypePeeredVpc
    , ResourceTypeLoadBalancer
    , ResourceTypeLoadBalancerTlsCertificate
    , ResourceTypeDisk
    , ResourceTypeDiskSnapshot
    , ResourceTypeRelationalDatabase
    , ResourceTypeRelationalDatabaseSnapshot
    , ResourceTypeExportSnapshotRecord
    , ResourceTypeCloudFormationStackRecord
    , ResourceTypeAlarm
    , ResourceTypeContactMethod
    , ResourceTypeDistribution
    , ResourceTypeCertificate
    , fromResourceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ResourceType = ResourceType'{fromResourceType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern ResourceTypeContainerService :: ResourceType
pattern ResourceTypeContainerService = ResourceType' "ContainerService"

pattern ResourceTypeInstance :: ResourceType
pattern ResourceTypeInstance = ResourceType' "Instance"

pattern ResourceTypeStaticIp :: ResourceType
pattern ResourceTypeStaticIp = ResourceType' "StaticIp"

pattern ResourceTypeKeyPair :: ResourceType
pattern ResourceTypeKeyPair = ResourceType' "KeyPair"

pattern ResourceTypeInstanceSnapshot :: ResourceType
pattern ResourceTypeInstanceSnapshot = ResourceType' "InstanceSnapshot"

pattern ResourceTypeDomain :: ResourceType
pattern ResourceTypeDomain = ResourceType' "Domain"

pattern ResourceTypePeeredVpc :: ResourceType
pattern ResourceTypePeeredVpc = ResourceType' "PeeredVpc"

pattern ResourceTypeLoadBalancer :: ResourceType
pattern ResourceTypeLoadBalancer = ResourceType' "LoadBalancer"

pattern ResourceTypeLoadBalancerTlsCertificate :: ResourceType
pattern ResourceTypeLoadBalancerTlsCertificate = ResourceType' "LoadBalancerTlsCertificate"

pattern ResourceTypeDisk :: ResourceType
pattern ResourceTypeDisk = ResourceType' "Disk"

pattern ResourceTypeDiskSnapshot :: ResourceType
pattern ResourceTypeDiskSnapshot = ResourceType' "DiskSnapshot"

pattern ResourceTypeRelationalDatabase :: ResourceType
pattern ResourceTypeRelationalDatabase = ResourceType' "RelationalDatabase"

pattern ResourceTypeRelationalDatabaseSnapshot :: ResourceType
pattern ResourceTypeRelationalDatabaseSnapshot = ResourceType' "RelationalDatabaseSnapshot"

pattern ResourceTypeExportSnapshotRecord :: ResourceType
pattern ResourceTypeExportSnapshotRecord = ResourceType' "ExportSnapshotRecord"

pattern ResourceTypeCloudFormationStackRecord :: ResourceType
pattern ResourceTypeCloudFormationStackRecord = ResourceType' "CloudFormationStackRecord"

pattern ResourceTypeAlarm :: ResourceType
pattern ResourceTypeAlarm = ResourceType' "Alarm"

pattern ResourceTypeContactMethod :: ResourceType
pattern ResourceTypeContactMethod = ResourceType' "ContactMethod"

pattern ResourceTypeDistribution :: ResourceType
pattern ResourceTypeDistribution = ResourceType' "Distribution"

pattern ResourceTypeCertificate :: ResourceType
pattern ResourceTypeCertificate = ResourceType' "Certificate"

{-# COMPLETE 
  ResourceTypeContainerService,

  ResourceTypeInstance,

  ResourceTypeStaticIp,

  ResourceTypeKeyPair,

  ResourceTypeInstanceSnapshot,

  ResourceTypeDomain,

  ResourceTypePeeredVpc,

  ResourceTypeLoadBalancer,

  ResourceTypeLoadBalancerTlsCertificate,

  ResourceTypeDisk,

  ResourceTypeDiskSnapshot,

  ResourceTypeRelationalDatabase,

  ResourceTypeRelationalDatabaseSnapshot,

  ResourceTypeExportSnapshotRecord,

  ResourceTypeCloudFormationStackRecord,

  ResourceTypeAlarm,

  ResourceTypeContactMethod,

  ResourceTypeDistribution,

  ResourceTypeCertificate,
  ResourceType'
  #-}
