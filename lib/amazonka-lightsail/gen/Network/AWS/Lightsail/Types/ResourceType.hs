{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceType
  ( ResourceType
      ( ResourceType',
        RTContainerService,
        RTInstance,
        RTStaticIP,
        RTKeyPair,
        RTInstanceSnapshot,
        RTDomain,
        RTPeeredVPC,
        RTLoadBalancer,
        RTLoadBalancerTLSCertificate,
        RTDisk,
        RTDiskSnapshot,
        RTRelationalDatabase,
        RTRelationalDatabaseSnapshot,
        RTExportSnapshotRecord,
        RTCloudFormationStackRecord,
        RTAlarm,
        RTContactMethod,
        RTDistribution,
        RTCertificate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceType = ResourceType' Lude.Text
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

pattern RTContainerService :: ResourceType
pattern RTContainerService = ResourceType' "ContainerService"

pattern RTInstance :: ResourceType
pattern RTInstance = ResourceType' "Instance"

pattern RTStaticIP :: ResourceType
pattern RTStaticIP = ResourceType' "StaticIp"

pattern RTKeyPair :: ResourceType
pattern RTKeyPair = ResourceType' "KeyPair"

pattern RTInstanceSnapshot :: ResourceType
pattern RTInstanceSnapshot = ResourceType' "InstanceSnapshot"

pattern RTDomain :: ResourceType
pattern RTDomain = ResourceType' "Domain"

pattern RTPeeredVPC :: ResourceType
pattern RTPeeredVPC = ResourceType' "PeeredVpc"

pattern RTLoadBalancer :: ResourceType
pattern RTLoadBalancer = ResourceType' "LoadBalancer"

pattern RTLoadBalancerTLSCertificate :: ResourceType
pattern RTLoadBalancerTLSCertificate = ResourceType' "LoadBalancerTlsCertificate"

pattern RTDisk :: ResourceType
pattern RTDisk = ResourceType' "Disk"

pattern RTDiskSnapshot :: ResourceType
pattern RTDiskSnapshot = ResourceType' "DiskSnapshot"

pattern RTRelationalDatabase :: ResourceType
pattern RTRelationalDatabase = ResourceType' "RelationalDatabase"

pattern RTRelationalDatabaseSnapshot :: ResourceType
pattern RTRelationalDatabaseSnapshot = ResourceType' "RelationalDatabaseSnapshot"

pattern RTExportSnapshotRecord :: ResourceType
pattern RTExportSnapshotRecord = ResourceType' "ExportSnapshotRecord"

pattern RTCloudFormationStackRecord :: ResourceType
pattern RTCloudFormationStackRecord = ResourceType' "CloudFormationStackRecord"

pattern RTAlarm :: ResourceType
pattern RTAlarm = ResourceType' "Alarm"

pattern RTContactMethod :: ResourceType
pattern RTContactMethod = ResourceType' "ContactMethod"

pattern RTDistribution :: ResourceType
pattern RTDistribution = ResourceType' "Distribution"

pattern RTCertificate :: ResourceType
pattern RTCertificate = ResourceType' "Certificate"

{-# COMPLETE
  RTContainerService,
  RTInstance,
  RTStaticIP,
  RTKeyPair,
  RTInstanceSnapshot,
  RTDomain,
  RTPeeredVPC,
  RTLoadBalancer,
  RTLoadBalancerTLSCertificate,
  RTDisk,
  RTDiskSnapshot,
  RTRelationalDatabase,
  RTRelationalDatabaseSnapshot,
  RTExportSnapshotRecord,
  RTCloudFormationStackRecord,
  RTAlarm,
  RTContactMethod,
  RTDistribution,
  RTCertificate,
  ResourceType'
  #-}
