{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceType where

import Network.AWS.Prelude

data ResourceType
  = RTAlarm
  | RTCertificate
  | RTCloudFormationStackRecord
  | RTContactMethod
  | RTContainerService
  | RTDisk
  | RTDiskSnapshot
  | RTDistribution
  | RTDomain
  | RTExportSnapshotRecord
  | RTInstance
  | RTInstanceSnapshot
  | RTKeyPair
  | RTLoadBalancer
  | RTLoadBalancerTLSCertificate
  | RTPeeredVPC
  | RTRelationalDatabase
  | RTRelationalDatabaseSnapshot
  | RTStaticIP
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

instance FromText ResourceType where
  parser =
    takeLowerText >>= \case
      "alarm" -> pure RTAlarm
      "certificate" -> pure RTCertificate
      "cloudformationstackrecord" -> pure RTCloudFormationStackRecord
      "contactmethod" -> pure RTContactMethod
      "containerservice" -> pure RTContainerService
      "disk" -> pure RTDisk
      "disksnapshot" -> pure RTDiskSnapshot
      "distribution" -> pure RTDistribution
      "domain" -> pure RTDomain
      "exportsnapshotrecord" -> pure RTExportSnapshotRecord
      "instance" -> pure RTInstance
      "instancesnapshot" -> pure RTInstanceSnapshot
      "keypair" -> pure RTKeyPair
      "loadbalancer" -> pure RTLoadBalancer
      "loadbalancertlscertificate" -> pure RTLoadBalancerTLSCertificate
      "peeredvpc" -> pure RTPeeredVPC
      "relationaldatabase" -> pure RTRelationalDatabase
      "relationaldatabasesnapshot" -> pure RTRelationalDatabaseSnapshot
      "staticip" -> pure RTStaticIP
      e ->
        fromTextError $
          "Failure parsing ResourceType from value: '" <> e
            <> "'. Accepted values: alarm, certificate, cloudformationstackrecord, contactmethod, containerservice, disk, disksnapshot, distribution, domain, exportsnapshotrecord, instance, instancesnapshot, keypair, loadbalancer, loadbalancertlscertificate, peeredvpc, relationaldatabase, relationaldatabasesnapshot, staticip"

instance ToText ResourceType where
  toText = \case
    RTAlarm -> "Alarm"
    RTCertificate -> "Certificate"
    RTCloudFormationStackRecord -> "CloudFormationStackRecord"
    RTContactMethod -> "ContactMethod"
    RTContainerService -> "ContainerService"
    RTDisk -> "Disk"
    RTDiskSnapshot -> "DiskSnapshot"
    RTDistribution -> "Distribution"
    RTDomain -> "Domain"
    RTExportSnapshotRecord -> "ExportSnapshotRecord"
    RTInstance -> "Instance"
    RTInstanceSnapshot -> "InstanceSnapshot"
    RTKeyPair -> "KeyPair"
    RTLoadBalancer -> "LoadBalancer"
    RTLoadBalancerTLSCertificate -> "LoadBalancerTlsCertificate"
    RTPeeredVPC -> "PeeredVpc"
    RTRelationalDatabase -> "RelationalDatabase"
    RTRelationalDatabaseSnapshot -> "RelationalDatabaseSnapshot"
    RTStaticIP -> "StaticIp"

instance Hashable ResourceType

instance NFData ResourceType

instance ToByteString ResourceType

instance ToQuery ResourceType

instance ToHeader ResourceType

instance FromJSON ResourceType where
  parseJSON = parseJSONText "ResourceType"
