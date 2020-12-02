{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.OperationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.OperationType where

import Network.AWS.Prelude

data OperationType
  = AllocateStaticIP
  | AttachCertificateToDistribution
  | AttachDisk
  | AttachInstancesToLoadBalancer
  | AttachLoadBalancerTLSCertificate
  | AttachStaticIP
  | CloseInstancePublicPorts
  | CreateCertificate
  | CreateContactMethod
  | CreateContainerService
  | CreateContainerServiceDeployment
  | CreateContainerServiceRegistryLogin
  | CreateDisk
  | CreateDiskFromSnapshot
  | CreateDiskSnapshot
  | CreateDistribution
  | CreateDomain
  | CreateInstance
  | CreateInstanceSnapshot
  | CreateInstancesFromSnapshot
  | CreateLoadBalancer
  | CreateLoadBalancerTLSCertificate
  | CreateRelationalDatabase
  | CreateRelationalDatabaseFromSnapshot
  | CreateRelationalDatabaseSnapshot
  | DeleteAlarm
  | DeleteCertificate
  | DeleteContactMethod
  | DeleteContainerImage
  | DeleteContainerService
  | DeleteDisk
  | DeleteDiskSnapshot
  | DeleteDistribution
  | DeleteDomain
  | DeleteDomainEntry
  | DeleteInstance
  | DeleteInstanceSnapshot
  | DeleteKnownHostKeys
  | DeleteLoadBalancer
  | DeleteLoadBalancerTLSCertificate
  | DeleteRelationalDatabase
  | DeleteRelationalDatabaseSnapshot
  | DetachCertificateFromDistribution
  | DetachDisk
  | DetachInstancesFromLoadBalancer
  | DetachStaticIP
  | DisableAddOn
  | EnableAddOn
  | GetAlarms
  | GetContactMethods
  | OpenInstancePublicPorts
  | PutAlarm
  | PutInstancePublicPorts
  | RebootInstance
  | RebootRelationalDatabase
  | RegisterContainerImage
  | ReleaseStaticIP
  | ResetDistributionCache
  | SendContactMethodVerification
  | StartInstance
  | StartRelationalDatabase
  | StopInstance
  | StopRelationalDatabase
  | TestAlarm
  | UpdateContainerService
  | UpdateDistribution
  | UpdateDistributionBundle
  | UpdateDomainEntry
  | UpdateLoadBalancerAttribute
  | UpdateRelationalDatabase
  | UpdateRelationalDatabaseParameters
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

instance FromText OperationType where
  parser =
    takeLowerText >>= \case
      "allocatestaticip" -> pure AllocateStaticIP
      "attachcertificatetodistribution" -> pure AttachCertificateToDistribution
      "attachdisk" -> pure AttachDisk
      "attachinstancestoloadbalancer" -> pure AttachInstancesToLoadBalancer
      "attachloadbalancertlscertificate" -> pure AttachLoadBalancerTLSCertificate
      "attachstaticip" -> pure AttachStaticIP
      "closeinstancepublicports" -> pure CloseInstancePublicPorts
      "createcertificate" -> pure CreateCertificate
      "createcontactmethod" -> pure CreateContactMethod
      "createcontainerservice" -> pure CreateContainerService
      "createcontainerservicedeployment" -> pure CreateContainerServiceDeployment
      "createcontainerserviceregistrylogin" -> pure CreateContainerServiceRegistryLogin
      "createdisk" -> pure CreateDisk
      "creatediskfromsnapshot" -> pure CreateDiskFromSnapshot
      "createdisksnapshot" -> pure CreateDiskSnapshot
      "createdistribution" -> pure CreateDistribution
      "createdomain" -> pure CreateDomain
      "createinstance" -> pure CreateInstance
      "createinstancesnapshot" -> pure CreateInstanceSnapshot
      "createinstancesfromsnapshot" -> pure CreateInstancesFromSnapshot
      "createloadbalancer" -> pure CreateLoadBalancer
      "createloadbalancertlscertificate" -> pure CreateLoadBalancerTLSCertificate
      "createrelationaldatabase" -> pure CreateRelationalDatabase
      "createrelationaldatabasefromsnapshot" -> pure CreateRelationalDatabaseFromSnapshot
      "createrelationaldatabasesnapshot" -> pure CreateRelationalDatabaseSnapshot
      "deletealarm" -> pure DeleteAlarm
      "deletecertificate" -> pure DeleteCertificate
      "deletecontactmethod" -> pure DeleteContactMethod
      "deletecontainerimage" -> pure DeleteContainerImage
      "deletecontainerservice" -> pure DeleteContainerService
      "deletedisk" -> pure DeleteDisk
      "deletedisksnapshot" -> pure DeleteDiskSnapshot
      "deletedistribution" -> pure DeleteDistribution
      "deletedomain" -> pure DeleteDomain
      "deletedomainentry" -> pure DeleteDomainEntry
      "deleteinstance" -> pure DeleteInstance
      "deleteinstancesnapshot" -> pure DeleteInstanceSnapshot
      "deleteknownhostkeys" -> pure DeleteKnownHostKeys
      "deleteloadbalancer" -> pure DeleteLoadBalancer
      "deleteloadbalancertlscertificate" -> pure DeleteLoadBalancerTLSCertificate
      "deleterelationaldatabase" -> pure DeleteRelationalDatabase
      "deleterelationaldatabasesnapshot" -> pure DeleteRelationalDatabaseSnapshot
      "detachcertificatefromdistribution" -> pure DetachCertificateFromDistribution
      "detachdisk" -> pure DetachDisk
      "detachinstancesfromloadbalancer" -> pure DetachInstancesFromLoadBalancer
      "detachstaticip" -> pure DetachStaticIP
      "disableaddon" -> pure DisableAddOn
      "enableaddon" -> pure EnableAddOn
      "getalarms" -> pure GetAlarms
      "getcontactmethods" -> pure GetContactMethods
      "openinstancepublicports" -> pure OpenInstancePublicPorts
      "putalarm" -> pure PutAlarm
      "putinstancepublicports" -> pure PutInstancePublicPorts
      "rebootinstance" -> pure RebootInstance
      "rebootrelationaldatabase" -> pure RebootRelationalDatabase
      "registercontainerimage" -> pure RegisterContainerImage
      "releasestaticip" -> pure ReleaseStaticIP
      "resetdistributioncache" -> pure ResetDistributionCache
      "sendcontactmethodverification" -> pure SendContactMethodVerification
      "startinstance" -> pure StartInstance
      "startrelationaldatabase" -> pure StartRelationalDatabase
      "stopinstance" -> pure StopInstance
      "stoprelationaldatabase" -> pure StopRelationalDatabase
      "testalarm" -> pure TestAlarm
      "updatecontainerservice" -> pure UpdateContainerService
      "updatedistribution" -> pure UpdateDistribution
      "updatedistributionbundle" -> pure UpdateDistributionBundle
      "updatedomainentry" -> pure UpdateDomainEntry
      "updateloadbalancerattribute" -> pure UpdateLoadBalancerAttribute
      "updaterelationaldatabase" -> pure UpdateRelationalDatabase
      "updaterelationaldatabaseparameters" -> pure UpdateRelationalDatabaseParameters
      e ->
        fromTextError $
          "Failure parsing OperationType from value: '" <> e
            <> "'. Accepted values: allocatestaticip, attachcertificatetodistribution, attachdisk, attachinstancestoloadbalancer, attachloadbalancertlscertificate, attachstaticip, closeinstancepublicports, createcertificate, createcontactmethod, createcontainerservice, createcontainerservicedeployment, createcontainerserviceregistrylogin, createdisk, creatediskfromsnapshot, createdisksnapshot, createdistribution, createdomain, createinstance, createinstancesnapshot, createinstancesfromsnapshot, createloadbalancer, createloadbalancertlscertificate, createrelationaldatabase, createrelationaldatabasefromsnapshot, createrelationaldatabasesnapshot, deletealarm, deletecertificate, deletecontactmethod, deletecontainerimage, deletecontainerservice, deletedisk, deletedisksnapshot, deletedistribution, deletedomain, deletedomainentry, deleteinstance, deleteinstancesnapshot, deleteknownhostkeys, deleteloadbalancer, deleteloadbalancertlscertificate, deleterelationaldatabase, deleterelationaldatabasesnapshot, detachcertificatefromdistribution, detachdisk, detachinstancesfromloadbalancer, detachstaticip, disableaddon, enableaddon, getalarms, getcontactmethods, openinstancepublicports, putalarm, putinstancepublicports, rebootinstance, rebootrelationaldatabase, registercontainerimage, releasestaticip, resetdistributioncache, sendcontactmethodverification, startinstance, startrelationaldatabase, stopinstance, stoprelationaldatabase, testalarm, updatecontainerservice, updatedistribution, updatedistributionbundle, updatedomainentry, updateloadbalancerattribute, updaterelationaldatabase, updaterelationaldatabaseparameters"

instance ToText OperationType where
  toText = \case
    AllocateStaticIP -> "AllocateStaticIp"
    AttachCertificateToDistribution -> "AttachCertificateToDistribution"
    AttachDisk -> "AttachDisk"
    AttachInstancesToLoadBalancer -> "AttachInstancesToLoadBalancer"
    AttachLoadBalancerTLSCertificate -> "AttachLoadBalancerTlsCertificate"
    AttachStaticIP -> "AttachStaticIp"
    CloseInstancePublicPorts -> "CloseInstancePublicPorts"
    CreateCertificate -> "CreateCertificate"
    CreateContactMethod -> "CreateContactMethod"
    CreateContainerService -> "CreateContainerService"
    CreateContainerServiceDeployment -> "CreateContainerServiceDeployment"
    CreateContainerServiceRegistryLogin -> "CreateContainerServiceRegistryLogin"
    CreateDisk -> "CreateDisk"
    CreateDiskFromSnapshot -> "CreateDiskFromSnapshot"
    CreateDiskSnapshot -> "CreateDiskSnapshot"
    CreateDistribution -> "CreateDistribution"
    CreateDomain -> "CreateDomain"
    CreateInstance -> "CreateInstance"
    CreateInstanceSnapshot -> "CreateInstanceSnapshot"
    CreateInstancesFromSnapshot -> "CreateInstancesFromSnapshot"
    CreateLoadBalancer -> "CreateLoadBalancer"
    CreateLoadBalancerTLSCertificate -> "CreateLoadBalancerTlsCertificate"
    CreateRelationalDatabase -> "CreateRelationalDatabase"
    CreateRelationalDatabaseFromSnapshot -> "CreateRelationalDatabaseFromSnapshot"
    CreateRelationalDatabaseSnapshot -> "CreateRelationalDatabaseSnapshot"
    DeleteAlarm -> "DeleteAlarm"
    DeleteCertificate -> "DeleteCertificate"
    DeleteContactMethod -> "DeleteContactMethod"
    DeleteContainerImage -> "DeleteContainerImage"
    DeleteContainerService -> "DeleteContainerService"
    DeleteDisk -> "DeleteDisk"
    DeleteDiskSnapshot -> "DeleteDiskSnapshot"
    DeleteDistribution -> "DeleteDistribution"
    DeleteDomain -> "DeleteDomain"
    DeleteDomainEntry -> "DeleteDomainEntry"
    DeleteInstance -> "DeleteInstance"
    DeleteInstanceSnapshot -> "DeleteInstanceSnapshot"
    DeleteKnownHostKeys -> "DeleteKnownHostKeys"
    DeleteLoadBalancer -> "DeleteLoadBalancer"
    DeleteLoadBalancerTLSCertificate -> "DeleteLoadBalancerTlsCertificate"
    DeleteRelationalDatabase -> "DeleteRelationalDatabase"
    DeleteRelationalDatabaseSnapshot -> "DeleteRelationalDatabaseSnapshot"
    DetachCertificateFromDistribution -> "DetachCertificateFromDistribution"
    DetachDisk -> "DetachDisk"
    DetachInstancesFromLoadBalancer -> "DetachInstancesFromLoadBalancer"
    DetachStaticIP -> "DetachStaticIp"
    DisableAddOn -> "DisableAddOn"
    EnableAddOn -> "EnableAddOn"
    GetAlarms -> "GetAlarms"
    GetContactMethods -> "GetContactMethods"
    OpenInstancePublicPorts -> "OpenInstancePublicPorts"
    PutAlarm -> "PutAlarm"
    PutInstancePublicPorts -> "PutInstancePublicPorts"
    RebootInstance -> "RebootInstance"
    RebootRelationalDatabase -> "RebootRelationalDatabase"
    RegisterContainerImage -> "RegisterContainerImage"
    ReleaseStaticIP -> "ReleaseStaticIp"
    ResetDistributionCache -> "ResetDistributionCache"
    SendContactMethodVerification -> "SendContactMethodVerification"
    StartInstance -> "StartInstance"
    StartRelationalDatabase -> "StartRelationalDatabase"
    StopInstance -> "StopInstance"
    StopRelationalDatabase -> "StopRelationalDatabase"
    TestAlarm -> "TestAlarm"
    UpdateContainerService -> "UpdateContainerService"
    UpdateDistribution -> "UpdateDistribution"
    UpdateDistributionBundle -> "UpdateDistributionBundle"
    UpdateDomainEntry -> "UpdateDomainEntry"
    UpdateLoadBalancerAttribute -> "UpdateLoadBalancerAttribute"
    UpdateRelationalDatabase -> "UpdateRelationalDatabase"
    UpdateRelationalDatabaseParameters -> "UpdateRelationalDatabaseParameters"

instance Hashable OperationType

instance NFData OperationType

instance ToByteString OperationType

instance ToQuery OperationType

instance ToHeader OperationType

instance FromJSON OperationType where
  parseJSON = parseJSONText "OperationType"
