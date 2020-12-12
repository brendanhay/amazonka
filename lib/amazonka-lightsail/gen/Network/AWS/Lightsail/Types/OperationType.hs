{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.OperationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.OperationType
  ( OperationType
      ( OperationType',
        AllocateStaticIP,
        AttachCertificateToDistribution,
        AttachDisk,
        AttachInstancesToLoadBalancer,
        AttachLoadBalancerTLSCertificate,
        AttachStaticIP,
        CloseInstancePublicPorts,
        CreateCertificate,
        CreateContactMethod,
        CreateContainerService,
        CreateContainerServiceDeployment,
        CreateContainerServiceRegistryLogin,
        CreateDisk,
        CreateDiskFromSnapshot,
        CreateDiskSnapshot,
        CreateDistribution,
        CreateDomain,
        CreateInstance,
        CreateInstanceSnapshot,
        CreateInstancesFromSnapshot,
        CreateLoadBalancer,
        CreateLoadBalancerTLSCertificate,
        CreateRelationalDatabase,
        CreateRelationalDatabaseFromSnapshot,
        CreateRelationalDatabaseSnapshot,
        DeleteAlarm,
        DeleteCertificate,
        DeleteContactMethod,
        DeleteContainerImage,
        DeleteContainerService,
        DeleteDisk,
        DeleteDiskSnapshot,
        DeleteDistribution,
        DeleteDomain,
        DeleteDomainEntry,
        DeleteInstance,
        DeleteInstanceSnapshot,
        DeleteKnownHostKeys,
        DeleteLoadBalancer,
        DeleteLoadBalancerTLSCertificate,
        DeleteRelationalDatabase,
        DeleteRelationalDatabaseSnapshot,
        DetachCertificateFromDistribution,
        DetachDisk,
        DetachInstancesFromLoadBalancer,
        DetachStaticIP,
        DisableAddOn,
        EnableAddOn,
        GetAlarms,
        GetContactMethods,
        OpenInstancePublicPorts,
        PutAlarm,
        PutInstancePublicPorts,
        RebootInstance,
        RebootRelationalDatabase,
        RegisterContainerImage,
        ReleaseStaticIP,
        ResetDistributionCache,
        SendContactMethodVerification,
        StartInstance,
        StartRelationalDatabase,
        StopInstance,
        StopRelationalDatabase,
        TestAlarm,
        UpdateContainerService,
        UpdateDistribution,
        UpdateDistributionBundle,
        UpdateDomainEntry,
        UpdateLoadBalancerAttribute,
        UpdateRelationalDatabase,
        UpdateRelationalDatabaseParameters
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OperationType = OperationType' Lude.Text
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

pattern AllocateStaticIP :: OperationType
pattern AllocateStaticIP = OperationType' "AllocateStaticIp"

pattern AttachCertificateToDistribution :: OperationType
pattern AttachCertificateToDistribution = OperationType' "AttachCertificateToDistribution"

pattern AttachDisk :: OperationType
pattern AttachDisk = OperationType' "AttachDisk"

pattern AttachInstancesToLoadBalancer :: OperationType
pattern AttachInstancesToLoadBalancer = OperationType' "AttachInstancesToLoadBalancer"

pattern AttachLoadBalancerTLSCertificate :: OperationType
pattern AttachLoadBalancerTLSCertificate = OperationType' "AttachLoadBalancerTlsCertificate"

pattern AttachStaticIP :: OperationType
pattern AttachStaticIP = OperationType' "AttachStaticIp"

pattern CloseInstancePublicPorts :: OperationType
pattern CloseInstancePublicPorts = OperationType' "CloseInstancePublicPorts"

pattern CreateCertificate :: OperationType
pattern CreateCertificate = OperationType' "CreateCertificate"

pattern CreateContactMethod :: OperationType
pattern CreateContactMethod = OperationType' "CreateContactMethod"

pattern CreateContainerService :: OperationType
pattern CreateContainerService = OperationType' "CreateContainerService"

pattern CreateContainerServiceDeployment :: OperationType
pattern CreateContainerServiceDeployment = OperationType' "CreateContainerServiceDeployment"

pattern CreateContainerServiceRegistryLogin :: OperationType
pattern CreateContainerServiceRegistryLogin = OperationType' "CreateContainerServiceRegistryLogin"

pattern CreateDisk :: OperationType
pattern CreateDisk = OperationType' "CreateDisk"

pattern CreateDiskFromSnapshot :: OperationType
pattern CreateDiskFromSnapshot = OperationType' "CreateDiskFromSnapshot"

pattern CreateDiskSnapshot :: OperationType
pattern CreateDiskSnapshot = OperationType' "CreateDiskSnapshot"

pattern CreateDistribution :: OperationType
pattern CreateDistribution = OperationType' "CreateDistribution"

pattern CreateDomain :: OperationType
pattern CreateDomain = OperationType' "CreateDomain"

pattern CreateInstance :: OperationType
pattern CreateInstance = OperationType' "CreateInstance"

pattern CreateInstanceSnapshot :: OperationType
pattern CreateInstanceSnapshot = OperationType' "CreateInstanceSnapshot"

pattern CreateInstancesFromSnapshot :: OperationType
pattern CreateInstancesFromSnapshot = OperationType' "CreateInstancesFromSnapshot"

pattern CreateLoadBalancer :: OperationType
pattern CreateLoadBalancer = OperationType' "CreateLoadBalancer"

pattern CreateLoadBalancerTLSCertificate :: OperationType
pattern CreateLoadBalancerTLSCertificate = OperationType' "CreateLoadBalancerTlsCertificate"

pattern CreateRelationalDatabase :: OperationType
pattern CreateRelationalDatabase = OperationType' "CreateRelationalDatabase"

pattern CreateRelationalDatabaseFromSnapshot :: OperationType
pattern CreateRelationalDatabaseFromSnapshot = OperationType' "CreateRelationalDatabaseFromSnapshot"

pattern CreateRelationalDatabaseSnapshot :: OperationType
pattern CreateRelationalDatabaseSnapshot = OperationType' "CreateRelationalDatabaseSnapshot"

pattern DeleteAlarm :: OperationType
pattern DeleteAlarm = OperationType' "DeleteAlarm"

pattern DeleteCertificate :: OperationType
pattern DeleteCertificate = OperationType' "DeleteCertificate"

pattern DeleteContactMethod :: OperationType
pattern DeleteContactMethod = OperationType' "DeleteContactMethod"

pattern DeleteContainerImage :: OperationType
pattern DeleteContainerImage = OperationType' "DeleteContainerImage"

pattern DeleteContainerService :: OperationType
pattern DeleteContainerService = OperationType' "DeleteContainerService"

pattern DeleteDisk :: OperationType
pattern DeleteDisk = OperationType' "DeleteDisk"

pattern DeleteDiskSnapshot :: OperationType
pattern DeleteDiskSnapshot = OperationType' "DeleteDiskSnapshot"

pattern DeleteDistribution :: OperationType
pattern DeleteDistribution = OperationType' "DeleteDistribution"

pattern DeleteDomain :: OperationType
pattern DeleteDomain = OperationType' "DeleteDomain"

pattern DeleteDomainEntry :: OperationType
pattern DeleteDomainEntry = OperationType' "DeleteDomainEntry"

pattern DeleteInstance :: OperationType
pattern DeleteInstance = OperationType' "DeleteInstance"

pattern DeleteInstanceSnapshot :: OperationType
pattern DeleteInstanceSnapshot = OperationType' "DeleteInstanceSnapshot"

pattern DeleteKnownHostKeys :: OperationType
pattern DeleteKnownHostKeys = OperationType' "DeleteKnownHostKeys"

pattern DeleteLoadBalancer :: OperationType
pattern DeleteLoadBalancer = OperationType' "DeleteLoadBalancer"

pattern DeleteLoadBalancerTLSCertificate :: OperationType
pattern DeleteLoadBalancerTLSCertificate = OperationType' "DeleteLoadBalancerTlsCertificate"

pattern DeleteRelationalDatabase :: OperationType
pattern DeleteRelationalDatabase = OperationType' "DeleteRelationalDatabase"

pattern DeleteRelationalDatabaseSnapshot :: OperationType
pattern DeleteRelationalDatabaseSnapshot = OperationType' "DeleteRelationalDatabaseSnapshot"

pattern DetachCertificateFromDistribution :: OperationType
pattern DetachCertificateFromDistribution = OperationType' "DetachCertificateFromDistribution"

pattern DetachDisk :: OperationType
pattern DetachDisk = OperationType' "DetachDisk"

pattern DetachInstancesFromLoadBalancer :: OperationType
pattern DetachInstancesFromLoadBalancer = OperationType' "DetachInstancesFromLoadBalancer"

pattern DetachStaticIP :: OperationType
pattern DetachStaticIP = OperationType' "DetachStaticIp"

pattern DisableAddOn :: OperationType
pattern DisableAddOn = OperationType' "DisableAddOn"

pattern EnableAddOn :: OperationType
pattern EnableAddOn = OperationType' "EnableAddOn"

pattern GetAlarms :: OperationType
pattern GetAlarms = OperationType' "GetAlarms"

pattern GetContactMethods :: OperationType
pattern GetContactMethods = OperationType' "GetContactMethods"

pattern OpenInstancePublicPorts :: OperationType
pattern OpenInstancePublicPorts = OperationType' "OpenInstancePublicPorts"

pattern PutAlarm :: OperationType
pattern PutAlarm = OperationType' "PutAlarm"

pattern PutInstancePublicPorts :: OperationType
pattern PutInstancePublicPorts = OperationType' "PutInstancePublicPorts"

pattern RebootInstance :: OperationType
pattern RebootInstance = OperationType' "RebootInstance"

pattern RebootRelationalDatabase :: OperationType
pattern RebootRelationalDatabase = OperationType' "RebootRelationalDatabase"

pattern RegisterContainerImage :: OperationType
pattern RegisterContainerImage = OperationType' "RegisterContainerImage"

pattern ReleaseStaticIP :: OperationType
pattern ReleaseStaticIP = OperationType' "ReleaseStaticIp"

pattern ResetDistributionCache :: OperationType
pattern ResetDistributionCache = OperationType' "ResetDistributionCache"

pattern SendContactMethodVerification :: OperationType
pattern SendContactMethodVerification = OperationType' "SendContactMethodVerification"

pattern StartInstance :: OperationType
pattern StartInstance = OperationType' "StartInstance"

pattern StartRelationalDatabase :: OperationType
pattern StartRelationalDatabase = OperationType' "StartRelationalDatabase"

pattern StopInstance :: OperationType
pattern StopInstance = OperationType' "StopInstance"

pattern StopRelationalDatabase :: OperationType
pattern StopRelationalDatabase = OperationType' "StopRelationalDatabase"

pattern TestAlarm :: OperationType
pattern TestAlarm = OperationType' "TestAlarm"

pattern UpdateContainerService :: OperationType
pattern UpdateContainerService = OperationType' "UpdateContainerService"

pattern UpdateDistribution :: OperationType
pattern UpdateDistribution = OperationType' "UpdateDistribution"

pattern UpdateDistributionBundle :: OperationType
pattern UpdateDistributionBundle = OperationType' "UpdateDistributionBundle"

pattern UpdateDomainEntry :: OperationType
pattern UpdateDomainEntry = OperationType' "UpdateDomainEntry"

pattern UpdateLoadBalancerAttribute :: OperationType
pattern UpdateLoadBalancerAttribute = OperationType' "UpdateLoadBalancerAttribute"

pattern UpdateRelationalDatabase :: OperationType
pattern UpdateRelationalDatabase = OperationType' "UpdateRelationalDatabase"

pattern UpdateRelationalDatabaseParameters :: OperationType
pattern UpdateRelationalDatabaseParameters = OperationType' "UpdateRelationalDatabaseParameters"

{-# COMPLETE
  AllocateStaticIP,
  AttachCertificateToDistribution,
  AttachDisk,
  AttachInstancesToLoadBalancer,
  AttachLoadBalancerTLSCertificate,
  AttachStaticIP,
  CloseInstancePublicPorts,
  CreateCertificate,
  CreateContactMethod,
  CreateContainerService,
  CreateContainerServiceDeployment,
  CreateContainerServiceRegistryLogin,
  CreateDisk,
  CreateDiskFromSnapshot,
  CreateDiskSnapshot,
  CreateDistribution,
  CreateDomain,
  CreateInstance,
  CreateInstanceSnapshot,
  CreateInstancesFromSnapshot,
  CreateLoadBalancer,
  CreateLoadBalancerTLSCertificate,
  CreateRelationalDatabase,
  CreateRelationalDatabaseFromSnapshot,
  CreateRelationalDatabaseSnapshot,
  DeleteAlarm,
  DeleteCertificate,
  DeleteContactMethod,
  DeleteContainerImage,
  DeleteContainerService,
  DeleteDisk,
  DeleteDiskSnapshot,
  DeleteDistribution,
  DeleteDomain,
  DeleteDomainEntry,
  DeleteInstance,
  DeleteInstanceSnapshot,
  DeleteKnownHostKeys,
  DeleteLoadBalancer,
  DeleteLoadBalancerTLSCertificate,
  DeleteRelationalDatabase,
  DeleteRelationalDatabaseSnapshot,
  DetachCertificateFromDistribution,
  DetachDisk,
  DetachInstancesFromLoadBalancer,
  DetachStaticIP,
  DisableAddOn,
  EnableAddOn,
  GetAlarms,
  GetContactMethods,
  OpenInstancePublicPorts,
  PutAlarm,
  PutInstancePublicPorts,
  RebootInstance,
  RebootRelationalDatabase,
  RegisterContainerImage,
  ReleaseStaticIP,
  ResetDistributionCache,
  SendContactMethodVerification,
  StartInstance,
  StartRelationalDatabase,
  StopInstance,
  StopRelationalDatabase,
  TestAlarm,
  UpdateContainerService,
  UpdateDistribution,
  UpdateDistributionBundle,
  UpdateDomainEntry,
  UpdateLoadBalancerAttribute,
  UpdateRelationalDatabase,
  UpdateRelationalDatabaseParameters,
  OperationType'
  #-}
