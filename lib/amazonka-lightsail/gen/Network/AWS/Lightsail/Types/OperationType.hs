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
        DeleteKnownHostKeys,
        DeleteInstance,
        CreateInstance,
        StopInstance,
        StartInstance,
        RebootInstance,
        OpenInstancePublicPorts,
        PutInstancePublicPorts,
        CloseInstancePublicPorts,
        AllocateStaticIP,
        ReleaseStaticIP,
        AttachStaticIP,
        DetachStaticIP,
        UpdateDomainEntry,
        DeleteDomainEntry,
        CreateDomain,
        DeleteDomain,
        CreateInstanceSnapshot,
        DeleteInstanceSnapshot,
        CreateInstancesFromSnapshot,
        CreateLoadBalancer,
        DeleteLoadBalancer,
        AttachInstancesToLoadBalancer,
        DetachInstancesFromLoadBalancer,
        UpdateLoadBalancerAttribute,
        CreateLoadBalancerTLSCertificate,
        DeleteLoadBalancerTLSCertificate,
        AttachLoadBalancerTLSCertificate,
        CreateDisk,
        DeleteDisk,
        AttachDisk,
        DetachDisk,
        CreateDiskSnapshot,
        DeleteDiskSnapshot,
        CreateDiskFromSnapshot,
        CreateRelationalDatabase,
        UpdateRelationalDatabase,
        DeleteRelationalDatabase,
        CreateRelationalDatabaseFromSnapshot,
        CreateRelationalDatabaseSnapshot,
        DeleteRelationalDatabaseSnapshot,
        UpdateRelationalDatabaseParameters,
        StartRelationalDatabase,
        RebootRelationalDatabase,
        StopRelationalDatabase,
        EnableAddOn,
        DisableAddOn,
        PutAlarm,
        GetAlarms,
        DeleteAlarm,
        TestAlarm,
        CreateContactMethod,
        GetContactMethods,
        SendContactMethodVerification,
        DeleteContactMethod,
        CreateDistribution,
        UpdateDistribution,
        DeleteDistribution,
        ResetDistributionCache,
        AttachCertificateToDistribution,
        DetachCertificateFromDistribution,
        UpdateDistributionBundle,
        CreateCertificate,
        DeleteCertificate,
        CreateContainerService,
        UpdateContainerService,
        DeleteContainerService,
        CreateContainerServiceDeployment,
        CreateContainerServiceRegistryLogin,
        RegisterContainerImage,
        DeleteContainerImage
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

pattern DeleteKnownHostKeys :: OperationType
pattern DeleteKnownHostKeys = OperationType' "DeleteKnownHostKeys"

pattern DeleteInstance :: OperationType
pattern DeleteInstance = OperationType' "DeleteInstance"

pattern CreateInstance :: OperationType
pattern CreateInstance = OperationType' "CreateInstance"

pattern StopInstance :: OperationType
pattern StopInstance = OperationType' "StopInstance"

pattern StartInstance :: OperationType
pattern StartInstance = OperationType' "StartInstance"

pattern RebootInstance :: OperationType
pattern RebootInstance = OperationType' "RebootInstance"

pattern OpenInstancePublicPorts :: OperationType
pattern OpenInstancePublicPorts = OperationType' "OpenInstancePublicPorts"

pattern PutInstancePublicPorts :: OperationType
pattern PutInstancePublicPorts = OperationType' "PutInstancePublicPorts"

pattern CloseInstancePublicPorts :: OperationType
pattern CloseInstancePublicPorts = OperationType' "CloseInstancePublicPorts"

pattern AllocateStaticIP :: OperationType
pattern AllocateStaticIP = OperationType' "AllocateStaticIp"

pattern ReleaseStaticIP :: OperationType
pattern ReleaseStaticIP = OperationType' "ReleaseStaticIp"

pattern AttachStaticIP :: OperationType
pattern AttachStaticIP = OperationType' "AttachStaticIp"

pattern DetachStaticIP :: OperationType
pattern DetachStaticIP = OperationType' "DetachStaticIp"

pattern UpdateDomainEntry :: OperationType
pattern UpdateDomainEntry = OperationType' "UpdateDomainEntry"

pattern DeleteDomainEntry :: OperationType
pattern DeleteDomainEntry = OperationType' "DeleteDomainEntry"

pattern CreateDomain :: OperationType
pattern CreateDomain = OperationType' "CreateDomain"

pattern DeleteDomain :: OperationType
pattern DeleteDomain = OperationType' "DeleteDomain"

pattern CreateInstanceSnapshot :: OperationType
pattern CreateInstanceSnapshot = OperationType' "CreateInstanceSnapshot"

pattern DeleteInstanceSnapshot :: OperationType
pattern DeleteInstanceSnapshot = OperationType' "DeleteInstanceSnapshot"

pattern CreateInstancesFromSnapshot :: OperationType
pattern CreateInstancesFromSnapshot = OperationType' "CreateInstancesFromSnapshot"

pattern CreateLoadBalancer :: OperationType
pattern CreateLoadBalancer = OperationType' "CreateLoadBalancer"

pattern DeleteLoadBalancer :: OperationType
pattern DeleteLoadBalancer = OperationType' "DeleteLoadBalancer"

pattern AttachInstancesToLoadBalancer :: OperationType
pattern AttachInstancesToLoadBalancer = OperationType' "AttachInstancesToLoadBalancer"

pattern DetachInstancesFromLoadBalancer :: OperationType
pattern DetachInstancesFromLoadBalancer = OperationType' "DetachInstancesFromLoadBalancer"

pattern UpdateLoadBalancerAttribute :: OperationType
pattern UpdateLoadBalancerAttribute = OperationType' "UpdateLoadBalancerAttribute"

pattern CreateLoadBalancerTLSCertificate :: OperationType
pattern CreateLoadBalancerTLSCertificate = OperationType' "CreateLoadBalancerTlsCertificate"

pattern DeleteLoadBalancerTLSCertificate :: OperationType
pattern DeleteLoadBalancerTLSCertificate = OperationType' "DeleteLoadBalancerTlsCertificate"

pattern AttachLoadBalancerTLSCertificate :: OperationType
pattern AttachLoadBalancerTLSCertificate = OperationType' "AttachLoadBalancerTlsCertificate"

pattern CreateDisk :: OperationType
pattern CreateDisk = OperationType' "CreateDisk"

pattern DeleteDisk :: OperationType
pattern DeleteDisk = OperationType' "DeleteDisk"

pattern AttachDisk :: OperationType
pattern AttachDisk = OperationType' "AttachDisk"

pattern DetachDisk :: OperationType
pattern DetachDisk = OperationType' "DetachDisk"

pattern CreateDiskSnapshot :: OperationType
pattern CreateDiskSnapshot = OperationType' "CreateDiskSnapshot"

pattern DeleteDiskSnapshot :: OperationType
pattern DeleteDiskSnapshot = OperationType' "DeleteDiskSnapshot"

pattern CreateDiskFromSnapshot :: OperationType
pattern CreateDiskFromSnapshot = OperationType' "CreateDiskFromSnapshot"

pattern CreateRelationalDatabase :: OperationType
pattern CreateRelationalDatabase = OperationType' "CreateRelationalDatabase"

pattern UpdateRelationalDatabase :: OperationType
pattern UpdateRelationalDatabase = OperationType' "UpdateRelationalDatabase"

pattern DeleteRelationalDatabase :: OperationType
pattern DeleteRelationalDatabase = OperationType' "DeleteRelationalDatabase"

pattern CreateRelationalDatabaseFromSnapshot :: OperationType
pattern CreateRelationalDatabaseFromSnapshot = OperationType' "CreateRelationalDatabaseFromSnapshot"

pattern CreateRelationalDatabaseSnapshot :: OperationType
pattern CreateRelationalDatabaseSnapshot = OperationType' "CreateRelationalDatabaseSnapshot"

pattern DeleteRelationalDatabaseSnapshot :: OperationType
pattern DeleteRelationalDatabaseSnapshot = OperationType' "DeleteRelationalDatabaseSnapshot"

pattern UpdateRelationalDatabaseParameters :: OperationType
pattern UpdateRelationalDatabaseParameters = OperationType' "UpdateRelationalDatabaseParameters"

pattern StartRelationalDatabase :: OperationType
pattern StartRelationalDatabase = OperationType' "StartRelationalDatabase"

pattern RebootRelationalDatabase :: OperationType
pattern RebootRelationalDatabase = OperationType' "RebootRelationalDatabase"

pattern StopRelationalDatabase :: OperationType
pattern StopRelationalDatabase = OperationType' "StopRelationalDatabase"

pattern EnableAddOn :: OperationType
pattern EnableAddOn = OperationType' "EnableAddOn"

pattern DisableAddOn :: OperationType
pattern DisableAddOn = OperationType' "DisableAddOn"

pattern PutAlarm :: OperationType
pattern PutAlarm = OperationType' "PutAlarm"

pattern GetAlarms :: OperationType
pattern GetAlarms = OperationType' "GetAlarms"

pattern DeleteAlarm :: OperationType
pattern DeleteAlarm = OperationType' "DeleteAlarm"

pattern TestAlarm :: OperationType
pattern TestAlarm = OperationType' "TestAlarm"

pattern CreateContactMethod :: OperationType
pattern CreateContactMethod = OperationType' "CreateContactMethod"

pattern GetContactMethods :: OperationType
pattern GetContactMethods = OperationType' "GetContactMethods"

pattern SendContactMethodVerification :: OperationType
pattern SendContactMethodVerification = OperationType' "SendContactMethodVerification"

pattern DeleteContactMethod :: OperationType
pattern DeleteContactMethod = OperationType' "DeleteContactMethod"

pattern CreateDistribution :: OperationType
pattern CreateDistribution = OperationType' "CreateDistribution"

pattern UpdateDistribution :: OperationType
pattern UpdateDistribution = OperationType' "UpdateDistribution"

pattern DeleteDistribution :: OperationType
pattern DeleteDistribution = OperationType' "DeleteDistribution"

pattern ResetDistributionCache :: OperationType
pattern ResetDistributionCache = OperationType' "ResetDistributionCache"

pattern AttachCertificateToDistribution :: OperationType
pattern AttachCertificateToDistribution = OperationType' "AttachCertificateToDistribution"

pattern DetachCertificateFromDistribution :: OperationType
pattern DetachCertificateFromDistribution = OperationType' "DetachCertificateFromDistribution"

pattern UpdateDistributionBundle :: OperationType
pattern UpdateDistributionBundle = OperationType' "UpdateDistributionBundle"

pattern CreateCertificate :: OperationType
pattern CreateCertificate = OperationType' "CreateCertificate"

pattern DeleteCertificate :: OperationType
pattern DeleteCertificate = OperationType' "DeleteCertificate"

pattern CreateContainerService :: OperationType
pattern CreateContainerService = OperationType' "CreateContainerService"

pattern UpdateContainerService :: OperationType
pattern UpdateContainerService = OperationType' "UpdateContainerService"

pattern DeleteContainerService :: OperationType
pattern DeleteContainerService = OperationType' "DeleteContainerService"

pattern CreateContainerServiceDeployment :: OperationType
pattern CreateContainerServiceDeployment = OperationType' "CreateContainerServiceDeployment"

pattern CreateContainerServiceRegistryLogin :: OperationType
pattern CreateContainerServiceRegistryLogin = OperationType' "CreateContainerServiceRegistryLogin"

pattern RegisterContainerImage :: OperationType
pattern RegisterContainerImage = OperationType' "RegisterContainerImage"

pattern DeleteContainerImage :: OperationType
pattern DeleteContainerImage = OperationType' "DeleteContainerImage"

{-# COMPLETE
  DeleteKnownHostKeys,
  DeleteInstance,
  CreateInstance,
  StopInstance,
  StartInstance,
  RebootInstance,
  OpenInstancePublicPorts,
  PutInstancePublicPorts,
  CloseInstancePublicPorts,
  AllocateStaticIP,
  ReleaseStaticIP,
  AttachStaticIP,
  DetachStaticIP,
  UpdateDomainEntry,
  DeleteDomainEntry,
  CreateDomain,
  DeleteDomain,
  CreateInstanceSnapshot,
  DeleteInstanceSnapshot,
  CreateInstancesFromSnapshot,
  CreateLoadBalancer,
  DeleteLoadBalancer,
  AttachInstancesToLoadBalancer,
  DetachInstancesFromLoadBalancer,
  UpdateLoadBalancerAttribute,
  CreateLoadBalancerTLSCertificate,
  DeleteLoadBalancerTLSCertificate,
  AttachLoadBalancerTLSCertificate,
  CreateDisk,
  DeleteDisk,
  AttachDisk,
  DetachDisk,
  CreateDiskSnapshot,
  DeleteDiskSnapshot,
  CreateDiskFromSnapshot,
  CreateRelationalDatabase,
  UpdateRelationalDatabase,
  DeleteRelationalDatabase,
  CreateRelationalDatabaseFromSnapshot,
  CreateRelationalDatabaseSnapshot,
  DeleteRelationalDatabaseSnapshot,
  UpdateRelationalDatabaseParameters,
  StartRelationalDatabase,
  RebootRelationalDatabase,
  StopRelationalDatabase,
  EnableAddOn,
  DisableAddOn,
  PutAlarm,
  GetAlarms,
  DeleteAlarm,
  TestAlarm,
  CreateContactMethod,
  GetContactMethods,
  SendContactMethodVerification,
  DeleteContactMethod,
  CreateDistribution,
  UpdateDistribution,
  DeleteDistribution,
  ResetDistributionCache,
  AttachCertificateToDistribution,
  DetachCertificateFromDistribution,
  UpdateDistributionBundle,
  CreateCertificate,
  DeleteCertificate,
  CreateContainerService,
  UpdateContainerService,
  DeleteContainerService,
  CreateContainerServiceDeployment,
  CreateContainerServiceRegistryLogin,
  RegisterContainerImage,
  DeleteContainerImage,
  OperationType'
  #-}
