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
        OperationTypeDeleteKnownHostKeys,
        OperationTypeDeleteInstance,
        OperationTypeCreateInstance,
        OperationTypeStopInstance,
        OperationTypeStartInstance,
        OperationTypeRebootInstance,
        OperationTypeOpenInstancePublicPorts,
        OperationTypePutInstancePublicPorts,
        OperationTypeCloseInstancePublicPorts,
        OperationTypeAllocateStaticIp,
        OperationTypeReleaseStaticIp,
        OperationTypeAttachStaticIp,
        OperationTypeDetachStaticIp,
        OperationTypeUpdateDomainEntry,
        OperationTypeDeleteDomainEntry,
        OperationTypeCreateDomain,
        OperationTypeDeleteDomain,
        OperationTypeCreateInstanceSnapshot,
        OperationTypeDeleteInstanceSnapshot,
        OperationTypeCreateInstancesFromSnapshot,
        OperationTypeCreateLoadBalancer,
        OperationTypeDeleteLoadBalancer,
        OperationTypeAttachInstancesToLoadBalancer,
        OperationTypeDetachInstancesFromLoadBalancer,
        OperationTypeUpdateLoadBalancerAttribute,
        OperationTypeCreateLoadBalancerTlsCertificate,
        OperationTypeDeleteLoadBalancerTlsCertificate,
        OperationTypeAttachLoadBalancerTlsCertificate,
        OperationTypeCreateDisk,
        OperationTypeDeleteDisk,
        OperationTypeAttachDisk,
        OperationTypeDetachDisk,
        OperationTypeCreateDiskSnapshot,
        OperationTypeDeleteDiskSnapshot,
        OperationTypeCreateDiskFromSnapshot,
        OperationTypeCreateRelationalDatabase,
        OperationTypeUpdateRelationalDatabase,
        OperationTypeDeleteRelationalDatabase,
        OperationTypeCreateRelationalDatabaseFromSnapshot,
        OperationTypeCreateRelationalDatabaseSnapshot,
        OperationTypeDeleteRelationalDatabaseSnapshot,
        OperationTypeUpdateRelationalDatabaseParameters,
        OperationTypeStartRelationalDatabase,
        OperationTypeRebootRelationalDatabase,
        OperationTypeStopRelationalDatabase,
        OperationTypeEnableAddOn,
        OperationTypeDisableAddOn,
        OperationTypePutAlarm,
        OperationTypeGetAlarms,
        OperationTypeDeleteAlarm,
        OperationTypeTestAlarm,
        OperationTypeCreateContactMethod,
        OperationTypeGetContactMethods,
        OperationTypeSendContactMethodVerification,
        OperationTypeDeleteContactMethod,
        OperationTypeCreateDistribution,
        OperationTypeUpdateDistribution,
        OperationTypeDeleteDistribution,
        OperationTypeResetDistributionCache,
        OperationTypeAttachCertificateToDistribution,
        OperationTypeDetachCertificateFromDistribution,
        OperationTypeUpdateDistributionBundle,
        OperationTypeCreateCertificate,
        OperationTypeDeleteCertificate,
        OperationTypeCreateContainerService,
        OperationTypeUpdateContainerService,
        OperationTypeDeleteContainerService,
        OperationTypeCreateContainerServiceDeployment,
        OperationTypeCreateContainerServiceRegistryLogin,
        OperationTypeRegisterContainerImage,
        OperationTypeDeleteContainerImage,
        fromOperationType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OperationType = OperationType'
  { fromOperationType ::
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

pattern OperationTypeDeleteKnownHostKeys :: OperationType
pattern OperationTypeDeleteKnownHostKeys = OperationType' "DeleteKnownHostKeys"

pattern OperationTypeDeleteInstance :: OperationType
pattern OperationTypeDeleteInstance = OperationType' "DeleteInstance"

pattern OperationTypeCreateInstance :: OperationType
pattern OperationTypeCreateInstance = OperationType' "CreateInstance"

pattern OperationTypeStopInstance :: OperationType
pattern OperationTypeStopInstance = OperationType' "StopInstance"

pattern OperationTypeStartInstance :: OperationType
pattern OperationTypeStartInstance = OperationType' "StartInstance"

pattern OperationTypeRebootInstance :: OperationType
pattern OperationTypeRebootInstance = OperationType' "RebootInstance"

pattern OperationTypeOpenInstancePublicPorts :: OperationType
pattern OperationTypeOpenInstancePublicPorts = OperationType' "OpenInstancePublicPorts"

pattern OperationTypePutInstancePublicPorts :: OperationType
pattern OperationTypePutInstancePublicPorts = OperationType' "PutInstancePublicPorts"

pattern OperationTypeCloseInstancePublicPorts :: OperationType
pattern OperationTypeCloseInstancePublicPorts = OperationType' "CloseInstancePublicPorts"

pattern OperationTypeAllocateStaticIp :: OperationType
pattern OperationTypeAllocateStaticIp = OperationType' "AllocateStaticIp"

pattern OperationTypeReleaseStaticIp :: OperationType
pattern OperationTypeReleaseStaticIp = OperationType' "ReleaseStaticIp"

pattern OperationTypeAttachStaticIp :: OperationType
pattern OperationTypeAttachStaticIp = OperationType' "AttachStaticIp"

pattern OperationTypeDetachStaticIp :: OperationType
pattern OperationTypeDetachStaticIp = OperationType' "DetachStaticIp"

pattern OperationTypeUpdateDomainEntry :: OperationType
pattern OperationTypeUpdateDomainEntry = OperationType' "UpdateDomainEntry"

pattern OperationTypeDeleteDomainEntry :: OperationType
pattern OperationTypeDeleteDomainEntry = OperationType' "DeleteDomainEntry"

pattern OperationTypeCreateDomain :: OperationType
pattern OperationTypeCreateDomain = OperationType' "CreateDomain"

pattern OperationTypeDeleteDomain :: OperationType
pattern OperationTypeDeleteDomain = OperationType' "DeleteDomain"

pattern OperationTypeCreateInstanceSnapshot :: OperationType
pattern OperationTypeCreateInstanceSnapshot = OperationType' "CreateInstanceSnapshot"

pattern OperationTypeDeleteInstanceSnapshot :: OperationType
pattern OperationTypeDeleteInstanceSnapshot = OperationType' "DeleteInstanceSnapshot"

pattern OperationTypeCreateInstancesFromSnapshot :: OperationType
pattern OperationTypeCreateInstancesFromSnapshot = OperationType' "CreateInstancesFromSnapshot"

pattern OperationTypeCreateLoadBalancer :: OperationType
pattern OperationTypeCreateLoadBalancer = OperationType' "CreateLoadBalancer"

pattern OperationTypeDeleteLoadBalancer :: OperationType
pattern OperationTypeDeleteLoadBalancer = OperationType' "DeleteLoadBalancer"

pattern OperationTypeAttachInstancesToLoadBalancer :: OperationType
pattern OperationTypeAttachInstancesToLoadBalancer = OperationType' "AttachInstancesToLoadBalancer"

pattern OperationTypeDetachInstancesFromLoadBalancer :: OperationType
pattern OperationTypeDetachInstancesFromLoadBalancer = OperationType' "DetachInstancesFromLoadBalancer"

pattern OperationTypeUpdateLoadBalancerAttribute :: OperationType
pattern OperationTypeUpdateLoadBalancerAttribute = OperationType' "UpdateLoadBalancerAttribute"

pattern OperationTypeCreateLoadBalancerTlsCertificate :: OperationType
pattern OperationTypeCreateLoadBalancerTlsCertificate = OperationType' "CreateLoadBalancerTlsCertificate"

pattern OperationTypeDeleteLoadBalancerTlsCertificate :: OperationType
pattern OperationTypeDeleteLoadBalancerTlsCertificate = OperationType' "DeleteLoadBalancerTlsCertificate"

pattern OperationTypeAttachLoadBalancerTlsCertificate :: OperationType
pattern OperationTypeAttachLoadBalancerTlsCertificate = OperationType' "AttachLoadBalancerTlsCertificate"

pattern OperationTypeCreateDisk :: OperationType
pattern OperationTypeCreateDisk = OperationType' "CreateDisk"

pattern OperationTypeDeleteDisk :: OperationType
pattern OperationTypeDeleteDisk = OperationType' "DeleteDisk"

pattern OperationTypeAttachDisk :: OperationType
pattern OperationTypeAttachDisk = OperationType' "AttachDisk"

pattern OperationTypeDetachDisk :: OperationType
pattern OperationTypeDetachDisk = OperationType' "DetachDisk"

pattern OperationTypeCreateDiskSnapshot :: OperationType
pattern OperationTypeCreateDiskSnapshot = OperationType' "CreateDiskSnapshot"

pattern OperationTypeDeleteDiskSnapshot :: OperationType
pattern OperationTypeDeleteDiskSnapshot = OperationType' "DeleteDiskSnapshot"

pattern OperationTypeCreateDiskFromSnapshot :: OperationType
pattern OperationTypeCreateDiskFromSnapshot = OperationType' "CreateDiskFromSnapshot"

pattern OperationTypeCreateRelationalDatabase :: OperationType
pattern OperationTypeCreateRelationalDatabase = OperationType' "CreateRelationalDatabase"

pattern OperationTypeUpdateRelationalDatabase :: OperationType
pattern OperationTypeUpdateRelationalDatabase = OperationType' "UpdateRelationalDatabase"

pattern OperationTypeDeleteRelationalDatabase :: OperationType
pattern OperationTypeDeleteRelationalDatabase = OperationType' "DeleteRelationalDatabase"

pattern OperationTypeCreateRelationalDatabaseFromSnapshot :: OperationType
pattern OperationTypeCreateRelationalDatabaseFromSnapshot = OperationType' "CreateRelationalDatabaseFromSnapshot"

pattern OperationTypeCreateRelationalDatabaseSnapshot :: OperationType
pattern OperationTypeCreateRelationalDatabaseSnapshot = OperationType' "CreateRelationalDatabaseSnapshot"

pattern OperationTypeDeleteRelationalDatabaseSnapshot :: OperationType
pattern OperationTypeDeleteRelationalDatabaseSnapshot = OperationType' "DeleteRelationalDatabaseSnapshot"

pattern OperationTypeUpdateRelationalDatabaseParameters :: OperationType
pattern OperationTypeUpdateRelationalDatabaseParameters = OperationType' "UpdateRelationalDatabaseParameters"

pattern OperationTypeStartRelationalDatabase :: OperationType
pattern OperationTypeStartRelationalDatabase = OperationType' "StartRelationalDatabase"

pattern OperationTypeRebootRelationalDatabase :: OperationType
pattern OperationTypeRebootRelationalDatabase = OperationType' "RebootRelationalDatabase"

pattern OperationTypeStopRelationalDatabase :: OperationType
pattern OperationTypeStopRelationalDatabase = OperationType' "StopRelationalDatabase"

pattern OperationTypeEnableAddOn :: OperationType
pattern OperationTypeEnableAddOn = OperationType' "EnableAddOn"

pattern OperationTypeDisableAddOn :: OperationType
pattern OperationTypeDisableAddOn = OperationType' "DisableAddOn"

pattern OperationTypePutAlarm :: OperationType
pattern OperationTypePutAlarm = OperationType' "PutAlarm"

pattern OperationTypeGetAlarms :: OperationType
pattern OperationTypeGetAlarms = OperationType' "GetAlarms"

pattern OperationTypeDeleteAlarm :: OperationType
pattern OperationTypeDeleteAlarm = OperationType' "DeleteAlarm"

pattern OperationTypeTestAlarm :: OperationType
pattern OperationTypeTestAlarm = OperationType' "TestAlarm"

pattern OperationTypeCreateContactMethod :: OperationType
pattern OperationTypeCreateContactMethod = OperationType' "CreateContactMethod"

pattern OperationTypeGetContactMethods :: OperationType
pattern OperationTypeGetContactMethods = OperationType' "GetContactMethods"

pattern OperationTypeSendContactMethodVerification :: OperationType
pattern OperationTypeSendContactMethodVerification = OperationType' "SendContactMethodVerification"

pattern OperationTypeDeleteContactMethod :: OperationType
pattern OperationTypeDeleteContactMethod = OperationType' "DeleteContactMethod"

pattern OperationTypeCreateDistribution :: OperationType
pattern OperationTypeCreateDistribution = OperationType' "CreateDistribution"

pattern OperationTypeUpdateDistribution :: OperationType
pattern OperationTypeUpdateDistribution = OperationType' "UpdateDistribution"

pattern OperationTypeDeleteDistribution :: OperationType
pattern OperationTypeDeleteDistribution = OperationType' "DeleteDistribution"

pattern OperationTypeResetDistributionCache :: OperationType
pattern OperationTypeResetDistributionCache = OperationType' "ResetDistributionCache"

pattern OperationTypeAttachCertificateToDistribution :: OperationType
pattern OperationTypeAttachCertificateToDistribution = OperationType' "AttachCertificateToDistribution"

pattern OperationTypeDetachCertificateFromDistribution :: OperationType
pattern OperationTypeDetachCertificateFromDistribution = OperationType' "DetachCertificateFromDistribution"

pattern OperationTypeUpdateDistributionBundle :: OperationType
pattern OperationTypeUpdateDistributionBundle = OperationType' "UpdateDistributionBundle"

pattern OperationTypeCreateCertificate :: OperationType
pattern OperationTypeCreateCertificate = OperationType' "CreateCertificate"

pattern OperationTypeDeleteCertificate :: OperationType
pattern OperationTypeDeleteCertificate = OperationType' "DeleteCertificate"

pattern OperationTypeCreateContainerService :: OperationType
pattern OperationTypeCreateContainerService = OperationType' "CreateContainerService"

pattern OperationTypeUpdateContainerService :: OperationType
pattern OperationTypeUpdateContainerService = OperationType' "UpdateContainerService"

pattern OperationTypeDeleteContainerService :: OperationType
pattern OperationTypeDeleteContainerService = OperationType' "DeleteContainerService"

pattern OperationTypeCreateContainerServiceDeployment :: OperationType
pattern OperationTypeCreateContainerServiceDeployment = OperationType' "CreateContainerServiceDeployment"

pattern OperationTypeCreateContainerServiceRegistryLogin :: OperationType
pattern OperationTypeCreateContainerServiceRegistryLogin = OperationType' "CreateContainerServiceRegistryLogin"

pattern OperationTypeRegisterContainerImage :: OperationType
pattern OperationTypeRegisterContainerImage = OperationType' "RegisterContainerImage"

pattern OperationTypeDeleteContainerImage :: OperationType
pattern OperationTypeDeleteContainerImage = OperationType' "DeleteContainerImage"

{-# COMPLETE
  OperationTypeDeleteKnownHostKeys,
  OperationTypeDeleteInstance,
  OperationTypeCreateInstance,
  OperationTypeStopInstance,
  OperationTypeStartInstance,
  OperationTypeRebootInstance,
  OperationTypeOpenInstancePublicPorts,
  OperationTypePutInstancePublicPorts,
  OperationTypeCloseInstancePublicPorts,
  OperationTypeAllocateStaticIp,
  OperationTypeReleaseStaticIp,
  OperationTypeAttachStaticIp,
  OperationTypeDetachStaticIp,
  OperationTypeUpdateDomainEntry,
  OperationTypeDeleteDomainEntry,
  OperationTypeCreateDomain,
  OperationTypeDeleteDomain,
  OperationTypeCreateInstanceSnapshot,
  OperationTypeDeleteInstanceSnapshot,
  OperationTypeCreateInstancesFromSnapshot,
  OperationTypeCreateLoadBalancer,
  OperationTypeDeleteLoadBalancer,
  OperationTypeAttachInstancesToLoadBalancer,
  OperationTypeDetachInstancesFromLoadBalancer,
  OperationTypeUpdateLoadBalancerAttribute,
  OperationTypeCreateLoadBalancerTlsCertificate,
  OperationTypeDeleteLoadBalancerTlsCertificate,
  OperationTypeAttachLoadBalancerTlsCertificate,
  OperationTypeCreateDisk,
  OperationTypeDeleteDisk,
  OperationTypeAttachDisk,
  OperationTypeDetachDisk,
  OperationTypeCreateDiskSnapshot,
  OperationTypeDeleteDiskSnapshot,
  OperationTypeCreateDiskFromSnapshot,
  OperationTypeCreateRelationalDatabase,
  OperationTypeUpdateRelationalDatabase,
  OperationTypeDeleteRelationalDatabase,
  OperationTypeCreateRelationalDatabaseFromSnapshot,
  OperationTypeCreateRelationalDatabaseSnapshot,
  OperationTypeDeleteRelationalDatabaseSnapshot,
  OperationTypeUpdateRelationalDatabaseParameters,
  OperationTypeStartRelationalDatabase,
  OperationTypeRebootRelationalDatabase,
  OperationTypeStopRelationalDatabase,
  OperationTypeEnableAddOn,
  OperationTypeDisableAddOn,
  OperationTypePutAlarm,
  OperationTypeGetAlarms,
  OperationTypeDeleteAlarm,
  OperationTypeTestAlarm,
  OperationTypeCreateContactMethod,
  OperationTypeGetContactMethods,
  OperationTypeSendContactMethodVerification,
  OperationTypeDeleteContactMethod,
  OperationTypeCreateDistribution,
  OperationTypeUpdateDistribution,
  OperationTypeDeleteDistribution,
  OperationTypeResetDistributionCache,
  OperationTypeAttachCertificateToDistribution,
  OperationTypeDetachCertificateFromDistribution,
  OperationTypeUpdateDistributionBundle,
  OperationTypeCreateCertificate,
  OperationTypeDeleteCertificate,
  OperationTypeCreateContainerService,
  OperationTypeUpdateContainerService,
  OperationTypeDeleteContainerService,
  OperationTypeCreateContainerServiceDeployment,
  OperationTypeCreateContainerServiceRegistryLogin,
  OperationTypeRegisterContainerImage,
  OperationTypeDeleteContainerImage,
  OperationType'
  #-}
