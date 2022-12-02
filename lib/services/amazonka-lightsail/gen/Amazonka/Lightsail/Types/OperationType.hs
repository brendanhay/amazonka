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
-- Module      : Amazonka.Lightsail.Types.OperationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.OperationType
  ( OperationType
      ( ..,
        OperationType_AllocateStaticIp,
        OperationType_AttachCertificateToDistribution,
        OperationType_AttachDisk,
        OperationType_AttachInstancesToLoadBalancer,
        OperationType_AttachLoadBalancerTlsCertificate,
        OperationType_AttachStaticIp,
        OperationType_CloseInstancePublicPorts,
        OperationType_CreateBucket,
        OperationType_CreateBucketAccessKey,
        OperationType_CreateCertificate,
        OperationType_CreateContactMethod,
        OperationType_CreateContainerService,
        OperationType_CreateContainerServiceDeployment,
        OperationType_CreateContainerServiceRegistryLogin,
        OperationType_CreateDisk,
        OperationType_CreateDiskFromSnapshot,
        OperationType_CreateDiskSnapshot,
        OperationType_CreateDistribution,
        OperationType_CreateDomain,
        OperationType_CreateInstance,
        OperationType_CreateInstanceSnapshot,
        OperationType_CreateInstancesFromSnapshot,
        OperationType_CreateLoadBalancer,
        OperationType_CreateLoadBalancerTlsCertificate,
        OperationType_CreateRelationalDatabase,
        OperationType_CreateRelationalDatabaseFromSnapshot,
        OperationType_CreateRelationalDatabaseSnapshot,
        OperationType_DeleteAlarm,
        OperationType_DeleteBucket,
        OperationType_DeleteBucketAccessKey,
        OperationType_DeleteCertificate,
        OperationType_DeleteContactMethod,
        OperationType_DeleteContainerImage,
        OperationType_DeleteContainerService,
        OperationType_DeleteDisk,
        OperationType_DeleteDiskSnapshot,
        OperationType_DeleteDistribution,
        OperationType_DeleteDomain,
        OperationType_DeleteDomainEntry,
        OperationType_DeleteInstance,
        OperationType_DeleteInstanceSnapshot,
        OperationType_DeleteKnownHostKeys,
        OperationType_DeleteLoadBalancer,
        OperationType_DeleteLoadBalancerTlsCertificate,
        OperationType_DeleteRelationalDatabase,
        OperationType_DeleteRelationalDatabaseSnapshot,
        OperationType_DetachCertificateFromDistribution,
        OperationType_DetachDisk,
        OperationType_DetachInstancesFromLoadBalancer,
        OperationType_DetachStaticIp,
        OperationType_DisableAddOn,
        OperationType_EnableAddOn,
        OperationType_GetAlarms,
        OperationType_GetContactMethods,
        OperationType_OpenInstancePublicPorts,
        OperationType_PutAlarm,
        OperationType_PutInstancePublicPorts,
        OperationType_RebootInstance,
        OperationType_RebootRelationalDatabase,
        OperationType_RegisterContainerImage,
        OperationType_ReleaseStaticIp,
        OperationType_ResetDistributionCache,
        OperationType_SendContactMethodVerification,
        OperationType_SetIpAddressType,
        OperationType_SetResourceAccessForBucket,
        OperationType_StartInstance,
        OperationType_StartRelationalDatabase,
        OperationType_StopInstance,
        OperationType_StopRelationalDatabase,
        OperationType_TestAlarm,
        OperationType_UpdateBucket,
        OperationType_UpdateBucketBundle,
        OperationType_UpdateContainerService,
        OperationType_UpdateDistribution,
        OperationType_UpdateDistributionBundle,
        OperationType_UpdateDomainEntry,
        OperationType_UpdateInstanceMetadataOptions,
        OperationType_UpdateLoadBalancerAttribute,
        OperationType_UpdateRelationalDatabase,
        OperationType_UpdateRelationalDatabaseParameters
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OperationType = OperationType'
  { fromOperationType ::
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

pattern OperationType_AllocateStaticIp :: OperationType
pattern OperationType_AllocateStaticIp = OperationType' "AllocateStaticIp"

pattern OperationType_AttachCertificateToDistribution :: OperationType
pattern OperationType_AttachCertificateToDistribution = OperationType' "AttachCertificateToDistribution"

pattern OperationType_AttachDisk :: OperationType
pattern OperationType_AttachDisk = OperationType' "AttachDisk"

pattern OperationType_AttachInstancesToLoadBalancer :: OperationType
pattern OperationType_AttachInstancesToLoadBalancer = OperationType' "AttachInstancesToLoadBalancer"

pattern OperationType_AttachLoadBalancerTlsCertificate :: OperationType
pattern OperationType_AttachLoadBalancerTlsCertificate = OperationType' "AttachLoadBalancerTlsCertificate"

pattern OperationType_AttachStaticIp :: OperationType
pattern OperationType_AttachStaticIp = OperationType' "AttachStaticIp"

pattern OperationType_CloseInstancePublicPorts :: OperationType
pattern OperationType_CloseInstancePublicPorts = OperationType' "CloseInstancePublicPorts"

pattern OperationType_CreateBucket :: OperationType
pattern OperationType_CreateBucket = OperationType' "CreateBucket"

pattern OperationType_CreateBucketAccessKey :: OperationType
pattern OperationType_CreateBucketAccessKey = OperationType' "CreateBucketAccessKey"

pattern OperationType_CreateCertificate :: OperationType
pattern OperationType_CreateCertificate = OperationType' "CreateCertificate"

pattern OperationType_CreateContactMethod :: OperationType
pattern OperationType_CreateContactMethod = OperationType' "CreateContactMethod"

pattern OperationType_CreateContainerService :: OperationType
pattern OperationType_CreateContainerService = OperationType' "CreateContainerService"

pattern OperationType_CreateContainerServiceDeployment :: OperationType
pattern OperationType_CreateContainerServiceDeployment = OperationType' "CreateContainerServiceDeployment"

pattern OperationType_CreateContainerServiceRegistryLogin :: OperationType
pattern OperationType_CreateContainerServiceRegistryLogin = OperationType' "CreateContainerServiceRegistryLogin"

pattern OperationType_CreateDisk :: OperationType
pattern OperationType_CreateDisk = OperationType' "CreateDisk"

pattern OperationType_CreateDiskFromSnapshot :: OperationType
pattern OperationType_CreateDiskFromSnapshot = OperationType' "CreateDiskFromSnapshot"

pattern OperationType_CreateDiskSnapshot :: OperationType
pattern OperationType_CreateDiskSnapshot = OperationType' "CreateDiskSnapshot"

pattern OperationType_CreateDistribution :: OperationType
pattern OperationType_CreateDistribution = OperationType' "CreateDistribution"

pattern OperationType_CreateDomain :: OperationType
pattern OperationType_CreateDomain = OperationType' "CreateDomain"

pattern OperationType_CreateInstance :: OperationType
pattern OperationType_CreateInstance = OperationType' "CreateInstance"

pattern OperationType_CreateInstanceSnapshot :: OperationType
pattern OperationType_CreateInstanceSnapshot = OperationType' "CreateInstanceSnapshot"

pattern OperationType_CreateInstancesFromSnapshot :: OperationType
pattern OperationType_CreateInstancesFromSnapshot = OperationType' "CreateInstancesFromSnapshot"

pattern OperationType_CreateLoadBalancer :: OperationType
pattern OperationType_CreateLoadBalancer = OperationType' "CreateLoadBalancer"

pattern OperationType_CreateLoadBalancerTlsCertificate :: OperationType
pattern OperationType_CreateLoadBalancerTlsCertificate = OperationType' "CreateLoadBalancerTlsCertificate"

pattern OperationType_CreateRelationalDatabase :: OperationType
pattern OperationType_CreateRelationalDatabase = OperationType' "CreateRelationalDatabase"

pattern OperationType_CreateRelationalDatabaseFromSnapshot :: OperationType
pattern OperationType_CreateRelationalDatabaseFromSnapshot = OperationType' "CreateRelationalDatabaseFromSnapshot"

pattern OperationType_CreateRelationalDatabaseSnapshot :: OperationType
pattern OperationType_CreateRelationalDatabaseSnapshot = OperationType' "CreateRelationalDatabaseSnapshot"

pattern OperationType_DeleteAlarm :: OperationType
pattern OperationType_DeleteAlarm = OperationType' "DeleteAlarm"

pattern OperationType_DeleteBucket :: OperationType
pattern OperationType_DeleteBucket = OperationType' "DeleteBucket"

pattern OperationType_DeleteBucketAccessKey :: OperationType
pattern OperationType_DeleteBucketAccessKey = OperationType' "DeleteBucketAccessKey"

pattern OperationType_DeleteCertificate :: OperationType
pattern OperationType_DeleteCertificate = OperationType' "DeleteCertificate"

pattern OperationType_DeleteContactMethod :: OperationType
pattern OperationType_DeleteContactMethod = OperationType' "DeleteContactMethod"

pattern OperationType_DeleteContainerImage :: OperationType
pattern OperationType_DeleteContainerImage = OperationType' "DeleteContainerImage"

pattern OperationType_DeleteContainerService :: OperationType
pattern OperationType_DeleteContainerService = OperationType' "DeleteContainerService"

pattern OperationType_DeleteDisk :: OperationType
pattern OperationType_DeleteDisk = OperationType' "DeleteDisk"

pattern OperationType_DeleteDiskSnapshot :: OperationType
pattern OperationType_DeleteDiskSnapshot = OperationType' "DeleteDiskSnapshot"

pattern OperationType_DeleteDistribution :: OperationType
pattern OperationType_DeleteDistribution = OperationType' "DeleteDistribution"

pattern OperationType_DeleteDomain :: OperationType
pattern OperationType_DeleteDomain = OperationType' "DeleteDomain"

pattern OperationType_DeleteDomainEntry :: OperationType
pattern OperationType_DeleteDomainEntry = OperationType' "DeleteDomainEntry"

pattern OperationType_DeleteInstance :: OperationType
pattern OperationType_DeleteInstance = OperationType' "DeleteInstance"

pattern OperationType_DeleteInstanceSnapshot :: OperationType
pattern OperationType_DeleteInstanceSnapshot = OperationType' "DeleteInstanceSnapshot"

pattern OperationType_DeleteKnownHostKeys :: OperationType
pattern OperationType_DeleteKnownHostKeys = OperationType' "DeleteKnownHostKeys"

pattern OperationType_DeleteLoadBalancer :: OperationType
pattern OperationType_DeleteLoadBalancer = OperationType' "DeleteLoadBalancer"

pattern OperationType_DeleteLoadBalancerTlsCertificate :: OperationType
pattern OperationType_DeleteLoadBalancerTlsCertificate = OperationType' "DeleteLoadBalancerTlsCertificate"

pattern OperationType_DeleteRelationalDatabase :: OperationType
pattern OperationType_DeleteRelationalDatabase = OperationType' "DeleteRelationalDatabase"

pattern OperationType_DeleteRelationalDatabaseSnapshot :: OperationType
pattern OperationType_DeleteRelationalDatabaseSnapshot = OperationType' "DeleteRelationalDatabaseSnapshot"

pattern OperationType_DetachCertificateFromDistribution :: OperationType
pattern OperationType_DetachCertificateFromDistribution = OperationType' "DetachCertificateFromDistribution"

pattern OperationType_DetachDisk :: OperationType
pattern OperationType_DetachDisk = OperationType' "DetachDisk"

pattern OperationType_DetachInstancesFromLoadBalancer :: OperationType
pattern OperationType_DetachInstancesFromLoadBalancer = OperationType' "DetachInstancesFromLoadBalancer"

pattern OperationType_DetachStaticIp :: OperationType
pattern OperationType_DetachStaticIp = OperationType' "DetachStaticIp"

pattern OperationType_DisableAddOn :: OperationType
pattern OperationType_DisableAddOn = OperationType' "DisableAddOn"

pattern OperationType_EnableAddOn :: OperationType
pattern OperationType_EnableAddOn = OperationType' "EnableAddOn"

pattern OperationType_GetAlarms :: OperationType
pattern OperationType_GetAlarms = OperationType' "GetAlarms"

pattern OperationType_GetContactMethods :: OperationType
pattern OperationType_GetContactMethods = OperationType' "GetContactMethods"

pattern OperationType_OpenInstancePublicPorts :: OperationType
pattern OperationType_OpenInstancePublicPorts = OperationType' "OpenInstancePublicPorts"

pattern OperationType_PutAlarm :: OperationType
pattern OperationType_PutAlarm = OperationType' "PutAlarm"

pattern OperationType_PutInstancePublicPorts :: OperationType
pattern OperationType_PutInstancePublicPorts = OperationType' "PutInstancePublicPorts"

pattern OperationType_RebootInstance :: OperationType
pattern OperationType_RebootInstance = OperationType' "RebootInstance"

pattern OperationType_RebootRelationalDatabase :: OperationType
pattern OperationType_RebootRelationalDatabase = OperationType' "RebootRelationalDatabase"

pattern OperationType_RegisterContainerImage :: OperationType
pattern OperationType_RegisterContainerImage = OperationType' "RegisterContainerImage"

pattern OperationType_ReleaseStaticIp :: OperationType
pattern OperationType_ReleaseStaticIp = OperationType' "ReleaseStaticIp"

pattern OperationType_ResetDistributionCache :: OperationType
pattern OperationType_ResetDistributionCache = OperationType' "ResetDistributionCache"

pattern OperationType_SendContactMethodVerification :: OperationType
pattern OperationType_SendContactMethodVerification = OperationType' "SendContactMethodVerification"

pattern OperationType_SetIpAddressType :: OperationType
pattern OperationType_SetIpAddressType = OperationType' "SetIpAddressType"

pattern OperationType_SetResourceAccessForBucket :: OperationType
pattern OperationType_SetResourceAccessForBucket = OperationType' "SetResourceAccessForBucket"

pattern OperationType_StartInstance :: OperationType
pattern OperationType_StartInstance = OperationType' "StartInstance"

pattern OperationType_StartRelationalDatabase :: OperationType
pattern OperationType_StartRelationalDatabase = OperationType' "StartRelationalDatabase"

pattern OperationType_StopInstance :: OperationType
pattern OperationType_StopInstance = OperationType' "StopInstance"

pattern OperationType_StopRelationalDatabase :: OperationType
pattern OperationType_StopRelationalDatabase = OperationType' "StopRelationalDatabase"

pattern OperationType_TestAlarm :: OperationType
pattern OperationType_TestAlarm = OperationType' "TestAlarm"

pattern OperationType_UpdateBucket :: OperationType
pattern OperationType_UpdateBucket = OperationType' "UpdateBucket"

pattern OperationType_UpdateBucketBundle :: OperationType
pattern OperationType_UpdateBucketBundle = OperationType' "UpdateBucketBundle"

pattern OperationType_UpdateContainerService :: OperationType
pattern OperationType_UpdateContainerService = OperationType' "UpdateContainerService"

pattern OperationType_UpdateDistribution :: OperationType
pattern OperationType_UpdateDistribution = OperationType' "UpdateDistribution"

pattern OperationType_UpdateDistributionBundle :: OperationType
pattern OperationType_UpdateDistributionBundle = OperationType' "UpdateDistributionBundle"

pattern OperationType_UpdateDomainEntry :: OperationType
pattern OperationType_UpdateDomainEntry = OperationType' "UpdateDomainEntry"

pattern OperationType_UpdateInstanceMetadataOptions :: OperationType
pattern OperationType_UpdateInstanceMetadataOptions = OperationType' "UpdateInstanceMetadataOptions"

pattern OperationType_UpdateLoadBalancerAttribute :: OperationType
pattern OperationType_UpdateLoadBalancerAttribute = OperationType' "UpdateLoadBalancerAttribute"

pattern OperationType_UpdateRelationalDatabase :: OperationType
pattern OperationType_UpdateRelationalDatabase = OperationType' "UpdateRelationalDatabase"

pattern OperationType_UpdateRelationalDatabaseParameters :: OperationType
pattern OperationType_UpdateRelationalDatabaseParameters = OperationType' "UpdateRelationalDatabaseParameters"

{-# COMPLETE
  OperationType_AllocateStaticIp,
  OperationType_AttachCertificateToDistribution,
  OperationType_AttachDisk,
  OperationType_AttachInstancesToLoadBalancer,
  OperationType_AttachLoadBalancerTlsCertificate,
  OperationType_AttachStaticIp,
  OperationType_CloseInstancePublicPorts,
  OperationType_CreateBucket,
  OperationType_CreateBucketAccessKey,
  OperationType_CreateCertificate,
  OperationType_CreateContactMethod,
  OperationType_CreateContainerService,
  OperationType_CreateContainerServiceDeployment,
  OperationType_CreateContainerServiceRegistryLogin,
  OperationType_CreateDisk,
  OperationType_CreateDiskFromSnapshot,
  OperationType_CreateDiskSnapshot,
  OperationType_CreateDistribution,
  OperationType_CreateDomain,
  OperationType_CreateInstance,
  OperationType_CreateInstanceSnapshot,
  OperationType_CreateInstancesFromSnapshot,
  OperationType_CreateLoadBalancer,
  OperationType_CreateLoadBalancerTlsCertificate,
  OperationType_CreateRelationalDatabase,
  OperationType_CreateRelationalDatabaseFromSnapshot,
  OperationType_CreateRelationalDatabaseSnapshot,
  OperationType_DeleteAlarm,
  OperationType_DeleteBucket,
  OperationType_DeleteBucketAccessKey,
  OperationType_DeleteCertificate,
  OperationType_DeleteContactMethod,
  OperationType_DeleteContainerImage,
  OperationType_DeleteContainerService,
  OperationType_DeleteDisk,
  OperationType_DeleteDiskSnapshot,
  OperationType_DeleteDistribution,
  OperationType_DeleteDomain,
  OperationType_DeleteDomainEntry,
  OperationType_DeleteInstance,
  OperationType_DeleteInstanceSnapshot,
  OperationType_DeleteKnownHostKeys,
  OperationType_DeleteLoadBalancer,
  OperationType_DeleteLoadBalancerTlsCertificate,
  OperationType_DeleteRelationalDatabase,
  OperationType_DeleteRelationalDatabaseSnapshot,
  OperationType_DetachCertificateFromDistribution,
  OperationType_DetachDisk,
  OperationType_DetachInstancesFromLoadBalancer,
  OperationType_DetachStaticIp,
  OperationType_DisableAddOn,
  OperationType_EnableAddOn,
  OperationType_GetAlarms,
  OperationType_GetContactMethods,
  OperationType_OpenInstancePublicPorts,
  OperationType_PutAlarm,
  OperationType_PutInstancePublicPorts,
  OperationType_RebootInstance,
  OperationType_RebootRelationalDatabase,
  OperationType_RegisterContainerImage,
  OperationType_ReleaseStaticIp,
  OperationType_ResetDistributionCache,
  OperationType_SendContactMethodVerification,
  OperationType_SetIpAddressType,
  OperationType_SetResourceAccessForBucket,
  OperationType_StartInstance,
  OperationType_StartRelationalDatabase,
  OperationType_StopInstance,
  OperationType_StopRelationalDatabase,
  OperationType_TestAlarm,
  OperationType_UpdateBucket,
  OperationType_UpdateBucketBundle,
  OperationType_UpdateContainerService,
  OperationType_UpdateDistribution,
  OperationType_UpdateDistributionBundle,
  OperationType_UpdateDomainEntry,
  OperationType_UpdateInstanceMetadataOptions,
  OperationType_UpdateLoadBalancerAttribute,
  OperationType_UpdateRelationalDatabase,
  OperationType_UpdateRelationalDatabaseParameters,
  OperationType'
  #-}
