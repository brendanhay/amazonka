{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Lens
  ( -- * Operations

    -- ** DeleteHapg
    deleteHapg_hapgArn,
    deleteHapgResponse_httpStatus,
    deleteHapgResponse_status,

    -- ** ListHapgs
    listHapgs_nextToken,
    listHapgsResponse_nextToken,
    listHapgsResponse_httpStatus,
    listHapgsResponse_hapgList,

    -- ** ModifyLunaClient
    modifyLunaClient_clientArn,
    modifyLunaClient_certificate,
    modifyLunaClientResponse_clientArn,
    modifyLunaClientResponse_httpStatus,

    -- ** ListHsms
    listHsms_nextToken,
    listHsmsResponse_nextToken,
    listHsmsResponse_hsmList,
    listHsmsResponse_httpStatus,

    -- ** DescribeLunaClient
    describeLunaClient_clientArn,
    describeLunaClient_certificateFingerprint,
    describeLunaClientResponse_clientArn,
    describeLunaClientResponse_lastModifiedTimestamp,
    describeLunaClientResponse_certificateFingerprint,
    describeLunaClientResponse_certificate,
    describeLunaClientResponse_label,
    describeLunaClientResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tagList,

    -- ** CreateHapg
    createHapg_label,
    createHapgResponse_hapgArn,
    createHapgResponse_httpStatus,

    -- ** CreateHsm
    createHsm_clientToken,
    createHsm_syslogIp,
    createHsm_externalId,
    createHsm_eniIp,
    createHsm_subnetId,
    createHsm_sshKey,
    createHsm_iamRoleArn,
    createHsm_subscriptionType,
    createHsmResponse_hsmArn,
    createHsmResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceArn,
    removeTagsFromResource_tagKeyList,
    removeTagsFromResourceResponse_httpStatus,
    removeTagsFromResourceResponse_status,

    -- ** DescribeHapg
    describeHapg_hapgArn,
    describeHapgResponse_state,
    describeHapgResponse_lastModifiedTimestamp,
    describeHapgResponse_hsmsPendingRegistration,
    describeHapgResponse_hsmsPendingDeletion,
    describeHapgResponse_hapgSerial,
    describeHapgResponse_hsmsLastActionFailed,
    describeHapgResponse_partitionSerialList,
    describeHapgResponse_hapgArn,
    describeHapgResponse_label,
    describeHapgResponse_httpStatus,

    -- ** CreateLunaClient
    createLunaClient_label,
    createLunaClient_certificate,
    createLunaClientResponse_clientArn,
    createLunaClientResponse_httpStatus,

    -- ** ListLunaClients
    listLunaClients_nextToken,
    listLunaClientsResponse_nextToken,
    listLunaClientsResponse_httpStatus,
    listLunaClientsResponse_clientList,

    -- ** AddTagsToResource
    addTagsToResource_resourceArn,
    addTagsToResource_tagList,
    addTagsToResourceResponse_httpStatus,
    addTagsToResourceResponse_status,

    -- ** GetConfig
    getConfig_clientArn,
    getConfig_clientVersion,
    getConfig_hapgList,
    getConfigResponse_configFile,
    getConfigResponse_configCred,
    getConfigResponse_configType,
    getConfigResponse_httpStatus,

    -- ** DeleteHsm
    deleteHsm_hsmArn,
    deleteHsmResponse_httpStatus,
    deleteHsmResponse_status,

    -- ** DescribeHsm
    describeHsm_hsmSerialNumber,
    describeHsm_hsmArn,
    describeHsmResponse_status,
    describeHsmResponse_iamRoleArn,
    describeHsmResponse_eniId,
    describeHsmResponse_vpcId,
    describeHsmResponse_sshKeyLastUpdated,
    describeHsmResponse_subscriptionEndDate,
    describeHsmResponse_serverCertUri,
    describeHsmResponse_subscriptionType,
    describeHsmResponse_sshPublicKey,
    describeHsmResponse_subnetId,
    describeHsmResponse_statusDetails,
    describeHsmResponse_partitions,
    describeHsmResponse_subscriptionStartDate,
    describeHsmResponse_availabilityZone,
    describeHsmResponse_serverCertLastUpdated,
    describeHsmResponse_softwareVersion,
    describeHsmResponse_vendorName,
    describeHsmResponse_serialNumber,
    describeHsmResponse_hsmArn,
    describeHsmResponse_eniIp,
    describeHsmResponse_hsmType,
    describeHsmResponse_httpStatus,

    -- ** ModifyHapg
    modifyHapg_partitionSerialList,
    modifyHapg_label,
    modifyHapg_hapgArn,
    modifyHapgResponse_hapgArn,
    modifyHapgResponse_httpStatus,

    -- ** DeleteLunaClient
    deleteLunaClient_clientArn,
    deleteLunaClientResponse_httpStatus,
    deleteLunaClientResponse_status,

    -- ** ModifyHsm
    modifyHsm_iamRoleArn,
    modifyHsm_subnetId,
    modifyHsm_syslogIp,
    modifyHsm_externalId,
    modifyHsm_eniIp,
    modifyHsm_hsmArn,
    modifyHsmResponse_hsmArn,
    modifyHsmResponse_httpStatus,

    -- ** ListAvailableZones
    listAvailableZonesResponse_aZList,
    listAvailableZonesResponse_httpStatus,

    -- * Types

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.CloudHSM.AddTagsToResource
import Network.AWS.CloudHSM.CreateHapg
import Network.AWS.CloudHSM.CreateHsm
import Network.AWS.CloudHSM.CreateLunaClient
import Network.AWS.CloudHSM.DeleteHapg
import Network.AWS.CloudHSM.DeleteHsm
import Network.AWS.CloudHSM.DeleteLunaClient
import Network.AWS.CloudHSM.DescribeHapg
import Network.AWS.CloudHSM.DescribeHsm
import Network.AWS.CloudHSM.DescribeLunaClient
import Network.AWS.CloudHSM.GetConfig
import Network.AWS.CloudHSM.ListAvailableZones
import Network.AWS.CloudHSM.ListHapgs
import Network.AWS.CloudHSM.ListHsms
import Network.AWS.CloudHSM.ListLunaClients
import Network.AWS.CloudHSM.ListTagsForResource
import Network.AWS.CloudHSM.ModifyHapg
import Network.AWS.CloudHSM.ModifyHsm
import Network.AWS.CloudHSM.ModifyLunaClient
import Network.AWS.CloudHSM.RemoveTagsFromResource
import Network.AWS.CloudHSM.Types.Tag
