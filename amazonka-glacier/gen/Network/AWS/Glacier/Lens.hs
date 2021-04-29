{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Lens
  ( -- * Operations

    -- ** PurchaseProvisionedCapacity
    purchaseProvisionedCapacity_accountId,
    purchaseProvisionedCapacityResponse_capacityId,
    purchaseProvisionedCapacityResponse_httpStatus,

    -- ** DescribeVault
    describeVault_accountId,
    describeVault_vaultName,
    describeVaultOutput_lastInventoryDate,
    describeVaultOutput_creationDate,
    describeVaultOutput_vaultName,
    describeVaultOutput_vaultARN,
    describeVaultOutput_sizeInBytes,
    describeVaultOutput_numberOfArchives,

    -- ** SetVaultAccessPolicy
    setVaultAccessPolicy_policy,
    setVaultAccessPolicy_accountId,
    setVaultAccessPolicy_vaultName,

    -- ** CompleteMultipartUpload
    completeMultipartUpload_archiveSize,
    completeMultipartUpload_checksum,
    completeMultipartUpload_accountId,
    completeMultipartUpload_vaultName,
    completeMultipartUpload_uploadId,
    archiveCreationOutput_archiveId,
    archiveCreationOutput_location,
    archiveCreationOutput_checksum,

    -- ** SetDataRetrievalPolicy
    setDataRetrievalPolicy_policy,
    setDataRetrievalPolicy_accountId,

    -- ** UploadArchive
    uploadArchive_archiveDescription,
    uploadArchive_checksum,
    uploadArchive_vaultName,
    uploadArchive_accountId,
    uploadArchive_body,
    archiveCreationOutput_archiveId,
    archiveCreationOutput_location,
    archiveCreationOutput_checksum,

    -- ** ListProvisionedCapacity
    listProvisionedCapacity_accountId,
    listProvisionedCapacityResponse_provisionedCapacityList,
    listProvisionedCapacityResponse_httpStatus,

    -- ** SetVaultNotifications
    setVaultNotifications_vaultNotificationConfig,
    setVaultNotifications_accountId,
    setVaultNotifications_vaultName,

    -- ** DeleteVault
    deleteVault_accountId,
    deleteVault_vaultName,

    -- ** AbortVaultLock
    abortVaultLock_accountId,
    abortVaultLock_vaultName,

    -- ** DeleteArchive
    deleteArchive_accountId,
    deleteArchive_vaultName,
    deleteArchive_archiveId,

    -- ** RemoveTagsFromVault
    removeTagsFromVault_tagKeys,
    removeTagsFromVault_accountId,
    removeTagsFromVault_vaultName,

    -- ** ListVaults
    listVaults_limit,
    listVaults_marker,
    listVaults_accountId,
    listVaultsResponse_vaultList,
    listVaultsResponse_marker,
    listVaultsResponse_httpStatus,

    -- ** InitiateVaultLock
    initiateVaultLock_policy,
    initiateVaultLock_accountId,
    initiateVaultLock_vaultName,
    initiateVaultLockResponse_lockId,
    initiateVaultLockResponse_httpStatus,

    -- ** DescribeJob
    describeJob_accountId,
    describeJob_vaultName,
    describeJob_jobId,
    glacierJobDescription_sHA256TreeHash,
    glacierJobDescription_statusMessage,
    glacierJobDescription_jobDescription,
    glacierJobDescription_retrievalByteRange,
    glacierJobDescription_creationDate,
    glacierJobDescription_jobOutputPath,
    glacierJobDescription_selectParameters,
    glacierJobDescription_vaultARN,
    glacierJobDescription_archiveId,
    glacierJobDescription_sNSTopic,
    glacierJobDescription_inventorySizeInBytes,
    glacierJobDescription_statusCode,
    glacierJobDescription_archiveSizeInBytes,
    glacierJobDescription_action,
    glacierJobDescription_inventoryRetrievalParameters,
    glacierJobDescription_completionDate,
    glacierJobDescription_archiveSHA256TreeHash,
    glacierJobDescription_completed,
    glacierJobDescription_jobId,
    glacierJobDescription_outputLocation,
    glacierJobDescription_tier,

    -- ** ListTagsForVault
    listTagsForVault_accountId,
    listTagsForVault_vaultName,
    listTagsForVaultResponse_tags,
    listTagsForVaultResponse_httpStatus,

    -- ** GetVaultLock
    getVaultLock_accountId,
    getVaultLock_vaultName,
    getVaultLockResponse_creationDate,
    getVaultLockResponse_state,
    getVaultLockResponse_expirationDate,
    getVaultLockResponse_policy,
    getVaultLockResponse_httpStatus,

    -- ** AbortMultipartUpload
    abortMultipartUpload_accountId,
    abortMultipartUpload_vaultName,
    abortMultipartUpload_uploadId,

    -- ** DeleteVaultAccessPolicy
    deleteVaultAccessPolicy_accountId,
    deleteVaultAccessPolicy_vaultName,

    -- ** InitiateJob
    initiateJob_jobParameters,
    initiateJob_accountId,
    initiateJob_vaultName,
    initiateJobResponse_jobOutputPath,
    initiateJobResponse_location,
    initiateJobResponse_jobId,
    initiateJobResponse_httpStatus,

    -- ** ListMultipartUploads
    listMultipartUploads_limit,
    listMultipartUploads_marker,
    listMultipartUploads_accountId,
    listMultipartUploads_vaultName,
    listMultipartUploadsResponse_uploadsList,
    listMultipartUploadsResponse_marker,
    listMultipartUploadsResponse_httpStatus,

    -- ** AddTagsToVault
    addTagsToVault_tags,
    addTagsToVault_accountId,
    addTagsToVault_vaultName,

    -- ** InitiateMultipartUpload
    initiateMultipartUpload_partSize,
    initiateMultipartUpload_archiveDescription,
    initiateMultipartUpload_accountId,
    initiateMultipartUpload_vaultName,
    initiateMultipartUploadResponse_uploadId,
    initiateMultipartUploadResponse_location,
    initiateMultipartUploadResponse_httpStatus,

    -- ** CreateVault
    createVault_accountId,
    createVault_vaultName,
    createVaultResponse_location,
    createVaultResponse_httpStatus,

    -- ** ListJobs
    listJobs_statuscode,
    listJobs_completed,
    listJobs_limit,
    listJobs_marker,
    listJobs_accountId,
    listJobs_vaultName,
    listJobsResponse_jobList,
    listJobsResponse_marker,
    listJobsResponse_httpStatus,

    -- ** ListParts
    listParts_limit,
    listParts_marker,
    listParts_accountId,
    listParts_vaultName,
    listParts_uploadId,
    listPartsResponse_partSizeInBytes,
    listPartsResponse_creationDate,
    listPartsResponse_vaultARN,
    listPartsResponse_archiveDescription,
    listPartsResponse_parts,
    listPartsResponse_multipartUploadId,
    listPartsResponse_marker,
    listPartsResponse_httpStatus,

    -- ** GetJobOutput
    getJobOutput_range,
    getJobOutput_accountId,
    getJobOutput_vaultName,
    getJobOutput_jobId,
    getJobOutputResponse_contentType,
    getJobOutputResponse_contentRange,
    getJobOutputResponse_archiveDescription,
    getJobOutputResponse_acceptRanges,
    getJobOutputResponse_checksum,
    getJobOutputResponse_status,
    getJobOutputResponse_body,

    -- ** CompleteVaultLock
    completeVaultLock_accountId,
    completeVaultLock_vaultName,
    completeVaultLock_lockId,

    -- ** GetVaultAccessPolicy
    getVaultAccessPolicy_accountId,
    getVaultAccessPolicy_vaultName,
    getVaultAccessPolicyResponse_policy,
    getVaultAccessPolicyResponse_httpStatus,

    -- ** GetDataRetrievalPolicy
    getDataRetrievalPolicy_accountId,
    getDataRetrievalPolicyResponse_policy,
    getDataRetrievalPolicyResponse_httpStatus,

    -- ** DeleteVaultNotifications
    deleteVaultNotifications_accountId,
    deleteVaultNotifications_vaultName,

    -- ** UploadMultipartPart
    uploadMultipartPart_range,
    uploadMultipartPart_checksum,
    uploadMultipartPart_accountId,
    uploadMultipartPart_vaultName,
    uploadMultipartPart_uploadId,
    uploadMultipartPart_body,
    uploadMultipartPartResponse_checksum,
    uploadMultipartPartResponse_httpStatus,

    -- ** GetVaultNotifications
    getVaultNotifications_accountId,
    getVaultNotifications_vaultName,
    getVaultNotificationsResponse_vaultNotificationConfig,
    getVaultNotificationsResponse_httpStatus,

    -- * Types

    -- ** ArchiveCreationOutput
    archiveCreationOutput_archiveId,
    archiveCreationOutput_location,
    archiveCreationOutput_checksum,

    -- ** CSVInput
    cSVInput_recordDelimiter,
    cSVInput_quoteCharacter,
    cSVInput_fileHeaderInfo,
    cSVInput_fieldDelimiter,
    cSVInput_comments,
    cSVInput_quoteEscapeCharacter,

    -- ** CSVOutput
    cSVOutput_recordDelimiter,
    cSVOutput_quoteCharacter,
    cSVOutput_fieldDelimiter,
    cSVOutput_quoteFields,
    cSVOutput_quoteEscapeCharacter,

    -- ** DataRetrievalPolicy
    dataRetrievalPolicy_rules,

    -- ** DataRetrievalRule
    dataRetrievalRule_bytesPerHour,
    dataRetrievalRule_strategy,

    -- ** DescribeVaultOutput
    describeVaultOutput_lastInventoryDate,
    describeVaultOutput_creationDate,
    describeVaultOutput_vaultName,
    describeVaultOutput_vaultARN,
    describeVaultOutput_sizeInBytes,
    describeVaultOutput_numberOfArchives,

    -- ** Encryption
    encryption_encryptionType,
    encryption_kmsKeyId,
    encryption_kmsContext,

    -- ** GlacierJobDescription
    glacierJobDescription_sHA256TreeHash,
    glacierJobDescription_statusMessage,
    glacierJobDescription_jobDescription,
    glacierJobDescription_retrievalByteRange,
    glacierJobDescription_creationDate,
    glacierJobDescription_jobOutputPath,
    glacierJobDescription_selectParameters,
    glacierJobDescription_vaultARN,
    glacierJobDescription_archiveId,
    glacierJobDescription_sNSTopic,
    glacierJobDescription_inventorySizeInBytes,
    glacierJobDescription_statusCode,
    glacierJobDescription_archiveSizeInBytes,
    glacierJobDescription_action,
    glacierJobDescription_inventoryRetrievalParameters,
    glacierJobDescription_completionDate,
    glacierJobDescription_archiveSHA256TreeHash,
    glacierJobDescription_completed,
    glacierJobDescription_jobId,
    glacierJobDescription_outputLocation,
    glacierJobDescription_tier,

    -- ** Grant
    grant_grantee,
    grant_permission,

    -- ** Grantee
    grantee_uri,
    grantee_id,
    grantee_displayName,
    grantee_emailAddress,
    grantee_type,

    -- ** InputSerialization
    inputSerialization_csv,

    -- ** InventoryRetrievalJobDescription
    inventoryRetrievalJobDescription_startDate,
    inventoryRetrievalJobDescription_format,
    inventoryRetrievalJobDescription_endDate,
    inventoryRetrievalJobDescription_limit,
    inventoryRetrievalJobDescription_marker,

    -- ** InventoryRetrievalJobInput
    inventoryRetrievalJobInput_startDate,
    inventoryRetrievalJobInput_endDate,
    inventoryRetrievalJobInput_limit,
    inventoryRetrievalJobInput_marker,

    -- ** JobParameters
    jobParameters_retrievalByteRange,
    jobParameters_format,
    jobParameters_selectParameters,
    jobParameters_archiveId,
    jobParameters_sNSTopic,
    jobParameters_description,
    jobParameters_inventoryRetrievalParameters,
    jobParameters_type,
    jobParameters_outputLocation,
    jobParameters_tier,

    -- ** OutputLocation
    outputLocation_s3,

    -- ** OutputSerialization
    outputSerialization_csv,

    -- ** PartListElement
    partListElement_sHA256TreeHash,
    partListElement_rangeInBytes,

    -- ** ProvisionedCapacityDescription
    provisionedCapacityDescription_startDate,
    provisionedCapacityDescription_capacityId,
    provisionedCapacityDescription_expirationDate,

    -- ** S3Location
    s3Location_bucketName,
    s3Location_prefix,
    s3Location_cannedACL,
    s3Location_encryption,
    s3Location_storageClass,
    s3Location_userMetadata,
    s3Location_accessControlList,
    s3Location_tagging,

    -- ** SelectParameters
    selectParameters_expressionType,
    selectParameters_outputSerialization,
    selectParameters_inputSerialization,
    selectParameters_expression,

    -- ** UploadListElement
    uploadListElement_partSizeInBytes,
    uploadListElement_creationDate,
    uploadListElement_vaultARN,
    uploadListElement_archiveDescription,
    uploadListElement_multipartUploadId,

    -- ** VaultAccessPolicy
    vaultAccessPolicy_policy,

    -- ** VaultLockPolicy
    vaultLockPolicy_policy,

    -- ** VaultNotificationConfig
    vaultNotificationConfig_events,
    vaultNotificationConfig_sNSTopic,
  )
where

import Network.AWS.Glacier.AbortMultipartUpload
import Network.AWS.Glacier.AbortVaultLock
import Network.AWS.Glacier.AddTagsToVault
import Network.AWS.Glacier.CompleteMultipartUpload
import Network.AWS.Glacier.CompleteVaultLock
import Network.AWS.Glacier.CreateVault
import Network.AWS.Glacier.DeleteArchive
import Network.AWS.Glacier.DeleteVault
import Network.AWS.Glacier.DeleteVaultAccessPolicy
import Network.AWS.Glacier.DeleteVaultNotifications
import Network.AWS.Glacier.DescribeJob
import Network.AWS.Glacier.DescribeVault
import Network.AWS.Glacier.GetDataRetrievalPolicy
import Network.AWS.Glacier.GetJobOutput
import Network.AWS.Glacier.GetVaultAccessPolicy
import Network.AWS.Glacier.GetVaultLock
import Network.AWS.Glacier.GetVaultNotifications
import Network.AWS.Glacier.InitiateJob
import Network.AWS.Glacier.InitiateMultipartUpload
import Network.AWS.Glacier.InitiateVaultLock
import Network.AWS.Glacier.ListJobs
import Network.AWS.Glacier.ListMultipartUploads
import Network.AWS.Glacier.ListParts
import Network.AWS.Glacier.ListProvisionedCapacity
import Network.AWS.Glacier.ListTagsForVault
import Network.AWS.Glacier.ListVaults
import Network.AWS.Glacier.PurchaseProvisionedCapacity
import Network.AWS.Glacier.RemoveTagsFromVault
import Network.AWS.Glacier.SetDataRetrievalPolicy
import Network.AWS.Glacier.SetVaultAccessPolicy
import Network.AWS.Glacier.SetVaultNotifications
import Network.AWS.Glacier.Types.ArchiveCreationOutput
import Network.AWS.Glacier.Types.CSVInput
import Network.AWS.Glacier.Types.CSVOutput
import Network.AWS.Glacier.Types.DataRetrievalPolicy
import Network.AWS.Glacier.Types.DataRetrievalRule
import Network.AWS.Glacier.Types.DescribeVaultOutput
import Network.AWS.Glacier.Types.Encryption
import Network.AWS.Glacier.Types.GlacierJobDescription
import Network.AWS.Glacier.Types.Grant
import Network.AWS.Glacier.Types.Grantee
import Network.AWS.Glacier.Types.InputSerialization
import Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
import Network.AWS.Glacier.Types.InventoryRetrievalJobInput
import Network.AWS.Glacier.Types.JobParameters
import Network.AWS.Glacier.Types.OutputLocation
import Network.AWS.Glacier.Types.OutputSerialization
import Network.AWS.Glacier.Types.PartListElement
import Network.AWS.Glacier.Types.ProvisionedCapacityDescription
import Network.AWS.Glacier.Types.S3Location
import Network.AWS.Glacier.Types.SelectParameters
import Network.AWS.Glacier.Types.UploadListElement
import Network.AWS.Glacier.Types.VaultAccessPolicy
import Network.AWS.Glacier.Types.VaultLockPolicy
import Network.AWS.Glacier.Types.VaultNotificationConfig
import Network.AWS.Glacier.UploadArchive
import Network.AWS.Glacier.UploadMultipartPart
