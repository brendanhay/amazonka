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

    -- ** SetVaultAccessPolicy
    setVaultAccessPolicy_policy,
    setVaultAccessPolicy_accountId,
    setVaultAccessPolicy_vaultName,

    -- ** InitiateJob
    initiateJob_jobParameters,
    initiateJob_accountId,
    initiateJob_vaultName,
    initiateJobResponse_jobId,
    initiateJobResponse_jobOutputPath,
    initiateJobResponse_location,
    initiateJobResponse_httpStatus,

    -- ** SetDataRetrievalPolicy
    setDataRetrievalPolicy_policy,
    setDataRetrievalPolicy_accountId,

    -- ** DeleteVaultAccessPolicy
    deleteVaultAccessPolicy_accountId,
    deleteVaultAccessPolicy_vaultName,

    -- ** GetVaultNotifications
    getVaultNotifications_accountId,
    getVaultNotifications_vaultName,
    getVaultNotificationsResponse_vaultNotificationConfig,
    getVaultNotificationsResponse_httpStatus,

    -- ** ListTagsForVault
    listTagsForVault_accountId,
    listTagsForVault_vaultName,
    listTagsForVaultResponse_tags,
    listTagsForVaultResponse_httpStatus,

    -- ** UploadMultipartPart
    uploadMultipartPart_accountId,
    uploadMultipartPart_vaultName,
    uploadMultipartPart_uploadId,
    uploadMultipartPart_range,
    uploadMultipartPart_checksum,
    uploadMultipartPart_body,
    uploadMultipartPartResponse_checksum,
    uploadMultipartPartResponse_httpStatus,

    -- ** DeleteVaultNotifications
    deleteVaultNotifications_accountId,
    deleteVaultNotifications_vaultName,

    -- ** CompleteVaultLock
    completeVaultLock_accountId,
    completeVaultLock_vaultName,
    completeVaultLock_lockId,

    -- ** AbortVaultLock
    abortVaultLock_accountId,
    abortVaultLock_vaultName,

    -- ** ListVaults
    listVaults_marker,
    listVaults_limit,
    listVaults_accountId,
    listVaultsResponse_marker,
    listVaultsResponse_vaultList,
    listVaultsResponse_httpStatus,

    -- ** ListProvisionedCapacity
    listProvisionedCapacity_accountId,
    listProvisionedCapacityResponse_provisionedCapacityList,
    listProvisionedCapacityResponse_httpStatus,

    -- ** ListJobs
    listJobs_marker,
    listJobs_completed,
    listJobs_limit,
    listJobs_statuscode,
    listJobs_accountId,
    listJobs_vaultName,
    listJobsResponse_marker,
    listJobsResponse_jobList,
    listJobsResponse_httpStatus,

    -- ** SetVaultNotifications
    setVaultNotifications_vaultNotificationConfig,
    setVaultNotifications_accountId,
    setVaultNotifications_vaultName,

    -- ** GetJobOutput
    getJobOutput_range,
    getJobOutput_accountId,
    getJobOutput_vaultName,
    getJobOutput_jobId,
    getJobOutputResponse_checksum,
    getJobOutputResponse_acceptRanges,
    getJobOutputResponse_archiveDescription,
    getJobOutputResponse_contentRange,
    getJobOutputResponse_contentType,
    getJobOutputResponse_status,
    getJobOutputResponse_body,

    -- ** CompleteMultipartUpload
    completeMultipartUpload_accountId,
    completeMultipartUpload_vaultName,
    completeMultipartUpload_uploadId,
    completeMultipartUpload_archiveSize,
    completeMultipartUpload_checksum,
    archiveCreationOutput_archiveId,
    archiveCreationOutput_checksum,
    archiveCreationOutput_location,

    -- ** ListMultipartUploads
    listMultipartUploads_marker,
    listMultipartUploads_limit,
    listMultipartUploads_accountId,
    listMultipartUploads_vaultName,
    listMultipartUploadsResponse_uploadsList,
    listMultipartUploadsResponse_marker,
    listMultipartUploadsResponse_httpStatus,

    -- ** AbortMultipartUpload
    abortMultipartUpload_accountId,
    abortMultipartUpload_vaultName,
    abortMultipartUpload_uploadId,

    -- ** PurchaseProvisionedCapacity
    purchaseProvisionedCapacity_accountId,
    purchaseProvisionedCapacityResponse_capacityId,
    purchaseProvisionedCapacityResponse_httpStatus,

    -- ** DescribeVault
    describeVault_accountId,
    describeVault_vaultName,
    describeVaultOutput_vaultName,
    describeVaultOutput_sizeInBytes,
    describeVaultOutput_lastInventoryDate,
    describeVaultOutput_vaultARN,
    describeVaultOutput_creationDate,
    describeVaultOutput_numberOfArchives,

    -- ** GetVaultLock
    getVaultLock_accountId,
    getVaultLock_vaultName,
    getVaultLockResponse_state,
    getVaultLockResponse_expirationDate,
    getVaultLockResponse_creationDate,
    getVaultLockResponse_policy,
    getVaultLockResponse_httpStatus,

    -- ** DescribeJob
    describeJob_accountId,
    describeJob_vaultName,
    describeJob_jobId,
    glacierJobDescription_sHA256TreeHash,
    glacierJobDescription_archiveId,
    glacierJobDescription_selectParameters,
    glacierJobDescription_jobId,
    glacierJobDescription_jobOutputPath,
    glacierJobDescription_retrievalByteRange,
    glacierJobDescription_inventoryRetrievalParameters,
    glacierJobDescription_action,
    glacierJobDescription_jobDescription,
    glacierJobDescription_sNSTopic,
    glacierJobDescription_statusMessage,
    glacierJobDescription_vaultARN,
    glacierJobDescription_outputLocation,
    glacierJobDescription_tier,
    glacierJobDescription_archiveSHA256TreeHash,
    glacierJobDescription_creationDate,
    glacierJobDescription_completed,
    glacierJobDescription_completionDate,
    glacierJobDescription_inventorySizeInBytes,
    glacierJobDescription_archiveSizeInBytes,
    glacierJobDescription_statusCode,

    -- ** InitiateVaultLock
    initiateVaultLock_policy,
    initiateVaultLock_accountId,
    initiateVaultLock_vaultName,
    initiateVaultLockResponse_lockId,
    initiateVaultLockResponse_httpStatus,

    -- ** GetVaultAccessPolicy
    getVaultAccessPolicy_accountId,
    getVaultAccessPolicy_vaultName,
    getVaultAccessPolicyResponse_policy,
    getVaultAccessPolicyResponse_httpStatus,

    -- ** GetDataRetrievalPolicy
    getDataRetrievalPolicy_accountId,
    getDataRetrievalPolicyResponse_policy,
    getDataRetrievalPolicyResponse_httpStatus,

    -- ** RemoveTagsFromVault
    removeTagsFromVault_tagKeys,
    removeTagsFromVault_accountId,
    removeTagsFromVault_vaultName,

    -- ** DeleteVault
    deleteVault_accountId,
    deleteVault_vaultName,

    -- ** DeleteArchive
    deleteArchive_accountId,
    deleteArchive_vaultName,
    deleteArchive_archiveId,

    -- ** CreateVault
    createVault_accountId,
    createVault_vaultName,
    createVaultResponse_location,
    createVaultResponse_httpStatus,

    -- ** InitiateMultipartUpload
    initiateMultipartUpload_archiveDescription,
    initiateMultipartUpload_accountId,
    initiateMultipartUpload_vaultName,
    initiateMultipartUpload_partSize,
    initiateMultipartUploadResponse_location,
    initiateMultipartUploadResponse_uploadId,
    initiateMultipartUploadResponse_httpStatus,

    -- ** ListParts
    listParts_marker,
    listParts_limit,
    listParts_accountId,
    listParts_vaultName,
    listParts_uploadId,
    listPartsResponse_parts,
    listPartsResponse_multipartUploadId,
    listPartsResponse_partSizeInBytes,
    listPartsResponse_archiveDescription,
    listPartsResponse_vaultARN,
    listPartsResponse_marker,
    listPartsResponse_creationDate,
    listPartsResponse_httpStatus,

    -- ** AddTagsToVault
    addTagsToVault_tags,
    addTagsToVault_accountId,
    addTagsToVault_vaultName,

    -- ** UploadArchive
    uploadArchive_checksum,
    uploadArchive_archiveDescription,
    uploadArchive_vaultName,
    uploadArchive_accountId,
    uploadArchive_body,
    archiveCreationOutput_archiveId,
    archiveCreationOutput_checksum,
    archiveCreationOutput_location,

    -- * Types

    -- ** ArchiveCreationOutput
    archiveCreationOutput_archiveId,
    archiveCreationOutput_checksum,
    archiveCreationOutput_location,

    -- ** CSVInput
    cSVInput_quoteCharacter,
    cSVInput_recordDelimiter,
    cSVInput_fileHeaderInfo,
    cSVInput_quoteEscapeCharacter,
    cSVInput_comments,
    cSVInput_fieldDelimiter,

    -- ** CSVOutput
    cSVOutput_quoteCharacter,
    cSVOutput_quoteFields,
    cSVOutput_recordDelimiter,
    cSVOutput_quoteEscapeCharacter,
    cSVOutput_fieldDelimiter,

    -- ** DataRetrievalPolicy
    dataRetrievalPolicy_rules,

    -- ** DataRetrievalRule
    dataRetrievalRule_strategy,
    dataRetrievalRule_bytesPerHour,

    -- ** DescribeVaultOutput
    describeVaultOutput_vaultName,
    describeVaultOutput_sizeInBytes,
    describeVaultOutput_lastInventoryDate,
    describeVaultOutput_vaultARN,
    describeVaultOutput_creationDate,
    describeVaultOutput_numberOfArchives,

    -- ** Encryption
    encryption_encryptionType,
    encryption_kmsKeyId,
    encryption_kmsContext,

    -- ** GlacierJobDescription
    glacierJobDescription_sHA256TreeHash,
    glacierJobDescription_archiveId,
    glacierJobDescription_selectParameters,
    glacierJobDescription_jobId,
    glacierJobDescription_jobOutputPath,
    glacierJobDescription_retrievalByteRange,
    glacierJobDescription_inventoryRetrievalParameters,
    glacierJobDescription_action,
    glacierJobDescription_jobDescription,
    glacierJobDescription_sNSTopic,
    glacierJobDescription_statusMessage,
    glacierJobDescription_vaultARN,
    glacierJobDescription_outputLocation,
    glacierJobDescription_tier,
    glacierJobDescription_archiveSHA256TreeHash,
    glacierJobDescription_creationDate,
    glacierJobDescription_completed,
    glacierJobDescription_completionDate,
    glacierJobDescription_inventorySizeInBytes,
    glacierJobDescription_archiveSizeInBytes,
    glacierJobDescription_statusCode,

    -- ** Grant
    grant_permission,
    grant_grantee,

    -- ** Grantee
    grantee_uri,
    grantee_emailAddress,
    grantee_displayName,
    grantee_id,
    grantee_type,

    -- ** InputSerialization
    inputSerialization_csv,

    -- ** InventoryRetrievalJobDescription
    inventoryRetrievalJobDescription_format,
    inventoryRetrievalJobDescription_endDate,
    inventoryRetrievalJobDescription_startDate,
    inventoryRetrievalJobDescription_marker,
    inventoryRetrievalJobDescription_limit,

    -- ** InventoryRetrievalJobInput
    inventoryRetrievalJobInput_endDate,
    inventoryRetrievalJobInput_startDate,
    inventoryRetrievalJobInput_marker,
    inventoryRetrievalJobInput_limit,

    -- ** JobParameters
    jobParameters_archiveId,
    jobParameters_selectParameters,
    jobParameters_format,
    jobParameters_retrievalByteRange,
    jobParameters_inventoryRetrievalParameters,
    jobParameters_sNSTopic,
    jobParameters_outputLocation,
    jobParameters_tier,
    jobParameters_type,
    jobParameters_description,

    -- ** OutputLocation
    outputLocation_s3,

    -- ** OutputSerialization
    outputSerialization_csv,

    -- ** PartListElement
    partListElement_sHA256TreeHash,
    partListElement_rangeInBytes,

    -- ** ProvisionedCapacityDescription
    provisionedCapacityDescription_capacityId,
    provisionedCapacityDescription_startDate,
    provisionedCapacityDescription_expirationDate,

    -- ** S3Location
    s3Location_cannedACL,
    s3Location_prefix,
    s3Location_bucketName,
    s3Location_accessControlList,
    s3Location_userMetadata,
    s3Location_encryption,
    s3Location_storageClass,
    s3Location_tagging,

    -- ** SelectParameters
    selectParameters_expressionType,
    selectParameters_outputSerialization,
    selectParameters_expression,
    selectParameters_inputSerialization,

    -- ** UploadListElement
    uploadListElement_multipartUploadId,
    uploadListElement_partSizeInBytes,
    uploadListElement_archiveDescription,
    uploadListElement_vaultARN,
    uploadListElement_creationDate,

    -- ** VaultAccessPolicy
    vaultAccessPolicy_policy,

    -- ** VaultLockPolicy
    vaultLockPolicy_policy,

    -- ** VaultNotificationConfig
    vaultNotificationConfig_sNSTopic,
    vaultNotificationConfig_events,
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
