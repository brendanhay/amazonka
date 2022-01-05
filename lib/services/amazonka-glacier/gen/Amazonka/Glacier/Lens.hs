{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glacier.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Lens
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
    initiateMultipartUploadResponse_httpStatus,
    initiateMultipartUploadResponse_uploadId,

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

import Amazonka.Glacier.AbortMultipartUpload
import Amazonka.Glacier.AbortVaultLock
import Amazonka.Glacier.AddTagsToVault
import Amazonka.Glacier.CompleteMultipartUpload
import Amazonka.Glacier.CompleteVaultLock
import Amazonka.Glacier.CreateVault
import Amazonka.Glacier.DeleteArchive
import Amazonka.Glacier.DeleteVault
import Amazonka.Glacier.DeleteVaultAccessPolicy
import Amazonka.Glacier.DeleteVaultNotifications
import Amazonka.Glacier.DescribeJob
import Amazonka.Glacier.DescribeVault
import Amazonka.Glacier.GetDataRetrievalPolicy
import Amazonka.Glacier.GetJobOutput
import Amazonka.Glacier.GetVaultAccessPolicy
import Amazonka.Glacier.GetVaultLock
import Amazonka.Glacier.GetVaultNotifications
import Amazonka.Glacier.InitiateJob
import Amazonka.Glacier.InitiateMultipartUpload
import Amazonka.Glacier.InitiateVaultLock
import Amazonka.Glacier.ListJobs
import Amazonka.Glacier.ListMultipartUploads
import Amazonka.Glacier.ListParts
import Amazonka.Glacier.ListProvisionedCapacity
import Amazonka.Glacier.ListTagsForVault
import Amazonka.Glacier.ListVaults
import Amazonka.Glacier.PurchaseProvisionedCapacity
import Amazonka.Glacier.RemoveTagsFromVault
import Amazonka.Glacier.SetDataRetrievalPolicy
import Amazonka.Glacier.SetVaultAccessPolicy
import Amazonka.Glacier.SetVaultNotifications
import Amazonka.Glacier.Types.ArchiveCreationOutput
import Amazonka.Glacier.Types.CSVInput
import Amazonka.Glacier.Types.CSVOutput
import Amazonka.Glacier.Types.DataRetrievalPolicy
import Amazonka.Glacier.Types.DataRetrievalRule
import Amazonka.Glacier.Types.DescribeVaultOutput
import Amazonka.Glacier.Types.Encryption
import Amazonka.Glacier.Types.GlacierJobDescription
import Amazonka.Glacier.Types.Grant
import Amazonka.Glacier.Types.Grantee
import Amazonka.Glacier.Types.InputSerialization
import Amazonka.Glacier.Types.InventoryRetrievalJobDescription
import Amazonka.Glacier.Types.InventoryRetrievalJobInput
import Amazonka.Glacier.Types.JobParameters
import Amazonka.Glacier.Types.OutputLocation
import Amazonka.Glacier.Types.OutputSerialization
import Amazonka.Glacier.Types.PartListElement
import Amazonka.Glacier.Types.ProvisionedCapacityDescription
import Amazonka.Glacier.Types.S3Location
import Amazonka.Glacier.Types.SelectParameters
import Amazonka.Glacier.Types.UploadListElement
import Amazonka.Glacier.Types.VaultAccessPolicy
import Amazonka.Glacier.Types.VaultLockPolicy
import Amazonka.Glacier.Types.VaultNotificationConfig
import Amazonka.Glacier.UploadArchive
import Amazonka.Glacier.UploadMultipartPart
