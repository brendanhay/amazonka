{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon S3 Glacier (Glacier) is a storage solution for \"cold data.\"
--
-- Glacier is an extremely low-cost storage service that provides secure,
-- durable, and easy-to-use storage for data backup and archival. With
-- Glacier, customers can store their data cost effectively for months,
-- years, or decades. Glacier also enables customers to offload the
-- administrative burdens of operating and scaling storage to AWS, so they
-- don\'t have to worry about capacity planning, hardware provisioning,
-- data replication, hardware failure and recovery, or time-consuming
-- hardware migrations.
--
-- Glacier is a great storage choice when low storage cost is paramount and
-- your data is rarely retrieved. If your application requires fast or
-- frequent access to your data, consider using Amazon S3. For more
-- information, see
-- <http://aws.amazon.com/s3/ Amazon Simple Storage Service (Amazon S3)>.
--
-- You can store any kind of data in any format. There is no maximum limit
-- on the total amount of data you can store in Glacier.
--
-- If you are a first-time user of Glacier, we recommend that you begin by
-- reading the following sections in the /Amazon S3 Glacier Developer
-- Guide/:
--
-- -   <https://docs.aws.amazon.com/amazonglacier/latest/dev/introduction.html What is Amazon S3 Glacier>
--     - This section of the Developer Guide describes the underlying data
--     model, the operations it supports, and the AWS SDKs that you can use
--     to interact with the service.
--
-- -   <https://docs.aws.amazon.com/amazonglacier/latest/dev/amazon-glacier-getting-started.html Getting Started with Amazon S3 Glacier>
--     - The Getting Started section walks you through the process of
--     creating a vault, uploading archives, creating jobs to download
--     archives, retrieving the job output, and deleting archives.
module Network.AWS.Glacier
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** PolicyEnforcedException
    _PolicyEnforcedException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MissingParameterValueException
    _MissingParameterValueException,

    -- ** InsufficientCapacityException
    _InsufficientCapacityException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** RequestTimeoutException
    _RequestTimeoutException,

    -- * Waiters
    -- $waiters

    -- ** VaultExists
    newVaultExists,

    -- ** VaultNotExists
    newVaultNotExists,

    -- * Operations
    -- $operations

    -- ** PurchaseProvisionedCapacity
    PurchaseProvisionedCapacity (PurchaseProvisionedCapacity'),
    newPurchaseProvisionedCapacity,
    PurchaseProvisionedCapacityResponse (PurchaseProvisionedCapacityResponse'),
    newPurchaseProvisionedCapacityResponse,

    -- ** DescribeVault
    DescribeVault (DescribeVault'),
    newDescribeVault,
    DescribeVaultOutput (DescribeVaultOutput'),
    newDescribeVaultOutput,

    -- ** SetVaultAccessPolicy
    SetVaultAccessPolicy (SetVaultAccessPolicy'),
    newSetVaultAccessPolicy,
    SetVaultAccessPolicyResponse (SetVaultAccessPolicyResponse'),
    newSetVaultAccessPolicyResponse,

    -- ** CompleteMultipartUpload
    CompleteMultipartUpload (CompleteMultipartUpload'),
    newCompleteMultipartUpload,
    ArchiveCreationOutput (ArchiveCreationOutput'),
    newArchiveCreationOutput,

    -- ** SetDataRetrievalPolicy
    SetDataRetrievalPolicy (SetDataRetrievalPolicy'),
    newSetDataRetrievalPolicy,
    SetDataRetrievalPolicyResponse (SetDataRetrievalPolicyResponse'),
    newSetDataRetrievalPolicyResponse,

    -- ** UploadArchive
    UploadArchive (UploadArchive'),
    newUploadArchive,
    ArchiveCreationOutput (ArchiveCreationOutput'),
    newArchiveCreationOutput,

    -- ** ListProvisionedCapacity
    ListProvisionedCapacity (ListProvisionedCapacity'),
    newListProvisionedCapacity,
    ListProvisionedCapacityResponse (ListProvisionedCapacityResponse'),
    newListProvisionedCapacityResponse,

    -- ** SetVaultNotifications
    SetVaultNotifications (SetVaultNotifications'),
    newSetVaultNotifications,
    SetVaultNotificationsResponse (SetVaultNotificationsResponse'),
    newSetVaultNotificationsResponse,

    -- ** DeleteVault
    DeleteVault (DeleteVault'),
    newDeleteVault,
    DeleteVaultResponse (DeleteVaultResponse'),
    newDeleteVaultResponse,

    -- ** AbortVaultLock
    AbortVaultLock (AbortVaultLock'),
    newAbortVaultLock,
    AbortVaultLockResponse (AbortVaultLockResponse'),
    newAbortVaultLockResponse,

    -- ** DeleteArchive
    DeleteArchive (DeleteArchive'),
    newDeleteArchive,
    DeleteArchiveResponse (DeleteArchiveResponse'),
    newDeleteArchiveResponse,

    -- ** RemoveTagsFromVault
    RemoveTagsFromVault (RemoveTagsFromVault'),
    newRemoveTagsFromVault,
    RemoveTagsFromVaultResponse (RemoveTagsFromVaultResponse'),
    newRemoveTagsFromVaultResponse,

    -- ** ListVaults (Paginated)
    ListVaults (ListVaults'),
    newListVaults,
    ListVaultsResponse (ListVaultsResponse'),
    newListVaultsResponse,

    -- ** InitiateVaultLock
    InitiateVaultLock (InitiateVaultLock'),
    newInitiateVaultLock,
    InitiateVaultLockResponse (InitiateVaultLockResponse'),
    newInitiateVaultLockResponse,

    -- ** DescribeJob
    DescribeJob (DescribeJob'),
    newDescribeJob,
    GlacierJobDescription (GlacierJobDescription'),
    newGlacierJobDescription,

    -- ** ListTagsForVault
    ListTagsForVault (ListTagsForVault'),
    newListTagsForVault,
    ListTagsForVaultResponse (ListTagsForVaultResponse'),
    newListTagsForVaultResponse,

    -- ** GetVaultLock
    GetVaultLock (GetVaultLock'),
    newGetVaultLock,
    GetVaultLockResponse (GetVaultLockResponse'),
    newGetVaultLockResponse,

    -- ** AbortMultipartUpload
    AbortMultipartUpload (AbortMultipartUpload'),
    newAbortMultipartUpload,
    AbortMultipartUploadResponse (AbortMultipartUploadResponse'),
    newAbortMultipartUploadResponse,

    -- ** DeleteVaultAccessPolicy
    DeleteVaultAccessPolicy (DeleteVaultAccessPolicy'),
    newDeleteVaultAccessPolicy,
    DeleteVaultAccessPolicyResponse (DeleteVaultAccessPolicyResponse'),
    newDeleteVaultAccessPolicyResponse,

    -- ** InitiateJob
    InitiateJob (InitiateJob'),
    newInitiateJob,
    InitiateJobResponse (InitiateJobResponse'),
    newInitiateJobResponse,

    -- ** ListMultipartUploads (Paginated)
    ListMultipartUploads (ListMultipartUploads'),
    newListMultipartUploads,
    ListMultipartUploadsResponse (ListMultipartUploadsResponse'),
    newListMultipartUploadsResponse,

    -- ** AddTagsToVault
    AddTagsToVault (AddTagsToVault'),
    newAddTagsToVault,
    AddTagsToVaultResponse (AddTagsToVaultResponse'),
    newAddTagsToVaultResponse,

    -- ** InitiateMultipartUpload
    InitiateMultipartUpload (InitiateMultipartUpload'),
    newInitiateMultipartUpload,
    InitiateMultipartUploadResponse (InitiateMultipartUploadResponse'),
    newInitiateMultipartUploadResponse,

    -- ** CreateVault
    CreateVault (CreateVault'),
    newCreateVault,
    CreateVaultResponse (CreateVaultResponse'),
    newCreateVaultResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ListParts (Paginated)
    ListParts (ListParts'),
    newListParts,
    ListPartsResponse (ListPartsResponse'),
    newListPartsResponse,

    -- ** GetJobOutput
    GetJobOutput (GetJobOutput'),
    newGetJobOutput,
    GetJobOutputResponse (GetJobOutputResponse'),
    newGetJobOutputResponse,

    -- ** CompleteVaultLock
    CompleteVaultLock (CompleteVaultLock'),
    newCompleteVaultLock,
    CompleteVaultLockResponse (CompleteVaultLockResponse'),
    newCompleteVaultLockResponse,

    -- ** GetVaultAccessPolicy
    GetVaultAccessPolicy (GetVaultAccessPolicy'),
    newGetVaultAccessPolicy,
    GetVaultAccessPolicyResponse (GetVaultAccessPolicyResponse'),
    newGetVaultAccessPolicyResponse,

    -- ** GetDataRetrievalPolicy
    GetDataRetrievalPolicy (GetDataRetrievalPolicy'),
    newGetDataRetrievalPolicy,
    GetDataRetrievalPolicyResponse (GetDataRetrievalPolicyResponse'),
    newGetDataRetrievalPolicyResponse,

    -- ** DeleteVaultNotifications
    DeleteVaultNotifications (DeleteVaultNotifications'),
    newDeleteVaultNotifications,
    DeleteVaultNotificationsResponse (DeleteVaultNotificationsResponse'),
    newDeleteVaultNotificationsResponse,

    -- ** UploadMultipartPart
    UploadMultipartPart (UploadMultipartPart'),
    newUploadMultipartPart,
    UploadMultipartPartResponse (UploadMultipartPartResponse'),
    newUploadMultipartPartResponse,

    -- ** GetVaultNotifications
    GetVaultNotifications (GetVaultNotifications'),
    newGetVaultNotifications,
    GetVaultNotificationsResponse (GetVaultNotificationsResponse'),
    newGetVaultNotificationsResponse,

    -- * Types

    -- ** ActionCode
    ActionCode (..),

    -- ** CannedACL
    CannedACL (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** ExpressionType
    ExpressionType (..),

    -- ** FileHeaderInfo
    FileHeaderInfo (..),

    -- ** Permission
    Permission (..),

    -- ** QuoteFields
    QuoteFields (..),

    -- ** StatusCode
    StatusCode (..),

    -- ** StorageClass
    StorageClass (..),

    -- ** Type
    Type (..),

    -- ** ArchiveCreationOutput
    ArchiveCreationOutput (ArchiveCreationOutput'),
    newArchiveCreationOutput,

    -- ** CSVInput
    CSVInput (CSVInput'),
    newCSVInput,

    -- ** CSVOutput
    CSVOutput (CSVOutput'),
    newCSVOutput,

    -- ** DataRetrievalPolicy
    DataRetrievalPolicy (DataRetrievalPolicy'),
    newDataRetrievalPolicy,

    -- ** DataRetrievalRule
    DataRetrievalRule (DataRetrievalRule'),
    newDataRetrievalRule,

    -- ** DescribeVaultOutput
    DescribeVaultOutput (DescribeVaultOutput'),
    newDescribeVaultOutput,

    -- ** Encryption
    Encryption (Encryption'),
    newEncryption,

    -- ** GlacierJobDescription
    GlacierJobDescription (GlacierJobDescription'),
    newGlacierJobDescription,

    -- ** Grant
    Grant (Grant'),
    newGrant,

    -- ** Grantee
    Grantee (Grantee'),
    newGrantee,

    -- ** InputSerialization
    InputSerialization (InputSerialization'),
    newInputSerialization,

    -- ** InventoryRetrievalJobDescription
    InventoryRetrievalJobDescription (InventoryRetrievalJobDescription'),
    newInventoryRetrievalJobDescription,

    -- ** InventoryRetrievalJobInput
    InventoryRetrievalJobInput (InventoryRetrievalJobInput'),
    newInventoryRetrievalJobInput,

    -- ** JobParameters
    JobParameters (JobParameters'),
    newJobParameters,

    -- ** OutputLocation
    OutputLocation (OutputLocation'),
    newOutputLocation,

    -- ** OutputSerialization
    OutputSerialization (OutputSerialization'),
    newOutputSerialization,

    -- ** PartListElement
    PartListElement (PartListElement'),
    newPartListElement,

    -- ** ProvisionedCapacityDescription
    ProvisionedCapacityDescription (ProvisionedCapacityDescription'),
    newProvisionedCapacityDescription,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** SelectParameters
    SelectParameters (SelectParameters'),
    newSelectParameters,

    -- ** UploadListElement
    UploadListElement (UploadListElement'),
    newUploadListElement,

    -- ** VaultAccessPolicy
    VaultAccessPolicy (VaultAccessPolicy'),
    newVaultAccessPolicy,

    -- ** VaultLockPolicy
    VaultLockPolicy (VaultLockPolicy'),
    newVaultLockPolicy,

    -- ** VaultNotificationConfig
    VaultNotificationConfig (VaultNotificationConfig'),
    newVaultNotificationConfig,
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
import Network.AWS.Glacier.Lens
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
import Network.AWS.Glacier.Types
import Network.AWS.Glacier.UploadArchive
import Network.AWS.Glacier.UploadMultipartPart
import Network.AWS.Glacier.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Glacier'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
