{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DataExchange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Data Exchange is a service that makes it easy for AWS customers to
-- exchange data in the cloud. You can use the AWS Data Exchange APIs to
-- create, update, manage, and access file-based data set in the AWS Cloud.
--
-- As a subscriber, you can view and access the data sets that you have an
-- entitlement to through a subscription. You can use the APIs to download
-- or copy your entitled data sets to Amazon Simple Storage Service (Amazon
-- S3) for use across a variety of AWS analytics and machine learning
-- services.
--
-- As a provider, you can create and manage your data sets that you would
-- like to publish to a product. Being able to package and provide your
-- data sets into products requires a few steps to determine eligibility.
-- For more information, visit the /AWS Data Exchange User Guide/.
--
-- A data set is a collection of data that can be changed or updated over
-- time. Data sets can be updated using revisions, which represent a new
-- version or incremental change to a data set. A revision contains one or
-- more assets. An asset in AWS Data Exchange is a piece of data that can
-- be stored as an Amazon S3 object, Redshift datashare, API Gateway API,
-- AWS Lake Formation data permission, or Amazon S3 data access. The asset
-- can be a structured data file, an image file, or some other data file.
-- Jobs are asynchronous import or export operations used to create or copy
-- assets.
module Amazonka.DataExchange
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceLimitExceededException
    _ServiceLimitExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** CreateDataSet
    CreateDataSet (CreateDataSet'),
    newCreateDataSet,
    CreateDataSetResponse (CreateDataSetResponse'),
    newCreateDataSetResponse,

    -- ** CreateEventAction
    CreateEventAction (CreateEventAction'),
    newCreateEventAction,
    CreateEventActionResponse (CreateEventActionResponse'),
    newCreateEventActionResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** CreateRevision
    CreateRevision (CreateRevision'),
    newCreateRevision,
    CreateRevisionResponse (CreateRevisionResponse'),
    newCreateRevisionResponse,

    -- ** DeleteAsset
    DeleteAsset (DeleteAsset'),
    newDeleteAsset,
    DeleteAssetResponse (DeleteAssetResponse'),
    newDeleteAssetResponse,

    -- ** DeleteDataSet
    DeleteDataSet (DeleteDataSet'),
    newDeleteDataSet,
    DeleteDataSetResponse (DeleteDataSetResponse'),
    newDeleteDataSetResponse,

    -- ** DeleteEventAction
    DeleteEventAction (DeleteEventAction'),
    newDeleteEventAction,
    DeleteEventActionResponse (DeleteEventActionResponse'),
    newDeleteEventActionResponse,

    -- ** DeleteRevision
    DeleteRevision (DeleteRevision'),
    newDeleteRevision,
    DeleteRevisionResponse (DeleteRevisionResponse'),
    newDeleteRevisionResponse,

    -- ** GetAsset
    GetAsset (GetAsset'),
    newGetAsset,
    GetAssetResponse (GetAssetResponse'),
    newGetAssetResponse,

    -- ** GetDataSet
    GetDataSet (GetDataSet'),
    newGetDataSet,
    GetDataSetResponse (GetDataSetResponse'),
    newGetDataSetResponse,

    -- ** GetEventAction
    GetEventAction (GetEventAction'),
    newGetEventAction,
    GetEventActionResponse (GetEventActionResponse'),
    newGetEventActionResponse,

    -- ** GetJob
    GetJob (GetJob'),
    newGetJob,
    GetJobResponse (GetJobResponse'),
    newGetJobResponse,

    -- ** GetRevision
    GetRevision (GetRevision'),
    newGetRevision,
    GetRevisionResponse (GetRevisionResponse'),
    newGetRevisionResponse,

    -- ** ListDataSetRevisions (Paginated)
    ListDataSetRevisions (ListDataSetRevisions'),
    newListDataSetRevisions,
    ListDataSetRevisionsResponse (ListDataSetRevisionsResponse'),
    newListDataSetRevisionsResponse,

    -- ** ListDataSets (Paginated)
    ListDataSets (ListDataSets'),
    newListDataSets,
    ListDataSetsResponse (ListDataSetsResponse'),
    newListDataSetsResponse,

    -- ** ListEventActions (Paginated)
    ListEventActions (ListEventActions'),
    newListEventActions,
    ListEventActionsResponse (ListEventActionsResponse'),
    newListEventActionsResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ListRevisionAssets (Paginated)
    ListRevisionAssets (ListRevisionAssets'),
    newListRevisionAssets,
    ListRevisionAssetsResponse (ListRevisionAssetsResponse'),
    newListRevisionAssetsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RevokeRevision
    RevokeRevision (RevokeRevision'),
    newRevokeRevision,
    RevokeRevisionResponse (RevokeRevisionResponse'),
    newRevokeRevisionResponse,

    -- ** SendApiAsset
    SendApiAsset (SendApiAsset'),
    newSendApiAsset,
    SendApiAssetResponse (SendApiAssetResponse'),
    newSendApiAssetResponse,

    -- ** StartJob
    StartJob (StartJob'),
    newStartJob,
    StartJobResponse (StartJobResponse'),
    newStartJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAsset
    UpdateAsset (UpdateAsset'),
    newUpdateAsset,
    UpdateAssetResponse (UpdateAssetResponse'),
    newUpdateAssetResponse,

    -- ** UpdateDataSet
    UpdateDataSet (UpdateDataSet'),
    newUpdateDataSet,
    UpdateDataSetResponse (UpdateDataSetResponse'),
    newUpdateDataSetResponse,

    -- ** UpdateEventAction
    UpdateEventAction (UpdateEventAction'),
    newUpdateEventAction,
    UpdateEventActionResponse (UpdateEventActionResponse'),
    newUpdateEventActionResponse,

    -- ** UpdateRevision
    UpdateRevision (UpdateRevision'),
    newUpdateRevision,
    UpdateRevisionResponse (UpdateRevisionResponse'),
    newUpdateRevisionResponse,

    -- * Types

    -- ** AssetType
    AssetType (..),

    -- ** Code
    Code (..),

    -- ** DatabaseLFTagPolicyPermission
    DatabaseLFTagPolicyPermission (..),

    -- ** JobErrorLimitName
    JobErrorLimitName (..),

    -- ** JobErrorResourceTypes
    JobErrorResourceTypes (..),

    -- ** LFPermission
    LFPermission (..),

    -- ** LFResourceType
    LFResourceType (..),

    -- ** LakeFormationDataPermissionType
    LakeFormationDataPermissionType (..),

    -- ** Origin
    Origin (..),

    -- ** ProtocolType
    ProtocolType (..),

    -- ** ServerSideEncryptionTypes
    ServerSideEncryptionTypes (..),

    -- ** State
    State (..),

    -- ** TableTagPolicyLFPermission
    TableTagPolicyLFPermission (..),

    -- ** Type
    Type (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** ApiGatewayApiAsset
    ApiGatewayApiAsset (ApiGatewayApiAsset'),
    newApiGatewayApiAsset,

    -- ** AssetDestinationEntry
    AssetDestinationEntry (AssetDestinationEntry'),
    newAssetDestinationEntry,

    -- ** AssetDetails
    AssetDetails (AssetDetails'),
    newAssetDetails,

    -- ** AssetEntry
    AssetEntry (AssetEntry'),
    newAssetEntry,

    -- ** AssetSourceEntry
    AssetSourceEntry (AssetSourceEntry'),
    newAssetSourceEntry,

    -- ** AutoExportRevisionDestinationEntry
    AutoExportRevisionDestinationEntry (AutoExportRevisionDestinationEntry'),
    newAutoExportRevisionDestinationEntry,

    -- ** AutoExportRevisionToS3RequestDetails
    AutoExportRevisionToS3RequestDetails (AutoExportRevisionToS3RequestDetails'),
    newAutoExportRevisionToS3RequestDetails,

    -- ** CreateS3DataAccessFromS3BucketRequestDetails
    CreateS3DataAccessFromS3BucketRequestDetails (CreateS3DataAccessFromS3BucketRequestDetails'),
    newCreateS3DataAccessFromS3BucketRequestDetails,

    -- ** CreateS3DataAccessFromS3BucketResponseDetails
    CreateS3DataAccessFromS3BucketResponseDetails (CreateS3DataAccessFromS3BucketResponseDetails'),
    newCreateS3DataAccessFromS3BucketResponseDetails,

    -- ** DataSetEntry
    DataSetEntry (DataSetEntry'),
    newDataSetEntry,

    -- ** DatabaseLFTagPolicy
    DatabaseLFTagPolicy (DatabaseLFTagPolicy'),
    newDatabaseLFTagPolicy,

    -- ** DatabaseLFTagPolicyAndPermissions
    DatabaseLFTagPolicyAndPermissions (DatabaseLFTagPolicyAndPermissions'),
    newDatabaseLFTagPolicyAndPermissions,

    -- ** Details
    Details (Details'),
    newDetails,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** EventActionEntry
    EventActionEntry (EventActionEntry'),
    newEventActionEntry,

    -- ** ExportAssetToSignedUrlRequestDetails
    ExportAssetToSignedUrlRequestDetails (ExportAssetToSignedUrlRequestDetails'),
    newExportAssetToSignedUrlRequestDetails,

    -- ** ExportAssetToSignedUrlResponseDetails
    ExportAssetToSignedUrlResponseDetails (ExportAssetToSignedUrlResponseDetails'),
    newExportAssetToSignedUrlResponseDetails,

    -- ** ExportAssetsToS3RequestDetails
    ExportAssetsToS3RequestDetails (ExportAssetsToS3RequestDetails'),
    newExportAssetsToS3RequestDetails,

    -- ** ExportAssetsToS3ResponseDetails
    ExportAssetsToS3ResponseDetails (ExportAssetsToS3ResponseDetails'),
    newExportAssetsToS3ResponseDetails,

    -- ** ExportRevisionsToS3RequestDetails
    ExportRevisionsToS3RequestDetails (ExportRevisionsToS3RequestDetails'),
    newExportRevisionsToS3RequestDetails,

    -- ** ExportRevisionsToS3ResponseDetails
    ExportRevisionsToS3ResponseDetails (ExportRevisionsToS3ResponseDetails'),
    newExportRevisionsToS3ResponseDetails,

    -- ** ExportServerSideEncryption
    ExportServerSideEncryption (ExportServerSideEncryption'),
    newExportServerSideEncryption,

    -- ** ImportAssetFromApiGatewayApiRequestDetails
    ImportAssetFromApiGatewayApiRequestDetails (ImportAssetFromApiGatewayApiRequestDetails'),
    newImportAssetFromApiGatewayApiRequestDetails,

    -- ** ImportAssetFromApiGatewayApiResponseDetails
    ImportAssetFromApiGatewayApiResponseDetails (ImportAssetFromApiGatewayApiResponseDetails'),
    newImportAssetFromApiGatewayApiResponseDetails,

    -- ** ImportAssetFromSignedUrlJobErrorDetails
    ImportAssetFromSignedUrlJobErrorDetails (ImportAssetFromSignedUrlJobErrorDetails'),
    newImportAssetFromSignedUrlJobErrorDetails,

    -- ** ImportAssetFromSignedUrlRequestDetails
    ImportAssetFromSignedUrlRequestDetails (ImportAssetFromSignedUrlRequestDetails'),
    newImportAssetFromSignedUrlRequestDetails,

    -- ** ImportAssetFromSignedUrlResponseDetails
    ImportAssetFromSignedUrlResponseDetails (ImportAssetFromSignedUrlResponseDetails'),
    newImportAssetFromSignedUrlResponseDetails,

    -- ** ImportAssetsFromLakeFormationTagPolicyRequestDetails
    ImportAssetsFromLakeFormationTagPolicyRequestDetails (ImportAssetsFromLakeFormationTagPolicyRequestDetails'),
    newImportAssetsFromLakeFormationTagPolicyRequestDetails,

    -- ** ImportAssetsFromLakeFormationTagPolicyResponseDetails
    ImportAssetsFromLakeFormationTagPolicyResponseDetails (ImportAssetsFromLakeFormationTagPolicyResponseDetails'),
    newImportAssetsFromLakeFormationTagPolicyResponseDetails,

    -- ** ImportAssetsFromRedshiftDataSharesRequestDetails
    ImportAssetsFromRedshiftDataSharesRequestDetails (ImportAssetsFromRedshiftDataSharesRequestDetails'),
    newImportAssetsFromRedshiftDataSharesRequestDetails,

    -- ** ImportAssetsFromRedshiftDataSharesResponseDetails
    ImportAssetsFromRedshiftDataSharesResponseDetails (ImportAssetsFromRedshiftDataSharesResponseDetails'),
    newImportAssetsFromRedshiftDataSharesResponseDetails,

    -- ** ImportAssetsFromS3RequestDetails
    ImportAssetsFromS3RequestDetails (ImportAssetsFromS3RequestDetails'),
    newImportAssetsFromS3RequestDetails,

    -- ** ImportAssetsFromS3ResponseDetails
    ImportAssetsFromS3ResponseDetails (ImportAssetsFromS3ResponseDetails'),
    newImportAssetsFromS3ResponseDetails,

    -- ** JobEntry
    JobEntry (JobEntry'),
    newJobEntry,

    -- ** JobError
    JobError (JobError'),
    newJobError,

    -- ** LFResourceDetails
    LFResourceDetails (LFResourceDetails'),
    newLFResourceDetails,

    -- ** LFTag
    LFTag (LFTag'),
    newLFTag,

    -- ** LFTagPolicyDetails
    LFTagPolicyDetails (LFTagPolicyDetails'),
    newLFTagPolicyDetails,

    -- ** LakeFormationDataPermissionAsset
    LakeFormationDataPermissionAsset (LakeFormationDataPermissionAsset'),
    newLakeFormationDataPermissionAsset,

    -- ** LakeFormationDataPermissionDetails
    LakeFormationDataPermissionDetails (LakeFormationDataPermissionDetails'),
    newLakeFormationDataPermissionDetails,

    -- ** OriginDetails
    OriginDetails (OriginDetails'),
    newOriginDetails,

    -- ** RedshiftDataShareAsset
    RedshiftDataShareAsset (RedshiftDataShareAsset'),
    newRedshiftDataShareAsset,

    -- ** RedshiftDataShareAssetSourceEntry
    RedshiftDataShareAssetSourceEntry (RedshiftDataShareAssetSourceEntry'),
    newRedshiftDataShareAssetSourceEntry,

    -- ** RequestDetails
    RequestDetails (RequestDetails'),
    newRequestDetails,

    -- ** ResponseDetails
    ResponseDetails (ResponseDetails'),
    newResponseDetails,

    -- ** RevisionDestinationEntry
    RevisionDestinationEntry (RevisionDestinationEntry'),
    newRevisionDestinationEntry,

    -- ** RevisionEntry
    RevisionEntry (RevisionEntry'),
    newRevisionEntry,

    -- ** RevisionPublished
    RevisionPublished (RevisionPublished'),
    newRevisionPublished,

    -- ** S3DataAccessAsset
    S3DataAccessAsset (S3DataAccessAsset'),
    newS3DataAccessAsset,

    -- ** S3DataAccessAssetSourceEntry
    S3DataAccessAssetSourceEntry (S3DataAccessAssetSourceEntry'),
    newS3DataAccessAssetSourceEntry,

    -- ** S3SnapshotAsset
    S3SnapshotAsset (S3SnapshotAsset'),
    newS3SnapshotAsset,

    -- ** TableLFTagPolicy
    TableLFTagPolicy (TableLFTagPolicy'),
    newTableLFTagPolicy,

    -- ** TableLFTagPolicyAndPermissions
    TableLFTagPolicyAndPermissions (TableLFTagPolicyAndPermissions'),
    newTableLFTagPolicyAndPermissions,
  )
where

import Amazonka.DataExchange.CancelJob
import Amazonka.DataExchange.CreateDataSet
import Amazonka.DataExchange.CreateEventAction
import Amazonka.DataExchange.CreateJob
import Amazonka.DataExchange.CreateRevision
import Amazonka.DataExchange.DeleteAsset
import Amazonka.DataExchange.DeleteDataSet
import Amazonka.DataExchange.DeleteEventAction
import Amazonka.DataExchange.DeleteRevision
import Amazonka.DataExchange.GetAsset
import Amazonka.DataExchange.GetDataSet
import Amazonka.DataExchange.GetEventAction
import Amazonka.DataExchange.GetJob
import Amazonka.DataExchange.GetRevision
import Amazonka.DataExchange.Lens
import Amazonka.DataExchange.ListDataSetRevisions
import Amazonka.DataExchange.ListDataSets
import Amazonka.DataExchange.ListEventActions
import Amazonka.DataExchange.ListJobs
import Amazonka.DataExchange.ListRevisionAssets
import Amazonka.DataExchange.ListTagsForResource
import Amazonka.DataExchange.RevokeRevision
import Amazonka.DataExchange.SendApiAsset
import Amazonka.DataExchange.StartJob
import Amazonka.DataExchange.TagResource
import Amazonka.DataExchange.Types
import Amazonka.DataExchange.UntagResource
import Amazonka.DataExchange.UpdateAsset
import Amazonka.DataExchange.UpdateDataSet
import Amazonka.DataExchange.UpdateEventAction
import Amazonka.DataExchange.UpdateRevision
import Amazonka.DataExchange.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DataExchange'.

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
