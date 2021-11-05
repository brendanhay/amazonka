{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DataExchange
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- entitlement to through a subscription. You can use the APIS to download
-- or copy your entitled data sets to Amazon S3 for use across a variety of
-- AWS analytics and machine learning services.
--
-- As a provider, you can create and manage your data sets that you would
-- like to publish to a product. Being able to package and provide your
-- data sets into products requires a few steps to determine eligibility.
-- For more information, visit the AWS Data Exchange User Guide.
--
-- A data set is a collection of data that can be changed or updated over
-- time. Data sets can be updated using revisions, which represent a new
-- version or incremental change to a data set. A revision contains one or
-- more assets. An asset in AWS Data Exchange is a piece of data that can
-- be stored as an Amazon S3 object. The asset can be a structured data
-- file, an image file, or some other data file. Jobs are asynchronous
-- import or export operations used to create or copy assets.
module Amazonka.DataExchange
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceLimitExceededException
    _ServiceLimitExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetEventAction
    GetEventAction (GetEventAction'),
    newGetEventAction,
    GetEventActionResponse (GetEventActionResponse'),
    newGetEventActionResponse,

    -- ** CreateRevision
    CreateRevision (CreateRevision'),
    newCreateRevision,
    CreateRevisionResponse (CreateRevisionResponse'),
    newCreateRevisionResponse,

    -- ** ListRevisionAssets (Paginated)
    ListRevisionAssets (ListRevisionAssets'),
    newListRevisionAssets,
    ListRevisionAssetsResponse (ListRevisionAssetsResponse'),
    newListRevisionAssetsResponse,

    -- ** DeleteRevision
    DeleteRevision (DeleteRevision'),
    newDeleteRevision,
    DeleteRevisionResponse (DeleteRevisionResponse'),
    newDeleteRevisionResponse,

    -- ** UpdateRevision
    UpdateRevision (UpdateRevision'),
    newUpdateRevision,
    UpdateRevisionResponse (UpdateRevisionResponse'),
    newUpdateRevisionResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetRevision
    GetRevision (GetRevision'),
    newGetRevision,
    GetRevisionResponse (GetRevisionResponse'),
    newGetRevisionResponse,

    -- ** DeleteDataSet
    DeleteDataSet (DeleteDataSet'),
    newDeleteDataSet,
    DeleteDataSetResponse (DeleteDataSetResponse'),
    newDeleteDataSetResponse,

    -- ** UpdateDataSet
    UpdateDataSet (UpdateDataSet'),
    newUpdateDataSet,
    UpdateDataSetResponse (UpdateDataSetResponse'),
    newUpdateDataSetResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** ListEventActions (Paginated)
    ListEventActions (ListEventActions'),
    newListEventActions,
    ListEventActionsResponse (ListEventActionsResponse'),
    newListEventActionsResponse,

    -- ** GetAsset
    GetAsset (GetAsset'),
    newGetAsset,
    GetAssetResponse (GetAssetResponse'),
    newGetAssetResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** CreateDataSet
    CreateDataSet (CreateDataSet'),
    newCreateDataSet,
    CreateDataSetResponse (CreateDataSetResponse'),
    newCreateDataSetResponse,

    -- ** DeleteAsset
    DeleteAsset (DeleteAsset'),
    newDeleteAsset,
    DeleteAssetResponse (DeleteAssetResponse'),
    newDeleteAssetResponse,

    -- ** UpdateAsset
    UpdateAsset (UpdateAsset'),
    newUpdateAsset,
    UpdateAssetResponse (UpdateAssetResponse'),
    newUpdateAssetResponse,

    -- ** GetJob
    GetJob (GetJob'),
    newGetJob,
    GetJobResponse (GetJobResponse'),
    newGetJobResponse,

    -- ** GetDataSet
    GetDataSet (GetDataSet'),
    newGetDataSet,
    GetDataSetResponse (GetDataSetResponse'),
    newGetDataSetResponse,

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

    -- ** ListDataSetRevisions (Paginated)
    ListDataSetRevisions (ListDataSetRevisions'),
    newListDataSetRevisions,
    ListDataSetRevisionsResponse (ListDataSetRevisionsResponse'),
    newListDataSetRevisionsResponse,

    -- ** DeleteEventAction
    DeleteEventAction (DeleteEventAction'),
    newDeleteEventAction,
    DeleteEventActionResponse (DeleteEventActionResponse'),
    newDeleteEventActionResponse,

    -- ** UpdateEventAction
    UpdateEventAction (UpdateEventAction'),
    newUpdateEventAction,
    UpdateEventActionResponse (UpdateEventActionResponse'),
    newUpdateEventActionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListDataSets (Paginated)
    ListDataSets (ListDataSets'),
    newListDataSets,
    ListDataSetsResponse (ListDataSetsResponse'),
    newListDataSetsResponse,

    -- ** CreateEventAction
    CreateEventAction (CreateEventAction'),
    newCreateEventAction,
    CreateEventActionResponse (CreateEventActionResponse'),
    newCreateEventActionResponse,

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- * Types

    -- ** AssetType
    AssetType (..),

    -- ** Code
    Code (..),

    -- ** JobErrorLimitName
    JobErrorLimitName (..),

    -- ** JobErrorResourceTypes
    JobErrorResourceTypes (..),

    -- ** Origin
    Origin (..),

    -- ** ServerSideEncryptionTypes
    ServerSideEncryptionTypes (..),

    -- ** State
    State (..),

    -- ** Type
    Type (..),

    -- ** Action
    Action (Action'),
    newAction,

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

    -- ** DataSetEntry
    DataSetEntry (DataSetEntry'),
    newDataSetEntry,

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

    -- ** ImportAssetFromSignedUrlJobErrorDetails
    ImportAssetFromSignedUrlJobErrorDetails (ImportAssetFromSignedUrlJobErrorDetails'),
    newImportAssetFromSignedUrlJobErrorDetails,

    -- ** ImportAssetFromSignedUrlRequestDetails
    ImportAssetFromSignedUrlRequestDetails (ImportAssetFromSignedUrlRequestDetails'),
    newImportAssetFromSignedUrlRequestDetails,

    -- ** ImportAssetFromSignedUrlResponseDetails
    ImportAssetFromSignedUrlResponseDetails (ImportAssetFromSignedUrlResponseDetails'),
    newImportAssetFromSignedUrlResponseDetails,

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

    -- ** S3SnapshotAsset
    S3SnapshotAsset (S3SnapshotAsset'),
    newS3SnapshotAsset,
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
