{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataExchange.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Lens
  ( -- * Operations

    -- ** GetEventAction
    getEventAction_eventActionId,
    getEventActionResponse_event,
    getEventActionResponse_arn,
    getEventActionResponse_createdAt,
    getEventActionResponse_action,
    getEventActionResponse_id,
    getEventActionResponse_updatedAt,
    getEventActionResponse_httpStatus,

    -- ** CreateRevision
    createRevision_comment,
    createRevision_tags,
    createRevision_dataSetId,
    createRevisionResponse_arn,
    createRevisionResponse_createdAt,
    createRevisionResponse_sourceId,
    createRevisionResponse_finalized,
    createRevisionResponse_dataSetId,
    createRevisionResponse_id,
    createRevisionResponse_updatedAt,
    createRevisionResponse_comment,
    createRevisionResponse_tags,
    createRevisionResponse_httpStatus,

    -- ** ListRevisionAssets
    listRevisionAssets_nextToken,
    listRevisionAssets_maxResults,
    listRevisionAssets_revisionId,
    listRevisionAssets_dataSetId,
    listRevisionAssetsResponse_nextToken,
    listRevisionAssetsResponse_assets,
    listRevisionAssetsResponse_httpStatus,

    -- ** DeleteRevision
    deleteRevision_revisionId,
    deleteRevision_dataSetId,

    -- ** UpdateRevision
    updateRevision_finalized,
    updateRevision_comment,
    updateRevision_revisionId,
    updateRevision_dataSetId,
    updateRevisionResponse_arn,
    updateRevisionResponse_createdAt,
    updateRevisionResponse_sourceId,
    updateRevisionResponse_finalized,
    updateRevisionResponse_dataSetId,
    updateRevisionResponse_id,
    updateRevisionResponse_updatedAt,
    updateRevisionResponse_comment,
    updateRevisionResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetRevision
    getRevision_revisionId,
    getRevision_dataSetId,
    getRevisionResponse_arn,
    getRevisionResponse_createdAt,
    getRevisionResponse_sourceId,
    getRevisionResponse_finalized,
    getRevisionResponse_dataSetId,
    getRevisionResponse_id,
    getRevisionResponse_updatedAt,
    getRevisionResponse_comment,
    getRevisionResponse_tags,
    getRevisionResponse_httpStatus,

    -- ** DeleteDataSet
    deleteDataSet_dataSetId,

    -- ** UpdateDataSet
    updateDataSet_name,
    updateDataSet_description,
    updateDataSet_dataSetId,
    updateDataSetResponse_origin,
    updateDataSetResponse_arn,
    updateDataSetResponse_createdAt,
    updateDataSetResponse_sourceId,
    updateDataSetResponse_originDetails,
    updateDataSetResponse_name,
    updateDataSetResponse_id,
    updateDataSetResponse_assetType,
    updateDataSetResponse_updatedAt,
    updateDataSetResponse_description,
    updateDataSetResponse_httpStatus,

    -- ** CreateJob
    createJob_type,
    createJob_details,
    createJobResponse_state,
    createJobResponse_arn,
    createJobResponse_createdAt,
    createJobResponse_details,
    createJobResponse_id,
    createJobResponse_type,
    createJobResponse_updatedAt,
    createJobResponse_errors,
    createJobResponse_httpStatus,

    -- ** ListEventActions
    listEventActions_eventSourceId,
    listEventActions_nextToken,
    listEventActions_maxResults,
    listEventActionsResponse_eventActions,
    listEventActionsResponse_nextToken,
    listEventActionsResponse_httpStatus,

    -- ** GetAsset
    getAsset_revisionId,
    getAsset_assetId,
    getAsset_dataSetId,
    getAssetResponse_arn,
    getAssetResponse_createdAt,
    getAssetResponse_sourceId,
    getAssetResponse_dataSetId,
    getAssetResponse_name,
    getAssetResponse_assetDetails,
    getAssetResponse_id,
    getAssetResponse_assetType,
    getAssetResponse_updatedAt,
    getAssetResponse_revisionId,
    getAssetResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_dataSetId,
    listJobs_maxResults,
    listJobs_revisionId,
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** CreateDataSet
    createDataSet_tags,
    createDataSet_assetType,
    createDataSet_description,
    createDataSet_name,
    createDataSetResponse_origin,
    createDataSetResponse_arn,
    createDataSetResponse_createdAt,
    createDataSetResponse_sourceId,
    createDataSetResponse_originDetails,
    createDataSetResponse_name,
    createDataSetResponse_id,
    createDataSetResponse_assetType,
    createDataSetResponse_updatedAt,
    createDataSetResponse_description,
    createDataSetResponse_tags,
    createDataSetResponse_httpStatus,

    -- ** DeleteAsset
    deleteAsset_revisionId,
    deleteAsset_assetId,
    deleteAsset_dataSetId,

    -- ** UpdateAsset
    updateAsset_revisionId,
    updateAsset_assetId,
    updateAsset_dataSetId,
    updateAsset_name,
    updateAssetResponse_arn,
    updateAssetResponse_createdAt,
    updateAssetResponse_sourceId,
    updateAssetResponse_dataSetId,
    updateAssetResponse_name,
    updateAssetResponse_assetDetails,
    updateAssetResponse_id,
    updateAssetResponse_assetType,
    updateAssetResponse_updatedAt,
    updateAssetResponse_revisionId,
    updateAssetResponse_httpStatus,

    -- ** GetJob
    getJob_jobId,
    getJobResponse_state,
    getJobResponse_arn,
    getJobResponse_createdAt,
    getJobResponse_details,
    getJobResponse_id,
    getJobResponse_type,
    getJobResponse_updatedAt,
    getJobResponse_errors,
    getJobResponse_httpStatus,

    -- ** GetDataSet
    getDataSet_dataSetId,
    getDataSetResponse_origin,
    getDataSetResponse_arn,
    getDataSetResponse_createdAt,
    getDataSetResponse_sourceId,
    getDataSetResponse_originDetails,
    getDataSetResponse_name,
    getDataSetResponse_id,
    getDataSetResponse_assetType,
    getDataSetResponse_updatedAt,
    getDataSetResponse_description,
    getDataSetResponse_tags,
    getDataSetResponse_httpStatus,

    -- ** StartJob
    startJob_jobId,
    startJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** ListDataSetRevisions
    listDataSetRevisions_nextToken,
    listDataSetRevisions_maxResults,
    listDataSetRevisions_dataSetId,
    listDataSetRevisionsResponse_nextToken,
    listDataSetRevisionsResponse_revisions,
    listDataSetRevisionsResponse_httpStatus,

    -- ** DeleteEventAction
    deleteEventAction_eventActionId,

    -- ** UpdateEventAction
    updateEventAction_action,
    updateEventAction_eventActionId,
    updateEventActionResponse_event,
    updateEventActionResponse_arn,
    updateEventActionResponse_createdAt,
    updateEventActionResponse_action,
    updateEventActionResponse_id,
    updateEventActionResponse_updatedAt,
    updateEventActionResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** ListDataSets
    listDataSets_origin,
    listDataSets_nextToken,
    listDataSets_maxResults,
    listDataSetsResponse_nextToken,
    listDataSetsResponse_dataSets,
    listDataSetsResponse_httpStatus,

    -- ** CreateEventAction
    createEventAction_action,
    createEventAction_event,
    createEventActionResponse_event,
    createEventActionResponse_arn,
    createEventActionResponse_createdAt,
    createEventActionResponse_action,
    createEventActionResponse_id,
    createEventActionResponse_updatedAt,
    createEventActionResponse_httpStatus,

    -- ** CancelJob
    cancelJob_jobId,

    -- * Types

    -- ** Action
    action_exportRevisionToS3,

    -- ** AssetDestinationEntry
    assetDestinationEntry_key,
    assetDestinationEntry_bucket,
    assetDestinationEntry_assetId,

    -- ** AssetDetails
    assetDetails_s3SnapshotAsset,
    assetDetails_redshiftDataShareAsset,

    -- ** AssetEntry
    assetEntry_sourceId,
    assetEntry_assetType,
    assetEntry_createdAt,
    assetEntry_dataSetId,
    assetEntry_id,
    assetEntry_arn,
    assetEntry_assetDetails,
    assetEntry_updatedAt,
    assetEntry_revisionId,
    assetEntry_name,

    -- ** AssetSourceEntry
    assetSourceEntry_bucket,
    assetSourceEntry_key,

    -- ** AutoExportRevisionDestinationEntry
    autoExportRevisionDestinationEntry_keyPattern,
    autoExportRevisionDestinationEntry_bucket,

    -- ** AutoExportRevisionToS3RequestDetails
    autoExportRevisionToS3RequestDetails_encryption,
    autoExportRevisionToS3RequestDetails_revisionDestination,

    -- ** DataSetEntry
    dataSetEntry_sourceId,
    dataSetEntry_originDetails,
    dataSetEntry_origin,
    dataSetEntry_assetType,
    dataSetEntry_description,
    dataSetEntry_createdAt,
    dataSetEntry_id,
    dataSetEntry_arn,
    dataSetEntry_updatedAt,
    dataSetEntry_name,

    -- ** Details
    details_importAssetFromSignedUrlJobErrorDetails,
    details_importAssetsFromS3JobErrorDetails,

    -- ** Event
    event_revisionPublished,

    -- ** EventActionEntry
    eventActionEntry_action,
    eventActionEntry_createdAt,
    eventActionEntry_event,
    eventActionEntry_id,
    eventActionEntry_arn,
    eventActionEntry_updatedAt,

    -- ** ExportAssetToSignedUrlRequestDetails
    exportAssetToSignedUrlRequestDetails_dataSetId,
    exportAssetToSignedUrlRequestDetails_assetId,
    exportAssetToSignedUrlRequestDetails_revisionId,

    -- ** ExportAssetToSignedUrlResponseDetails
    exportAssetToSignedUrlResponseDetails_signedUrl,
    exportAssetToSignedUrlResponseDetails_signedUrlExpiresAt,
    exportAssetToSignedUrlResponseDetails_dataSetId,
    exportAssetToSignedUrlResponseDetails_assetId,
    exportAssetToSignedUrlResponseDetails_revisionId,

    -- ** ExportAssetsToS3RequestDetails
    exportAssetsToS3RequestDetails_encryption,
    exportAssetsToS3RequestDetails_assetDestinations,
    exportAssetsToS3RequestDetails_dataSetId,
    exportAssetsToS3RequestDetails_revisionId,

    -- ** ExportAssetsToS3ResponseDetails
    exportAssetsToS3ResponseDetails_encryption,
    exportAssetsToS3ResponseDetails_assetDestinations,
    exportAssetsToS3ResponseDetails_dataSetId,
    exportAssetsToS3ResponseDetails_revisionId,

    -- ** ExportRevisionsToS3RequestDetails
    exportRevisionsToS3RequestDetails_encryption,
    exportRevisionsToS3RequestDetails_revisionDestinations,
    exportRevisionsToS3RequestDetails_dataSetId,

    -- ** ExportRevisionsToS3ResponseDetails
    exportRevisionsToS3ResponseDetails_encryption,
    exportRevisionsToS3ResponseDetails_eventActionArn,
    exportRevisionsToS3ResponseDetails_revisionDestinations,
    exportRevisionsToS3ResponseDetails_dataSetId,

    -- ** ExportServerSideEncryption
    exportServerSideEncryption_kmsKeyArn,
    exportServerSideEncryption_type,

    -- ** ImportAssetFromSignedUrlJobErrorDetails
    importAssetFromSignedUrlJobErrorDetails_assetName,

    -- ** ImportAssetFromSignedUrlRequestDetails
    importAssetFromSignedUrlRequestDetails_dataSetId,
    importAssetFromSignedUrlRequestDetails_md5Hash,
    importAssetFromSignedUrlRequestDetails_assetName,
    importAssetFromSignedUrlRequestDetails_revisionId,

    -- ** ImportAssetFromSignedUrlResponseDetails
    importAssetFromSignedUrlResponseDetails_signedUrl,
    importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt,
    importAssetFromSignedUrlResponseDetails_md5Hash,
    importAssetFromSignedUrlResponseDetails_dataSetId,
    importAssetFromSignedUrlResponseDetails_assetName,
    importAssetFromSignedUrlResponseDetails_revisionId,

    -- ** ImportAssetsFromRedshiftDataSharesRequestDetails
    importAssetsFromRedshiftDataSharesRequestDetails_dataSetId,
    importAssetsFromRedshiftDataSharesRequestDetails_assetSources,
    importAssetsFromRedshiftDataSharesRequestDetails_revisionId,

    -- ** ImportAssetsFromRedshiftDataSharesResponseDetails
    importAssetsFromRedshiftDataSharesResponseDetails_dataSetId,
    importAssetsFromRedshiftDataSharesResponseDetails_assetSources,
    importAssetsFromRedshiftDataSharesResponseDetails_revisionId,

    -- ** ImportAssetsFromS3RequestDetails
    importAssetsFromS3RequestDetails_dataSetId,
    importAssetsFromS3RequestDetails_assetSources,
    importAssetsFromS3RequestDetails_revisionId,

    -- ** ImportAssetsFromS3ResponseDetails
    importAssetsFromS3ResponseDetails_dataSetId,
    importAssetsFromS3ResponseDetails_assetSources,
    importAssetsFromS3ResponseDetails_revisionId,

    -- ** JobEntry
    jobEntry_errors,
    jobEntry_type,
    jobEntry_details,
    jobEntry_state,
    jobEntry_createdAt,
    jobEntry_id,
    jobEntry_arn,
    jobEntry_updatedAt,

    -- ** JobError
    jobError_resourceId,
    jobError_limitName,
    jobError_resourceType,
    jobError_details,
    jobError_limitValue,
    jobError_message,
    jobError_code,

    -- ** OriginDetails
    originDetails_productId,

    -- ** RedshiftDataShareAsset
    redshiftDataShareAsset_arn,

    -- ** RedshiftDataShareAssetSourceEntry
    redshiftDataShareAssetSourceEntry_dataShareArn,

    -- ** RequestDetails
    requestDetails_exportAssetsToS3,
    requestDetails_exportRevisionsToS3,
    requestDetails_importAssetFromSignedUrl,
    requestDetails_importAssetsFromRedshiftDataShares,
    requestDetails_importAssetsFromS3,
    requestDetails_exportAssetToSignedUrl,

    -- ** ResponseDetails
    responseDetails_exportAssetsToS3,
    responseDetails_exportRevisionsToS3,
    responseDetails_importAssetFromSignedUrl,
    responseDetails_importAssetsFromRedshiftDataShares,
    responseDetails_importAssetsFromS3,
    responseDetails_exportAssetToSignedUrl,

    -- ** RevisionDestinationEntry
    revisionDestinationEntry_keyPattern,
    revisionDestinationEntry_bucket,
    revisionDestinationEntry_revisionId,

    -- ** RevisionEntry
    revisionEntry_sourceId,
    revisionEntry_finalized,
    revisionEntry_comment,
    revisionEntry_createdAt,
    revisionEntry_dataSetId,
    revisionEntry_id,
    revisionEntry_arn,
    revisionEntry_updatedAt,

    -- ** RevisionPublished
    revisionPublished_dataSetId,

    -- ** S3SnapshotAsset
    s3SnapshotAsset_size,
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
import Amazonka.DataExchange.ListDataSetRevisions
import Amazonka.DataExchange.ListDataSets
import Amazonka.DataExchange.ListEventActions
import Amazonka.DataExchange.ListJobs
import Amazonka.DataExchange.ListRevisionAssets
import Amazonka.DataExchange.ListTagsForResource
import Amazonka.DataExchange.StartJob
import Amazonka.DataExchange.TagResource
import Amazonka.DataExchange.Types.Action
import Amazonka.DataExchange.Types.AssetDestinationEntry
import Amazonka.DataExchange.Types.AssetDetails
import Amazonka.DataExchange.Types.AssetEntry
import Amazonka.DataExchange.Types.AssetSourceEntry
import Amazonka.DataExchange.Types.AutoExportRevisionDestinationEntry
import Amazonka.DataExchange.Types.AutoExportRevisionToS3RequestDetails
import Amazonka.DataExchange.Types.DataSetEntry
import Amazonka.DataExchange.Types.Details
import Amazonka.DataExchange.Types.Event
import Amazonka.DataExchange.Types.EventActionEntry
import Amazonka.DataExchange.Types.ExportAssetToSignedUrlRequestDetails
import Amazonka.DataExchange.Types.ExportAssetToSignedUrlResponseDetails
import Amazonka.DataExchange.Types.ExportAssetsToS3RequestDetails
import Amazonka.DataExchange.Types.ExportAssetsToS3ResponseDetails
import Amazonka.DataExchange.Types.ExportRevisionsToS3RequestDetails
import Amazonka.DataExchange.Types.ExportRevisionsToS3ResponseDetails
import Amazonka.DataExchange.Types.ExportServerSideEncryption
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlJobErrorDetails
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesRequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3RequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3ResponseDetails
import Amazonka.DataExchange.Types.JobEntry
import Amazonka.DataExchange.Types.JobError
import Amazonka.DataExchange.Types.OriginDetails
import Amazonka.DataExchange.Types.RedshiftDataShareAsset
import Amazonka.DataExchange.Types.RedshiftDataShareAssetSourceEntry
import Amazonka.DataExchange.Types.RequestDetails
import Amazonka.DataExchange.Types.ResponseDetails
import Amazonka.DataExchange.Types.RevisionDestinationEntry
import Amazonka.DataExchange.Types.RevisionEntry
import Amazonka.DataExchange.Types.RevisionPublished
import Amazonka.DataExchange.Types.S3SnapshotAsset
import Amazonka.DataExchange.UntagResource
import Amazonka.DataExchange.UpdateAsset
import Amazonka.DataExchange.UpdateDataSet
import Amazonka.DataExchange.UpdateEventAction
import Amazonka.DataExchange.UpdateRevision
