{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataExchange.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Lens
  ( -- * Operations

    -- ** CancelJob
    cancelJob_jobId,

    -- ** CreateDataSet
    createDataSet_tags,
    createDataSet_assetType,
    createDataSet_description,
    createDataSet_name,
    createDataSetResponse_tags,
    createDataSetResponse_name,
    createDataSetResponse_sourceId,
    createDataSetResponse_originDetails,
    createDataSetResponse_arn,
    createDataSetResponse_id,
    createDataSetResponse_assetType,
    createDataSetResponse_description,
    createDataSetResponse_origin,
    createDataSetResponse_createdAt,
    createDataSetResponse_updatedAt,
    createDataSetResponse_httpStatus,

    -- ** CreateEventAction
    createEventAction_action,
    createEventAction_event,
    createEventActionResponse_arn,
    createEventActionResponse_id,
    createEventActionResponse_event,
    createEventActionResponse_action,
    createEventActionResponse_createdAt,
    createEventActionResponse_updatedAt,
    createEventActionResponse_httpStatus,

    -- ** CreateJob
    createJob_details,
    createJob_type,
    createJobResponse_type,
    createJobResponse_arn,
    createJobResponse_state,
    createJobResponse_id,
    createJobResponse_details,
    createJobResponse_errors,
    createJobResponse_createdAt,
    createJobResponse_updatedAt,
    createJobResponse_httpStatus,

    -- ** CreateRevision
    createRevision_tags,
    createRevision_comment,
    createRevision_dataSetId,
    createRevisionResponse_tags,
    createRevisionResponse_sourceId,
    createRevisionResponse_revocationComment,
    createRevisionResponse_arn,
    createRevisionResponse_id,
    createRevisionResponse_comment,
    createRevisionResponse_finalized,
    createRevisionResponse_dataSetId,
    createRevisionResponse_revokedAt,
    createRevisionResponse_revoked,
    createRevisionResponse_createdAt,
    createRevisionResponse_updatedAt,
    createRevisionResponse_httpStatus,

    -- ** DeleteAsset
    deleteAsset_assetId,
    deleteAsset_dataSetId,
    deleteAsset_revisionId,

    -- ** DeleteDataSet
    deleteDataSet_dataSetId,

    -- ** DeleteEventAction
    deleteEventAction_eventActionId,

    -- ** DeleteRevision
    deleteRevision_dataSetId,
    deleteRevision_revisionId,

    -- ** GetAsset
    getAsset_assetId,
    getAsset_dataSetId,
    getAsset_revisionId,
    getAssetResponse_name,
    getAssetResponse_sourceId,
    getAssetResponse_assetDetails,
    getAssetResponse_arn,
    getAssetResponse_id,
    getAssetResponse_assetType,
    getAssetResponse_revisionId,
    getAssetResponse_dataSetId,
    getAssetResponse_createdAt,
    getAssetResponse_updatedAt,
    getAssetResponse_httpStatus,

    -- ** GetDataSet
    getDataSet_dataSetId,
    getDataSetResponse_tags,
    getDataSetResponse_name,
    getDataSetResponse_sourceId,
    getDataSetResponse_originDetails,
    getDataSetResponse_arn,
    getDataSetResponse_id,
    getDataSetResponse_assetType,
    getDataSetResponse_description,
    getDataSetResponse_origin,
    getDataSetResponse_createdAt,
    getDataSetResponse_updatedAt,
    getDataSetResponse_httpStatus,

    -- ** GetEventAction
    getEventAction_eventActionId,
    getEventActionResponse_arn,
    getEventActionResponse_id,
    getEventActionResponse_event,
    getEventActionResponse_action,
    getEventActionResponse_createdAt,
    getEventActionResponse_updatedAt,
    getEventActionResponse_httpStatus,

    -- ** GetJob
    getJob_jobId,
    getJobResponse_type,
    getJobResponse_arn,
    getJobResponse_state,
    getJobResponse_id,
    getJobResponse_details,
    getJobResponse_errors,
    getJobResponse_createdAt,
    getJobResponse_updatedAt,
    getJobResponse_httpStatus,

    -- ** GetRevision
    getRevision_dataSetId,
    getRevision_revisionId,
    getRevisionResponse_tags,
    getRevisionResponse_sourceId,
    getRevisionResponse_revocationComment,
    getRevisionResponse_arn,
    getRevisionResponse_id,
    getRevisionResponse_comment,
    getRevisionResponse_finalized,
    getRevisionResponse_dataSetId,
    getRevisionResponse_revokedAt,
    getRevisionResponse_revoked,
    getRevisionResponse_createdAt,
    getRevisionResponse_updatedAt,
    getRevisionResponse_httpStatus,

    -- ** ListDataSetRevisions
    listDataSetRevisions_nextToken,
    listDataSetRevisions_maxResults,
    listDataSetRevisions_dataSetId,
    listDataSetRevisionsResponse_nextToken,
    listDataSetRevisionsResponse_revisions,
    listDataSetRevisionsResponse_httpStatus,

    -- ** ListDataSets
    listDataSets_nextToken,
    listDataSets_maxResults,
    listDataSets_origin,
    listDataSetsResponse_nextToken,
    listDataSetsResponse_dataSets,
    listDataSetsResponse_httpStatus,

    -- ** ListEventActions
    listEventActions_nextToken,
    listEventActions_maxResults,
    listEventActions_eventSourceId,
    listEventActionsResponse_nextToken,
    listEventActionsResponse_eventActions,
    listEventActionsResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_maxResults,
    listJobs_revisionId,
    listJobs_dataSetId,
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** ListRevisionAssets
    listRevisionAssets_nextToken,
    listRevisionAssets_maxResults,
    listRevisionAssets_dataSetId,
    listRevisionAssets_revisionId,
    listRevisionAssetsResponse_nextToken,
    listRevisionAssetsResponse_assets,
    listRevisionAssetsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RevokeRevision
    revokeRevision_dataSetId,
    revokeRevision_revisionId,
    revokeRevision_revocationComment,
    revokeRevisionResponse_sourceId,
    revokeRevisionResponse_revocationComment,
    revokeRevisionResponse_arn,
    revokeRevisionResponse_id,
    revokeRevisionResponse_comment,
    revokeRevisionResponse_finalized,
    revokeRevisionResponse_dataSetId,
    revokeRevisionResponse_revokedAt,
    revokeRevisionResponse_revoked,
    revokeRevisionResponse_createdAt,
    revokeRevisionResponse_updatedAt,
    revokeRevisionResponse_httpStatus,

    -- ** SendApiAsset
    sendApiAsset_method,
    sendApiAsset_queryStringParameters,
    sendApiAsset_requestHeaders,
    sendApiAsset_body,
    sendApiAsset_path,
    sendApiAsset_assetId,
    sendApiAsset_dataSetId,
    sendApiAsset_revisionId,
    sendApiAssetResponse_responseHeaders,
    sendApiAssetResponse_body,
    sendApiAssetResponse_httpStatus,

    -- ** StartJob
    startJob_jobId,
    startJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateAsset
    updateAsset_assetId,
    updateAsset_dataSetId,
    updateAsset_name,
    updateAsset_revisionId,
    updateAssetResponse_name,
    updateAssetResponse_sourceId,
    updateAssetResponse_assetDetails,
    updateAssetResponse_arn,
    updateAssetResponse_id,
    updateAssetResponse_assetType,
    updateAssetResponse_revisionId,
    updateAssetResponse_dataSetId,
    updateAssetResponse_createdAt,
    updateAssetResponse_updatedAt,
    updateAssetResponse_httpStatus,

    -- ** UpdateDataSet
    updateDataSet_name,
    updateDataSet_description,
    updateDataSet_dataSetId,
    updateDataSetResponse_name,
    updateDataSetResponse_sourceId,
    updateDataSetResponse_originDetails,
    updateDataSetResponse_arn,
    updateDataSetResponse_id,
    updateDataSetResponse_assetType,
    updateDataSetResponse_description,
    updateDataSetResponse_origin,
    updateDataSetResponse_createdAt,
    updateDataSetResponse_updatedAt,
    updateDataSetResponse_httpStatus,

    -- ** UpdateEventAction
    updateEventAction_action,
    updateEventAction_eventActionId,
    updateEventActionResponse_arn,
    updateEventActionResponse_id,
    updateEventActionResponse_event,
    updateEventActionResponse_action,
    updateEventActionResponse_createdAt,
    updateEventActionResponse_updatedAt,
    updateEventActionResponse_httpStatus,

    -- ** UpdateRevision
    updateRevision_comment,
    updateRevision_finalized,
    updateRevision_dataSetId,
    updateRevision_revisionId,
    updateRevisionResponse_sourceId,
    updateRevisionResponse_revocationComment,
    updateRevisionResponse_arn,
    updateRevisionResponse_id,
    updateRevisionResponse_comment,
    updateRevisionResponse_finalized,
    updateRevisionResponse_dataSetId,
    updateRevisionResponse_revokedAt,
    updateRevisionResponse_revoked,
    updateRevisionResponse_createdAt,
    updateRevisionResponse_updatedAt,
    updateRevisionResponse_httpStatus,

    -- * Types

    -- ** Action
    action_exportRevisionToS3,

    -- ** ApiGatewayApiAsset
    apiGatewayApiAsset_apiEndpoint,
    apiGatewayApiAsset_apiId,
    apiGatewayApiAsset_apiSpecificationDownloadUrlExpiresAt,
    apiGatewayApiAsset_apiKey,
    apiGatewayApiAsset_apiSpecificationDownloadUrl,
    apiGatewayApiAsset_protocolType,
    apiGatewayApiAsset_stage,
    apiGatewayApiAsset_apiName,
    apiGatewayApiAsset_apiDescription,

    -- ** AssetDestinationEntry
    assetDestinationEntry_key,
    assetDestinationEntry_assetId,
    assetDestinationEntry_bucket,

    -- ** AssetDetails
    assetDetails_s3SnapshotAsset,
    assetDetails_apiGatewayApiAsset,
    assetDetails_redshiftDataShareAsset,

    -- ** AssetEntry
    assetEntry_sourceId,
    assetEntry_arn,
    assetEntry_assetDetails,
    assetEntry_assetType,
    assetEntry_createdAt,
    assetEntry_dataSetId,
    assetEntry_id,
    assetEntry_name,
    assetEntry_revisionId,
    assetEntry_updatedAt,

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
    dataSetEntry_arn,
    dataSetEntry_assetType,
    dataSetEntry_createdAt,
    dataSetEntry_description,
    dataSetEntry_id,
    dataSetEntry_name,
    dataSetEntry_origin,
    dataSetEntry_updatedAt,

    -- ** Details
    details_importAssetsFromS3JobErrorDetails,
    details_importAssetFromSignedUrlJobErrorDetails,

    -- ** Event
    event_revisionPublished,

    -- ** EventActionEntry
    eventActionEntry_action,
    eventActionEntry_arn,
    eventActionEntry_createdAt,
    eventActionEntry_event,
    eventActionEntry_id,
    eventActionEntry_updatedAt,

    -- ** ExportAssetToSignedUrlRequestDetails
    exportAssetToSignedUrlRequestDetails_assetId,
    exportAssetToSignedUrlRequestDetails_dataSetId,
    exportAssetToSignedUrlRequestDetails_revisionId,

    -- ** ExportAssetToSignedUrlResponseDetails
    exportAssetToSignedUrlResponseDetails_signedUrlExpiresAt,
    exportAssetToSignedUrlResponseDetails_signedUrl,
    exportAssetToSignedUrlResponseDetails_assetId,
    exportAssetToSignedUrlResponseDetails_dataSetId,
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
    exportRevisionsToS3RequestDetails_dataSetId,
    exportRevisionsToS3RequestDetails_revisionDestinations,

    -- ** ExportRevisionsToS3ResponseDetails
    exportRevisionsToS3ResponseDetails_eventActionArn,
    exportRevisionsToS3ResponseDetails_encryption,
    exportRevisionsToS3ResponseDetails_dataSetId,
    exportRevisionsToS3ResponseDetails_revisionDestinations,

    -- ** ExportServerSideEncryption
    exportServerSideEncryption_kmsKeyArn,
    exportServerSideEncryption_type,

    -- ** ImportAssetFromApiGatewayApiRequestDetails
    importAssetFromApiGatewayApiRequestDetails_apiKey,
    importAssetFromApiGatewayApiRequestDetails_apiDescription,
    importAssetFromApiGatewayApiRequestDetails_apiId,
    importAssetFromApiGatewayApiRequestDetails_apiName,
    importAssetFromApiGatewayApiRequestDetails_apiSpecificationMd5Hash,
    importAssetFromApiGatewayApiRequestDetails_dataSetId,
    importAssetFromApiGatewayApiRequestDetails_protocolType,
    importAssetFromApiGatewayApiRequestDetails_revisionId,
    importAssetFromApiGatewayApiRequestDetails_stage,

    -- ** ImportAssetFromApiGatewayApiResponseDetails
    importAssetFromApiGatewayApiResponseDetails_apiKey,
    importAssetFromApiGatewayApiResponseDetails_apiDescription,
    importAssetFromApiGatewayApiResponseDetails_apiId,
    importAssetFromApiGatewayApiResponseDetails_apiName,
    importAssetFromApiGatewayApiResponseDetails_apiSpecificationMd5Hash,
    importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrl,
    importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrlExpiresAt,
    importAssetFromApiGatewayApiResponseDetails_dataSetId,
    importAssetFromApiGatewayApiResponseDetails_protocolType,
    importAssetFromApiGatewayApiResponseDetails_revisionId,
    importAssetFromApiGatewayApiResponseDetails_stage,

    -- ** ImportAssetFromSignedUrlJobErrorDetails
    importAssetFromSignedUrlJobErrorDetails_assetName,

    -- ** ImportAssetFromSignedUrlRequestDetails
    importAssetFromSignedUrlRequestDetails_assetName,
    importAssetFromSignedUrlRequestDetails_dataSetId,
    importAssetFromSignedUrlRequestDetails_md5Hash,
    importAssetFromSignedUrlRequestDetails_revisionId,

    -- ** ImportAssetFromSignedUrlResponseDetails
    importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt,
    importAssetFromSignedUrlResponseDetails_signedUrl,
    importAssetFromSignedUrlResponseDetails_md5Hash,
    importAssetFromSignedUrlResponseDetails_assetName,
    importAssetFromSignedUrlResponseDetails_dataSetId,
    importAssetFromSignedUrlResponseDetails_revisionId,

    -- ** ImportAssetsFromRedshiftDataSharesRequestDetails
    importAssetsFromRedshiftDataSharesRequestDetails_assetSources,
    importAssetsFromRedshiftDataSharesRequestDetails_dataSetId,
    importAssetsFromRedshiftDataSharesRequestDetails_revisionId,

    -- ** ImportAssetsFromRedshiftDataSharesResponseDetails
    importAssetsFromRedshiftDataSharesResponseDetails_assetSources,
    importAssetsFromRedshiftDataSharesResponseDetails_dataSetId,
    importAssetsFromRedshiftDataSharesResponseDetails_revisionId,

    -- ** ImportAssetsFromS3RequestDetails
    importAssetsFromS3RequestDetails_assetSources,
    importAssetsFromS3RequestDetails_dataSetId,
    importAssetsFromS3RequestDetails_revisionId,

    -- ** ImportAssetsFromS3ResponseDetails
    importAssetsFromS3ResponseDetails_assetSources,
    importAssetsFromS3ResponseDetails_dataSetId,
    importAssetsFromS3ResponseDetails_revisionId,

    -- ** JobEntry
    jobEntry_errors,
    jobEntry_arn,
    jobEntry_createdAt,
    jobEntry_details,
    jobEntry_id,
    jobEntry_state,
    jobEntry_type,
    jobEntry_updatedAt,

    -- ** JobError
    jobError_resourceId,
    jobError_resourceType,
    jobError_limitValue,
    jobError_limitName,
    jobError_details,
    jobError_code,
    jobError_message,

    -- ** OriginDetails
    originDetails_productId,

    -- ** RedshiftDataShareAsset
    redshiftDataShareAsset_arn,

    -- ** RedshiftDataShareAssetSourceEntry
    redshiftDataShareAssetSourceEntry_dataShareArn,

    -- ** RequestDetails
    requestDetails_importAssetFromSignedUrl,
    requestDetails_importAssetsFromRedshiftDataShares,
    requestDetails_exportAssetToSignedUrl,
    requestDetails_exportRevisionsToS3,
    requestDetails_exportAssetsToS3,
    requestDetails_importAssetsFromS3,
    requestDetails_importAssetFromApiGatewayApi,

    -- ** ResponseDetails
    responseDetails_importAssetFromSignedUrl,
    responseDetails_importAssetsFromRedshiftDataShares,
    responseDetails_exportAssetToSignedUrl,
    responseDetails_exportRevisionsToS3,
    responseDetails_exportAssetsToS3,
    responseDetails_importAssetsFromS3,
    responseDetails_importAssetFromApiGatewayApi,

    -- ** RevisionDestinationEntry
    revisionDestinationEntry_keyPattern,
    revisionDestinationEntry_bucket,
    revisionDestinationEntry_revisionId,

    -- ** RevisionEntry
    revisionEntry_sourceId,
    revisionEntry_revocationComment,
    revisionEntry_comment,
    revisionEntry_finalized,
    revisionEntry_revokedAt,
    revisionEntry_revoked,
    revisionEntry_arn,
    revisionEntry_createdAt,
    revisionEntry_dataSetId,
    revisionEntry_id,
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
import Amazonka.DataExchange.RevokeRevision
import Amazonka.DataExchange.SendApiAsset
import Amazonka.DataExchange.StartJob
import Amazonka.DataExchange.TagResource
import Amazonka.DataExchange.Types.Action
import Amazonka.DataExchange.Types.ApiGatewayApiAsset
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
import Amazonka.DataExchange.Types.ImportAssetFromApiGatewayApiRequestDetails
import Amazonka.DataExchange.Types.ImportAssetFromApiGatewayApiResponseDetails
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
