{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataExchange.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createDataSetResponse_arn,
    createDataSetResponse_assetType,
    createDataSetResponse_createdAt,
    createDataSetResponse_description,
    createDataSetResponse_id,
    createDataSetResponse_name,
    createDataSetResponse_origin,
    createDataSetResponse_originDetails,
    createDataSetResponse_sourceId,
    createDataSetResponse_tags,
    createDataSetResponse_updatedAt,
    createDataSetResponse_httpStatus,

    -- ** CreateEventAction
    createEventAction_action,
    createEventAction_event,
    createEventActionResponse_action,
    createEventActionResponse_arn,
    createEventActionResponse_createdAt,
    createEventActionResponse_event,
    createEventActionResponse_id,
    createEventActionResponse_updatedAt,
    createEventActionResponse_httpStatus,

    -- ** CreateJob
    createJob_details,
    createJob_type,
    createJobResponse_arn,
    createJobResponse_createdAt,
    createJobResponse_details,
    createJobResponse_errors,
    createJobResponse_id,
    createJobResponse_state,
    createJobResponse_type,
    createJobResponse_updatedAt,
    createJobResponse_httpStatus,

    -- ** CreateRevision
    createRevision_comment,
    createRevision_tags,
    createRevision_dataSetId,
    createRevisionResponse_arn,
    createRevisionResponse_comment,
    createRevisionResponse_createdAt,
    createRevisionResponse_dataSetId,
    createRevisionResponse_finalized,
    createRevisionResponse_id,
    createRevisionResponse_revocationComment,
    createRevisionResponse_revoked,
    createRevisionResponse_revokedAt,
    createRevisionResponse_sourceId,
    createRevisionResponse_tags,
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
    getAssetResponse_arn,
    getAssetResponse_assetDetails,
    getAssetResponse_assetType,
    getAssetResponse_createdAt,
    getAssetResponse_dataSetId,
    getAssetResponse_id,
    getAssetResponse_name,
    getAssetResponse_revisionId,
    getAssetResponse_sourceId,
    getAssetResponse_updatedAt,
    getAssetResponse_httpStatus,

    -- ** GetDataSet
    getDataSet_dataSetId,
    getDataSetResponse_arn,
    getDataSetResponse_assetType,
    getDataSetResponse_createdAt,
    getDataSetResponse_description,
    getDataSetResponse_id,
    getDataSetResponse_name,
    getDataSetResponse_origin,
    getDataSetResponse_originDetails,
    getDataSetResponse_sourceId,
    getDataSetResponse_tags,
    getDataSetResponse_updatedAt,
    getDataSetResponse_httpStatus,

    -- ** GetEventAction
    getEventAction_eventActionId,
    getEventActionResponse_action,
    getEventActionResponse_arn,
    getEventActionResponse_createdAt,
    getEventActionResponse_event,
    getEventActionResponse_id,
    getEventActionResponse_updatedAt,
    getEventActionResponse_httpStatus,

    -- ** GetJob
    getJob_jobId,
    getJobResponse_arn,
    getJobResponse_createdAt,
    getJobResponse_details,
    getJobResponse_errors,
    getJobResponse_id,
    getJobResponse_state,
    getJobResponse_type,
    getJobResponse_updatedAt,
    getJobResponse_httpStatus,

    -- ** GetRevision
    getRevision_dataSetId,
    getRevision_revisionId,
    getRevisionResponse_arn,
    getRevisionResponse_comment,
    getRevisionResponse_createdAt,
    getRevisionResponse_dataSetId,
    getRevisionResponse_finalized,
    getRevisionResponse_id,
    getRevisionResponse_revocationComment,
    getRevisionResponse_revoked,
    getRevisionResponse_revokedAt,
    getRevisionResponse_sourceId,
    getRevisionResponse_tags,
    getRevisionResponse_updatedAt,
    getRevisionResponse_httpStatus,

    -- ** ListDataSetRevisions
    listDataSetRevisions_maxResults,
    listDataSetRevisions_nextToken,
    listDataSetRevisions_dataSetId,
    listDataSetRevisionsResponse_nextToken,
    listDataSetRevisionsResponse_revisions,
    listDataSetRevisionsResponse_httpStatus,

    -- ** ListDataSets
    listDataSets_maxResults,
    listDataSets_nextToken,
    listDataSets_origin,
    listDataSetsResponse_dataSets,
    listDataSetsResponse_nextToken,
    listDataSetsResponse_httpStatus,

    -- ** ListEventActions
    listEventActions_eventSourceId,
    listEventActions_maxResults,
    listEventActions_nextToken,
    listEventActionsResponse_eventActions,
    listEventActionsResponse_nextToken,
    listEventActionsResponse_httpStatus,

    -- ** ListJobs
    listJobs_dataSetId,
    listJobs_maxResults,
    listJobs_nextToken,
    listJobs_revisionId,
    listJobsResponse_jobs,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,

    -- ** ListRevisionAssets
    listRevisionAssets_maxResults,
    listRevisionAssets_nextToken,
    listRevisionAssets_dataSetId,
    listRevisionAssets_revisionId,
    listRevisionAssetsResponse_assets,
    listRevisionAssetsResponse_nextToken,
    listRevisionAssetsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RevokeRevision
    revokeRevision_dataSetId,
    revokeRevision_revisionId,
    revokeRevision_revocationComment,
    revokeRevisionResponse_arn,
    revokeRevisionResponse_comment,
    revokeRevisionResponse_createdAt,
    revokeRevisionResponse_dataSetId,
    revokeRevisionResponse_finalized,
    revokeRevisionResponse_id,
    revokeRevisionResponse_revocationComment,
    revokeRevisionResponse_revoked,
    revokeRevisionResponse_revokedAt,
    revokeRevisionResponse_sourceId,
    revokeRevisionResponse_updatedAt,
    revokeRevisionResponse_httpStatus,

    -- ** SendApiAsset
    sendApiAsset_body,
    sendApiAsset_method,
    sendApiAsset_path,
    sendApiAsset_queryStringParameters,
    sendApiAsset_requestHeaders,
    sendApiAsset_assetId,
    sendApiAsset_dataSetId,
    sendApiAsset_revisionId,
    sendApiAssetResponse_body,
    sendApiAssetResponse_responseHeaders,
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
    updateAssetResponse_arn,
    updateAssetResponse_assetDetails,
    updateAssetResponse_assetType,
    updateAssetResponse_createdAt,
    updateAssetResponse_dataSetId,
    updateAssetResponse_id,
    updateAssetResponse_name,
    updateAssetResponse_revisionId,
    updateAssetResponse_sourceId,
    updateAssetResponse_updatedAt,
    updateAssetResponse_httpStatus,

    -- ** UpdateDataSet
    updateDataSet_description,
    updateDataSet_name,
    updateDataSet_dataSetId,
    updateDataSetResponse_arn,
    updateDataSetResponse_assetType,
    updateDataSetResponse_createdAt,
    updateDataSetResponse_description,
    updateDataSetResponse_id,
    updateDataSetResponse_name,
    updateDataSetResponse_origin,
    updateDataSetResponse_originDetails,
    updateDataSetResponse_sourceId,
    updateDataSetResponse_updatedAt,
    updateDataSetResponse_httpStatus,

    -- ** UpdateEventAction
    updateEventAction_action,
    updateEventAction_eventActionId,
    updateEventActionResponse_action,
    updateEventActionResponse_arn,
    updateEventActionResponse_createdAt,
    updateEventActionResponse_event,
    updateEventActionResponse_id,
    updateEventActionResponse_updatedAt,
    updateEventActionResponse_httpStatus,

    -- ** UpdateRevision
    updateRevision_comment,
    updateRevision_finalized,
    updateRevision_dataSetId,
    updateRevision_revisionId,
    updateRevisionResponse_arn,
    updateRevisionResponse_comment,
    updateRevisionResponse_createdAt,
    updateRevisionResponse_dataSetId,
    updateRevisionResponse_finalized,
    updateRevisionResponse_id,
    updateRevisionResponse_revocationComment,
    updateRevisionResponse_revoked,
    updateRevisionResponse_revokedAt,
    updateRevisionResponse_sourceId,
    updateRevisionResponse_updatedAt,
    updateRevisionResponse_httpStatus,

    -- * Types

    -- ** Action
    action_exportRevisionToS3,

    -- ** ApiGatewayApiAsset
    apiGatewayApiAsset_apiDescription,
    apiGatewayApiAsset_apiEndpoint,
    apiGatewayApiAsset_apiId,
    apiGatewayApiAsset_apiKey,
    apiGatewayApiAsset_apiName,
    apiGatewayApiAsset_apiSpecificationDownloadUrl,
    apiGatewayApiAsset_apiSpecificationDownloadUrlExpiresAt,
    apiGatewayApiAsset_protocolType,
    apiGatewayApiAsset_stage,

    -- ** AssetDestinationEntry
    assetDestinationEntry_key,
    assetDestinationEntry_assetId,
    assetDestinationEntry_bucket,

    -- ** AssetDetails
    assetDetails_apiGatewayApiAsset,
    assetDetails_lakeFormationDataPermissionAsset,
    assetDetails_redshiftDataShareAsset,
    assetDetails_s3DataAccessAsset,
    assetDetails_s3SnapshotAsset,

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

    -- ** CreateS3DataAccessFromS3BucketRequestDetails
    createS3DataAccessFromS3BucketRequestDetails_assetSource,
    createS3DataAccessFromS3BucketRequestDetails_dataSetId,
    createS3DataAccessFromS3BucketRequestDetails_revisionId,

    -- ** CreateS3DataAccessFromS3BucketResponseDetails
    createS3DataAccessFromS3BucketResponseDetails_assetSource,
    createS3DataAccessFromS3BucketResponseDetails_dataSetId,
    createS3DataAccessFromS3BucketResponseDetails_revisionId,

    -- ** DataSetEntry
    dataSetEntry_originDetails,
    dataSetEntry_sourceId,
    dataSetEntry_arn,
    dataSetEntry_assetType,
    dataSetEntry_createdAt,
    dataSetEntry_description,
    dataSetEntry_id,
    dataSetEntry_name,
    dataSetEntry_origin,
    dataSetEntry_updatedAt,

    -- ** DatabaseLFTagPolicy
    databaseLFTagPolicy_expression,

    -- ** DatabaseLFTagPolicyAndPermissions
    databaseLFTagPolicyAndPermissions_expression,
    databaseLFTagPolicyAndPermissions_permissions,

    -- ** Details
    details_importAssetFromSignedUrlJobErrorDetails,
    details_importAssetsFromS3JobErrorDetails,

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
    exportAssetToSignedUrlResponseDetails_signedUrl,
    exportAssetToSignedUrlResponseDetails_signedUrlExpiresAt,
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
    exportRevisionsToS3ResponseDetails_encryption,
    exportRevisionsToS3ResponseDetails_eventActionArn,
    exportRevisionsToS3ResponseDetails_dataSetId,
    exportRevisionsToS3ResponseDetails_revisionDestinations,

    -- ** ExportServerSideEncryption
    exportServerSideEncryption_kmsKeyArn,
    exportServerSideEncryption_type,

    -- ** ImportAssetFromApiGatewayApiRequestDetails
    importAssetFromApiGatewayApiRequestDetails_apiDescription,
    importAssetFromApiGatewayApiRequestDetails_apiKey,
    importAssetFromApiGatewayApiRequestDetails_apiId,
    importAssetFromApiGatewayApiRequestDetails_apiName,
    importAssetFromApiGatewayApiRequestDetails_apiSpecificationMd5Hash,
    importAssetFromApiGatewayApiRequestDetails_dataSetId,
    importAssetFromApiGatewayApiRequestDetails_protocolType,
    importAssetFromApiGatewayApiRequestDetails_revisionId,
    importAssetFromApiGatewayApiRequestDetails_stage,

    -- ** ImportAssetFromApiGatewayApiResponseDetails
    importAssetFromApiGatewayApiResponseDetails_apiDescription,
    importAssetFromApiGatewayApiResponseDetails_apiKey,
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
    importAssetFromSignedUrlResponseDetails_md5Hash,
    importAssetFromSignedUrlResponseDetails_signedUrl,
    importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt,
    importAssetFromSignedUrlResponseDetails_assetName,
    importAssetFromSignedUrlResponseDetails_dataSetId,
    importAssetFromSignedUrlResponseDetails_revisionId,

    -- ** ImportAssetsFromLakeFormationTagPolicyRequestDetails
    importAssetsFromLakeFormationTagPolicyRequestDetails_database,
    importAssetsFromLakeFormationTagPolicyRequestDetails_table,
    importAssetsFromLakeFormationTagPolicyRequestDetails_catalogId,
    importAssetsFromLakeFormationTagPolicyRequestDetails_roleArn,
    importAssetsFromLakeFormationTagPolicyRequestDetails_dataSetId,
    importAssetsFromLakeFormationTagPolicyRequestDetails_revisionId,

    -- ** ImportAssetsFromLakeFormationTagPolicyResponseDetails
    importAssetsFromLakeFormationTagPolicyResponseDetails_database,
    importAssetsFromLakeFormationTagPolicyResponseDetails_table,
    importAssetsFromLakeFormationTagPolicyResponseDetails_catalogId,
    importAssetsFromLakeFormationTagPolicyResponseDetails_roleArn,
    importAssetsFromLakeFormationTagPolicyResponseDetails_dataSetId,
    importAssetsFromLakeFormationTagPolicyResponseDetails_revisionId,

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
    jobError_details,
    jobError_limitName,
    jobError_limitValue,
    jobError_resourceId,
    jobError_resourceType,
    jobError_code,
    jobError_message,

    -- ** LFResourceDetails
    lFResourceDetails_database,
    lFResourceDetails_table,

    -- ** LFTag
    lFTag_tagKey,
    lFTag_tagValues,

    -- ** LFTagPolicyDetails
    lFTagPolicyDetails_catalogId,
    lFTagPolicyDetails_resourceType,
    lFTagPolicyDetails_resourceDetails,

    -- ** LakeFormationDataPermissionAsset
    lakeFormationDataPermissionAsset_roleArn,
    lakeFormationDataPermissionAsset_lakeFormationDataPermissionDetails,
    lakeFormationDataPermissionAsset_lakeFormationDataPermissionType,
    lakeFormationDataPermissionAsset_permissions,

    -- ** LakeFormationDataPermissionDetails
    lakeFormationDataPermissionDetails_lFTagPolicy,

    -- ** OriginDetails
    originDetails_productId,

    -- ** RedshiftDataShareAsset
    redshiftDataShareAsset_arn,

    -- ** RedshiftDataShareAssetSourceEntry
    redshiftDataShareAssetSourceEntry_dataShareArn,

    -- ** RequestDetails
    requestDetails_createS3DataAccessFromS3Bucket,
    requestDetails_exportAssetToSignedUrl,
    requestDetails_exportAssetsToS3,
    requestDetails_exportRevisionsToS3,
    requestDetails_importAssetFromApiGatewayApi,
    requestDetails_importAssetFromSignedUrl,
    requestDetails_importAssetsFromLakeFormationTagPolicy,
    requestDetails_importAssetsFromRedshiftDataShares,
    requestDetails_importAssetsFromS3,

    -- ** ResponseDetails
    responseDetails_createS3DataAccessFromS3Bucket,
    responseDetails_exportAssetToSignedUrl,
    responseDetails_exportAssetsToS3,
    responseDetails_exportRevisionsToS3,
    responseDetails_importAssetFromApiGatewayApi,
    responseDetails_importAssetFromSignedUrl,
    responseDetails_importAssetsFromLakeFormationTagPolicy,
    responseDetails_importAssetsFromRedshiftDataShares,
    responseDetails_importAssetsFromS3,

    -- ** RevisionDestinationEntry
    revisionDestinationEntry_keyPattern,
    revisionDestinationEntry_bucket,
    revisionDestinationEntry_revisionId,

    -- ** RevisionEntry
    revisionEntry_comment,
    revisionEntry_finalized,
    revisionEntry_revocationComment,
    revisionEntry_revoked,
    revisionEntry_revokedAt,
    revisionEntry_sourceId,
    revisionEntry_arn,
    revisionEntry_createdAt,
    revisionEntry_dataSetId,
    revisionEntry_id,
    revisionEntry_updatedAt,

    -- ** RevisionPublished
    revisionPublished_dataSetId,

    -- ** S3DataAccessAsset
    s3DataAccessAsset_keyPrefixes,
    s3DataAccessAsset_keys,
    s3DataAccessAsset_s3AccessPointAlias,
    s3DataAccessAsset_s3AccessPointArn,
    s3DataAccessAsset_bucket,

    -- ** S3DataAccessAssetSourceEntry
    s3DataAccessAssetSourceEntry_keyPrefixes,
    s3DataAccessAssetSourceEntry_keys,
    s3DataAccessAssetSourceEntry_bucket,

    -- ** S3SnapshotAsset
    s3SnapshotAsset_size,

    -- ** TableLFTagPolicy
    tableLFTagPolicy_expression,

    -- ** TableLFTagPolicyAndPermissions
    tableLFTagPolicyAndPermissions_expression,
    tableLFTagPolicyAndPermissions_permissions,
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
import Amazonka.DataExchange.Types.CreateS3DataAccessFromS3BucketRequestDetails
import Amazonka.DataExchange.Types.CreateS3DataAccessFromS3BucketResponseDetails
import Amazonka.DataExchange.Types.DataSetEntry
import Amazonka.DataExchange.Types.DatabaseLFTagPolicy
import Amazonka.DataExchange.Types.DatabaseLFTagPolicyAndPermissions
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
import Amazonka.DataExchange.Types.ImportAssetsFromLakeFormationTagPolicyRequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromLakeFormationTagPolicyResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesRequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3RequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3ResponseDetails
import Amazonka.DataExchange.Types.JobEntry
import Amazonka.DataExchange.Types.JobError
import Amazonka.DataExchange.Types.LFResourceDetails
import Amazonka.DataExchange.Types.LFTag
import Amazonka.DataExchange.Types.LFTagPolicyDetails
import Amazonka.DataExchange.Types.LakeFormationDataPermissionAsset
import Amazonka.DataExchange.Types.LakeFormationDataPermissionDetails
import Amazonka.DataExchange.Types.OriginDetails
import Amazonka.DataExchange.Types.RedshiftDataShareAsset
import Amazonka.DataExchange.Types.RedshiftDataShareAssetSourceEntry
import Amazonka.DataExchange.Types.RequestDetails
import Amazonka.DataExchange.Types.ResponseDetails
import Amazonka.DataExchange.Types.RevisionDestinationEntry
import Amazonka.DataExchange.Types.RevisionEntry
import Amazonka.DataExchange.Types.RevisionPublished
import Amazonka.DataExchange.Types.S3DataAccessAsset
import Amazonka.DataExchange.Types.S3DataAccessAssetSourceEntry
import Amazonka.DataExchange.Types.S3SnapshotAsset
import Amazonka.DataExchange.Types.TableLFTagPolicy
import Amazonka.DataExchange.Types.TableLFTagPolicyAndPermissions
import Amazonka.DataExchange.UntagResource
import Amazonka.DataExchange.UpdateAsset
import Amazonka.DataExchange.UpdateDataSet
import Amazonka.DataExchange.UpdateEventAction
import Amazonka.DataExchange.UpdateRevision
