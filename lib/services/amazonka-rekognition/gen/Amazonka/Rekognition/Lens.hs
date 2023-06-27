{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rekognition.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Lens
  ( -- * Operations

    -- ** AssociateFaces
    associateFaces_clientRequestToken,
    associateFaces_userMatchThreshold,
    associateFaces_collectionId,
    associateFaces_userId,
    associateFaces_faceIds,
    associateFacesResponse_associatedFaces,
    associateFacesResponse_unsuccessfulFaceAssociations,
    associateFacesResponse_userStatus,
    associateFacesResponse_httpStatus,

    -- ** CompareFaces
    compareFaces_qualityFilter,
    compareFaces_similarityThreshold,
    compareFaces_sourceImage,
    compareFaces_targetImage,
    compareFacesResponse_faceMatches,
    compareFacesResponse_sourceImageFace,
    compareFacesResponse_sourceImageOrientationCorrection,
    compareFacesResponse_targetImageOrientationCorrection,
    compareFacesResponse_unmatchedFaces,
    compareFacesResponse_httpStatus,

    -- ** CopyProjectVersion
    copyProjectVersion_kmsKeyId,
    copyProjectVersion_tags,
    copyProjectVersion_sourceProjectArn,
    copyProjectVersion_sourceProjectVersionArn,
    copyProjectVersion_destinationProjectArn,
    copyProjectVersion_versionName,
    copyProjectVersion_outputConfig,
    copyProjectVersionResponse_projectVersionArn,
    copyProjectVersionResponse_httpStatus,

    -- ** CreateCollection
    createCollection_tags,
    createCollection_collectionId,
    createCollectionResponse_collectionArn,
    createCollectionResponse_faceModelVersion,
    createCollectionResponse_statusCode,
    createCollectionResponse_httpStatus,

    -- ** CreateDataset
    createDataset_datasetSource,
    createDataset_datasetType,
    createDataset_projectArn,
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,

    -- ** CreateFaceLivenessSession
    createFaceLivenessSession_clientRequestToken,
    createFaceLivenessSession_kmsKeyId,
    createFaceLivenessSession_settings,
    createFaceLivenessSessionResponse_httpStatus,
    createFaceLivenessSessionResponse_sessionId,

    -- ** CreateProject
    createProject_projectName,
    createProjectResponse_projectArn,
    createProjectResponse_httpStatus,

    -- ** CreateProjectVersion
    createProjectVersion_kmsKeyId,
    createProjectVersion_tags,
    createProjectVersion_testingData,
    createProjectVersion_trainingData,
    createProjectVersion_projectArn,
    createProjectVersion_versionName,
    createProjectVersion_outputConfig,
    createProjectVersionResponse_projectVersionArn,
    createProjectVersionResponse_httpStatus,

    -- ** CreateStreamProcessor
    createStreamProcessor_dataSharingPreference,
    createStreamProcessor_kmsKeyId,
    createStreamProcessor_notificationChannel,
    createStreamProcessor_regionsOfInterest,
    createStreamProcessor_tags,
    createStreamProcessor_input,
    createStreamProcessor_output,
    createStreamProcessor_name,
    createStreamProcessor_settings,
    createStreamProcessor_roleArn,
    createStreamProcessorResponse_streamProcessorArn,
    createStreamProcessorResponse_httpStatus,

    -- ** CreateUser
    createUser_clientRequestToken,
    createUser_collectionId,
    createUser_userId,
    createUserResponse_httpStatus,

    -- ** DeleteCollection
    deleteCollection_collectionId,
    deleteCollectionResponse_statusCode,
    deleteCollectionResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_datasetArn,
    deleteDatasetResponse_httpStatus,

    -- ** DeleteFaces
    deleteFaces_collectionId,
    deleteFaces_faceIds,
    deleteFacesResponse_deletedFaces,
    deleteFacesResponse_unsuccessfulFaceDeletions,
    deleteFacesResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_projectArn,
    deleteProjectResponse_status,
    deleteProjectResponse_httpStatus,

    -- ** DeleteProjectPolicy
    deleteProjectPolicy_policyRevisionId,
    deleteProjectPolicy_projectArn,
    deleteProjectPolicy_policyName,
    deleteProjectPolicyResponse_httpStatus,

    -- ** DeleteProjectVersion
    deleteProjectVersion_projectVersionArn,
    deleteProjectVersionResponse_status,
    deleteProjectVersionResponse_httpStatus,

    -- ** DeleteStreamProcessor
    deleteStreamProcessor_name,
    deleteStreamProcessorResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_clientRequestToken,
    deleteUser_collectionId,
    deleteUser_userId,
    deleteUserResponse_httpStatus,

    -- ** DescribeCollection
    describeCollection_collectionId,
    describeCollectionResponse_collectionARN,
    describeCollectionResponse_creationTimestamp,
    describeCollectionResponse_faceCount,
    describeCollectionResponse_faceModelVersion,
    describeCollectionResponse_userCount,
    describeCollectionResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetArn,
    describeDatasetResponse_datasetDescription,
    describeDatasetResponse_httpStatus,

    -- ** DescribeProjectVersions
    describeProjectVersions_maxResults,
    describeProjectVersions_nextToken,
    describeProjectVersions_versionNames,
    describeProjectVersions_projectArn,
    describeProjectVersionsResponse_nextToken,
    describeProjectVersionsResponse_projectVersionDescriptions,
    describeProjectVersionsResponse_httpStatus,

    -- ** DescribeProjects
    describeProjects_maxResults,
    describeProjects_nextToken,
    describeProjects_projectNames,
    describeProjectsResponse_nextToken,
    describeProjectsResponse_projectDescriptions,
    describeProjectsResponse_httpStatus,

    -- ** DescribeStreamProcessor
    describeStreamProcessor_name,
    describeStreamProcessorResponse_creationTimestamp,
    describeStreamProcessorResponse_dataSharingPreference,
    describeStreamProcessorResponse_input,
    describeStreamProcessorResponse_kmsKeyId,
    describeStreamProcessorResponse_lastUpdateTimestamp,
    describeStreamProcessorResponse_name,
    describeStreamProcessorResponse_notificationChannel,
    describeStreamProcessorResponse_output,
    describeStreamProcessorResponse_regionsOfInterest,
    describeStreamProcessorResponse_roleArn,
    describeStreamProcessorResponse_settings,
    describeStreamProcessorResponse_status,
    describeStreamProcessorResponse_statusMessage,
    describeStreamProcessorResponse_streamProcessorArn,
    describeStreamProcessorResponse_httpStatus,

    -- ** DetectCustomLabels
    detectCustomLabels_maxResults,
    detectCustomLabels_minConfidence,
    detectCustomLabels_projectVersionArn,
    detectCustomLabels_image,
    detectCustomLabelsResponse_customLabels,
    detectCustomLabelsResponse_httpStatus,

    -- ** DetectFaces
    detectFaces_attributes,
    detectFaces_image,
    detectFacesResponse_faceDetails,
    detectFacesResponse_orientationCorrection,
    detectFacesResponse_httpStatus,

    -- ** DetectLabels
    detectLabels_features,
    detectLabels_maxLabels,
    detectLabels_minConfidence,
    detectLabels_settings,
    detectLabels_image,
    detectLabelsResponse_imageProperties,
    detectLabelsResponse_labelModelVersion,
    detectLabelsResponse_labels,
    detectLabelsResponse_orientationCorrection,
    detectLabelsResponse_httpStatus,

    -- ** DetectModerationLabels
    detectModerationLabels_humanLoopConfig,
    detectModerationLabels_minConfidence,
    detectModerationLabels_image,
    detectModerationLabelsResponse_humanLoopActivationOutput,
    detectModerationLabelsResponse_moderationLabels,
    detectModerationLabelsResponse_moderationModelVersion,
    detectModerationLabelsResponse_httpStatus,

    -- ** DetectProtectiveEquipment
    detectProtectiveEquipment_summarizationAttributes,
    detectProtectiveEquipment_image,
    detectProtectiveEquipmentResponse_persons,
    detectProtectiveEquipmentResponse_protectiveEquipmentModelVersion,
    detectProtectiveEquipmentResponse_summary,
    detectProtectiveEquipmentResponse_httpStatus,

    -- ** DetectText
    detectText_filters,
    detectText_image,
    detectTextResponse_textDetections,
    detectTextResponse_textModelVersion,
    detectTextResponse_httpStatus,

    -- ** DisassociateFaces
    disassociateFaces_clientRequestToken,
    disassociateFaces_collectionId,
    disassociateFaces_userId,
    disassociateFaces_faceIds,
    disassociateFacesResponse_disassociatedFaces,
    disassociateFacesResponse_unsuccessfulFaceDisassociations,
    disassociateFacesResponse_userStatus,
    disassociateFacesResponse_httpStatus,

    -- ** DistributeDatasetEntries
    distributeDatasetEntries_datasets,
    distributeDatasetEntriesResponse_httpStatus,

    -- ** GetCelebrityInfo
    getCelebrityInfo_id,
    getCelebrityInfoResponse_knownGender,
    getCelebrityInfoResponse_name,
    getCelebrityInfoResponse_urls,
    getCelebrityInfoResponse_httpStatus,

    -- ** GetCelebrityRecognition
    getCelebrityRecognition_maxResults,
    getCelebrityRecognition_nextToken,
    getCelebrityRecognition_sortBy,
    getCelebrityRecognition_jobId,
    getCelebrityRecognitionResponse_celebrities,
    getCelebrityRecognitionResponse_jobId,
    getCelebrityRecognitionResponse_jobStatus,
    getCelebrityRecognitionResponse_jobTag,
    getCelebrityRecognitionResponse_nextToken,
    getCelebrityRecognitionResponse_statusMessage,
    getCelebrityRecognitionResponse_video,
    getCelebrityRecognitionResponse_videoMetadata,
    getCelebrityRecognitionResponse_httpStatus,

    -- ** GetContentModeration
    getContentModeration_aggregateBy,
    getContentModeration_maxResults,
    getContentModeration_nextToken,
    getContentModeration_sortBy,
    getContentModeration_jobId,
    getContentModerationResponse_getRequestMetadata,
    getContentModerationResponse_jobId,
    getContentModerationResponse_jobStatus,
    getContentModerationResponse_jobTag,
    getContentModerationResponse_moderationLabels,
    getContentModerationResponse_moderationModelVersion,
    getContentModerationResponse_nextToken,
    getContentModerationResponse_statusMessage,
    getContentModerationResponse_video,
    getContentModerationResponse_videoMetadata,
    getContentModerationResponse_httpStatus,

    -- ** GetFaceDetection
    getFaceDetection_maxResults,
    getFaceDetection_nextToken,
    getFaceDetection_jobId,
    getFaceDetectionResponse_faces,
    getFaceDetectionResponse_jobId,
    getFaceDetectionResponse_jobStatus,
    getFaceDetectionResponse_jobTag,
    getFaceDetectionResponse_nextToken,
    getFaceDetectionResponse_statusMessage,
    getFaceDetectionResponse_video,
    getFaceDetectionResponse_videoMetadata,
    getFaceDetectionResponse_httpStatus,

    -- ** GetFaceLivenessSessionResults
    getFaceLivenessSessionResults_sessionId,
    getFaceLivenessSessionResultsResponse_auditImages,
    getFaceLivenessSessionResultsResponse_confidence,
    getFaceLivenessSessionResultsResponse_referenceImage,
    getFaceLivenessSessionResultsResponse_httpStatus,
    getFaceLivenessSessionResultsResponse_sessionId,
    getFaceLivenessSessionResultsResponse_status,

    -- ** GetFaceSearch
    getFaceSearch_maxResults,
    getFaceSearch_nextToken,
    getFaceSearch_sortBy,
    getFaceSearch_jobId,
    getFaceSearchResponse_jobId,
    getFaceSearchResponse_jobStatus,
    getFaceSearchResponse_jobTag,
    getFaceSearchResponse_nextToken,
    getFaceSearchResponse_persons,
    getFaceSearchResponse_statusMessage,
    getFaceSearchResponse_video,
    getFaceSearchResponse_videoMetadata,
    getFaceSearchResponse_httpStatus,

    -- ** GetLabelDetection
    getLabelDetection_aggregateBy,
    getLabelDetection_maxResults,
    getLabelDetection_nextToken,
    getLabelDetection_sortBy,
    getLabelDetection_jobId,
    getLabelDetectionResponse_getRequestMetadata,
    getLabelDetectionResponse_jobId,
    getLabelDetectionResponse_jobStatus,
    getLabelDetectionResponse_jobTag,
    getLabelDetectionResponse_labelModelVersion,
    getLabelDetectionResponse_labels,
    getLabelDetectionResponse_nextToken,
    getLabelDetectionResponse_statusMessage,
    getLabelDetectionResponse_video,
    getLabelDetectionResponse_videoMetadata,
    getLabelDetectionResponse_httpStatus,

    -- ** GetPersonTracking
    getPersonTracking_maxResults,
    getPersonTracking_nextToken,
    getPersonTracking_sortBy,
    getPersonTracking_jobId,
    getPersonTrackingResponse_jobId,
    getPersonTrackingResponse_jobStatus,
    getPersonTrackingResponse_jobTag,
    getPersonTrackingResponse_nextToken,
    getPersonTrackingResponse_persons,
    getPersonTrackingResponse_statusMessage,
    getPersonTrackingResponse_video,
    getPersonTrackingResponse_videoMetadata,
    getPersonTrackingResponse_httpStatus,

    -- ** GetSegmentDetection
    getSegmentDetection_maxResults,
    getSegmentDetection_nextToken,
    getSegmentDetection_jobId,
    getSegmentDetectionResponse_audioMetadata,
    getSegmentDetectionResponse_jobId,
    getSegmentDetectionResponse_jobStatus,
    getSegmentDetectionResponse_jobTag,
    getSegmentDetectionResponse_nextToken,
    getSegmentDetectionResponse_segments,
    getSegmentDetectionResponse_selectedSegmentTypes,
    getSegmentDetectionResponse_statusMessage,
    getSegmentDetectionResponse_video,
    getSegmentDetectionResponse_videoMetadata,
    getSegmentDetectionResponse_httpStatus,

    -- ** GetTextDetection
    getTextDetection_maxResults,
    getTextDetection_nextToken,
    getTextDetection_jobId,
    getTextDetectionResponse_jobId,
    getTextDetectionResponse_jobStatus,
    getTextDetectionResponse_jobTag,
    getTextDetectionResponse_nextToken,
    getTextDetectionResponse_statusMessage,
    getTextDetectionResponse_textDetections,
    getTextDetectionResponse_textModelVersion,
    getTextDetectionResponse_video,
    getTextDetectionResponse_videoMetadata,
    getTextDetectionResponse_httpStatus,

    -- ** IndexFaces
    indexFaces_detectionAttributes,
    indexFaces_externalImageId,
    indexFaces_maxFaces,
    indexFaces_qualityFilter,
    indexFaces_collectionId,
    indexFaces_image,
    indexFacesResponse_faceModelVersion,
    indexFacesResponse_faceRecords,
    indexFacesResponse_orientationCorrection,
    indexFacesResponse_unindexedFaces,
    indexFacesResponse_httpStatus,

    -- ** ListCollections
    listCollections_maxResults,
    listCollections_nextToken,
    listCollectionsResponse_collectionIds,
    listCollectionsResponse_faceModelVersions,
    listCollectionsResponse_nextToken,
    listCollectionsResponse_httpStatus,

    -- ** ListDatasetEntries
    listDatasetEntries_containsLabels,
    listDatasetEntries_hasErrors,
    listDatasetEntries_labeled,
    listDatasetEntries_maxResults,
    listDatasetEntries_nextToken,
    listDatasetEntries_sourceRefContains,
    listDatasetEntries_datasetArn,
    listDatasetEntriesResponse_datasetEntries,
    listDatasetEntriesResponse_nextToken,
    listDatasetEntriesResponse_httpStatus,

    -- ** ListDatasetLabels
    listDatasetLabels_maxResults,
    listDatasetLabels_nextToken,
    listDatasetLabels_datasetArn,
    listDatasetLabelsResponse_datasetLabelDescriptions,
    listDatasetLabelsResponse_nextToken,
    listDatasetLabelsResponse_httpStatus,

    -- ** ListFaces
    listFaces_faceIds,
    listFaces_maxResults,
    listFaces_nextToken,
    listFaces_userId,
    listFaces_collectionId,
    listFacesResponse_faceModelVersion,
    listFacesResponse_faces,
    listFacesResponse_nextToken,
    listFacesResponse_httpStatus,

    -- ** ListProjectPolicies
    listProjectPolicies_maxResults,
    listProjectPolicies_nextToken,
    listProjectPolicies_projectArn,
    listProjectPoliciesResponse_nextToken,
    listProjectPoliciesResponse_projectPolicies,
    listProjectPoliciesResponse_httpStatus,

    -- ** ListStreamProcessors
    listStreamProcessors_maxResults,
    listStreamProcessors_nextToken,
    listStreamProcessorsResponse_nextToken,
    listStreamProcessorsResponse_streamProcessors,
    listStreamProcessorsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUsers
    listUsers_maxResults,
    listUsers_nextToken,
    listUsers_collectionId,
    listUsersResponse_nextToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** PutProjectPolicy
    putProjectPolicy_policyRevisionId,
    putProjectPolicy_projectArn,
    putProjectPolicy_policyName,
    putProjectPolicy_policyDocument,
    putProjectPolicyResponse_policyRevisionId,
    putProjectPolicyResponse_httpStatus,

    -- ** RecognizeCelebrities
    recognizeCelebrities_image,
    recognizeCelebritiesResponse_celebrityFaces,
    recognizeCelebritiesResponse_orientationCorrection,
    recognizeCelebritiesResponse_unrecognizedFaces,
    recognizeCelebritiesResponse_httpStatus,

    -- ** SearchFaces
    searchFaces_faceMatchThreshold,
    searchFaces_maxFaces,
    searchFaces_collectionId,
    searchFaces_faceId,
    searchFacesResponse_faceMatches,
    searchFacesResponse_faceModelVersion,
    searchFacesResponse_searchedFaceId,
    searchFacesResponse_httpStatus,

    -- ** SearchFacesByImage
    searchFacesByImage_faceMatchThreshold,
    searchFacesByImage_maxFaces,
    searchFacesByImage_qualityFilter,
    searchFacesByImage_collectionId,
    searchFacesByImage_image,
    searchFacesByImageResponse_faceMatches,
    searchFacesByImageResponse_faceModelVersion,
    searchFacesByImageResponse_searchedFaceBoundingBox,
    searchFacesByImageResponse_searchedFaceConfidence,
    searchFacesByImageResponse_httpStatus,

    -- ** SearchUsers
    searchUsers_faceId,
    searchUsers_maxUsers,
    searchUsers_userId,
    searchUsers_userMatchThreshold,
    searchUsers_collectionId,
    searchUsersResponse_faceModelVersion,
    searchUsersResponse_searchedFace,
    searchUsersResponse_searchedUser,
    searchUsersResponse_userMatches,
    searchUsersResponse_httpStatus,

    -- ** SearchUsersByImage
    searchUsersByImage_maxUsers,
    searchUsersByImage_qualityFilter,
    searchUsersByImage_userMatchThreshold,
    searchUsersByImage_collectionId,
    searchUsersByImage_image,
    searchUsersByImageResponse_faceModelVersion,
    searchUsersByImageResponse_searchedFace,
    searchUsersByImageResponse_unsearchedFaces,
    searchUsersByImageResponse_userMatches,
    searchUsersByImageResponse_httpStatus,

    -- ** StartCelebrityRecognition
    startCelebrityRecognition_clientRequestToken,
    startCelebrityRecognition_jobTag,
    startCelebrityRecognition_notificationChannel,
    startCelebrityRecognition_video,
    startCelebrityRecognitionResponse_jobId,
    startCelebrityRecognitionResponse_httpStatus,

    -- ** StartContentModeration
    startContentModeration_clientRequestToken,
    startContentModeration_jobTag,
    startContentModeration_minConfidence,
    startContentModeration_notificationChannel,
    startContentModeration_video,
    startContentModerationResponse_jobId,
    startContentModerationResponse_httpStatus,

    -- ** StartFaceDetection
    startFaceDetection_clientRequestToken,
    startFaceDetection_faceAttributes,
    startFaceDetection_jobTag,
    startFaceDetection_notificationChannel,
    startFaceDetection_video,
    startFaceDetectionResponse_jobId,
    startFaceDetectionResponse_httpStatus,

    -- ** StartFaceSearch
    startFaceSearch_clientRequestToken,
    startFaceSearch_faceMatchThreshold,
    startFaceSearch_jobTag,
    startFaceSearch_notificationChannel,
    startFaceSearch_video,
    startFaceSearch_collectionId,
    startFaceSearchResponse_jobId,
    startFaceSearchResponse_httpStatus,

    -- ** StartLabelDetection
    startLabelDetection_clientRequestToken,
    startLabelDetection_features,
    startLabelDetection_jobTag,
    startLabelDetection_minConfidence,
    startLabelDetection_notificationChannel,
    startLabelDetection_settings,
    startLabelDetection_video,
    startLabelDetectionResponse_jobId,
    startLabelDetectionResponse_httpStatus,

    -- ** StartPersonTracking
    startPersonTracking_clientRequestToken,
    startPersonTracking_jobTag,
    startPersonTracking_notificationChannel,
    startPersonTracking_video,
    startPersonTrackingResponse_jobId,
    startPersonTrackingResponse_httpStatus,

    -- ** StartProjectVersion
    startProjectVersion_maxInferenceUnits,
    startProjectVersion_projectVersionArn,
    startProjectVersion_minInferenceUnits,
    startProjectVersionResponse_status,
    startProjectVersionResponse_httpStatus,

    -- ** StartSegmentDetection
    startSegmentDetection_clientRequestToken,
    startSegmentDetection_filters,
    startSegmentDetection_jobTag,
    startSegmentDetection_notificationChannel,
    startSegmentDetection_video,
    startSegmentDetection_segmentTypes,
    startSegmentDetectionResponse_jobId,
    startSegmentDetectionResponse_httpStatus,

    -- ** StartStreamProcessor
    startStreamProcessor_startSelector,
    startStreamProcessor_stopSelector,
    startStreamProcessor_name,
    startStreamProcessorResponse_sessionId,
    startStreamProcessorResponse_httpStatus,

    -- ** StartTextDetection
    startTextDetection_clientRequestToken,
    startTextDetection_filters,
    startTextDetection_jobTag,
    startTextDetection_notificationChannel,
    startTextDetection_video,
    startTextDetectionResponse_jobId,
    startTextDetectionResponse_httpStatus,

    -- ** StopProjectVersion
    stopProjectVersion_projectVersionArn,
    stopProjectVersionResponse_status,
    stopProjectVersionResponse_httpStatus,

    -- ** StopStreamProcessor
    stopStreamProcessor_name,
    stopStreamProcessorResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDatasetEntries
    updateDatasetEntries_datasetArn,
    updateDatasetEntries_changes,
    updateDatasetEntriesResponse_httpStatus,

    -- ** UpdateStreamProcessor
    updateStreamProcessor_dataSharingPreferenceForUpdate,
    updateStreamProcessor_parametersToDelete,
    updateStreamProcessor_regionsOfInterestForUpdate,
    updateStreamProcessor_settingsForUpdate,
    updateStreamProcessor_name,
    updateStreamProcessorResponse_httpStatus,

    -- * Types

    -- ** AgeRange
    ageRange_high,
    ageRange_low,

    -- ** Asset
    asset_groundTruthManifest,

    -- ** AssociatedFace
    associatedFace_faceId,

    -- ** AudioMetadata
    audioMetadata_codec,
    audioMetadata_durationMillis,
    audioMetadata_numberOfChannels,
    audioMetadata_sampleRate,

    -- ** AuditImage
    auditImage_boundingBox,
    auditImage_bytes,
    auditImage_s3Object,

    -- ** Beard
    beard_confidence,
    beard_value,

    -- ** BlackFrame
    blackFrame_maxPixelThreshold,
    blackFrame_minCoveragePercentage,

    -- ** BoundingBox
    boundingBox_height,
    boundingBox_left,
    boundingBox_top,
    boundingBox_width,

    -- ** Celebrity
    celebrity_face,
    celebrity_id,
    celebrity_knownGender,
    celebrity_matchConfidence,
    celebrity_name,
    celebrity_urls,

    -- ** CelebrityDetail
    celebrityDetail_boundingBox,
    celebrityDetail_confidence,
    celebrityDetail_face,
    celebrityDetail_id,
    celebrityDetail_knownGender,
    celebrityDetail_name,
    celebrityDetail_urls,

    -- ** CelebrityRecognition
    celebrityRecognition_celebrity,
    celebrityRecognition_timestamp,

    -- ** CompareFacesMatch
    compareFacesMatch_face,
    compareFacesMatch_similarity,

    -- ** ComparedFace
    comparedFace_boundingBox,
    comparedFace_confidence,
    comparedFace_emotions,
    comparedFace_landmarks,
    comparedFace_pose,
    comparedFace_quality,
    comparedFace_smile,

    -- ** ComparedSourceImageFace
    comparedSourceImageFace_boundingBox,
    comparedSourceImageFace_confidence,

    -- ** ConnectedHomeSettings
    connectedHomeSettings_minConfidence,
    connectedHomeSettings_labels,

    -- ** ConnectedHomeSettingsForUpdate
    connectedHomeSettingsForUpdate_labels,
    connectedHomeSettingsForUpdate_minConfidence,

    -- ** ContentModerationDetection
    contentModerationDetection_durationMillis,
    contentModerationDetection_endTimestampMillis,
    contentModerationDetection_moderationLabel,
    contentModerationDetection_startTimestampMillis,
    contentModerationDetection_timestamp,

    -- ** CoversBodyPart
    coversBodyPart_confidence,
    coversBodyPart_value,

    -- ** CreateFaceLivenessSessionRequestSettings
    createFaceLivenessSessionRequestSettings_auditImagesLimit,
    createFaceLivenessSessionRequestSettings_outputConfig,

    -- ** CustomLabel
    customLabel_confidence,
    customLabel_geometry,
    customLabel_name,

    -- ** DatasetChanges
    datasetChanges_groundTruth,

    -- ** DatasetDescription
    datasetDescription_creationTimestamp,
    datasetDescription_datasetStats,
    datasetDescription_lastUpdatedTimestamp,
    datasetDescription_status,
    datasetDescription_statusMessage,
    datasetDescription_statusMessageCode,

    -- ** DatasetLabelDescription
    datasetLabelDescription_labelName,
    datasetLabelDescription_labelStats,

    -- ** DatasetLabelStats
    datasetLabelStats_boundingBoxCount,
    datasetLabelStats_entryCount,

    -- ** DatasetMetadata
    datasetMetadata_creationTimestamp,
    datasetMetadata_datasetArn,
    datasetMetadata_datasetType,
    datasetMetadata_status,
    datasetMetadata_statusMessage,
    datasetMetadata_statusMessageCode,

    -- ** DatasetSource
    datasetSource_datasetArn,
    datasetSource_groundTruthManifest,

    -- ** DatasetStats
    datasetStats_errorEntries,
    datasetStats_labeledEntries,
    datasetStats_totalEntries,
    datasetStats_totalLabels,

    -- ** DetectLabelsImageBackground
    detectLabelsImageBackground_dominantColors,
    detectLabelsImageBackground_quality,

    -- ** DetectLabelsImageForeground
    detectLabelsImageForeground_dominantColors,
    detectLabelsImageForeground_quality,

    -- ** DetectLabelsImageProperties
    detectLabelsImageProperties_background,
    detectLabelsImageProperties_dominantColors,
    detectLabelsImageProperties_foreground,
    detectLabelsImageProperties_quality,

    -- ** DetectLabelsImagePropertiesSettings
    detectLabelsImagePropertiesSettings_maxDominantColors,

    -- ** DetectLabelsImageQuality
    detectLabelsImageQuality_brightness,
    detectLabelsImageQuality_contrast,
    detectLabelsImageQuality_sharpness,

    -- ** DetectLabelsSettings
    detectLabelsSettings_generalLabels,
    detectLabelsSettings_imageProperties,

    -- ** DetectTextFilters
    detectTextFilters_regionsOfInterest,
    detectTextFilters_wordFilter,

    -- ** DetectionFilter
    detectionFilter_minBoundingBoxHeight,
    detectionFilter_minBoundingBoxWidth,
    detectionFilter_minConfidence,

    -- ** DisassociatedFace
    disassociatedFace_faceId,

    -- ** DistributeDataset
    distributeDataset_arn,

    -- ** DominantColor
    dominantColor_blue,
    dominantColor_cSSColor,
    dominantColor_green,
    dominantColor_hexCode,
    dominantColor_pixelPercent,
    dominantColor_red,
    dominantColor_simplifiedColor,

    -- ** Emotion
    emotion_confidence,
    emotion_type,

    -- ** EquipmentDetection
    equipmentDetection_boundingBox,
    equipmentDetection_confidence,
    equipmentDetection_coversBodyPart,
    equipmentDetection_type,

    -- ** EvaluationResult
    evaluationResult_f1Score,
    evaluationResult_summary,

    -- ** EyeDirection
    eyeDirection_confidence,
    eyeDirection_pitch,
    eyeDirection_yaw,

    -- ** EyeOpen
    eyeOpen_confidence,
    eyeOpen_value,

    -- ** Eyeglasses
    eyeglasses_confidence,
    eyeglasses_value,

    -- ** Face
    face_boundingBox,
    face_confidence,
    face_externalImageId,
    face_faceId,
    face_imageId,
    face_indexFacesModelVersion,
    face_userId,

    -- ** FaceDetail
    faceDetail_ageRange,
    faceDetail_beard,
    faceDetail_boundingBox,
    faceDetail_confidence,
    faceDetail_emotions,
    faceDetail_eyeDirection,
    faceDetail_eyeglasses,
    faceDetail_eyesOpen,
    faceDetail_faceOccluded,
    faceDetail_gender,
    faceDetail_landmarks,
    faceDetail_mouthOpen,
    faceDetail_mustache,
    faceDetail_pose,
    faceDetail_quality,
    faceDetail_smile,
    faceDetail_sunglasses,

    -- ** FaceDetection
    faceDetection_face,
    faceDetection_timestamp,

    -- ** FaceMatch
    faceMatch_face,
    faceMatch_similarity,

    -- ** FaceOccluded
    faceOccluded_confidence,
    faceOccluded_value,

    -- ** FaceRecord
    faceRecord_face,
    faceRecord_faceDetail,

    -- ** FaceSearchSettings
    faceSearchSettings_collectionId,
    faceSearchSettings_faceMatchThreshold,

    -- ** Gender
    gender_confidence,
    gender_value,

    -- ** GeneralLabelsSettings
    generalLabelsSettings_labelCategoryExclusionFilters,
    generalLabelsSettings_labelCategoryInclusionFilters,
    generalLabelsSettings_labelExclusionFilters,
    generalLabelsSettings_labelInclusionFilters,

    -- ** Geometry
    geometry_boundingBox,
    geometry_polygon,

    -- ** GetContentModerationRequestMetadata
    getContentModerationRequestMetadata_aggregateBy,
    getContentModerationRequestMetadata_sortBy,

    -- ** GetLabelDetectionRequestMetadata
    getLabelDetectionRequestMetadata_aggregateBy,
    getLabelDetectionRequestMetadata_sortBy,

    -- ** GroundTruthManifest
    groundTruthManifest_s3Object,

    -- ** HumanLoopActivationOutput
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,
    humanLoopActivationOutput_humanLoopActivationReasons,
    humanLoopActivationOutput_humanLoopArn,

    -- ** HumanLoopConfig
    humanLoopConfig_dataAttributes,
    humanLoopConfig_humanLoopName,
    humanLoopConfig_flowDefinitionArn,

    -- ** HumanLoopDataAttributes
    humanLoopDataAttributes_contentClassifiers,

    -- ** Image
    image_bytes,
    image_s3Object,

    -- ** ImageQuality
    imageQuality_brightness,
    imageQuality_sharpness,

    -- ** Instance
    instance_boundingBox,
    instance_confidence,
    instance_dominantColors,

    -- ** KinesisDataStream
    kinesisDataStream_arn,

    -- ** KinesisVideoStream
    kinesisVideoStream_arn,

    -- ** KinesisVideoStreamStartSelector
    kinesisVideoStreamStartSelector_fragmentNumber,
    kinesisVideoStreamStartSelector_producerTimestamp,

    -- ** KnownGender
    knownGender_type,

    -- ** Label
    label_aliases,
    label_categories,
    label_confidence,
    label_instances,
    label_name,
    label_parents,

    -- ** LabelAlias
    labelAlias_name,

    -- ** LabelCategory
    labelCategory_name,

    -- ** LabelDetection
    labelDetection_durationMillis,
    labelDetection_endTimestampMillis,
    labelDetection_label,
    labelDetection_startTimestampMillis,
    labelDetection_timestamp,

    -- ** LabelDetectionSettings
    labelDetectionSettings_generalLabels,

    -- ** Landmark
    landmark_type,
    landmark_x,
    landmark_y,

    -- ** LivenessOutputConfig
    livenessOutputConfig_s3KeyPrefix,
    livenessOutputConfig_s3Bucket,

    -- ** MatchedUser
    matchedUser_userId,
    matchedUser_userStatus,

    -- ** ModerationLabel
    moderationLabel_confidence,
    moderationLabel_name,
    moderationLabel_parentName,

    -- ** MouthOpen
    mouthOpen_confidence,
    mouthOpen_value,

    -- ** Mustache
    mustache_confidence,
    mustache_value,

    -- ** NotificationChannel
    notificationChannel_sNSTopicArn,
    notificationChannel_roleArn,

    -- ** OutputConfig
    outputConfig_s3Bucket,
    outputConfig_s3KeyPrefix,

    -- ** Parent
    parent_name,

    -- ** PersonDetail
    personDetail_boundingBox,
    personDetail_face,
    personDetail_index,

    -- ** PersonDetection
    personDetection_person,
    personDetection_timestamp,

    -- ** PersonMatch
    personMatch_faceMatches,
    personMatch_person,
    personMatch_timestamp,

    -- ** Point
    point_x,
    point_y,

    -- ** Pose
    pose_pitch,
    pose_roll,
    pose_yaw,

    -- ** ProjectDescription
    projectDescription_creationTimestamp,
    projectDescription_datasets,
    projectDescription_projectArn,
    projectDescription_status,

    -- ** ProjectPolicy
    projectPolicy_creationTimestamp,
    projectPolicy_lastUpdatedTimestamp,
    projectPolicy_policyDocument,
    projectPolicy_policyName,
    projectPolicy_policyRevisionId,
    projectPolicy_projectArn,

    -- ** ProjectVersionDescription
    projectVersionDescription_billableTrainingTimeInSeconds,
    projectVersionDescription_creationTimestamp,
    projectVersionDescription_evaluationResult,
    projectVersionDescription_kmsKeyId,
    projectVersionDescription_manifestSummary,
    projectVersionDescription_maxInferenceUnits,
    projectVersionDescription_minInferenceUnits,
    projectVersionDescription_outputConfig,
    projectVersionDescription_projectVersionArn,
    projectVersionDescription_sourceProjectVersionArn,
    projectVersionDescription_status,
    projectVersionDescription_statusMessage,
    projectVersionDescription_testingDataResult,
    projectVersionDescription_trainingDataResult,
    projectVersionDescription_trainingEndTimestamp,

    -- ** ProtectiveEquipmentBodyPart
    protectiveEquipmentBodyPart_confidence,
    protectiveEquipmentBodyPart_equipmentDetections,
    protectiveEquipmentBodyPart_name,

    -- ** ProtectiveEquipmentPerson
    protectiveEquipmentPerson_bodyParts,
    protectiveEquipmentPerson_boundingBox,
    protectiveEquipmentPerson_confidence,
    protectiveEquipmentPerson_id,

    -- ** ProtectiveEquipmentSummarizationAttributes
    protectiveEquipmentSummarizationAttributes_minConfidence,
    protectiveEquipmentSummarizationAttributes_requiredEquipmentTypes,

    -- ** ProtectiveEquipmentSummary
    protectiveEquipmentSummary_personsIndeterminate,
    protectiveEquipmentSummary_personsWithRequiredEquipment,
    protectiveEquipmentSummary_personsWithoutRequiredEquipment,

    -- ** RegionOfInterest
    regionOfInterest_boundingBox,
    regionOfInterest_polygon,

    -- ** S3Destination
    s3Destination_bucket,
    s3Destination_keyPrefix,

    -- ** S3Object
    s3Object_bucket,
    s3Object_name,
    s3Object_version,

    -- ** SearchedFace
    searchedFace_faceId,

    -- ** SearchedFaceDetails
    searchedFaceDetails_faceDetail,

    -- ** SearchedUser
    searchedUser_userId,

    -- ** SegmentDetection
    segmentDetection_durationFrames,
    segmentDetection_durationMillis,
    segmentDetection_durationSMPTE,
    segmentDetection_endFrameNumber,
    segmentDetection_endTimecodeSMPTE,
    segmentDetection_endTimestampMillis,
    segmentDetection_shotSegment,
    segmentDetection_startFrameNumber,
    segmentDetection_startTimecodeSMPTE,
    segmentDetection_startTimestampMillis,
    segmentDetection_technicalCueSegment,
    segmentDetection_type,

    -- ** SegmentTypeInfo
    segmentTypeInfo_modelVersion,
    segmentTypeInfo_type,

    -- ** ShotSegment
    shotSegment_confidence,
    shotSegment_index,

    -- ** Smile
    smile_confidence,
    smile_value,

    -- ** StartSegmentDetectionFilters
    startSegmentDetectionFilters_shotFilter,
    startSegmentDetectionFilters_technicalCueFilter,

    -- ** StartShotDetectionFilter
    startShotDetectionFilter_minSegmentConfidence,

    -- ** StartTechnicalCueDetectionFilter
    startTechnicalCueDetectionFilter_blackFrame,
    startTechnicalCueDetectionFilter_minSegmentConfidence,

    -- ** StartTextDetectionFilters
    startTextDetectionFilters_regionsOfInterest,
    startTextDetectionFilters_wordFilter,

    -- ** StreamProcessingStartSelector
    streamProcessingStartSelector_kVSStreamStartSelector,

    -- ** StreamProcessingStopSelector
    streamProcessingStopSelector_maxDurationInSeconds,

    -- ** StreamProcessor
    streamProcessor_name,
    streamProcessor_status,

    -- ** StreamProcessorDataSharingPreference
    streamProcessorDataSharingPreference_optIn,

    -- ** StreamProcessorInput
    streamProcessorInput_kinesisVideoStream,

    -- ** StreamProcessorNotificationChannel
    streamProcessorNotificationChannel_sNSTopicArn,

    -- ** StreamProcessorOutput
    streamProcessorOutput_kinesisDataStream,
    streamProcessorOutput_s3Destination,

    -- ** StreamProcessorSettings
    streamProcessorSettings_connectedHome,
    streamProcessorSettings_faceSearch,

    -- ** StreamProcessorSettingsForUpdate
    streamProcessorSettingsForUpdate_connectedHomeForUpdate,

    -- ** Summary
    summary_s3Object,

    -- ** Sunglasses
    sunglasses_confidence,
    sunglasses_value,

    -- ** TechnicalCueSegment
    technicalCueSegment_confidence,
    technicalCueSegment_type,

    -- ** TestingData
    testingData_assets,
    testingData_autoCreate,

    -- ** TestingDataResult
    testingDataResult_input,
    testingDataResult_output,
    testingDataResult_validation,

    -- ** TextDetection
    textDetection_confidence,
    textDetection_detectedText,
    textDetection_geometry,
    textDetection_id,
    textDetection_parentId,
    textDetection_type,

    -- ** TextDetectionResult
    textDetectionResult_textDetection,
    textDetectionResult_timestamp,

    -- ** TrainingData
    trainingData_assets,

    -- ** TrainingDataResult
    trainingDataResult_input,
    trainingDataResult_output,
    trainingDataResult_validation,

    -- ** UnindexedFace
    unindexedFace_faceDetail,
    unindexedFace_reasons,

    -- ** UnsearchedFace
    unsearchedFace_faceDetails,
    unsearchedFace_reasons,

    -- ** UnsuccessfulFaceAssociation
    unsuccessfulFaceAssociation_confidence,
    unsuccessfulFaceAssociation_faceId,
    unsuccessfulFaceAssociation_reasons,
    unsuccessfulFaceAssociation_userId,

    -- ** UnsuccessfulFaceDeletion
    unsuccessfulFaceDeletion_faceId,
    unsuccessfulFaceDeletion_reasons,
    unsuccessfulFaceDeletion_userId,

    -- ** UnsuccessfulFaceDisassociation
    unsuccessfulFaceDisassociation_faceId,
    unsuccessfulFaceDisassociation_reasons,
    unsuccessfulFaceDisassociation_userId,

    -- ** User
    user_userId,
    user_userStatus,

    -- ** UserMatch
    userMatch_similarity,
    userMatch_user,

    -- ** ValidationData
    validationData_assets,

    -- ** Video
    video_s3Object,

    -- ** VideoMetadata
    videoMetadata_codec,
    videoMetadata_colorRange,
    videoMetadata_durationMillis,
    videoMetadata_format,
    videoMetadata_frameHeight,
    videoMetadata_frameRate,
    videoMetadata_frameWidth,
  )
where

import Amazonka.Rekognition.AssociateFaces
import Amazonka.Rekognition.CompareFaces
import Amazonka.Rekognition.CopyProjectVersion
import Amazonka.Rekognition.CreateCollection
import Amazonka.Rekognition.CreateDataset
import Amazonka.Rekognition.CreateFaceLivenessSession
import Amazonka.Rekognition.CreateProject
import Amazonka.Rekognition.CreateProjectVersion
import Amazonka.Rekognition.CreateStreamProcessor
import Amazonka.Rekognition.CreateUser
import Amazonka.Rekognition.DeleteCollection
import Amazonka.Rekognition.DeleteDataset
import Amazonka.Rekognition.DeleteFaces
import Amazonka.Rekognition.DeleteProject
import Amazonka.Rekognition.DeleteProjectPolicy
import Amazonka.Rekognition.DeleteProjectVersion
import Amazonka.Rekognition.DeleteStreamProcessor
import Amazonka.Rekognition.DeleteUser
import Amazonka.Rekognition.DescribeCollection
import Amazonka.Rekognition.DescribeDataset
import Amazonka.Rekognition.DescribeProjectVersions
import Amazonka.Rekognition.DescribeProjects
import Amazonka.Rekognition.DescribeStreamProcessor
import Amazonka.Rekognition.DetectCustomLabels
import Amazonka.Rekognition.DetectFaces
import Amazonka.Rekognition.DetectLabels
import Amazonka.Rekognition.DetectModerationLabels
import Amazonka.Rekognition.DetectProtectiveEquipment
import Amazonka.Rekognition.DetectText
import Amazonka.Rekognition.DisassociateFaces
import Amazonka.Rekognition.DistributeDatasetEntries
import Amazonka.Rekognition.GetCelebrityInfo
import Amazonka.Rekognition.GetCelebrityRecognition
import Amazonka.Rekognition.GetContentModeration
import Amazonka.Rekognition.GetFaceDetection
import Amazonka.Rekognition.GetFaceLivenessSessionResults
import Amazonka.Rekognition.GetFaceSearch
import Amazonka.Rekognition.GetLabelDetection
import Amazonka.Rekognition.GetPersonTracking
import Amazonka.Rekognition.GetSegmentDetection
import Amazonka.Rekognition.GetTextDetection
import Amazonka.Rekognition.IndexFaces
import Amazonka.Rekognition.ListCollections
import Amazonka.Rekognition.ListDatasetEntries
import Amazonka.Rekognition.ListDatasetLabels
import Amazonka.Rekognition.ListFaces
import Amazonka.Rekognition.ListProjectPolicies
import Amazonka.Rekognition.ListStreamProcessors
import Amazonka.Rekognition.ListTagsForResource
import Amazonka.Rekognition.ListUsers
import Amazonka.Rekognition.PutProjectPolicy
import Amazonka.Rekognition.RecognizeCelebrities
import Amazonka.Rekognition.SearchFaces
import Amazonka.Rekognition.SearchFacesByImage
import Amazonka.Rekognition.SearchUsers
import Amazonka.Rekognition.SearchUsersByImage
import Amazonka.Rekognition.StartCelebrityRecognition
import Amazonka.Rekognition.StartContentModeration
import Amazonka.Rekognition.StartFaceDetection
import Amazonka.Rekognition.StartFaceSearch
import Amazonka.Rekognition.StartLabelDetection
import Amazonka.Rekognition.StartPersonTracking
import Amazonka.Rekognition.StartProjectVersion
import Amazonka.Rekognition.StartSegmentDetection
import Amazonka.Rekognition.StartStreamProcessor
import Amazonka.Rekognition.StartTextDetection
import Amazonka.Rekognition.StopProjectVersion
import Amazonka.Rekognition.StopStreamProcessor
import Amazonka.Rekognition.TagResource
import Amazonka.Rekognition.Types.AgeRange
import Amazonka.Rekognition.Types.Asset
import Amazonka.Rekognition.Types.AssociatedFace
import Amazonka.Rekognition.Types.AudioMetadata
import Amazonka.Rekognition.Types.AuditImage
import Amazonka.Rekognition.Types.Beard
import Amazonka.Rekognition.Types.BlackFrame
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.Celebrity
import Amazonka.Rekognition.Types.CelebrityDetail
import Amazonka.Rekognition.Types.CelebrityRecognition
import Amazonka.Rekognition.Types.CompareFacesMatch
import Amazonka.Rekognition.Types.ComparedFace
import Amazonka.Rekognition.Types.ComparedSourceImageFace
import Amazonka.Rekognition.Types.ConnectedHomeSettings
import Amazonka.Rekognition.Types.ConnectedHomeSettingsForUpdate
import Amazonka.Rekognition.Types.ContentModerationDetection
import Amazonka.Rekognition.Types.CoversBodyPart
import Amazonka.Rekognition.Types.CreateFaceLivenessSessionRequestSettings
import Amazonka.Rekognition.Types.CustomLabel
import Amazonka.Rekognition.Types.DatasetChanges
import Amazonka.Rekognition.Types.DatasetDescription
import Amazonka.Rekognition.Types.DatasetLabelDescription
import Amazonka.Rekognition.Types.DatasetLabelStats
import Amazonka.Rekognition.Types.DatasetMetadata
import Amazonka.Rekognition.Types.DatasetSource
import Amazonka.Rekognition.Types.DatasetStats
import Amazonka.Rekognition.Types.DetectLabelsImageBackground
import Amazonka.Rekognition.Types.DetectLabelsImageForeground
import Amazonka.Rekognition.Types.DetectLabelsImageProperties
import Amazonka.Rekognition.Types.DetectLabelsImagePropertiesSettings
import Amazonka.Rekognition.Types.DetectLabelsImageQuality
import Amazonka.Rekognition.Types.DetectLabelsSettings
import Amazonka.Rekognition.Types.DetectTextFilters
import Amazonka.Rekognition.Types.DetectionFilter
import Amazonka.Rekognition.Types.DisassociatedFace
import Amazonka.Rekognition.Types.DistributeDataset
import Amazonka.Rekognition.Types.DominantColor
import Amazonka.Rekognition.Types.Emotion
import Amazonka.Rekognition.Types.EquipmentDetection
import Amazonka.Rekognition.Types.EvaluationResult
import Amazonka.Rekognition.Types.EyeDirection
import Amazonka.Rekognition.Types.EyeOpen
import Amazonka.Rekognition.Types.Eyeglasses
import Amazonka.Rekognition.Types.Face
import Amazonka.Rekognition.Types.FaceDetail
import Amazonka.Rekognition.Types.FaceDetection
import Amazonka.Rekognition.Types.FaceMatch
import Amazonka.Rekognition.Types.FaceOccluded
import Amazonka.Rekognition.Types.FaceRecord
import Amazonka.Rekognition.Types.FaceSearchSettings
import Amazonka.Rekognition.Types.Gender
import Amazonka.Rekognition.Types.GeneralLabelsSettings
import Amazonka.Rekognition.Types.Geometry
import Amazonka.Rekognition.Types.GetContentModerationRequestMetadata
import Amazonka.Rekognition.Types.GetLabelDetectionRequestMetadata
import Amazonka.Rekognition.Types.GroundTruthManifest
import Amazonka.Rekognition.Types.HumanLoopActivationOutput
import Amazonka.Rekognition.Types.HumanLoopConfig
import Amazonka.Rekognition.Types.HumanLoopDataAttributes
import Amazonka.Rekognition.Types.Image
import Amazonka.Rekognition.Types.ImageQuality
import Amazonka.Rekognition.Types.Instance
import Amazonka.Rekognition.Types.KinesisDataStream
import Amazonka.Rekognition.Types.KinesisVideoStream
import Amazonka.Rekognition.Types.KinesisVideoStreamStartSelector
import Amazonka.Rekognition.Types.KnownGender
import Amazonka.Rekognition.Types.Label
import Amazonka.Rekognition.Types.LabelAlias
import Amazonka.Rekognition.Types.LabelCategory
import Amazonka.Rekognition.Types.LabelDetection
import Amazonka.Rekognition.Types.LabelDetectionSettings
import Amazonka.Rekognition.Types.Landmark
import Amazonka.Rekognition.Types.LivenessOutputConfig
import Amazonka.Rekognition.Types.MatchedUser
import Amazonka.Rekognition.Types.ModerationLabel
import Amazonka.Rekognition.Types.MouthOpen
import Amazonka.Rekognition.Types.Mustache
import Amazonka.Rekognition.Types.NotificationChannel
import Amazonka.Rekognition.Types.OutputConfig
import Amazonka.Rekognition.Types.Parent
import Amazonka.Rekognition.Types.PersonDetail
import Amazonka.Rekognition.Types.PersonDetection
import Amazonka.Rekognition.Types.PersonMatch
import Amazonka.Rekognition.Types.Point
import Amazonka.Rekognition.Types.Pose
import Amazonka.Rekognition.Types.ProjectDescription
import Amazonka.Rekognition.Types.ProjectPolicy
import Amazonka.Rekognition.Types.ProjectVersionDescription
import Amazonka.Rekognition.Types.ProtectiveEquipmentBodyPart
import Amazonka.Rekognition.Types.ProtectiveEquipmentPerson
import Amazonka.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes
import Amazonka.Rekognition.Types.ProtectiveEquipmentSummary
import Amazonka.Rekognition.Types.RegionOfInterest
import Amazonka.Rekognition.Types.S3Destination
import Amazonka.Rekognition.Types.S3Object
import Amazonka.Rekognition.Types.SearchedFace
import Amazonka.Rekognition.Types.SearchedFaceDetails
import Amazonka.Rekognition.Types.SearchedUser
import Amazonka.Rekognition.Types.SegmentDetection
import Amazonka.Rekognition.Types.SegmentTypeInfo
import Amazonka.Rekognition.Types.ShotSegment
import Amazonka.Rekognition.Types.Smile
import Amazonka.Rekognition.Types.StartSegmentDetectionFilters
import Amazonka.Rekognition.Types.StartShotDetectionFilter
import Amazonka.Rekognition.Types.StartTechnicalCueDetectionFilter
import Amazonka.Rekognition.Types.StartTextDetectionFilters
import Amazonka.Rekognition.Types.StreamProcessingStartSelector
import Amazonka.Rekognition.Types.StreamProcessingStopSelector
import Amazonka.Rekognition.Types.StreamProcessor
import Amazonka.Rekognition.Types.StreamProcessorDataSharingPreference
import Amazonka.Rekognition.Types.StreamProcessorInput
import Amazonka.Rekognition.Types.StreamProcessorNotificationChannel
import Amazonka.Rekognition.Types.StreamProcessorOutput
import Amazonka.Rekognition.Types.StreamProcessorSettings
import Amazonka.Rekognition.Types.StreamProcessorSettingsForUpdate
import Amazonka.Rekognition.Types.Summary
import Amazonka.Rekognition.Types.Sunglasses
import Amazonka.Rekognition.Types.TechnicalCueSegment
import Amazonka.Rekognition.Types.TestingData
import Amazonka.Rekognition.Types.TestingDataResult
import Amazonka.Rekognition.Types.TextDetection
import Amazonka.Rekognition.Types.TextDetectionResult
import Amazonka.Rekognition.Types.TrainingData
import Amazonka.Rekognition.Types.TrainingDataResult
import Amazonka.Rekognition.Types.UnindexedFace
import Amazonka.Rekognition.Types.UnsearchedFace
import Amazonka.Rekognition.Types.UnsuccessfulFaceAssociation
import Amazonka.Rekognition.Types.UnsuccessfulFaceDeletion
import Amazonka.Rekognition.Types.UnsuccessfulFaceDisassociation
import Amazonka.Rekognition.Types.User
import Amazonka.Rekognition.Types.UserMatch
import Amazonka.Rekognition.Types.ValidationData
import Amazonka.Rekognition.Types.Video
import Amazonka.Rekognition.Types.VideoMetadata
import Amazonka.Rekognition.UntagResource
import Amazonka.Rekognition.UpdateDatasetEntries
import Amazonka.Rekognition.UpdateStreamProcessor
