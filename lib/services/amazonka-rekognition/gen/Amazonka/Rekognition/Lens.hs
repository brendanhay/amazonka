{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rekognition.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Lens
  ( -- * Operations

    -- ** CompareFaces
    compareFaces_qualityFilter,
    compareFaces_similarityThreshold,
    compareFaces_sourceImage,
    compareFaces_targetImage,
    compareFacesResponse_targetImageOrientationCorrection,
    compareFacesResponse_sourceImageOrientationCorrection,
    compareFacesResponse_faceMatches,
    compareFacesResponse_unmatchedFaces,
    compareFacesResponse_sourceImageFace,
    compareFacesResponse_httpStatus,

    -- ** CopyProjectVersion
    copyProjectVersion_tags,
    copyProjectVersion_kmsKeyId,
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
    createCollectionResponse_statusCode,
    createCollectionResponse_faceModelVersion,
    createCollectionResponse_collectionArn,
    createCollectionResponse_httpStatus,

    -- ** CreateDataset
    createDataset_datasetSource,
    createDataset_datasetType,
    createDataset_projectArn,
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,

    -- ** CreateProject
    createProject_projectName,
    createProjectResponse_projectArn,
    createProjectResponse_httpStatus,

    -- ** CreateProjectVersion
    createProjectVersion_tags,
    createProjectVersion_testingData,
    createProjectVersion_kmsKeyId,
    createProjectVersion_trainingData,
    createProjectVersion_projectArn,
    createProjectVersion_versionName,
    createProjectVersion_outputConfig,
    createProjectVersionResponse_projectVersionArn,
    createProjectVersionResponse_httpStatus,

    -- ** CreateStreamProcessor
    createStreamProcessor_tags,
    createStreamProcessor_regionsOfInterest,
    createStreamProcessor_kmsKeyId,
    createStreamProcessor_dataSharingPreference,
    createStreamProcessor_notificationChannel,
    createStreamProcessor_input,
    createStreamProcessor_output,
    createStreamProcessor_name,
    createStreamProcessor_settings,
    createStreamProcessor_roleArn,
    createStreamProcessorResponse_streamProcessorArn,
    createStreamProcessorResponse_httpStatus,

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

    -- ** DescribeCollection
    describeCollection_collectionId,
    describeCollectionResponse_creationTimestamp,
    describeCollectionResponse_faceCount,
    describeCollectionResponse_faceModelVersion,
    describeCollectionResponse_collectionARN,
    describeCollectionResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetArn,
    describeDatasetResponse_datasetDescription,
    describeDatasetResponse_httpStatus,

    -- ** DescribeProjectVersions
    describeProjectVersions_nextToken,
    describeProjectVersions_versionNames,
    describeProjectVersions_maxResults,
    describeProjectVersions_projectArn,
    describeProjectVersionsResponse_nextToken,
    describeProjectVersionsResponse_projectVersionDescriptions,
    describeProjectVersionsResponse_httpStatus,

    -- ** DescribeProjects
    describeProjects_nextToken,
    describeProjects_projectNames,
    describeProjects_maxResults,
    describeProjectsResponse_nextToken,
    describeProjectsResponse_projectDescriptions,
    describeProjectsResponse_httpStatus,

    -- ** DescribeStreamProcessor
    describeStreamProcessor_name,
    describeStreamProcessorResponse_streamProcessorArn,
    describeStreamProcessorResponse_regionsOfInterest,
    describeStreamProcessorResponse_name,
    describeStreamProcessorResponse_lastUpdateTimestamp,
    describeStreamProcessorResponse_roleArn,
    describeStreamProcessorResponse_status,
    describeStreamProcessorResponse_creationTimestamp,
    describeStreamProcessorResponse_input,
    describeStreamProcessorResponse_settings,
    describeStreamProcessorResponse_output,
    describeStreamProcessorResponse_kmsKeyId,
    describeStreamProcessorResponse_dataSharingPreference,
    describeStreamProcessorResponse_statusMessage,
    describeStreamProcessorResponse_notificationChannel,
    describeStreamProcessorResponse_httpStatus,

    -- ** DetectCustomLabels
    detectCustomLabels_minConfidence,
    detectCustomLabels_maxResults,
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
    detectLabels_maxLabels,
    detectLabels_features,
    detectLabels_settings,
    detectLabels_minConfidence,
    detectLabels_image,
    detectLabelsResponse_labelModelVersion,
    detectLabelsResponse_orientationCorrection,
    detectLabelsResponse_labels,
    detectLabelsResponse_imageProperties,
    detectLabelsResponse_httpStatus,

    -- ** DetectModerationLabels
    detectModerationLabels_humanLoopConfig,
    detectModerationLabels_minConfidence,
    detectModerationLabels_image,
    detectModerationLabelsResponse_moderationLabels,
    detectModerationLabelsResponse_humanLoopActivationOutput,
    detectModerationLabelsResponse_moderationModelVersion,
    detectModerationLabelsResponse_httpStatus,

    -- ** DetectProtectiveEquipment
    detectProtectiveEquipment_summarizationAttributes,
    detectProtectiveEquipment_image,
    detectProtectiveEquipmentResponse_protectiveEquipmentModelVersion,
    detectProtectiveEquipmentResponse_summary,
    detectProtectiveEquipmentResponse_persons,
    detectProtectiveEquipmentResponse_httpStatus,

    -- ** DetectText
    detectText_filters,
    detectText_image,
    detectTextResponse_textDetections,
    detectTextResponse_textModelVersion,
    detectTextResponse_httpStatus,

    -- ** DistributeDatasetEntries
    distributeDatasetEntries_datasets,
    distributeDatasetEntriesResponse_httpStatus,

    -- ** GetCelebrityInfo
    getCelebrityInfo_id,
    getCelebrityInfoResponse_name,
    getCelebrityInfoResponse_knownGender,
    getCelebrityInfoResponse_urls,
    getCelebrityInfoResponse_httpStatus,

    -- ** GetCelebrityRecognition
    getCelebrityRecognition_nextToken,
    getCelebrityRecognition_sortBy,
    getCelebrityRecognition_maxResults,
    getCelebrityRecognition_jobId,
    getCelebrityRecognitionResponse_nextToken,
    getCelebrityRecognitionResponse_jobStatus,
    getCelebrityRecognitionResponse_celebrities,
    getCelebrityRecognitionResponse_videoMetadata,
    getCelebrityRecognitionResponse_statusMessage,
    getCelebrityRecognitionResponse_httpStatus,

    -- ** GetContentModeration
    getContentModeration_nextToken,
    getContentModeration_sortBy,
    getContentModeration_maxResults,
    getContentModeration_jobId,
    getContentModerationResponse_nextToken,
    getContentModerationResponse_jobStatus,
    getContentModerationResponse_moderationLabels,
    getContentModerationResponse_videoMetadata,
    getContentModerationResponse_statusMessage,
    getContentModerationResponse_moderationModelVersion,
    getContentModerationResponse_httpStatus,

    -- ** GetFaceDetection
    getFaceDetection_nextToken,
    getFaceDetection_maxResults,
    getFaceDetection_jobId,
    getFaceDetectionResponse_nextToken,
    getFaceDetectionResponse_jobStatus,
    getFaceDetectionResponse_faces,
    getFaceDetectionResponse_videoMetadata,
    getFaceDetectionResponse_statusMessage,
    getFaceDetectionResponse_httpStatus,

    -- ** GetFaceSearch
    getFaceSearch_nextToken,
    getFaceSearch_sortBy,
    getFaceSearch_maxResults,
    getFaceSearch_jobId,
    getFaceSearchResponse_nextToken,
    getFaceSearchResponse_jobStatus,
    getFaceSearchResponse_videoMetadata,
    getFaceSearchResponse_persons,
    getFaceSearchResponse_statusMessage,
    getFaceSearchResponse_httpStatus,

    -- ** GetLabelDetection
    getLabelDetection_nextToken,
    getLabelDetection_sortBy,
    getLabelDetection_maxResults,
    getLabelDetection_jobId,
    getLabelDetectionResponse_nextToken,
    getLabelDetectionResponse_jobStatus,
    getLabelDetectionResponse_labelModelVersion,
    getLabelDetectionResponse_labels,
    getLabelDetectionResponse_videoMetadata,
    getLabelDetectionResponse_statusMessage,
    getLabelDetectionResponse_httpStatus,

    -- ** GetPersonTracking
    getPersonTracking_nextToken,
    getPersonTracking_sortBy,
    getPersonTracking_maxResults,
    getPersonTracking_jobId,
    getPersonTrackingResponse_nextToken,
    getPersonTrackingResponse_jobStatus,
    getPersonTrackingResponse_videoMetadata,
    getPersonTrackingResponse_persons,
    getPersonTrackingResponse_statusMessage,
    getPersonTrackingResponse_httpStatus,

    -- ** GetSegmentDetection
    getSegmentDetection_nextToken,
    getSegmentDetection_maxResults,
    getSegmentDetection_jobId,
    getSegmentDetectionResponse_nextToken,
    getSegmentDetectionResponse_jobStatus,
    getSegmentDetectionResponse_videoMetadata,
    getSegmentDetectionResponse_selectedSegmentTypes,
    getSegmentDetectionResponse_statusMessage,
    getSegmentDetectionResponse_segments,
    getSegmentDetectionResponse_audioMetadata,
    getSegmentDetectionResponse_httpStatus,

    -- ** GetTextDetection
    getTextDetection_nextToken,
    getTextDetection_maxResults,
    getTextDetection_jobId,
    getTextDetectionResponse_nextToken,
    getTextDetectionResponse_jobStatus,
    getTextDetectionResponse_textDetections,
    getTextDetectionResponse_videoMetadata,
    getTextDetectionResponse_textModelVersion,
    getTextDetectionResponse_statusMessage,
    getTextDetectionResponse_httpStatus,

    -- ** IndexFaces
    indexFaces_qualityFilter,
    indexFaces_detectionAttributes,
    indexFaces_externalImageId,
    indexFaces_maxFaces,
    indexFaces_collectionId,
    indexFaces_image,
    indexFacesResponse_unindexedFaces,
    indexFacesResponse_faceRecords,
    indexFacesResponse_orientationCorrection,
    indexFacesResponse_faceModelVersion,
    indexFacesResponse_httpStatus,

    -- ** ListCollections
    listCollections_nextToken,
    listCollections_maxResults,
    listCollectionsResponse_collectionIds,
    listCollectionsResponse_nextToken,
    listCollectionsResponse_faceModelVersions,
    listCollectionsResponse_httpStatus,

    -- ** ListDatasetEntries
    listDatasetEntries_nextToken,
    listDatasetEntries_labeled,
    listDatasetEntries_containsLabels,
    listDatasetEntries_sourceRefContains,
    listDatasetEntries_hasErrors,
    listDatasetEntries_maxResults,
    listDatasetEntries_datasetArn,
    listDatasetEntriesResponse_nextToken,
    listDatasetEntriesResponse_datasetEntries,
    listDatasetEntriesResponse_httpStatus,

    -- ** ListDatasetLabels
    listDatasetLabels_nextToken,
    listDatasetLabels_maxResults,
    listDatasetLabels_datasetArn,
    listDatasetLabelsResponse_nextToken,
    listDatasetLabelsResponse_datasetLabelDescriptions,
    listDatasetLabelsResponse_httpStatus,

    -- ** ListFaces
    listFaces_nextToken,
    listFaces_maxResults,
    listFaces_collectionId,
    listFacesResponse_nextToken,
    listFacesResponse_faces,
    listFacesResponse_faceModelVersion,
    listFacesResponse_httpStatus,

    -- ** ListProjectPolicies
    listProjectPolicies_nextToken,
    listProjectPolicies_maxResults,
    listProjectPolicies_projectArn,
    listProjectPoliciesResponse_projectPolicies,
    listProjectPoliciesResponse_nextToken,
    listProjectPoliciesResponse_httpStatus,

    -- ** ListStreamProcessors
    listStreamProcessors_nextToken,
    listStreamProcessors_maxResults,
    listStreamProcessorsResponse_nextToken,
    listStreamProcessorsResponse_streamProcessors,
    listStreamProcessorsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutProjectPolicy
    putProjectPolicy_policyRevisionId,
    putProjectPolicy_projectArn,
    putProjectPolicy_policyName,
    putProjectPolicy_policyDocument,
    putProjectPolicyResponse_policyRevisionId,
    putProjectPolicyResponse_httpStatus,

    -- ** RecognizeCelebrities
    recognizeCelebrities_image,
    recognizeCelebritiesResponse_unrecognizedFaces,
    recognizeCelebritiesResponse_celebrityFaces,
    recognizeCelebritiesResponse_orientationCorrection,
    recognizeCelebritiesResponse_httpStatus,

    -- ** SearchFaces
    searchFaces_faceMatchThreshold,
    searchFaces_maxFaces,
    searchFaces_collectionId,
    searchFaces_faceId,
    searchFacesResponse_faceMatches,
    searchFacesResponse_searchedFaceId,
    searchFacesResponse_faceModelVersion,
    searchFacesResponse_httpStatus,

    -- ** SearchFacesByImage
    searchFacesByImage_qualityFilter,
    searchFacesByImage_faceMatchThreshold,
    searchFacesByImage_maxFaces,
    searchFacesByImage_collectionId,
    searchFacesByImage_image,
    searchFacesByImageResponse_searchedFaceConfidence,
    searchFacesByImageResponse_faceMatches,
    searchFacesByImageResponse_searchedFaceBoundingBox,
    searchFacesByImageResponse_faceModelVersion,
    searchFacesByImageResponse_httpStatus,

    -- ** StartCelebrityRecognition
    startCelebrityRecognition_clientRequestToken,
    startCelebrityRecognition_jobTag,
    startCelebrityRecognition_notificationChannel,
    startCelebrityRecognition_video,
    startCelebrityRecognitionResponse_jobId,
    startCelebrityRecognitionResponse_httpStatus,

    -- ** StartContentModeration
    startContentModeration_clientRequestToken,
    startContentModeration_minConfidence,
    startContentModeration_jobTag,
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
    startLabelDetection_minConfidence,
    startLabelDetection_jobTag,
    startLabelDetection_notificationChannel,
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
    updateStreamProcessor_settingsForUpdate,
    updateStreamProcessor_regionsOfInterestForUpdate,
    updateStreamProcessor_parametersToDelete,
    updateStreamProcessor_name,
    updateStreamProcessorResponse_httpStatus,

    -- * Types

    -- ** AgeRange
    ageRange_low,
    ageRange_high,

    -- ** Asset
    asset_groundTruthManifest,

    -- ** AudioMetadata
    audioMetadata_numberOfChannels,
    audioMetadata_sampleRate,
    audioMetadata_codec,
    audioMetadata_durationMillis,

    -- ** Beard
    beard_confidence,
    beard_value,

    -- ** BlackFrame
    blackFrame_minCoveragePercentage,
    blackFrame_maxPixelThreshold,

    -- ** BoundingBox
    boundingBox_width,
    boundingBox_top,
    boundingBox_left,
    boundingBox_height,

    -- ** Celebrity
    celebrity_name,
    celebrity_matchConfidence,
    celebrity_knownGender,
    celebrity_id,
    celebrity_face,
    celebrity_urls,

    -- ** CelebrityDetail
    celebrityDetail_name,
    celebrityDetail_knownGender,
    celebrityDetail_confidence,
    celebrityDetail_id,
    celebrityDetail_face,
    celebrityDetail_boundingBox,
    celebrityDetail_urls,

    -- ** CelebrityRecognition
    celebrityRecognition_timestamp,
    celebrityRecognition_celebrity,

    -- ** CompareFacesMatch
    compareFacesMatch_similarity,
    compareFacesMatch_face,

    -- ** ComparedFace
    comparedFace_quality,
    comparedFace_pose,
    comparedFace_confidence,
    comparedFace_boundingBox,
    comparedFace_landmarks,
    comparedFace_emotions,
    comparedFace_smile,

    -- ** ComparedSourceImageFace
    comparedSourceImageFace_confidence,
    comparedSourceImageFace_boundingBox,

    -- ** ConnectedHomeSettings
    connectedHomeSettings_minConfidence,
    connectedHomeSettings_labels,

    -- ** ConnectedHomeSettingsForUpdate
    connectedHomeSettingsForUpdate_minConfidence,
    connectedHomeSettingsForUpdate_labels,

    -- ** ContentModerationDetection
    contentModerationDetection_timestamp,
    contentModerationDetection_moderationLabel,

    -- ** CoversBodyPart
    coversBodyPart_confidence,
    coversBodyPart_value,

    -- ** CustomLabel
    customLabel_name,
    customLabel_confidence,
    customLabel_geometry,

    -- ** DatasetChanges
    datasetChanges_groundTruth,

    -- ** DatasetDescription
    datasetDescription_statusMessageCode,
    datasetDescription_lastUpdatedTimestamp,
    datasetDescription_status,
    datasetDescription_datasetStats,
    datasetDescription_creationTimestamp,
    datasetDescription_statusMessage,

    -- ** DatasetLabelDescription
    datasetLabelDescription_labelName,
    datasetLabelDescription_labelStats,

    -- ** DatasetLabelStats
    datasetLabelStats_boundingBoxCount,
    datasetLabelStats_entryCount,

    -- ** DatasetMetadata
    datasetMetadata_statusMessageCode,
    datasetMetadata_datasetType,
    datasetMetadata_status,
    datasetMetadata_creationTimestamp,
    datasetMetadata_datasetArn,
    datasetMetadata_statusMessage,

    -- ** DatasetSource
    datasetSource_datasetArn,
    datasetSource_groundTruthManifest,

    -- ** DatasetStats
    datasetStats_errorEntries,
    datasetStats_totalEntries,
    datasetStats_totalLabels,
    datasetStats_labeledEntries,

    -- ** DetectLabelsImageBackground
    detectLabelsImageBackground_quality,
    detectLabelsImageBackground_dominantColors,

    -- ** DetectLabelsImageForeground
    detectLabelsImageForeground_quality,
    detectLabelsImageForeground_dominantColors,

    -- ** DetectLabelsImageProperties
    detectLabelsImageProperties_foreground,
    detectLabelsImageProperties_quality,
    detectLabelsImageProperties_background,
    detectLabelsImageProperties_dominantColors,

    -- ** DetectLabelsImagePropertiesSettings
    detectLabelsImagePropertiesSettings_maxDominantColors,

    -- ** DetectLabelsImageQuality
    detectLabelsImageQuality_sharpness,
    detectLabelsImageQuality_brightness,
    detectLabelsImageQuality_contrast,

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

    -- ** DistributeDataset
    distributeDataset_arn,

    -- ** DominantColor
    dominantColor_simplifiedColor,
    dominantColor_hexCode,
    dominantColor_pixelPercent,
    dominantColor_green,
    dominantColor_cSSColor,
    dominantColor_blue,
    dominantColor_red,

    -- ** Emotion
    emotion_type,
    emotion_confidence,

    -- ** EquipmentDetection
    equipmentDetection_type,
    equipmentDetection_confidence,
    equipmentDetection_boundingBox,
    equipmentDetection_coversBodyPart,

    -- ** EvaluationResult
    evaluationResult_f1Score,
    evaluationResult_summary,

    -- ** EyeOpen
    eyeOpen_confidence,
    eyeOpen_value,

    -- ** Eyeglasses
    eyeglasses_confidence,
    eyeglasses_value,

    -- ** Face
    face_indexFacesModelVersion,
    face_confidence,
    face_faceId,
    face_boundingBox,
    face_externalImageId,
    face_imageId,

    -- ** FaceDetail
    faceDetail_beard,
    faceDetail_ageRange,
    faceDetail_quality,
    faceDetail_pose,
    faceDetail_confidence,
    faceDetail_mouthOpen,
    faceDetail_sunglasses,
    faceDetail_boundingBox,
    faceDetail_landmarks,
    faceDetail_gender,
    faceDetail_eyeglasses,
    faceDetail_emotions,
    faceDetail_smile,
    faceDetail_eyesOpen,
    faceDetail_mustache,

    -- ** FaceDetection
    faceDetection_timestamp,
    faceDetection_face,

    -- ** FaceMatch
    faceMatch_similarity,
    faceMatch_face,

    -- ** FaceRecord
    faceRecord_faceDetail,
    faceRecord_face,

    -- ** FaceSearchSettings
    faceSearchSettings_faceMatchThreshold,
    faceSearchSettings_collectionId,

    -- ** Gender
    gender_confidence,
    gender_value,

    -- ** GeneralLabelsSettings
    generalLabelsSettings_labelCategoryInclusionFilters,
    generalLabelsSettings_labelExclusionFilters,
    generalLabelsSettings_labelInclusionFilters,
    generalLabelsSettings_labelCategoryExclusionFilters,

    -- ** Geometry
    geometry_polygon,
    geometry_boundingBox,

    -- ** GroundTruthManifest
    groundTruthManifest_s3Object,

    -- ** HumanLoopActivationOutput
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,
    humanLoopActivationOutput_humanLoopArn,
    humanLoopActivationOutput_humanLoopActivationReasons,

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
    imageQuality_sharpness,
    imageQuality_brightness,

    -- ** Instance
    instance_confidence,
    instance_boundingBox,
    instance_dominantColors,

    -- ** KinesisDataStream
    kinesisDataStream_arn,

    -- ** KinesisVideoStream
    kinesisVideoStream_arn,

    -- ** KinesisVideoStreamStartSelector
    kinesisVideoStreamStartSelector_producerTimestamp,
    kinesisVideoStreamStartSelector_fragmentNumber,

    -- ** KnownGender
    knownGender_type,

    -- ** Label
    label_instances,
    label_name,
    label_aliases,
    label_confidence,
    label_parents,
    label_categories,

    -- ** LabelAlias
    labelAlias_name,

    -- ** LabelCategory
    labelCategory_name,

    -- ** LabelDetection
    labelDetection_label,
    labelDetection_timestamp,

    -- ** Landmark
    landmark_type,
    landmark_x,
    landmark_y,

    -- ** ModerationLabel
    moderationLabel_name,
    moderationLabel_confidence,
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
    personDetail_index,
    personDetail_face,
    personDetail_boundingBox,

    -- ** PersonDetection
    personDetection_person,
    personDetection_timestamp,

    -- ** PersonMatch
    personMatch_person,
    personMatch_timestamp,
    personMatch_faceMatches,

    -- ** Point
    point_x,
    point_y,

    -- ** Pose
    pose_roll,
    pose_pitch,
    pose_yaw,

    -- ** ProjectDescription
    projectDescription_datasets,
    projectDescription_status,
    projectDescription_creationTimestamp,
    projectDescription_projectArn,

    -- ** ProjectPolicy
    projectPolicy_policyName,
    projectPolicy_lastUpdatedTimestamp,
    projectPolicy_creationTimestamp,
    projectPolicy_policyRevisionId,
    projectPolicy_policyDocument,
    projectPolicy_projectArn,

    -- ** ProjectVersionDescription
    projectVersionDescription_trainingEndTimestamp,
    projectVersionDescription_minInferenceUnits,
    projectVersionDescription_sourceProjectVersionArn,
    projectVersionDescription_status,
    projectVersionDescription_testingDataResult,
    projectVersionDescription_creationTimestamp,
    projectVersionDescription_manifestSummary,
    projectVersionDescription_trainingDataResult,
    projectVersionDescription_evaluationResult,
    projectVersionDescription_kmsKeyId,
    projectVersionDescription_projectVersionArn,
    projectVersionDescription_maxInferenceUnits,
    projectVersionDescription_billableTrainingTimeInSeconds,
    projectVersionDescription_statusMessage,
    projectVersionDescription_outputConfig,

    -- ** ProtectiveEquipmentBodyPart
    protectiveEquipmentBodyPart_name,
    protectiveEquipmentBodyPart_confidence,
    protectiveEquipmentBodyPart_equipmentDetections,

    -- ** ProtectiveEquipmentPerson
    protectiveEquipmentPerson_confidence,
    protectiveEquipmentPerson_bodyParts,
    protectiveEquipmentPerson_id,
    protectiveEquipmentPerson_boundingBox,

    -- ** ProtectiveEquipmentSummarizationAttributes
    protectiveEquipmentSummarizationAttributes_minConfidence,
    protectiveEquipmentSummarizationAttributes_requiredEquipmentTypes,

    -- ** ProtectiveEquipmentSummary
    protectiveEquipmentSummary_personsWithRequiredEquipment,
    protectiveEquipmentSummary_personsWithoutRequiredEquipment,
    protectiveEquipmentSummary_personsIndeterminate,

    -- ** RegionOfInterest
    regionOfInterest_polygon,
    regionOfInterest_boundingBox,

    -- ** S3Destination
    s3Destination_bucket,
    s3Destination_keyPrefix,

    -- ** S3Object
    s3Object_name,
    s3Object_bucket,
    s3Object_version,

    -- ** SegmentDetection
    segmentDetection_startTimecodeSMPTE,
    segmentDetection_type,
    segmentDetection_startTimestampMillis,
    segmentDetection_durationSMPTE,
    segmentDetection_startFrameNumber,
    segmentDetection_shotSegment,
    segmentDetection_technicalCueSegment,
    segmentDetection_endTimestampMillis,
    segmentDetection_endTimecodeSMPTE,
    segmentDetection_durationFrames,
    segmentDetection_durationMillis,
    segmentDetection_endFrameNumber,

    -- ** SegmentTypeInfo
    segmentTypeInfo_type,
    segmentTypeInfo_modelVersion,

    -- ** ShotSegment
    shotSegment_index,
    shotSegment_confidence,

    -- ** Smile
    smile_confidence,
    smile_value,

    -- ** StartSegmentDetectionFilters
    startSegmentDetectionFilters_technicalCueFilter,
    startSegmentDetectionFilters_shotFilter,

    -- ** StartShotDetectionFilter
    startShotDetectionFilter_minSegmentConfidence,

    -- ** StartTechnicalCueDetectionFilter
    startTechnicalCueDetectionFilter_minSegmentConfidence,
    startTechnicalCueDetectionFilter_blackFrame,

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
    streamProcessorOutput_s3Destination,
    streamProcessorOutput_kinesisDataStream,

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
    technicalCueSegment_type,
    technicalCueSegment_confidence,

    -- ** TestingData
    testingData_assets,
    testingData_autoCreate,

    -- ** TestingDataResult
    testingDataResult_validation,
    testingDataResult_input,
    testingDataResult_output,

    -- ** TextDetection
    textDetection_type,
    textDetection_confidence,
    textDetection_parentId,
    textDetection_detectedText,
    textDetection_id,
    textDetection_geometry,

    -- ** TextDetectionResult
    textDetectionResult_timestamp,
    textDetectionResult_textDetection,

    -- ** TrainingData
    trainingData_assets,

    -- ** TrainingDataResult
    trainingDataResult_validation,
    trainingDataResult_input,
    trainingDataResult_output,

    -- ** UnindexedFace
    unindexedFace_faceDetail,
    unindexedFace_reasons,

    -- ** ValidationData
    validationData_assets,

    -- ** Video
    video_s3Object,

    -- ** VideoMetadata
    videoMetadata_format,
    videoMetadata_frameHeight,
    videoMetadata_codec,
    videoMetadata_colorRange,
    videoMetadata_durationMillis,
    videoMetadata_frameWidth,
    videoMetadata_frameRate,
  )
where

import Amazonka.Rekognition.CompareFaces
import Amazonka.Rekognition.CopyProjectVersion
import Amazonka.Rekognition.CreateCollection
import Amazonka.Rekognition.CreateDataset
import Amazonka.Rekognition.CreateProject
import Amazonka.Rekognition.CreateProjectVersion
import Amazonka.Rekognition.CreateStreamProcessor
import Amazonka.Rekognition.DeleteCollection
import Amazonka.Rekognition.DeleteDataset
import Amazonka.Rekognition.DeleteFaces
import Amazonka.Rekognition.DeleteProject
import Amazonka.Rekognition.DeleteProjectPolicy
import Amazonka.Rekognition.DeleteProjectVersion
import Amazonka.Rekognition.DeleteStreamProcessor
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
import Amazonka.Rekognition.DistributeDatasetEntries
import Amazonka.Rekognition.GetCelebrityInfo
import Amazonka.Rekognition.GetCelebrityRecognition
import Amazonka.Rekognition.GetContentModeration
import Amazonka.Rekognition.GetFaceDetection
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
import Amazonka.Rekognition.PutProjectPolicy
import Amazonka.Rekognition.RecognizeCelebrities
import Amazonka.Rekognition.SearchFaces
import Amazonka.Rekognition.SearchFacesByImage
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
import Amazonka.Rekognition.Types.AudioMetadata
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
import Amazonka.Rekognition.Types.DistributeDataset
import Amazonka.Rekognition.Types.DominantColor
import Amazonka.Rekognition.Types.Emotion
import Amazonka.Rekognition.Types.EquipmentDetection
import Amazonka.Rekognition.Types.EvaluationResult
import Amazonka.Rekognition.Types.EyeOpen
import Amazonka.Rekognition.Types.Eyeglasses
import Amazonka.Rekognition.Types.Face
import Amazonka.Rekognition.Types.FaceDetail
import Amazonka.Rekognition.Types.FaceDetection
import Amazonka.Rekognition.Types.FaceMatch
import Amazonka.Rekognition.Types.FaceRecord
import Amazonka.Rekognition.Types.FaceSearchSettings
import Amazonka.Rekognition.Types.Gender
import Amazonka.Rekognition.Types.GeneralLabelsSettings
import Amazonka.Rekognition.Types.Geometry
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
import Amazonka.Rekognition.Types.Landmark
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
import Amazonka.Rekognition.Types.ValidationData
import Amazonka.Rekognition.Types.Video
import Amazonka.Rekognition.Types.VideoMetadata
import Amazonka.Rekognition.UntagResource
import Amazonka.Rekognition.UpdateDatasetEntries
import Amazonka.Rekognition.UpdateStreamProcessor
