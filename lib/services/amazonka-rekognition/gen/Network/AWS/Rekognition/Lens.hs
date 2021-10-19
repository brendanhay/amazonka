{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Lens
  ( -- * Operations

    -- ** DetectProtectiveEquipment
    detectProtectiveEquipment_summarizationAttributes,
    detectProtectiveEquipment_image,
    detectProtectiveEquipmentResponse_summary,
    detectProtectiveEquipmentResponse_protectiveEquipmentModelVersion,
    detectProtectiveEquipmentResponse_persons,
    detectProtectiveEquipmentResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_projectArn,
    deleteProjectResponse_status,
    deleteProjectResponse_httpStatus,

    -- ** StartCelebrityRecognition
    startCelebrityRecognition_jobTag,
    startCelebrityRecognition_notificationChannel,
    startCelebrityRecognition_clientRequestToken,
    startCelebrityRecognition_video,
    startCelebrityRecognitionResponse_jobId,
    startCelebrityRecognitionResponse_httpStatus,

    -- ** GetPersonTracking
    getPersonTracking_nextToken,
    getPersonTracking_maxResults,
    getPersonTracking_sortBy,
    getPersonTracking_jobId,
    getPersonTrackingResponse_nextToken,
    getPersonTrackingResponse_videoMetadata,
    getPersonTrackingResponse_statusMessage,
    getPersonTrackingResponse_jobStatus,
    getPersonTrackingResponse_persons,
    getPersonTrackingResponse_httpStatus,

    -- ** GetTextDetection
    getTextDetection_nextToken,
    getTextDetection_maxResults,
    getTextDetection_jobId,
    getTextDetectionResponse_textDetections,
    getTextDetectionResponse_nextToken,
    getTextDetectionResponse_videoMetadata,
    getTextDetectionResponse_statusMessage,
    getTextDetectionResponse_textModelVersion,
    getTextDetectionResponse_jobStatus,
    getTextDetectionResponse_httpStatus,

    -- ** StartSegmentDetection
    startSegmentDetection_jobTag,
    startSegmentDetection_filters,
    startSegmentDetection_notificationChannel,
    startSegmentDetection_clientRequestToken,
    startSegmentDetection_video,
    startSegmentDetection_segmentTypes,
    startSegmentDetectionResponse_jobId,
    startSegmentDetectionResponse_httpStatus,

    -- ** ListCollections
    listCollections_nextToken,
    listCollections_maxResults,
    listCollectionsResponse_collectionIds,
    listCollectionsResponse_nextToken,
    listCollectionsResponse_faceModelVersions,
    listCollectionsResponse_httpStatus,

    -- ** StartProjectVersion
    startProjectVersion_projectVersionArn,
    startProjectVersion_minInferenceUnits,
    startProjectVersionResponse_status,
    startProjectVersionResponse_httpStatus,

    -- ** DeleteCollection
    deleteCollection_collectionId,
    deleteCollectionResponse_statusCode,
    deleteCollectionResponse_httpStatus,

    -- ** CreateCollection
    createCollection_tags,
    createCollection_collectionId,
    createCollectionResponse_faceModelVersion,
    createCollectionResponse_collectionArn,
    createCollectionResponse_statusCode,
    createCollectionResponse_httpStatus,

    -- ** StopStreamProcessor
    stopStreamProcessor_name,
    stopStreamProcessorResponse_httpStatus,

    -- ** DetectLabels
    detectLabels_minConfidence,
    detectLabels_maxLabels,
    detectLabels_image,
    detectLabelsResponse_labels,
    detectLabelsResponse_orientationCorrection,
    detectLabelsResponse_labelModelVersion,
    detectLabelsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartContentModeration
    startContentModeration_jobTag,
    startContentModeration_notificationChannel,
    startContentModeration_clientRequestToken,
    startContentModeration_minConfidence,
    startContentModeration_video,
    startContentModerationResponse_jobId,
    startContentModerationResponse_httpStatus,

    -- ** SearchFacesByImage
    searchFacesByImage_qualityFilter,
    searchFacesByImage_faceMatchThreshold,
    searchFacesByImage_maxFaces,
    searchFacesByImage_collectionId,
    searchFacesByImage_image,
    searchFacesByImageResponse_faceMatches,
    searchFacesByImageResponse_faceModelVersion,
    searchFacesByImageResponse_searchedFaceBoundingBox,
    searchFacesByImageResponse_searchedFaceConfidence,
    searchFacesByImageResponse_httpStatus,

    -- ** ListStreamProcessors
    listStreamProcessors_nextToken,
    listStreamProcessors_maxResults,
    listStreamProcessorsResponse_streamProcessors,
    listStreamProcessorsResponse_nextToken,
    listStreamProcessorsResponse_httpStatus,

    -- ** DescribeCollection
    describeCollection_collectionId,
    describeCollectionResponse_faceModelVersion,
    describeCollectionResponse_faceCount,
    describeCollectionResponse_creationTimestamp,
    describeCollectionResponse_collectionARN,
    describeCollectionResponse_httpStatus,

    -- ** DeleteProjectVersion
    deleteProjectVersion_projectVersionArn,
    deleteProjectVersionResponse_status,
    deleteProjectVersionResponse_httpStatus,

    -- ** DescribeProjectVersions
    describeProjectVersions_nextToken,
    describeProjectVersions_versionNames,
    describeProjectVersions_maxResults,
    describeProjectVersions_projectArn,
    describeProjectVersionsResponse_nextToken,
    describeProjectVersionsResponse_projectVersionDescriptions,
    describeProjectVersionsResponse_httpStatus,

    -- ** RecognizeCelebrities
    recognizeCelebrities_image,
    recognizeCelebritiesResponse_celebrityFaces,
    recognizeCelebritiesResponse_orientationCorrection,
    recognizeCelebritiesResponse_unrecognizedFaces,
    recognizeCelebritiesResponse_httpStatus,

    -- ** DetectCustomLabels
    detectCustomLabels_minConfidence,
    detectCustomLabels_maxResults,
    detectCustomLabels_projectVersionArn,
    detectCustomLabels_image,
    detectCustomLabelsResponse_customLabels,
    detectCustomLabelsResponse_httpStatus,

    -- ** GetFaceSearch
    getFaceSearch_nextToken,
    getFaceSearch_maxResults,
    getFaceSearch_sortBy,
    getFaceSearch_jobId,
    getFaceSearchResponse_nextToken,
    getFaceSearchResponse_videoMetadata,
    getFaceSearchResponse_statusMessage,
    getFaceSearchResponse_jobStatus,
    getFaceSearchResponse_persons,
    getFaceSearchResponse_httpStatus,

    -- ** StartLabelDetection
    startLabelDetection_jobTag,
    startLabelDetection_notificationChannel,
    startLabelDetection_clientRequestToken,
    startLabelDetection_minConfidence,
    startLabelDetection_video,
    startLabelDetectionResponse_jobId,
    startLabelDetectionResponse_httpStatus,

    -- ** SearchFaces
    searchFaces_faceMatchThreshold,
    searchFaces_maxFaces,
    searchFaces_collectionId,
    searchFaces_faceId,
    searchFacesResponse_faceMatches,
    searchFacesResponse_faceModelVersion,
    searchFacesResponse_searchedFaceId,
    searchFacesResponse_httpStatus,

    -- ** IndexFaces
    indexFaces_externalImageId,
    indexFaces_qualityFilter,
    indexFaces_maxFaces,
    indexFaces_detectionAttributes,
    indexFaces_collectionId,
    indexFaces_image,
    indexFacesResponse_faceModelVersion,
    indexFacesResponse_faceRecords,
    indexFacesResponse_orientationCorrection,
    indexFacesResponse_unindexedFaces,
    indexFacesResponse_httpStatus,

    -- ** GetLabelDetection
    getLabelDetection_nextToken,
    getLabelDetection_maxResults,
    getLabelDetection_sortBy,
    getLabelDetection_jobId,
    getLabelDetectionResponse_nextToken,
    getLabelDetectionResponse_videoMetadata,
    getLabelDetectionResponse_statusMessage,
    getLabelDetectionResponse_labels,
    getLabelDetectionResponse_jobStatus,
    getLabelDetectionResponse_labelModelVersion,
    getLabelDetectionResponse_httpStatus,

    -- ** StopProjectVersion
    stopProjectVersion_projectVersionArn,
    stopProjectVersionResponse_status,
    stopProjectVersionResponse_httpStatus,

    -- ** DescribeStreamProcessor
    describeStreamProcessor_name,
    describeStreamProcessorResponse_status,
    describeStreamProcessorResponse_settings,
    describeStreamProcessorResponse_input,
    describeStreamProcessorResponse_output,
    describeStreamProcessorResponse_streamProcessorArn,
    describeStreamProcessorResponse_statusMessage,
    describeStreamProcessorResponse_name,
    describeStreamProcessorResponse_creationTimestamp,
    describeStreamProcessorResponse_lastUpdateTimestamp,
    describeStreamProcessorResponse_roleArn,
    describeStreamProcessorResponse_httpStatus,

    -- ** StartFaceSearch
    startFaceSearch_faceMatchThreshold,
    startFaceSearch_jobTag,
    startFaceSearch_notificationChannel,
    startFaceSearch_clientRequestToken,
    startFaceSearch_video,
    startFaceSearch_collectionId,
    startFaceSearchResponse_jobId,
    startFaceSearchResponse_httpStatus,

    -- ** StartTextDetection
    startTextDetection_jobTag,
    startTextDetection_filters,
    startTextDetection_notificationChannel,
    startTextDetection_clientRequestToken,
    startTextDetection_video,
    startTextDetectionResponse_jobId,
    startTextDetectionResponse_httpStatus,

    -- ** StartPersonTracking
    startPersonTracking_jobTag,
    startPersonTracking_notificationChannel,
    startPersonTracking_clientRequestToken,
    startPersonTracking_video,
    startPersonTrackingResponse_jobId,
    startPersonTrackingResponse_httpStatus,

    -- ** GetCelebrityRecognition
    getCelebrityRecognition_nextToken,
    getCelebrityRecognition_maxResults,
    getCelebrityRecognition_sortBy,
    getCelebrityRecognition_jobId,
    getCelebrityRecognitionResponse_nextToken,
    getCelebrityRecognitionResponse_videoMetadata,
    getCelebrityRecognitionResponse_statusMessage,
    getCelebrityRecognitionResponse_celebrities,
    getCelebrityRecognitionResponse_jobStatus,
    getCelebrityRecognitionResponse_httpStatus,

    -- ** StartStreamProcessor
    startStreamProcessor_name,
    startStreamProcessorResponse_httpStatus,

    -- ** DetectText
    detectText_filters,
    detectText_image,
    detectTextResponse_textDetections,
    detectTextResponse_textModelVersion,
    detectTextResponse_httpStatus,

    -- ** GetSegmentDetection
    getSegmentDetection_nextToken,
    getSegmentDetection_maxResults,
    getSegmentDetection_jobId,
    getSegmentDetectionResponse_selectedSegmentTypes,
    getSegmentDetectionResponse_nextToken,
    getSegmentDetectionResponse_videoMetadata,
    getSegmentDetectionResponse_statusMessage,
    getSegmentDetectionResponse_segments,
    getSegmentDetectionResponse_jobStatus,
    getSegmentDetectionResponse_audioMetadata,
    getSegmentDetectionResponse_httpStatus,

    -- ** CompareFaces
    compareFaces_qualityFilter,
    compareFaces_similarityThreshold,
    compareFaces_sourceImage,
    compareFaces_targetImage,
    compareFacesResponse_faceMatches,
    compareFacesResponse_unmatchedFaces,
    compareFacesResponse_targetImageOrientationCorrection,
    compareFacesResponse_sourceImageOrientationCorrection,
    compareFacesResponse_sourceImageFace,
    compareFacesResponse_httpStatus,

    -- ** DetectFaces
    detectFaces_attributes,
    detectFaces_image,
    detectFacesResponse_orientationCorrection,
    detectFacesResponse_faceDetails,
    detectFacesResponse_httpStatus,

    -- ** GetFaceDetection
    getFaceDetection_nextToken,
    getFaceDetection_maxResults,
    getFaceDetection_jobId,
    getFaceDetectionResponse_nextToken,
    getFaceDetectionResponse_videoMetadata,
    getFaceDetectionResponse_statusMessage,
    getFaceDetectionResponse_faces,
    getFaceDetectionResponse_jobStatus,
    getFaceDetectionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListFaces
    listFaces_nextToken,
    listFaces_maxResults,
    listFaces_collectionId,
    listFacesResponse_faceModelVersion,
    listFacesResponse_nextToken,
    listFacesResponse_faces,
    listFacesResponse_httpStatus,

    -- ** CreateProjectVersion
    createProjectVersion_kmsKeyId,
    createProjectVersion_tags,
    createProjectVersion_projectArn,
    createProjectVersion_versionName,
    createProjectVersion_outputConfig,
    createProjectVersion_trainingData,
    createProjectVersion_testingData,
    createProjectVersionResponse_projectVersionArn,
    createProjectVersionResponse_httpStatus,

    -- ** DescribeProjects
    describeProjects_nextToken,
    describeProjects_maxResults,
    describeProjectsResponse_nextToken,
    describeProjectsResponse_projectDescriptions,
    describeProjectsResponse_httpStatus,

    -- ** GetContentModeration
    getContentModeration_nextToken,
    getContentModeration_maxResults,
    getContentModeration_sortBy,
    getContentModeration_jobId,
    getContentModerationResponse_nextToken,
    getContentModerationResponse_videoMetadata,
    getContentModerationResponse_statusMessage,
    getContentModerationResponse_jobStatus,
    getContentModerationResponse_moderationModelVersion,
    getContentModerationResponse_moderationLabels,
    getContentModerationResponse_httpStatus,

    -- ** DeleteFaces
    deleteFaces_collectionId,
    deleteFaces_faceIds,
    deleteFacesResponse_deletedFaces,
    deleteFacesResponse_httpStatus,

    -- ** GetCelebrityInfo
    getCelebrityInfo_id,
    getCelebrityInfoResponse_urls,
    getCelebrityInfoResponse_knownGender,
    getCelebrityInfoResponse_name,
    getCelebrityInfoResponse_httpStatus,

    -- ** DeleteStreamProcessor
    deleteStreamProcessor_name,
    deleteStreamProcessorResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DetectModerationLabels
    detectModerationLabels_humanLoopConfig,
    detectModerationLabels_minConfidence,
    detectModerationLabels_image,
    detectModerationLabelsResponse_humanLoopActivationOutput,
    detectModerationLabelsResponse_moderationModelVersion,
    detectModerationLabelsResponse_moderationLabels,
    detectModerationLabelsResponse_httpStatus,

    -- ** CreateStreamProcessor
    createStreamProcessor_tags,
    createStreamProcessor_input,
    createStreamProcessor_output,
    createStreamProcessor_name,
    createStreamProcessor_settings,
    createStreamProcessor_roleArn,
    createStreamProcessorResponse_streamProcessorArn,
    createStreamProcessorResponse_httpStatus,

    -- ** StartFaceDetection
    startFaceDetection_jobTag,
    startFaceDetection_notificationChannel,
    startFaceDetection_clientRequestToken,
    startFaceDetection_faceAttributes,
    startFaceDetection_video,
    startFaceDetectionResponse_jobId,
    startFaceDetectionResponse_httpStatus,

    -- ** CreateProject
    createProject_projectName,
    createProjectResponse_projectArn,
    createProjectResponse_httpStatus,

    -- * Types

    -- ** AgeRange
    ageRange_low,
    ageRange_high,

    -- ** Asset
    asset_groundTruthManifest,

    -- ** AudioMetadata
    audioMetadata_codec,
    audioMetadata_sampleRate,
    audioMetadata_numberOfChannels,
    audioMetadata_durationMillis,

    -- ** Beard
    beard_value,
    beard_confidence,

    -- ** BlackFrame
    blackFrame_maxPixelThreshold,
    blackFrame_minCoveragePercentage,

    -- ** BoundingBox
    boundingBox_height,
    boundingBox_left,
    boundingBox_width,
    boundingBox_top,

    -- ** Celebrity
    celebrity_matchConfidence,
    celebrity_urls,
    celebrity_knownGender,
    celebrity_name,
    celebrity_id,
    celebrity_face,

    -- ** CelebrityDetail
    celebrityDetail_boundingBox,
    celebrityDetail_urls,
    celebrityDetail_confidence,
    celebrityDetail_name,
    celebrityDetail_id,
    celebrityDetail_face,

    -- ** CelebrityRecognition
    celebrityRecognition_celebrity,
    celebrityRecognition_timestamp,

    -- ** CompareFacesMatch
    compareFacesMatch_similarity,
    compareFacesMatch_face,

    -- ** ComparedFace
    comparedFace_boundingBox,
    comparedFace_emotions,
    comparedFace_pose,
    comparedFace_confidence,
    comparedFace_quality,
    comparedFace_smile,
    comparedFace_landmarks,

    -- ** ComparedSourceImageFace
    comparedSourceImageFace_boundingBox,
    comparedSourceImageFace_confidence,

    -- ** ContentModerationDetection
    contentModerationDetection_moderationLabel,
    contentModerationDetection_timestamp,

    -- ** CoversBodyPart
    coversBodyPart_value,
    coversBodyPart_confidence,

    -- ** CustomLabel
    customLabel_confidence,
    customLabel_name,
    customLabel_geometry,

    -- ** DetectTextFilters
    detectTextFilters_regionsOfInterest,
    detectTextFilters_wordFilter,

    -- ** DetectionFilter
    detectionFilter_minBoundingBoxHeight,
    detectionFilter_minBoundingBoxWidth,
    detectionFilter_minConfidence,

    -- ** Emotion
    emotion_confidence,
    emotion_type,

    -- ** EquipmentDetection
    equipmentDetection_boundingBox,
    equipmentDetection_coversBodyPart,
    equipmentDetection_confidence,
    equipmentDetection_type,

    -- ** EvaluationResult
    evaluationResult_summary,
    evaluationResult_f1Score,

    -- ** EyeOpen
    eyeOpen_value,
    eyeOpen_confidence,

    -- ** Eyeglasses
    eyeglasses_value,
    eyeglasses_confidence,

    -- ** Face
    face_faceId,
    face_boundingBox,
    face_externalImageId,
    face_confidence,
    face_imageId,

    -- ** FaceDetail
    faceDetail_ageRange,
    faceDetail_sunglasses,
    faceDetail_mouthOpen,
    faceDetail_boundingBox,
    faceDetail_emotions,
    faceDetail_eyesOpen,
    faceDetail_pose,
    faceDetail_confidence,
    faceDetail_gender,
    faceDetail_quality,
    faceDetail_eyeglasses,
    faceDetail_beard,
    faceDetail_mustache,
    faceDetail_smile,
    faceDetail_landmarks,

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
    gender_value,
    gender_confidence,

    -- ** Geometry
    geometry_boundingBox,
    geometry_polygon,

    -- ** GroundTruthManifest
    groundTruthManifest_s3Object,

    -- ** HumanLoopActivationOutput
    humanLoopActivationOutput_humanLoopActivationReasons,
    humanLoopActivationOutput_humanLoopArn,
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,

    -- ** HumanLoopConfig
    humanLoopConfig_dataAttributes,
    humanLoopConfig_humanLoopName,
    humanLoopConfig_flowDefinitionArn,

    -- ** HumanLoopDataAttributes
    humanLoopDataAttributes_contentClassifiers,

    -- ** Image
    image_s3Object,
    image_bytes,

    -- ** ImageQuality
    imageQuality_sharpness,
    imageQuality_brightness,

    -- ** Instance
    instance_boundingBox,
    instance_confidence,

    -- ** KinesisDataStream
    kinesisDataStream_arn,

    -- ** KinesisVideoStream
    kinesisVideoStream_arn,

    -- ** KnownGender
    knownGender_type,

    -- ** Label
    label_confidence,
    label_parents,
    label_name,
    label_instances,

    -- ** LabelDetection
    labelDetection_label,
    labelDetection_timestamp,

    -- ** Landmark
    landmark_type,
    landmark_x,
    landmark_y,

    -- ** ModerationLabel
    moderationLabel_confidence,
    moderationLabel_name,
    moderationLabel_parentName,

    -- ** MouthOpen
    mouthOpen_value,
    mouthOpen_confidence,

    -- ** Mustache
    mustache_value,
    mustache_confidence,

    -- ** NotificationChannel
    notificationChannel_sNSTopicArn,
    notificationChannel_roleArn,

    -- ** OutputConfig
    outputConfig_s3KeyPrefix,
    outputConfig_s3Bucket,

    -- ** Parent
    parent_name,

    -- ** PersonDetail
    personDetail_boundingBox,
    personDetail_index,
    personDetail_face,

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
    pose_yaw,
    pose_roll,
    pose_pitch,

    -- ** ProjectDescription
    projectDescription_status,
    projectDescription_creationTimestamp,
    projectDescription_projectArn,

    -- ** ProjectVersionDescription
    projectVersionDescription_minInferenceUnits,
    projectVersionDescription_status,
    projectVersionDescription_evaluationResult,
    projectVersionDescription_manifestSummary,
    projectVersionDescription_kmsKeyId,
    projectVersionDescription_testingDataResult,
    projectVersionDescription_statusMessage,
    projectVersionDescription_creationTimestamp,
    projectVersionDescription_projectVersionArn,
    projectVersionDescription_outputConfig,
    projectVersionDescription_billableTrainingTimeInSeconds,
    projectVersionDescription_trainingEndTimestamp,
    projectVersionDescription_trainingDataResult,

    -- ** ProtectiveEquipmentBodyPart
    protectiveEquipmentBodyPart_equipmentDetections,
    protectiveEquipmentBodyPart_confidence,
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
    protectiveEquipmentSummary_personsWithRequiredEquipment,
    protectiveEquipmentSummary_personsWithoutRequiredEquipment,
    protectiveEquipmentSummary_personsIndeterminate,

    -- ** RegionOfInterest
    regionOfInterest_boundingBox,

    -- ** S3Object
    s3Object_bucket,
    s3Object_name,
    s3Object_version,

    -- ** SegmentDetection
    segmentDetection_technicalCueSegment,
    segmentDetection_endFrameNumber,
    segmentDetection_durationSMPTE,
    segmentDetection_endTimestampMillis,
    segmentDetection_startTimecodeSMPTE,
    segmentDetection_endTimecodeSMPTE,
    segmentDetection_durationMillis,
    segmentDetection_durationFrames,
    segmentDetection_startTimestampMillis,
    segmentDetection_type,
    segmentDetection_shotSegment,
    segmentDetection_startFrameNumber,

    -- ** SegmentTypeInfo
    segmentTypeInfo_modelVersion,
    segmentTypeInfo_type,

    -- ** ShotSegment
    shotSegment_confidence,
    shotSegment_index,

    -- ** Smile
    smile_value,
    smile_confidence,

    -- ** StartSegmentDetectionFilters
    startSegmentDetectionFilters_technicalCueFilter,
    startSegmentDetectionFilters_shotFilter,

    -- ** StartShotDetectionFilter
    startShotDetectionFilter_minSegmentConfidence,

    -- ** StartTechnicalCueDetectionFilter
    startTechnicalCueDetectionFilter_blackFrame,
    startTechnicalCueDetectionFilter_minSegmentConfidence,

    -- ** StartTextDetectionFilters
    startTextDetectionFilters_regionsOfInterest,
    startTextDetectionFilters_wordFilter,

    -- ** StreamProcessor
    streamProcessor_status,
    streamProcessor_name,

    -- ** StreamProcessorInput
    streamProcessorInput_kinesisVideoStream,

    -- ** StreamProcessorOutput
    streamProcessorOutput_kinesisDataStream,

    -- ** StreamProcessorSettings
    streamProcessorSettings_faceSearch,

    -- ** Summary
    summary_s3Object,

    -- ** Sunglasses
    sunglasses_value,
    sunglasses_confidence,

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
    textDetection_detectedText,
    textDetection_confidence,
    textDetection_geometry,
    textDetection_id,
    textDetection_type,
    textDetection_parentId,

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
    unindexedFace_reasons,
    unindexedFace_faceDetail,

    -- ** ValidationData
    validationData_assets,

    -- ** Video
    video_s3Object,

    -- ** VideoMetadata
    videoMetadata_frameRate,
    videoMetadata_colorRange,
    videoMetadata_format,
    videoMetadata_codec,
    videoMetadata_frameHeight,
    videoMetadata_durationMillis,
    videoMetadata_frameWidth,
  )
where

import Network.AWS.Rekognition.CompareFaces
import Network.AWS.Rekognition.CreateCollection
import Network.AWS.Rekognition.CreateProject
import Network.AWS.Rekognition.CreateProjectVersion
import Network.AWS.Rekognition.CreateStreamProcessor
import Network.AWS.Rekognition.DeleteCollection
import Network.AWS.Rekognition.DeleteFaces
import Network.AWS.Rekognition.DeleteProject
import Network.AWS.Rekognition.DeleteProjectVersion
import Network.AWS.Rekognition.DeleteStreamProcessor
import Network.AWS.Rekognition.DescribeCollection
import Network.AWS.Rekognition.DescribeProjectVersions
import Network.AWS.Rekognition.DescribeProjects
import Network.AWS.Rekognition.DescribeStreamProcessor
import Network.AWS.Rekognition.DetectCustomLabels
import Network.AWS.Rekognition.DetectFaces
import Network.AWS.Rekognition.DetectLabels
import Network.AWS.Rekognition.DetectModerationLabels
import Network.AWS.Rekognition.DetectProtectiveEquipment
import Network.AWS.Rekognition.DetectText
import Network.AWS.Rekognition.GetCelebrityInfo
import Network.AWS.Rekognition.GetCelebrityRecognition
import Network.AWS.Rekognition.GetContentModeration
import Network.AWS.Rekognition.GetFaceDetection
import Network.AWS.Rekognition.GetFaceSearch
import Network.AWS.Rekognition.GetLabelDetection
import Network.AWS.Rekognition.GetPersonTracking
import Network.AWS.Rekognition.GetSegmentDetection
import Network.AWS.Rekognition.GetTextDetection
import Network.AWS.Rekognition.IndexFaces
import Network.AWS.Rekognition.ListCollections
import Network.AWS.Rekognition.ListFaces
import Network.AWS.Rekognition.ListStreamProcessors
import Network.AWS.Rekognition.ListTagsForResource
import Network.AWS.Rekognition.RecognizeCelebrities
import Network.AWS.Rekognition.SearchFaces
import Network.AWS.Rekognition.SearchFacesByImage
import Network.AWS.Rekognition.StartCelebrityRecognition
import Network.AWS.Rekognition.StartContentModeration
import Network.AWS.Rekognition.StartFaceDetection
import Network.AWS.Rekognition.StartFaceSearch
import Network.AWS.Rekognition.StartLabelDetection
import Network.AWS.Rekognition.StartPersonTracking
import Network.AWS.Rekognition.StartProjectVersion
import Network.AWS.Rekognition.StartSegmentDetection
import Network.AWS.Rekognition.StartStreamProcessor
import Network.AWS.Rekognition.StartTextDetection
import Network.AWS.Rekognition.StopProjectVersion
import Network.AWS.Rekognition.StopStreamProcessor
import Network.AWS.Rekognition.TagResource
import Network.AWS.Rekognition.Types.AgeRange
import Network.AWS.Rekognition.Types.Asset
import Network.AWS.Rekognition.Types.AudioMetadata
import Network.AWS.Rekognition.Types.Beard
import Network.AWS.Rekognition.Types.BlackFrame
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.Celebrity
import Network.AWS.Rekognition.Types.CelebrityDetail
import Network.AWS.Rekognition.Types.CelebrityRecognition
import Network.AWS.Rekognition.Types.CompareFacesMatch
import Network.AWS.Rekognition.Types.ComparedFace
import Network.AWS.Rekognition.Types.ComparedSourceImageFace
import Network.AWS.Rekognition.Types.ContentModerationDetection
import Network.AWS.Rekognition.Types.CoversBodyPart
import Network.AWS.Rekognition.Types.CustomLabel
import Network.AWS.Rekognition.Types.DetectTextFilters
import Network.AWS.Rekognition.Types.DetectionFilter
import Network.AWS.Rekognition.Types.Emotion
import Network.AWS.Rekognition.Types.EquipmentDetection
import Network.AWS.Rekognition.Types.EvaluationResult
import Network.AWS.Rekognition.Types.EyeOpen
import Network.AWS.Rekognition.Types.Eyeglasses
import Network.AWS.Rekognition.Types.Face
import Network.AWS.Rekognition.Types.FaceDetail
import Network.AWS.Rekognition.Types.FaceDetection
import Network.AWS.Rekognition.Types.FaceMatch
import Network.AWS.Rekognition.Types.FaceRecord
import Network.AWS.Rekognition.Types.FaceSearchSettings
import Network.AWS.Rekognition.Types.Gender
import Network.AWS.Rekognition.Types.Geometry
import Network.AWS.Rekognition.Types.GroundTruthManifest
import Network.AWS.Rekognition.Types.HumanLoopActivationOutput
import Network.AWS.Rekognition.Types.HumanLoopConfig
import Network.AWS.Rekognition.Types.HumanLoopDataAttributes
import Network.AWS.Rekognition.Types.Image
import Network.AWS.Rekognition.Types.ImageQuality
import Network.AWS.Rekognition.Types.Instance
import Network.AWS.Rekognition.Types.KinesisDataStream
import Network.AWS.Rekognition.Types.KinesisVideoStream
import Network.AWS.Rekognition.Types.KnownGender
import Network.AWS.Rekognition.Types.Label
import Network.AWS.Rekognition.Types.LabelDetection
import Network.AWS.Rekognition.Types.Landmark
import Network.AWS.Rekognition.Types.ModerationLabel
import Network.AWS.Rekognition.Types.MouthOpen
import Network.AWS.Rekognition.Types.Mustache
import Network.AWS.Rekognition.Types.NotificationChannel
import Network.AWS.Rekognition.Types.OutputConfig
import Network.AWS.Rekognition.Types.Parent
import Network.AWS.Rekognition.Types.PersonDetail
import Network.AWS.Rekognition.Types.PersonDetection
import Network.AWS.Rekognition.Types.PersonMatch
import Network.AWS.Rekognition.Types.Point
import Network.AWS.Rekognition.Types.Pose
import Network.AWS.Rekognition.Types.ProjectDescription
import Network.AWS.Rekognition.Types.ProjectVersionDescription
import Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart
import Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson
import Network.AWS.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes
import Network.AWS.Rekognition.Types.ProtectiveEquipmentSummary
import Network.AWS.Rekognition.Types.RegionOfInterest
import Network.AWS.Rekognition.Types.S3Object
import Network.AWS.Rekognition.Types.SegmentDetection
import Network.AWS.Rekognition.Types.SegmentTypeInfo
import Network.AWS.Rekognition.Types.ShotSegment
import Network.AWS.Rekognition.Types.Smile
import Network.AWS.Rekognition.Types.StartSegmentDetectionFilters
import Network.AWS.Rekognition.Types.StartShotDetectionFilter
import Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter
import Network.AWS.Rekognition.Types.StartTextDetectionFilters
import Network.AWS.Rekognition.Types.StreamProcessor
import Network.AWS.Rekognition.Types.StreamProcessorInput
import Network.AWS.Rekognition.Types.StreamProcessorOutput
import Network.AWS.Rekognition.Types.StreamProcessorSettings
import Network.AWS.Rekognition.Types.Summary
import Network.AWS.Rekognition.Types.Sunglasses
import Network.AWS.Rekognition.Types.TechnicalCueSegment
import Network.AWS.Rekognition.Types.TestingData
import Network.AWS.Rekognition.Types.TestingDataResult
import Network.AWS.Rekognition.Types.TextDetection
import Network.AWS.Rekognition.Types.TextDetectionResult
import Network.AWS.Rekognition.Types.TrainingData
import Network.AWS.Rekognition.Types.TrainingDataResult
import Network.AWS.Rekognition.Types.UnindexedFace
import Network.AWS.Rekognition.Types.ValidationData
import Network.AWS.Rekognition.Types.Video
import Network.AWS.Rekognition.Types.VideoMetadata
import Network.AWS.Rekognition.UntagResource
