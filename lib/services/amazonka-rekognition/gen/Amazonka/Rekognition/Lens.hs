{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rekognition.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Lens
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

import Amazonka.Rekognition.CompareFaces
import Amazonka.Rekognition.CreateCollection
import Amazonka.Rekognition.CreateProject
import Amazonka.Rekognition.CreateProjectVersion
import Amazonka.Rekognition.CreateStreamProcessor
import Amazonka.Rekognition.DeleteCollection
import Amazonka.Rekognition.DeleteFaces
import Amazonka.Rekognition.DeleteProject
import Amazonka.Rekognition.DeleteProjectVersion
import Amazonka.Rekognition.DeleteStreamProcessor
import Amazonka.Rekognition.DescribeCollection
import Amazonka.Rekognition.DescribeProjectVersions
import Amazonka.Rekognition.DescribeProjects
import Amazonka.Rekognition.DescribeStreamProcessor
import Amazonka.Rekognition.DetectCustomLabels
import Amazonka.Rekognition.DetectFaces
import Amazonka.Rekognition.DetectLabels
import Amazonka.Rekognition.DetectModerationLabels
import Amazonka.Rekognition.DetectProtectiveEquipment
import Amazonka.Rekognition.DetectText
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
import Amazonka.Rekognition.ListFaces
import Amazonka.Rekognition.ListStreamProcessors
import Amazonka.Rekognition.ListTagsForResource
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
import Amazonka.Rekognition.Types.ContentModerationDetection
import Amazonka.Rekognition.Types.CoversBodyPart
import Amazonka.Rekognition.Types.CustomLabel
import Amazonka.Rekognition.Types.DetectTextFilters
import Amazonka.Rekognition.Types.DetectionFilter
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
import Amazonka.Rekognition.Types.KnownGender
import Amazonka.Rekognition.Types.Label
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
import Amazonka.Rekognition.Types.ProjectVersionDescription
import Amazonka.Rekognition.Types.ProtectiveEquipmentBodyPart
import Amazonka.Rekognition.Types.ProtectiveEquipmentPerson
import Amazonka.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes
import Amazonka.Rekognition.Types.ProtectiveEquipmentSummary
import Amazonka.Rekognition.Types.RegionOfInterest
import Amazonka.Rekognition.Types.S3Object
import Amazonka.Rekognition.Types.SegmentDetection
import Amazonka.Rekognition.Types.SegmentTypeInfo
import Amazonka.Rekognition.Types.ShotSegment
import Amazonka.Rekognition.Types.Smile
import Amazonka.Rekognition.Types.StartSegmentDetectionFilters
import Amazonka.Rekognition.Types.StartShotDetectionFilter
import Amazonka.Rekognition.Types.StartTechnicalCueDetectionFilter
import Amazonka.Rekognition.Types.StartTextDetectionFilters
import Amazonka.Rekognition.Types.StreamProcessor
import Amazonka.Rekognition.Types.StreamProcessorInput
import Amazonka.Rekognition.Types.StreamProcessorOutput
import Amazonka.Rekognition.Types.StreamProcessorSettings
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
