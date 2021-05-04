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

    -- ** StartFaceSearch
    startFaceSearch_notificationChannel,
    startFaceSearch_clientRequestToken,
    startFaceSearch_jobTag,
    startFaceSearch_faceMatchThreshold,
    startFaceSearch_video,
    startFaceSearch_collectionId,
    startFaceSearchResponse_jobId,
    startFaceSearchResponse_httpStatus,

    -- ** DescribeStreamProcessor
    describeStreamProcessor_name,
    describeStreamProcessorResponse_creationTimestamp,
    describeStreamProcessorResponse_statusMessage,
    describeStreamProcessorResponse_status,
    describeStreamProcessorResponse_roleArn,
    describeStreamProcessorResponse_input,
    describeStreamProcessorResponse_streamProcessorArn,
    describeStreamProcessorResponse_output,
    describeStreamProcessorResponse_name,
    describeStreamProcessorResponse_lastUpdateTimestamp,
    describeStreamProcessorResponse_settings,
    describeStreamProcessorResponse_httpStatus,

    -- ** DeleteCollection
    deleteCollection_collectionId,
    deleteCollectionResponse_statusCode,
    deleteCollectionResponse_httpStatus,

    -- ** GetLabelDetection
    getLabelDetection_nextToken,
    getLabelDetection_maxResults,
    getLabelDetection_sortBy,
    getLabelDetection_jobId,
    getLabelDetectionResponse_statusMessage,
    getLabelDetectionResponse_videoMetadata,
    getLabelDetectionResponse_nextToken,
    getLabelDetectionResponse_labelModelVersion,
    getLabelDetectionResponse_jobStatus,
    getLabelDetectionResponse_labels,
    getLabelDetectionResponse_httpStatus,

    -- ** SearchFaces
    searchFaces_maxFaces,
    searchFaces_faceMatchThreshold,
    searchFaces_collectionId,
    searchFaces_faceId,
    searchFacesResponse_faceModelVersion,
    searchFacesResponse_faceMatches,
    searchFacesResponse_searchedFaceId,
    searchFacesResponse_httpStatus,

    -- ** GetTextDetection
    getTextDetection_nextToken,
    getTextDetection_maxResults,
    getTextDetection_jobId,
    getTextDetectionResponse_statusMessage,
    getTextDetectionResponse_videoMetadata,
    getTextDetectionResponse_nextToken,
    getTextDetectionResponse_textDetections,
    getTextDetectionResponse_jobStatus,
    getTextDetectionResponse_textModelVersion,
    getTextDetectionResponse_httpStatus,

    -- ** CreateProject
    createProject_projectName,
    createProjectResponse_projectArn,
    createProjectResponse_httpStatus,

    -- ** DetectCustomLabels
    detectCustomLabels_maxResults,
    detectCustomLabels_minConfidence,
    detectCustomLabels_projectVersionArn,
    detectCustomLabels_image,
    detectCustomLabelsResponse_customLabels,
    detectCustomLabelsResponse_httpStatus,

    -- ** RecognizeCelebrities
    recognizeCelebrities_image,
    recognizeCelebritiesResponse_unrecognizedFaces,
    recognizeCelebritiesResponse_celebrityFaces,
    recognizeCelebritiesResponse_orientationCorrection,
    recognizeCelebritiesResponse_httpStatus,

    -- ** StartFaceDetection
    startFaceDetection_notificationChannel,
    startFaceDetection_faceAttributes,
    startFaceDetection_clientRequestToken,
    startFaceDetection_jobTag,
    startFaceDetection_video,
    startFaceDetectionResponse_jobId,
    startFaceDetectionResponse_httpStatus,

    -- ** DetectModerationLabels
    detectModerationLabels_humanLoopConfig,
    detectModerationLabels_minConfidence,
    detectModerationLabels_image,
    detectModerationLabelsResponse_moderationLabels,
    detectModerationLabelsResponse_moderationModelVersion,
    detectModerationLabelsResponse_humanLoopActivationOutput,
    detectModerationLabelsResponse_httpStatus,

    -- ** DeleteFaces
    deleteFaces_collectionId,
    deleteFaces_faceIds,
    deleteFacesResponse_deletedFaces,
    deleteFacesResponse_httpStatus,

    -- ** ListStreamProcessors
    listStreamProcessors_nextToken,
    listStreamProcessors_maxResults,
    listStreamProcessorsResponse_nextToken,
    listStreamProcessorsResponse_streamProcessors,
    listStreamProcessorsResponse_httpStatus,

    -- ** DescribeCollection
    describeCollection_collectionId,
    describeCollectionResponse_creationTimestamp,
    describeCollectionResponse_faceModelVersion,
    describeCollectionResponse_collectionARN,
    describeCollectionResponse_faceCount,
    describeCollectionResponse_httpStatus,

    -- ** DeleteStreamProcessor
    deleteStreamProcessor_name,
    deleteStreamProcessorResponse_httpStatus,

    -- ** ListFaces
    listFaces_nextToken,
    listFaces_maxResults,
    listFaces_collectionId,
    listFacesResponse_faceModelVersion,
    listFacesResponse_nextToken,
    listFacesResponse_faces,
    listFacesResponse_httpStatus,

    -- ** SearchFacesByImage
    searchFacesByImage_qualityFilter,
    searchFacesByImage_maxFaces,
    searchFacesByImage_faceMatchThreshold,
    searchFacesByImage_collectionId,
    searchFacesByImage_image,
    searchFacesByImageResponse_faceModelVersion,
    searchFacesByImageResponse_faceMatches,
    searchFacesByImageResponse_searchedFaceBoundingBox,
    searchFacesByImageResponse_searchedFaceConfidence,
    searchFacesByImageResponse_httpStatus,

    -- ** CompareFaces
    compareFaces_qualityFilter,
    compareFaces_similarityThreshold,
    compareFaces_sourceImage,
    compareFaces_targetImage,
    compareFacesResponse_sourceImageFace,
    compareFacesResponse_unmatchedFaces,
    compareFacesResponse_faceMatches,
    compareFacesResponse_targetImageOrientationCorrection,
    compareFacesResponse_sourceImageOrientationCorrection,
    compareFacesResponse_httpStatus,

    -- ** DetectLabels
    detectLabels_maxLabels,
    detectLabels_minConfidence,
    detectLabels_image,
    detectLabelsResponse_labelModelVersion,
    detectLabelsResponse_orientationCorrection,
    detectLabelsResponse_labels,
    detectLabelsResponse_httpStatus,

    -- ** GetSegmentDetection
    getSegmentDetection_nextToken,
    getSegmentDetection_maxResults,
    getSegmentDetection_jobId,
    getSegmentDetectionResponse_statusMessage,
    getSegmentDetectionResponse_videoMetadata,
    getSegmentDetectionResponse_nextToken,
    getSegmentDetectionResponse_selectedSegmentTypes,
    getSegmentDetectionResponse_audioMetadata,
    getSegmentDetectionResponse_jobStatus,
    getSegmentDetectionResponse_segments,
    getSegmentDetectionResponse_httpStatus,

    -- ** GetCelebrityRecognition
    getCelebrityRecognition_nextToken,
    getCelebrityRecognition_maxResults,
    getCelebrityRecognition_sortBy,
    getCelebrityRecognition_jobId,
    getCelebrityRecognitionResponse_statusMessage,
    getCelebrityRecognitionResponse_videoMetadata,
    getCelebrityRecognitionResponse_nextToken,
    getCelebrityRecognitionResponse_jobStatus,
    getCelebrityRecognitionResponse_celebrities,
    getCelebrityRecognitionResponse_httpStatus,

    -- ** StartPersonTracking
    startPersonTracking_notificationChannel,
    startPersonTracking_clientRequestToken,
    startPersonTracking_jobTag,
    startPersonTracking_video,
    startPersonTrackingResponse_jobId,
    startPersonTrackingResponse_httpStatus,

    -- ** CreateCollection
    createCollection_collectionId,
    createCollectionResponse_faceModelVersion,
    createCollectionResponse_collectionArn,
    createCollectionResponse_statusCode,
    createCollectionResponse_httpStatus,

    -- ** StopProjectVersion
    stopProjectVersion_projectVersionArn,
    stopProjectVersionResponse_status,
    stopProjectVersionResponse_httpStatus,

    -- ** StartProjectVersion
    startProjectVersion_projectVersionArn,
    startProjectVersion_minInferenceUnits,
    startProjectVersionResponse_status,
    startProjectVersionResponse_httpStatus,

    -- ** ListCollections
    listCollections_nextToken,
    listCollections_maxResults,
    listCollectionsResponse_faceModelVersions,
    listCollectionsResponse_nextToken,
    listCollectionsResponse_collectionIds,
    listCollectionsResponse_httpStatus,

    -- ** DetectProtectiveEquipment
    detectProtectiveEquipment_summarizationAttributes,
    detectProtectiveEquipment_image,
    detectProtectiveEquipmentResponse_protectiveEquipmentModelVersion,
    detectProtectiveEquipmentResponse_summary,
    detectProtectiveEquipmentResponse_persons,
    detectProtectiveEquipmentResponse_httpStatus,

    -- ** GetPersonTracking
    getPersonTracking_nextToken,
    getPersonTracking_maxResults,
    getPersonTracking_sortBy,
    getPersonTracking_jobId,
    getPersonTrackingResponse_statusMessage,
    getPersonTrackingResponse_videoMetadata,
    getPersonTrackingResponse_nextToken,
    getPersonTrackingResponse_jobStatus,
    getPersonTrackingResponse_persons,
    getPersonTrackingResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_projectArn,
    deleteProjectResponse_status,
    deleteProjectResponse_httpStatus,

    -- ** IndexFaces
    indexFaces_detectionAttributes,
    indexFaces_qualityFilter,
    indexFaces_externalImageId,
    indexFaces_maxFaces,
    indexFaces_collectionId,
    indexFaces_image,
    indexFacesResponse_faceRecords,
    indexFacesResponse_faceModelVersion,
    indexFacesResponse_unindexedFaces,
    indexFacesResponse_orientationCorrection,
    indexFacesResponse_httpStatus,

    -- ** StartSegmentDetection
    startSegmentDetection_notificationChannel,
    startSegmentDetection_filters,
    startSegmentDetection_clientRequestToken,
    startSegmentDetection_jobTag,
    startSegmentDetection_video,
    startSegmentDetection_segmentTypes,
    startSegmentDetectionResponse_jobId,
    startSegmentDetectionResponse_httpStatus,

    -- ** StartCelebrityRecognition
    startCelebrityRecognition_notificationChannel,
    startCelebrityRecognition_clientRequestToken,
    startCelebrityRecognition_jobTag,
    startCelebrityRecognition_video,
    startCelebrityRecognitionResponse_jobId,
    startCelebrityRecognitionResponse_httpStatus,

    -- ** GetFaceSearch
    getFaceSearch_nextToken,
    getFaceSearch_maxResults,
    getFaceSearch_sortBy,
    getFaceSearch_jobId,
    getFaceSearchResponse_statusMessage,
    getFaceSearchResponse_videoMetadata,
    getFaceSearchResponse_nextToken,
    getFaceSearchResponse_jobStatus,
    getFaceSearchResponse_persons,
    getFaceSearchResponse_httpStatus,

    -- ** StartLabelDetection
    startLabelDetection_notificationChannel,
    startLabelDetection_minConfidence,
    startLabelDetection_clientRequestToken,
    startLabelDetection_jobTag,
    startLabelDetection_video,
    startLabelDetectionResponse_jobId,
    startLabelDetectionResponse_httpStatus,

    -- ** DescribeProjectVersions
    describeProjectVersions_nextToken,
    describeProjectVersions_versionNames,
    describeProjectVersions_maxResults,
    describeProjectVersions_projectArn,
    describeProjectVersionsResponse_nextToken,
    describeProjectVersionsResponse_projectVersionDescriptions,
    describeProjectVersionsResponse_httpStatus,

    -- ** DeleteProjectVersion
    deleteProjectVersion_projectVersionArn,
    deleteProjectVersionResponse_status,
    deleteProjectVersionResponse_httpStatus,

    -- ** CreateStreamProcessor
    createStreamProcessor_input,
    createStreamProcessor_output,
    createStreamProcessor_name,
    createStreamProcessor_settings,
    createStreamProcessor_roleArn,
    createStreamProcessorResponse_streamProcessorArn,
    createStreamProcessorResponse_httpStatus,

    -- ** GetContentModeration
    getContentModeration_nextToken,
    getContentModeration_maxResults,
    getContentModeration_sortBy,
    getContentModeration_jobId,
    getContentModerationResponse_statusMessage,
    getContentModerationResponse_videoMetadata,
    getContentModerationResponse_nextToken,
    getContentModerationResponse_jobStatus,
    getContentModerationResponse_moderationLabels,
    getContentModerationResponse_moderationModelVersion,
    getContentModerationResponse_httpStatus,

    -- ** GetCelebrityInfo
    getCelebrityInfo_id,
    getCelebrityInfoResponse_urls,
    getCelebrityInfoResponse_name,
    getCelebrityInfoResponse_httpStatus,

    -- ** DescribeProjects
    describeProjects_nextToken,
    describeProjects_maxResults,
    describeProjectsResponse_nextToken,
    describeProjectsResponse_projectDescriptions,
    describeProjectsResponse_httpStatus,

    -- ** CreateProjectVersion
    createProjectVersion_projectArn,
    createProjectVersion_versionName,
    createProjectVersion_outputConfig,
    createProjectVersion_trainingData,
    createProjectVersion_testingData,
    createProjectVersionResponse_projectVersionArn,
    createProjectVersionResponse_httpStatus,

    -- ** GetFaceDetection
    getFaceDetection_nextToken,
    getFaceDetection_maxResults,
    getFaceDetection_jobId,
    getFaceDetectionResponse_statusMessage,
    getFaceDetectionResponse_videoMetadata,
    getFaceDetectionResponse_nextToken,
    getFaceDetectionResponse_jobStatus,
    getFaceDetectionResponse_faces,
    getFaceDetectionResponse_httpStatus,

    -- ** StartContentModeration
    startContentModeration_notificationChannel,
    startContentModeration_minConfidence,
    startContentModeration_clientRequestToken,
    startContentModeration_jobTag,
    startContentModeration_video,
    startContentModerationResponse_jobId,
    startContentModerationResponse_httpStatus,

    -- ** DetectFaces
    detectFaces_attributes,
    detectFaces_image,
    detectFacesResponse_faceDetails,
    detectFacesResponse_orientationCorrection,
    detectFacesResponse_httpStatus,

    -- ** DetectText
    detectText_filters,
    detectText_image,
    detectTextResponse_textDetections,
    detectTextResponse_textModelVersion,
    detectTextResponse_httpStatus,

    -- ** StartTextDetection
    startTextDetection_notificationChannel,
    startTextDetection_filters,
    startTextDetection_clientRequestToken,
    startTextDetection_jobTag,
    startTextDetection_video,
    startTextDetectionResponse_jobId,
    startTextDetectionResponse_httpStatus,

    -- ** StopStreamProcessor
    stopStreamProcessor_name,
    stopStreamProcessorResponse_httpStatus,

    -- ** StartStreamProcessor
    startStreamProcessor_name,
    startStreamProcessorResponse_httpStatus,

    -- * Types

    -- ** AgeRange
    ageRange_high,
    ageRange_low,

    -- ** Asset
    asset_groundTruthManifest,

    -- ** AudioMetadata
    audioMetadata_codec,
    audioMetadata_sampleRate,
    audioMetadata_durationMillis,
    audioMetadata_numberOfChannels,

    -- ** Beard
    beard_confidence,
    beard_value,

    -- ** BoundingBox
    boundingBox_height,
    boundingBox_width,
    boundingBox_top,
    boundingBox_left,

    -- ** Celebrity
    celebrity_urls,
    celebrity_id,
    celebrity_matchConfidence,
    celebrity_name,
    celebrity_face,

    -- ** CelebrityDetail
    celebrityDetail_urls,
    celebrityDetail_id,
    celebrityDetail_boundingBox,
    celebrityDetail_name,
    celebrityDetail_confidence,
    celebrityDetail_face,

    -- ** CelebrityRecognition
    celebrityRecognition_timestamp,
    celebrityRecognition_celebrity,

    -- ** CompareFacesMatch
    compareFacesMatch_similarity,
    compareFacesMatch_face,

    -- ** ComparedFace
    comparedFace_pose,
    comparedFace_landmarks,
    comparedFace_boundingBox,
    comparedFace_confidence,
    comparedFace_quality,

    -- ** ComparedSourceImageFace
    comparedSourceImageFace_boundingBox,
    comparedSourceImageFace_confidence,

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

    -- ** DetectTextFilters
    detectTextFilters_regionsOfInterest,
    detectTextFilters_wordFilter,

    -- ** DetectionFilter
    detectionFilter_minBoundingBoxWidth,
    detectionFilter_minConfidence,
    detectionFilter_minBoundingBoxHeight,

    -- ** Emotion
    emotion_confidence,
    emotion_type,

    -- ** EquipmentDetection
    equipmentDetection_coversBodyPart,
    equipmentDetection_boundingBox,
    equipmentDetection_confidence,
    equipmentDetection_type,

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
    face_faceId,
    face_imageId,
    face_externalImageId,
    face_boundingBox,
    face_confidence,

    -- ** FaceDetail
    faceDetail_sunglasses,
    faceDetail_ageRange,
    faceDetail_pose,
    faceDetail_landmarks,
    faceDetail_beard,
    faceDetail_emotions,
    faceDetail_eyeglasses,
    faceDetail_gender,
    faceDetail_boundingBox,
    faceDetail_mouthOpen,
    faceDetail_confidence,
    faceDetail_smile,
    faceDetail_eyesOpen,
    faceDetail_mustache,
    faceDetail_quality,

    -- ** FaceDetection
    faceDetection_face,
    faceDetection_timestamp,

    -- ** FaceMatch
    faceMatch_similarity,
    faceMatch_face,

    -- ** FaceRecord
    faceRecord_faceDetail,
    faceRecord_face,

    -- ** FaceSearchSettings
    faceSearchSettings_collectionId,
    faceSearchSettings_faceMatchThreshold,

    -- ** Gender
    gender_confidence,
    gender_value,

    -- ** Geometry
    geometry_polygon,
    geometry_boundingBox,

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
    image_bytes,
    image_s3Object,

    -- ** ImageQuality
    imageQuality_brightness,
    imageQuality_sharpness,

    -- ** Instance
    instance_boundingBox,
    instance_confidence,

    -- ** KinesisDataStream
    kinesisDataStream_arn,

    -- ** KinesisVideoStream
    kinesisVideoStream_arn,

    -- ** Label
    label_parents,
    label_instances,
    label_name,
    label_confidence,

    -- ** LabelDetection
    labelDetection_label,
    labelDetection_timestamp,

    -- ** Landmark
    landmark_y,
    landmark_x,
    landmark_type,

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
    personDetail_boundingBox,
    personDetail_face,
    personDetail_index,

    -- ** PersonDetection
    personDetection_timestamp,
    personDetection_person,

    -- ** PersonMatch
    personMatch_faceMatches,
    personMatch_timestamp,
    personMatch_person,

    -- ** Point
    point_y,
    point_x,

    -- ** Pose
    pose_yaw,
    pose_pitch,
    pose_roll,

    -- ** ProjectDescription
    projectDescription_creationTimestamp,
    projectDescription_status,
    projectDescription_projectArn,

    -- ** ProjectVersionDescription
    projectVersionDescription_creationTimestamp,
    projectVersionDescription_statusMessage,
    projectVersionDescription_testingDataResult,
    projectVersionDescription_evaluationResult,
    projectVersionDescription_status,
    projectVersionDescription_billableTrainingTimeInSeconds,
    projectVersionDescription_outputConfig,
    projectVersionDescription_projectVersionArn,
    projectVersionDescription_minInferenceUnits,
    projectVersionDescription_trainingDataResult,
    projectVersionDescription_trainingEndTimestamp,
    projectVersionDescription_manifestSummary,

    -- ** ProtectiveEquipmentBodyPart
    protectiveEquipmentBodyPart_equipmentDetections,
    protectiveEquipmentBodyPart_name,
    protectiveEquipmentBodyPart_confidence,

    -- ** ProtectiveEquipmentPerson
    protectiveEquipmentPerson_id,
    protectiveEquipmentPerson_boundingBox,
    protectiveEquipmentPerson_bodyParts,
    protectiveEquipmentPerson_confidence,

    -- ** ProtectiveEquipmentSummarizationAttributes
    protectiveEquipmentSummarizationAttributes_minConfidence,
    protectiveEquipmentSummarizationAttributes_requiredEquipmentTypes,

    -- ** ProtectiveEquipmentSummary
    protectiveEquipmentSummary_personsWithRequiredEquipment,
    protectiveEquipmentSummary_personsIndeterminate,
    protectiveEquipmentSummary_personsWithoutRequiredEquipment,

    -- ** RegionOfInterest
    regionOfInterest_boundingBox,

    -- ** S3Object
    s3Object_version,
    s3Object_name,
    s3Object_bucket,

    -- ** SegmentDetection
    segmentDetection_shotSegment,
    segmentDetection_endTimestampMillis,
    segmentDetection_startTimecodeSMPTE,
    segmentDetection_durationSMPTE,
    segmentDetection_technicalCueSegment,
    segmentDetection_type,
    segmentDetection_durationMillis,
    segmentDetection_endTimecodeSMPTE,
    segmentDetection_startTimestampMillis,

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
    startSegmentDetectionFilters_technicalCueFilter,
    startSegmentDetectionFilters_shotFilter,

    -- ** StartShotDetectionFilter
    startShotDetectionFilter_minSegmentConfidence,

    -- ** StartTechnicalCueDetectionFilter
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
    sunglasses_confidence,
    sunglasses_value,

    -- ** TechnicalCueSegment
    technicalCueSegment_confidence,
    technicalCueSegment_type,

    -- ** TestingData
    testingData_autoCreate,
    testingData_assets,

    -- ** TestingDataResult
    testingDataResult_input,
    testingDataResult_output,
    testingDataResult_validation,

    -- ** TextDetection
    textDetection_detectedText,
    textDetection_id,
    textDetection_confidence,
    textDetection_parentId,
    textDetection_type,
    textDetection_geometry,

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

    -- ** ValidationData
    validationData_assets,

    -- ** Video
    video_s3Object,

    -- ** VideoMetadata
    videoMetadata_codec,
    videoMetadata_format,
    videoMetadata_frameHeight,
    videoMetadata_frameRate,
    videoMetadata_frameWidth,
    videoMetadata_durationMillis,
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
import Network.AWS.Rekognition.Types.AgeRange
import Network.AWS.Rekognition.Types.Asset
import Network.AWS.Rekognition.Types.AudioMetadata
import Network.AWS.Rekognition.Types.Beard
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
