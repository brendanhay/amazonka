{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rekognition.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _HumanLoopQuotaExceededException,
    _IdempotentParameterMismatchException,
    _ImageTooLargeException,
    _InternalServerError,
    _InvalidImageFormatException,
    _InvalidPaginationTokenException,
    _InvalidParameterException,
    _InvalidPolicyRevisionIdException,
    _InvalidS3ObjectException,
    _LimitExceededException,
    _MalformedPolicyDocumentException,
    _ProvisionedThroughputExceededException,
    _ResourceAlreadyExistsException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ResourceNotReadyException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _VideoTooLargeException,

    -- * Attribute
    Attribute (..),

    -- * BodyPart
    BodyPart (..),

    -- * CelebrityRecognitionSortBy
    CelebrityRecognitionSortBy (..),

    -- * ContentClassifier
    ContentClassifier (..),

    -- * ContentModerationSortBy
    ContentModerationSortBy (..),

    -- * DatasetStatus
    DatasetStatus (..),

    -- * DatasetStatusMessageCode
    DatasetStatusMessageCode (..),

    -- * DatasetType
    DatasetType (..),

    -- * DetectLabelsFeatureName
    DetectLabelsFeatureName (..),

    -- * EmotionName
    EmotionName (..),

    -- * FaceAttributes
    FaceAttributes (..),

    -- * FaceSearchSortBy
    FaceSearchSortBy (..),

    -- * GenderType
    GenderType (..),

    -- * KnownGenderType
    KnownGenderType (..),

    -- * LabelDetectionAggregateBy
    LabelDetectionAggregateBy (..),

    -- * LabelDetectionFeatureName
    LabelDetectionFeatureName (..),

    -- * LabelDetectionSortBy
    LabelDetectionSortBy (..),

    -- * LandmarkType
    LandmarkType (..),

    -- * OrientationCorrection
    OrientationCorrection (..),

    -- * PersonTrackingSortBy
    PersonTrackingSortBy (..),

    -- * ProjectStatus
    ProjectStatus (..),

    -- * ProjectVersionStatus
    ProjectVersionStatus (..),

    -- * ProtectiveEquipmentType
    ProtectiveEquipmentType (..),

    -- * QualityFilter
    QualityFilter (..),

    -- * Reason
    Reason (..),

    -- * SegmentType
    SegmentType (..),

    -- * StreamProcessorParameterToDelete
    StreamProcessorParameterToDelete (..),

    -- * StreamProcessorStatus
    StreamProcessorStatus (..),

    -- * TechnicalCueType
    TechnicalCueType (..),

    -- * TextTypes
    TextTypes (..),

    -- * VideoColorRange
    VideoColorRange (..),

    -- * VideoJobStatus
    VideoJobStatus (..),

    -- * AgeRange
    AgeRange (..),
    newAgeRange,
    ageRange_high,
    ageRange_low,

    -- * Asset
    Asset (..),
    newAsset,
    asset_groundTruthManifest,

    -- * AudioMetadata
    AudioMetadata (..),
    newAudioMetadata,
    audioMetadata_codec,
    audioMetadata_durationMillis,
    audioMetadata_numberOfChannels,
    audioMetadata_sampleRate,

    -- * Beard
    Beard (..),
    newBeard,
    beard_confidence,
    beard_value,

    -- * BlackFrame
    BlackFrame (..),
    newBlackFrame,
    blackFrame_maxPixelThreshold,
    blackFrame_minCoveragePercentage,

    -- * BoundingBox
    BoundingBox (..),
    newBoundingBox,
    boundingBox_height,
    boundingBox_left,
    boundingBox_top,
    boundingBox_width,

    -- * Celebrity
    Celebrity (..),
    newCelebrity,
    celebrity_face,
    celebrity_id,
    celebrity_knownGender,
    celebrity_matchConfidence,
    celebrity_name,
    celebrity_urls,

    -- * CelebrityDetail
    CelebrityDetail (..),
    newCelebrityDetail,
    celebrityDetail_boundingBox,
    celebrityDetail_confidence,
    celebrityDetail_face,
    celebrityDetail_id,
    celebrityDetail_knownGender,
    celebrityDetail_name,
    celebrityDetail_urls,

    -- * CelebrityRecognition
    CelebrityRecognition (..),
    newCelebrityRecognition,
    celebrityRecognition_celebrity,
    celebrityRecognition_timestamp,

    -- * CompareFacesMatch
    CompareFacesMatch (..),
    newCompareFacesMatch,
    compareFacesMatch_face,
    compareFacesMatch_similarity,

    -- * ComparedFace
    ComparedFace (..),
    newComparedFace,
    comparedFace_boundingBox,
    comparedFace_confidence,
    comparedFace_emotions,
    comparedFace_landmarks,
    comparedFace_pose,
    comparedFace_quality,
    comparedFace_smile,

    -- * ComparedSourceImageFace
    ComparedSourceImageFace (..),
    newComparedSourceImageFace,
    comparedSourceImageFace_boundingBox,
    comparedSourceImageFace_confidence,

    -- * ConnectedHomeSettings
    ConnectedHomeSettings (..),
    newConnectedHomeSettings,
    connectedHomeSettings_minConfidence,
    connectedHomeSettings_labels,

    -- * ConnectedHomeSettingsForUpdate
    ConnectedHomeSettingsForUpdate (..),
    newConnectedHomeSettingsForUpdate,
    connectedHomeSettingsForUpdate_labels,
    connectedHomeSettingsForUpdate_minConfidence,

    -- * ContentModerationDetection
    ContentModerationDetection (..),
    newContentModerationDetection,
    contentModerationDetection_moderationLabel,
    contentModerationDetection_timestamp,

    -- * CoversBodyPart
    CoversBodyPart (..),
    newCoversBodyPart,
    coversBodyPart_confidence,
    coversBodyPart_value,

    -- * CustomLabel
    CustomLabel (..),
    newCustomLabel,
    customLabel_confidence,
    customLabel_geometry,
    customLabel_name,

    -- * DatasetChanges
    DatasetChanges (..),
    newDatasetChanges,
    datasetChanges_groundTruth,

    -- * DatasetDescription
    DatasetDescription (..),
    newDatasetDescription,
    datasetDescription_creationTimestamp,
    datasetDescription_datasetStats,
    datasetDescription_lastUpdatedTimestamp,
    datasetDescription_status,
    datasetDescription_statusMessage,
    datasetDescription_statusMessageCode,

    -- * DatasetLabelDescription
    DatasetLabelDescription (..),
    newDatasetLabelDescription,
    datasetLabelDescription_labelName,
    datasetLabelDescription_labelStats,

    -- * DatasetLabelStats
    DatasetLabelStats (..),
    newDatasetLabelStats,
    datasetLabelStats_boundingBoxCount,
    datasetLabelStats_entryCount,

    -- * DatasetMetadata
    DatasetMetadata (..),
    newDatasetMetadata,
    datasetMetadata_creationTimestamp,
    datasetMetadata_datasetArn,
    datasetMetadata_datasetType,
    datasetMetadata_status,
    datasetMetadata_statusMessage,
    datasetMetadata_statusMessageCode,

    -- * DatasetSource
    DatasetSource (..),
    newDatasetSource,
    datasetSource_datasetArn,
    datasetSource_groundTruthManifest,

    -- * DatasetStats
    DatasetStats (..),
    newDatasetStats,
    datasetStats_errorEntries,
    datasetStats_labeledEntries,
    datasetStats_totalEntries,
    datasetStats_totalLabels,

    -- * DetectLabelsImageBackground
    DetectLabelsImageBackground (..),
    newDetectLabelsImageBackground,
    detectLabelsImageBackground_dominantColors,
    detectLabelsImageBackground_quality,

    -- * DetectLabelsImageForeground
    DetectLabelsImageForeground (..),
    newDetectLabelsImageForeground,
    detectLabelsImageForeground_dominantColors,
    detectLabelsImageForeground_quality,

    -- * DetectLabelsImageProperties
    DetectLabelsImageProperties (..),
    newDetectLabelsImageProperties,
    detectLabelsImageProperties_background,
    detectLabelsImageProperties_dominantColors,
    detectLabelsImageProperties_foreground,
    detectLabelsImageProperties_quality,

    -- * DetectLabelsImagePropertiesSettings
    DetectLabelsImagePropertiesSettings (..),
    newDetectLabelsImagePropertiesSettings,
    detectLabelsImagePropertiesSettings_maxDominantColors,

    -- * DetectLabelsImageQuality
    DetectLabelsImageQuality (..),
    newDetectLabelsImageQuality,
    detectLabelsImageQuality_brightness,
    detectLabelsImageQuality_contrast,
    detectLabelsImageQuality_sharpness,

    -- * DetectLabelsSettings
    DetectLabelsSettings (..),
    newDetectLabelsSettings,
    detectLabelsSettings_generalLabels,
    detectLabelsSettings_imageProperties,

    -- * DetectTextFilters
    DetectTextFilters (..),
    newDetectTextFilters,
    detectTextFilters_regionsOfInterest,
    detectTextFilters_wordFilter,

    -- * DetectionFilter
    DetectionFilter (..),
    newDetectionFilter,
    detectionFilter_minBoundingBoxHeight,
    detectionFilter_minBoundingBoxWidth,
    detectionFilter_minConfidence,

    -- * DistributeDataset
    DistributeDataset (..),
    newDistributeDataset,
    distributeDataset_arn,

    -- * DominantColor
    DominantColor (..),
    newDominantColor,
    dominantColor_blue,
    dominantColor_cSSColor,
    dominantColor_green,
    dominantColor_hexCode,
    dominantColor_pixelPercent,
    dominantColor_red,
    dominantColor_simplifiedColor,

    -- * Emotion
    Emotion (..),
    newEmotion,
    emotion_confidence,
    emotion_type,

    -- * EquipmentDetection
    EquipmentDetection (..),
    newEquipmentDetection,
    equipmentDetection_boundingBox,
    equipmentDetection_confidence,
    equipmentDetection_coversBodyPart,
    equipmentDetection_type,

    -- * EvaluationResult
    EvaluationResult (..),
    newEvaluationResult,
    evaluationResult_f1Score,
    evaluationResult_summary,

    -- * EyeOpen
    EyeOpen (..),
    newEyeOpen,
    eyeOpen_confidence,
    eyeOpen_value,

    -- * Eyeglasses
    Eyeglasses (..),
    newEyeglasses,
    eyeglasses_confidence,
    eyeglasses_value,

    -- * Face
    Face (..),
    newFace,
    face_boundingBox,
    face_confidence,
    face_externalImageId,
    face_faceId,
    face_imageId,
    face_indexFacesModelVersion,

    -- * FaceDetail
    FaceDetail (..),
    newFaceDetail,
    faceDetail_ageRange,
    faceDetail_beard,
    faceDetail_boundingBox,
    faceDetail_confidence,
    faceDetail_emotions,
    faceDetail_eyeglasses,
    faceDetail_eyesOpen,
    faceDetail_gender,
    faceDetail_landmarks,
    faceDetail_mouthOpen,
    faceDetail_mustache,
    faceDetail_pose,
    faceDetail_quality,
    faceDetail_smile,
    faceDetail_sunglasses,

    -- * FaceDetection
    FaceDetection (..),
    newFaceDetection,
    faceDetection_face,
    faceDetection_timestamp,

    -- * FaceMatch
    FaceMatch (..),
    newFaceMatch,
    faceMatch_face,
    faceMatch_similarity,

    -- * FaceRecord
    FaceRecord (..),
    newFaceRecord,
    faceRecord_face,
    faceRecord_faceDetail,

    -- * FaceSearchSettings
    FaceSearchSettings (..),
    newFaceSearchSettings,
    faceSearchSettings_collectionId,
    faceSearchSettings_faceMatchThreshold,

    -- * Gender
    Gender (..),
    newGender,
    gender_confidence,
    gender_value,

    -- * GeneralLabelsSettings
    GeneralLabelsSettings (..),
    newGeneralLabelsSettings,
    generalLabelsSettings_labelCategoryExclusionFilters,
    generalLabelsSettings_labelCategoryInclusionFilters,
    generalLabelsSettings_labelExclusionFilters,
    generalLabelsSettings_labelInclusionFilters,

    -- * Geometry
    Geometry (..),
    newGeometry,
    geometry_boundingBox,
    geometry_polygon,

    -- * GroundTruthManifest
    GroundTruthManifest (..),
    newGroundTruthManifest,
    groundTruthManifest_s3Object,

    -- * HumanLoopActivationOutput
    HumanLoopActivationOutput (..),
    newHumanLoopActivationOutput,
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,
    humanLoopActivationOutput_humanLoopActivationReasons,
    humanLoopActivationOutput_humanLoopArn,

    -- * HumanLoopConfig
    HumanLoopConfig (..),
    newHumanLoopConfig,
    humanLoopConfig_dataAttributes,
    humanLoopConfig_humanLoopName,
    humanLoopConfig_flowDefinitionArn,

    -- * HumanLoopDataAttributes
    HumanLoopDataAttributes (..),
    newHumanLoopDataAttributes,
    humanLoopDataAttributes_contentClassifiers,

    -- * Image
    Image (..),
    newImage,
    image_bytes,
    image_s3Object,

    -- * ImageQuality
    ImageQuality (..),
    newImageQuality,
    imageQuality_brightness,
    imageQuality_sharpness,

    -- * Instance
    Instance (..),
    newInstance,
    instance_boundingBox,
    instance_confidence,
    instance_dominantColors,

    -- * KinesisDataStream
    KinesisDataStream (..),
    newKinesisDataStream,
    kinesisDataStream_arn,

    -- * KinesisVideoStream
    KinesisVideoStream (..),
    newKinesisVideoStream,
    kinesisVideoStream_arn,

    -- * KinesisVideoStreamStartSelector
    KinesisVideoStreamStartSelector (..),
    newKinesisVideoStreamStartSelector,
    kinesisVideoStreamStartSelector_fragmentNumber,
    kinesisVideoStreamStartSelector_producerTimestamp,

    -- * KnownGender
    KnownGender (..),
    newKnownGender,
    knownGender_type,

    -- * Label
    Label (..),
    newLabel,
    label_aliases,
    label_categories,
    label_confidence,
    label_instances,
    label_name,
    label_parents,

    -- * LabelAlias
    LabelAlias (..),
    newLabelAlias,
    labelAlias_name,

    -- * LabelCategory
    LabelCategory (..),
    newLabelCategory,
    labelCategory_name,

    -- * LabelDetection
    LabelDetection (..),
    newLabelDetection,
    labelDetection_durationMillis,
    labelDetection_endTimestampMillis,
    labelDetection_label,
    labelDetection_startTimestampMillis,
    labelDetection_timestamp,

    -- * LabelDetectionSettings
    LabelDetectionSettings (..),
    newLabelDetectionSettings,
    labelDetectionSettings_generalLabels,

    -- * Landmark
    Landmark (..),
    newLandmark,
    landmark_type,
    landmark_x,
    landmark_y,

    -- * ModerationLabel
    ModerationLabel (..),
    newModerationLabel,
    moderationLabel_confidence,
    moderationLabel_name,
    moderationLabel_parentName,

    -- * MouthOpen
    MouthOpen (..),
    newMouthOpen,
    mouthOpen_confidence,
    mouthOpen_value,

    -- * Mustache
    Mustache (..),
    newMustache,
    mustache_confidence,
    mustache_value,

    -- * NotificationChannel
    NotificationChannel (..),
    newNotificationChannel,
    notificationChannel_sNSTopicArn,
    notificationChannel_roleArn,

    -- * OutputConfig
    OutputConfig (..),
    newOutputConfig,
    outputConfig_s3Bucket,
    outputConfig_s3KeyPrefix,

    -- * Parent
    Parent (..),
    newParent,
    parent_name,

    -- * PersonDetail
    PersonDetail (..),
    newPersonDetail,
    personDetail_boundingBox,
    personDetail_face,
    personDetail_index,

    -- * PersonDetection
    PersonDetection (..),
    newPersonDetection,
    personDetection_person,
    personDetection_timestamp,

    -- * PersonMatch
    PersonMatch (..),
    newPersonMatch,
    personMatch_faceMatches,
    personMatch_person,
    personMatch_timestamp,

    -- * Point
    Point (..),
    newPoint,
    point_x,
    point_y,

    -- * Pose
    Pose (..),
    newPose,
    pose_pitch,
    pose_roll,
    pose_yaw,

    -- * ProjectDescription
    ProjectDescription (..),
    newProjectDescription,
    projectDescription_creationTimestamp,
    projectDescription_datasets,
    projectDescription_projectArn,
    projectDescription_status,

    -- * ProjectPolicy
    ProjectPolicy (..),
    newProjectPolicy,
    projectPolicy_creationTimestamp,
    projectPolicy_lastUpdatedTimestamp,
    projectPolicy_policyDocument,
    projectPolicy_policyName,
    projectPolicy_policyRevisionId,
    projectPolicy_projectArn,

    -- * ProjectVersionDescription
    ProjectVersionDescription (..),
    newProjectVersionDescription,
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

    -- * ProtectiveEquipmentBodyPart
    ProtectiveEquipmentBodyPart (..),
    newProtectiveEquipmentBodyPart,
    protectiveEquipmentBodyPart_confidence,
    protectiveEquipmentBodyPart_equipmentDetections,
    protectiveEquipmentBodyPart_name,

    -- * ProtectiveEquipmentPerson
    ProtectiveEquipmentPerson (..),
    newProtectiveEquipmentPerson,
    protectiveEquipmentPerson_bodyParts,
    protectiveEquipmentPerson_boundingBox,
    protectiveEquipmentPerson_confidence,
    protectiveEquipmentPerson_id,

    -- * ProtectiveEquipmentSummarizationAttributes
    ProtectiveEquipmentSummarizationAttributes (..),
    newProtectiveEquipmentSummarizationAttributes,
    protectiveEquipmentSummarizationAttributes_minConfidence,
    protectiveEquipmentSummarizationAttributes_requiredEquipmentTypes,

    -- * ProtectiveEquipmentSummary
    ProtectiveEquipmentSummary (..),
    newProtectiveEquipmentSummary,
    protectiveEquipmentSummary_personsIndeterminate,
    protectiveEquipmentSummary_personsWithRequiredEquipment,
    protectiveEquipmentSummary_personsWithoutRequiredEquipment,

    -- * RegionOfInterest
    RegionOfInterest (..),
    newRegionOfInterest,
    regionOfInterest_boundingBox,
    regionOfInterest_polygon,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_bucket,
    s3Destination_keyPrefix,

    -- * S3Object
    S3Object (..),
    newS3Object,
    s3Object_bucket,
    s3Object_name,
    s3Object_version,

    -- * SegmentDetection
    SegmentDetection (..),
    newSegmentDetection,
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

    -- * SegmentTypeInfo
    SegmentTypeInfo (..),
    newSegmentTypeInfo,
    segmentTypeInfo_modelVersion,
    segmentTypeInfo_type,

    -- * ShotSegment
    ShotSegment (..),
    newShotSegment,
    shotSegment_confidence,
    shotSegment_index,

    -- * Smile
    Smile (..),
    newSmile,
    smile_confidence,
    smile_value,

    -- * StartSegmentDetectionFilters
    StartSegmentDetectionFilters (..),
    newStartSegmentDetectionFilters,
    startSegmentDetectionFilters_shotFilter,
    startSegmentDetectionFilters_technicalCueFilter,

    -- * StartShotDetectionFilter
    StartShotDetectionFilter (..),
    newStartShotDetectionFilter,
    startShotDetectionFilter_minSegmentConfidence,

    -- * StartTechnicalCueDetectionFilter
    StartTechnicalCueDetectionFilter (..),
    newStartTechnicalCueDetectionFilter,
    startTechnicalCueDetectionFilter_blackFrame,
    startTechnicalCueDetectionFilter_minSegmentConfidence,

    -- * StartTextDetectionFilters
    StartTextDetectionFilters (..),
    newStartTextDetectionFilters,
    startTextDetectionFilters_regionsOfInterest,
    startTextDetectionFilters_wordFilter,

    -- * StreamProcessingStartSelector
    StreamProcessingStartSelector (..),
    newStreamProcessingStartSelector,
    streamProcessingStartSelector_kVSStreamStartSelector,

    -- * StreamProcessingStopSelector
    StreamProcessingStopSelector (..),
    newStreamProcessingStopSelector,
    streamProcessingStopSelector_maxDurationInSeconds,

    -- * StreamProcessor
    StreamProcessor (..),
    newStreamProcessor,
    streamProcessor_name,
    streamProcessor_status,

    -- * StreamProcessorDataSharingPreference
    StreamProcessorDataSharingPreference (..),
    newStreamProcessorDataSharingPreference,
    streamProcessorDataSharingPreference_optIn,

    -- * StreamProcessorInput
    StreamProcessorInput (..),
    newStreamProcessorInput,
    streamProcessorInput_kinesisVideoStream,

    -- * StreamProcessorNotificationChannel
    StreamProcessorNotificationChannel (..),
    newStreamProcessorNotificationChannel,
    streamProcessorNotificationChannel_sNSTopicArn,

    -- * StreamProcessorOutput
    StreamProcessorOutput (..),
    newStreamProcessorOutput,
    streamProcessorOutput_kinesisDataStream,
    streamProcessorOutput_s3Destination,

    -- * StreamProcessorSettings
    StreamProcessorSettings (..),
    newStreamProcessorSettings,
    streamProcessorSettings_connectedHome,
    streamProcessorSettings_faceSearch,

    -- * StreamProcessorSettingsForUpdate
    StreamProcessorSettingsForUpdate (..),
    newStreamProcessorSettingsForUpdate,
    streamProcessorSettingsForUpdate_connectedHomeForUpdate,

    -- * Summary
    Summary (..),
    newSummary,
    summary_s3Object,

    -- * Sunglasses
    Sunglasses (..),
    newSunglasses,
    sunglasses_confidence,
    sunglasses_value,

    -- * TechnicalCueSegment
    TechnicalCueSegment (..),
    newTechnicalCueSegment,
    technicalCueSegment_confidence,
    technicalCueSegment_type,

    -- * TestingData
    TestingData (..),
    newTestingData,
    testingData_assets,
    testingData_autoCreate,

    -- * TestingDataResult
    TestingDataResult (..),
    newTestingDataResult,
    testingDataResult_input,
    testingDataResult_output,
    testingDataResult_validation,

    -- * TextDetection
    TextDetection (..),
    newTextDetection,
    textDetection_confidence,
    textDetection_detectedText,
    textDetection_geometry,
    textDetection_id,
    textDetection_parentId,
    textDetection_type,

    -- * TextDetectionResult
    TextDetectionResult (..),
    newTextDetectionResult,
    textDetectionResult_textDetection,
    textDetectionResult_timestamp,

    -- * TrainingData
    TrainingData (..),
    newTrainingData,
    trainingData_assets,

    -- * TrainingDataResult
    TrainingDataResult (..),
    newTrainingDataResult,
    trainingDataResult_input,
    trainingDataResult_output,
    trainingDataResult_validation,

    -- * UnindexedFace
    UnindexedFace (..),
    newUnindexedFace,
    unindexedFace_faceDetail,
    unindexedFace_reasons,

    -- * ValidationData
    ValidationData (..),
    newValidationData,
    validationData_assets,

    -- * Video
    Video (..),
    newVideo,
    video_s3Object,

    -- * VideoMetadata
    VideoMetadata (..),
    newVideoMetadata,
    videoMetadata_codec,
    videoMetadata_colorRange,
    videoMetadata_durationMillis,
    videoMetadata_format,
    videoMetadata_frameHeight,
    videoMetadata_frameRate,
    videoMetadata_frameWidth,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.AgeRange
import Amazonka.Rekognition.Types.Asset
import Amazonka.Rekognition.Types.Attribute
import Amazonka.Rekognition.Types.AudioMetadata
import Amazonka.Rekognition.Types.Beard
import Amazonka.Rekognition.Types.BlackFrame
import Amazonka.Rekognition.Types.BodyPart
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.Celebrity
import Amazonka.Rekognition.Types.CelebrityDetail
import Amazonka.Rekognition.Types.CelebrityRecognition
import Amazonka.Rekognition.Types.CelebrityRecognitionSortBy
import Amazonka.Rekognition.Types.CompareFacesMatch
import Amazonka.Rekognition.Types.ComparedFace
import Amazonka.Rekognition.Types.ComparedSourceImageFace
import Amazonka.Rekognition.Types.ConnectedHomeSettings
import Amazonka.Rekognition.Types.ConnectedHomeSettingsForUpdate
import Amazonka.Rekognition.Types.ContentClassifier
import Amazonka.Rekognition.Types.ContentModerationDetection
import Amazonka.Rekognition.Types.ContentModerationSortBy
import Amazonka.Rekognition.Types.CoversBodyPart
import Amazonka.Rekognition.Types.CustomLabel
import Amazonka.Rekognition.Types.DatasetChanges
import Amazonka.Rekognition.Types.DatasetDescription
import Amazonka.Rekognition.Types.DatasetLabelDescription
import Amazonka.Rekognition.Types.DatasetLabelStats
import Amazonka.Rekognition.Types.DatasetMetadata
import Amazonka.Rekognition.Types.DatasetSource
import Amazonka.Rekognition.Types.DatasetStats
import Amazonka.Rekognition.Types.DatasetStatus
import Amazonka.Rekognition.Types.DatasetStatusMessageCode
import Amazonka.Rekognition.Types.DatasetType
import Amazonka.Rekognition.Types.DetectLabelsFeatureName
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
import Amazonka.Rekognition.Types.EmotionName
import Amazonka.Rekognition.Types.EquipmentDetection
import Amazonka.Rekognition.Types.EvaluationResult
import Amazonka.Rekognition.Types.EyeOpen
import Amazonka.Rekognition.Types.Eyeglasses
import Amazonka.Rekognition.Types.Face
import Amazonka.Rekognition.Types.FaceAttributes
import Amazonka.Rekognition.Types.FaceDetail
import Amazonka.Rekognition.Types.FaceDetection
import Amazonka.Rekognition.Types.FaceMatch
import Amazonka.Rekognition.Types.FaceRecord
import Amazonka.Rekognition.Types.FaceSearchSettings
import Amazonka.Rekognition.Types.FaceSearchSortBy
import Amazonka.Rekognition.Types.Gender
import Amazonka.Rekognition.Types.GenderType
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
import Amazonka.Rekognition.Types.KnownGenderType
import Amazonka.Rekognition.Types.Label
import Amazonka.Rekognition.Types.LabelAlias
import Amazonka.Rekognition.Types.LabelCategory
import Amazonka.Rekognition.Types.LabelDetection
import Amazonka.Rekognition.Types.LabelDetectionAggregateBy
import Amazonka.Rekognition.Types.LabelDetectionFeatureName
import Amazonka.Rekognition.Types.LabelDetectionSettings
import Amazonka.Rekognition.Types.LabelDetectionSortBy
import Amazonka.Rekognition.Types.Landmark
import Amazonka.Rekognition.Types.LandmarkType
import Amazonka.Rekognition.Types.ModerationLabel
import Amazonka.Rekognition.Types.MouthOpen
import Amazonka.Rekognition.Types.Mustache
import Amazonka.Rekognition.Types.NotificationChannel
import Amazonka.Rekognition.Types.OrientationCorrection
import Amazonka.Rekognition.Types.OutputConfig
import Amazonka.Rekognition.Types.Parent
import Amazonka.Rekognition.Types.PersonDetail
import Amazonka.Rekognition.Types.PersonDetection
import Amazonka.Rekognition.Types.PersonMatch
import Amazonka.Rekognition.Types.PersonTrackingSortBy
import Amazonka.Rekognition.Types.Point
import Amazonka.Rekognition.Types.Pose
import Amazonka.Rekognition.Types.ProjectDescription
import Amazonka.Rekognition.Types.ProjectPolicy
import Amazonka.Rekognition.Types.ProjectStatus
import Amazonka.Rekognition.Types.ProjectVersionDescription
import Amazonka.Rekognition.Types.ProjectVersionStatus
import Amazonka.Rekognition.Types.ProtectiveEquipmentBodyPart
import Amazonka.Rekognition.Types.ProtectiveEquipmentPerson
import Amazonka.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes
import Amazonka.Rekognition.Types.ProtectiveEquipmentSummary
import Amazonka.Rekognition.Types.ProtectiveEquipmentType
import Amazonka.Rekognition.Types.QualityFilter
import Amazonka.Rekognition.Types.Reason
import Amazonka.Rekognition.Types.RegionOfInterest
import Amazonka.Rekognition.Types.S3Destination
import Amazonka.Rekognition.Types.S3Object
import Amazonka.Rekognition.Types.SegmentDetection
import Amazonka.Rekognition.Types.SegmentType
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
import Amazonka.Rekognition.Types.StreamProcessorParameterToDelete
import Amazonka.Rekognition.Types.StreamProcessorSettings
import Amazonka.Rekognition.Types.StreamProcessorSettingsForUpdate
import Amazonka.Rekognition.Types.StreamProcessorStatus
import Amazonka.Rekognition.Types.Summary
import Amazonka.Rekognition.Types.Sunglasses
import Amazonka.Rekognition.Types.TechnicalCueSegment
import Amazonka.Rekognition.Types.TechnicalCueType
import Amazonka.Rekognition.Types.TestingData
import Amazonka.Rekognition.Types.TestingDataResult
import Amazonka.Rekognition.Types.TextDetection
import Amazonka.Rekognition.Types.TextDetectionResult
import Amazonka.Rekognition.Types.TextTypes
import Amazonka.Rekognition.Types.TrainingData
import Amazonka.Rekognition.Types.TrainingDataResult
import Amazonka.Rekognition.Types.UnindexedFace
import Amazonka.Rekognition.Types.ValidationData
import Amazonka.Rekognition.Types.Video
import Amazonka.Rekognition.Types.VideoColorRange
import Amazonka.Rekognition.Types.VideoJobStatus
import Amazonka.Rekognition.Types.VideoMetadata
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-06-27@ of the Amazon Rekognition SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Rekognition",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "rekognition",
      Core.signingName = "rekognition",
      Core.version = "2016-06-27",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Rekognition",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You are not authorized to perform the action.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The number of in-progress human reviews you have has exceeded the number
-- allowed.
_HumanLoopQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_HumanLoopQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "HumanLoopQuotaExceededException"

-- | A @ClientRequestToken@ input parameter was reused with an operation, but
-- at least one of the other input parameters is different from the
-- previous call to the operation.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Fold a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | The input image size exceeds the allowed limit. If you are calling
-- DetectProtectiveEquipment, the image size or resolution exceeds the
-- allowed limit. For more information, see Guidelines and quotas in Amazon
-- Rekognition in the Amazon Rekognition Developer Guide.
_ImageTooLargeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ImageTooLargeException =
  Core._MatchServiceError
    defaultService
    "ImageTooLargeException"

-- | Amazon Rekognition experienced a service issue. Try your call again.
_InternalServerError :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | The provided image format is not supported.
_InvalidImageFormatException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidImageFormatException =
  Core._MatchServiceError
    defaultService
    "InvalidImageFormatException"

-- | Pagination token in the request is not valid.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"

-- | Input parameter violated a constraint. Validate your parameter before
-- calling the API operation again.
_InvalidParameterException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The supplied revision id for the project policy is invalid.
_InvalidPolicyRevisionIdException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidPolicyRevisionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyRevisionIdException"

-- | Amazon Rekognition is unable to access the S3 object specified in the
-- request.
_InvalidS3ObjectException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidS3ObjectException =
  Core._MatchServiceError
    defaultService
    "InvalidS3ObjectException"

-- | An Amazon Rekognition service limit was exceeded. For example, if you
-- start too many Amazon Rekognition Video jobs concurrently, calls to
-- start operations (@StartLabelDetection@, for example) will raise a
-- @LimitExceededException@ exception (HTTP status code: 400) until the
-- number of concurrently running jobs is below the Amazon Rekognition
-- service limit.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The format of the project policy document that you supplied to
-- @PutProjectPolicy@ is incorrect.
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Fold a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyDocumentException"

-- | The number of requests exceeded your throughput limit. If you want to
-- increase this limit, contact Amazon Rekognition.
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | A resource with the specified ID already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The specified resource is already being used.
_ResourceInUseException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The resource specified in the request cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The requested resource isn\'t ready. For example, this exception occurs
-- when you call @DetectCustomLabels@ with a model version that isn\'t
-- deployed.
_ResourceNotReadyException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"

-- | The size of the collection exceeds the allowed limit. For more
-- information, see Guidelines and quotas in Amazon Rekognition in the
-- Amazon Rekognition Developer Guide.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | Amazon Rekognition is temporarily unable to process the request. Try
-- your call again.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The file size or duration of the supplied media is too large. The
-- maximum file size is 10GB. The maximum duration is 6 hours.
_VideoTooLargeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_VideoTooLargeException =
  Core._MatchServiceError
    defaultService
    "VideoTooLargeException"
