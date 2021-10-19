{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _VideoTooLargeException,
    _InvalidParameterException,
    _InvalidImageFormatException,
    _ResourceAlreadyExistsException,
    _InvalidS3ObjectException,
    _ProvisionedThroughputExceededException,
    _ImageTooLargeException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerError,
    _IdempotentParameterMismatchException,
    _ResourceNotReadyException,
    _ResourceNotFoundException,
    _HumanLoopQuotaExceededException,
    _InvalidPaginationTokenException,
    _LimitExceededException,
    _ResourceInUseException,

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
    ageRange_low,
    ageRange_high,

    -- * Asset
    Asset (..),
    newAsset,
    asset_groundTruthManifest,

    -- * AudioMetadata
    AudioMetadata (..),
    newAudioMetadata,
    audioMetadata_codec,
    audioMetadata_sampleRate,
    audioMetadata_numberOfChannels,
    audioMetadata_durationMillis,

    -- * Beard
    Beard (..),
    newBeard,
    beard_value,
    beard_confidence,

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
    boundingBox_width,
    boundingBox_top,

    -- * Celebrity
    Celebrity (..),
    newCelebrity,
    celebrity_matchConfidence,
    celebrity_urls,
    celebrity_knownGender,
    celebrity_name,
    celebrity_id,
    celebrity_face,

    -- * CelebrityDetail
    CelebrityDetail (..),
    newCelebrityDetail,
    celebrityDetail_boundingBox,
    celebrityDetail_urls,
    celebrityDetail_confidence,
    celebrityDetail_name,
    celebrityDetail_id,
    celebrityDetail_face,

    -- * CelebrityRecognition
    CelebrityRecognition (..),
    newCelebrityRecognition,
    celebrityRecognition_celebrity,
    celebrityRecognition_timestamp,

    -- * CompareFacesMatch
    CompareFacesMatch (..),
    newCompareFacesMatch,
    compareFacesMatch_similarity,
    compareFacesMatch_face,

    -- * ComparedFace
    ComparedFace (..),
    newComparedFace,
    comparedFace_boundingBox,
    comparedFace_emotions,
    comparedFace_pose,
    comparedFace_confidence,
    comparedFace_quality,
    comparedFace_smile,
    comparedFace_landmarks,

    -- * ComparedSourceImageFace
    ComparedSourceImageFace (..),
    newComparedSourceImageFace,
    comparedSourceImageFace_boundingBox,
    comparedSourceImageFace_confidence,

    -- * ContentModerationDetection
    ContentModerationDetection (..),
    newContentModerationDetection,
    contentModerationDetection_moderationLabel,
    contentModerationDetection_timestamp,

    -- * CoversBodyPart
    CoversBodyPart (..),
    newCoversBodyPart,
    coversBodyPart_value,
    coversBodyPart_confidence,

    -- * CustomLabel
    CustomLabel (..),
    newCustomLabel,
    customLabel_confidence,
    customLabel_name,
    customLabel_geometry,

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

    -- * Emotion
    Emotion (..),
    newEmotion,
    emotion_confidence,
    emotion_type,

    -- * EquipmentDetection
    EquipmentDetection (..),
    newEquipmentDetection,
    equipmentDetection_boundingBox,
    equipmentDetection_coversBodyPart,
    equipmentDetection_confidence,
    equipmentDetection_type,

    -- * EvaluationResult
    EvaluationResult (..),
    newEvaluationResult,
    evaluationResult_summary,
    evaluationResult_f1Score,

    -- * EyeOpen
    EyeOpen (..),
    newEyeOpen,
    eyeOpen_value,
    eyeOpen_confidence,

    -- * Eyeglasses
    Eyeglasses (..),
    newEyeglasses,
    eyeglasses_value,
    eyeglasses_confidence,

    -- * Face
    Face (..),
    newFace,
    face_faceId,
    face_boundingBox,
    face_externalImageId,
    face_confidence,
    face_imageId,

    -- * FaceDetail
    FaceDetail (..),
    newFaceDetail,
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

    -- * FaceDetection
    FaceDetection (..),
    newFaceDetection,
    faceDetection_timestamp,
    faceDetection_face,

    -- * FaceMatch
    FaceMatch (..),
    newFaceMatch,
    faceMatch_similarity,
    faceMatch_face,

    -- * FaceRecord
    FaceRecord (..),
    newFaceRecord,
    faceRecord_faceDetail,
    faceRecord_face,

    -- * FaceSearchSettings
    FaceSearchSettings (..),
    newFaceSearchSettings,
    faceSearchSettings_faceMatchThreshold,
    faceSearchSettings_collectionId,

    -- * Gender
    Gender (..),
    newGender,
    gender_value,
    gender_confidence,

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
    humanLoopActivationOutput_humanLoopActivationReasons,
    humanLoopActivationOutput_humanLoopArn,
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,

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
    image_s3Object,
    image_bytes,

    -- * ImageQuality
    ImageQuality (..),
    newImageQuality,
    imageQuality_sharpness,
    imageQuality_brightness,

    -- * Instance
    Instance (..),
    newInstance,
    instance_boundingBox,
    instance_confidence,

    -- * KinesisDataStream
    KinesisDataStream (..),
    newKinesisDataStream,
    kinesisDataStream_arn,

    -- * KinesisVideoStream
    KinesisVideoStream (..),
    newKinesisVideoStream,
    kinesisVideoStream_arn,

    -- * KnownGender
    KnownGender (..),
    newKnownGender,
    knownGender_type,

    -- * Label
    Label (..),
    newLabel,
    label_confidence,
    label_parents,
    label_name,
    label_instances,

    -- * LabelDetection
    LabelDetection (..),
    newLabelDetection,
    labelDetection_label,
    labelDetection_timestamp,

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
    mouthOpen_value,
    mouthOpen_confidence,

    -- * Mustache
    Mustache (..),
    newMustache,
    mustache_value,
    mustache_confidence,

    -- * NotificationChannel
    NotificationChannel (..),
    newNotificationChannel,
    notificationChannel_sNSTopicArn,
    notificationChannel_roleArn,

    -- * OutputConfig
    OutputConfig (..),
    newOutputConfig,
    outputConfig_s3KeyPrefix,
    outputConfig_s3Bucket,

    -- * Parent
    Parent (..),
    newParent,
    parent_name,

    -- * PersonDetail
    PersonDetail (..),
    newPersonDetail,
    personDetail_boundingBox,
    personDetail_index,
    personDetail_face,

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
    pose_yaw,
    pose_roll,
    pose_pitch,

    -- * ProjectDescription
    ProjectDescription (..),
    newProjectDescription,
    projectDescription_status,
    projectDescription_creationTimestamp,
    projectDescription_projectArn,

    -- * ProjectVersionDescription
    ProjectVersionDescription (..),
    newProjectVersionDescription,
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

    -- * ProtectiveEquipmentBodyPart
    ProtectiveEquipmentBodyPart (..),
    newProtectiveEquipmentBodyPart,
    protectiveEquipmentBodyPart_equipmentDetections,
    protectiveEquipmentBodyPart_confidence,
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
    protectiveEquipmentSummary_personsWithRequiredEquipment,
    protectiveEquipmentSummary_personsWithoutRequiredEquipment,
    protectiveEquipmentSummary_personsIndeterminate,

    -- * RegionOfInterest
    RegionOfInterest (..),
    newRegionOfInterest,
    regionOfInterest_boundingBox,

    -- * S3Object
    S3Object (..),
    newS3Object,
    s3Object_bucket,
    s3Object_name,
    s3Object_version,

    -- * SegmentDetection
    SegmentDetection (..),
    newSegmentDetection,
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
    smile_value,
    smile_confidence,

    -- * StartSegmentDetectionFilters
    StartSegmentDetectionFilters (..),
    newStartSegmentDetectionFilters,
    startSegmentDetectionFilters_technicalCueFilter,
    startSegmentDetectionFilters_shotFilter,

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

    -- * StreamProcessor
    StreamProcessor (..),
    newStreamProcessor,
    streamProcessor_status,
    streamProcessor_name,

    -- * StreamProcessorInput
    StreamProcessorInput (..),
    newStreamProcessorInput,
    streamProcessorInput_kinesisVideoStream,

    -- * StreamProcessorOutput
    StreamProcessorOutput (..),
    newStreamProcessorOutput,
    streamProcessorOutput_kinesisDataStream,

    -- * StreamProcessorSettings
    StreamProcessorSettings (..),
    newStreamProcessorSettings,
    streamProcessorSettings_faceSearch,

    -- * Summary
    Summary (..),
    newSummary,
    summary_s3Object,

    -- * Sunglasses
    Sunglasses (..),
    newSunglasses,
    sunglasses_value,
    sunglasses_confidence,

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
    textDetection_detectedText,
    textDetection_confidence,
    textDetection_geometry,
    textDetection_id,
    textDetection_type,
    textDetection_parentId,

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
    unindexedFace_reasons,
    unindexedFace_faceDetail,

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
    videoMetadata_frameRate,
    videoMetadata_colorRange,
    videoMetadata_format,
    videoMetadata_codec,
    videoMetadata_frameHeight,
    videoMetadata_durationMillis,
    videoMetadata_frameWidth,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.AgeRange
import Network.AWS.Rekognition.Types.Asset
import Network.AWS.Rekognition.Types.Attribute
import Network.AWS.Rekognition.Types.AudioMetadata
import Network.AWS.Rekognition.Types.Beard
import Network.AWS.Rekognition.Types.BlackFrame
import Network.AWS.Rekognition.Types.BodyPart
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.Celebrity
import Network.AWS.Rekognition.Types.CelebrityDetail
import Network.AWS.Rekognition.Types.CelebrityRecognition
import Network.AWS.Rekognition.Types.CelebrityRecognitionSortBy
import Network.AWS.Rekognition.Types.CompareFacesMatch
import Network.AWS.Rekognition.Types.ComparedFace
import Network.AWS.Rekognition.Types.ComparedSourceImageFace
import Network.AWS.Rekognition.Types.ContentClassifier
import Network.AWS.Rekognition.Types.ContentModerationDetection
import Network.AWS.Rekognition.Types.ContentModerationSortBy
import Network.AWS.Rekognition.Types.CoversBodyPart
import Network.AWS.Rekognition.Types.CustomLabel
import Network.AWS.Rekognition.Types.DetectTextFilters
import Network.AWS.Rekognition.Types.DetectionFilter
import Network.AWS.Rekognition.Types.Emotion
import Network.AWS.Rekognition.Types.EmotionName
import Network.AWS.Rekognition.Types.EquipmentDetection
import Network.AWS.Rekognition.Types.EvaluationResult
import Network.AWS.Rekognition.Types.EyeOpen
import Network.AWS.Rekognition.Types.Eyeglasses
import Network.AWS.Rekognition.Types.Face
import Network.AWS.Rekognition.Types.FaceAttributes
import Network.AWS.Rekognition.Types.FaceDetail
import Network.AWS.Rekognition.Types.FaceDetection
import Network.AWS.Rekognition.Types.FaceMatch
import Network.AWS.Rekognition.Types.FaceRecord
import Network.AWS.Rekognition.Types.FaceSearchSettings
import Network.AWS.Rekognition.Types.FaceSearchSortBy
import Network.AWS.Rekognition.Types.Gender
import Network.AWS.Rekognition.Types.GenderType
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
import Network.AWS.Rekognition.Types.KnownGenderType
import Network.AWS.Rekognition.Types.Label
import Network.AWS.Rekognition.Types.LabelDetection
import Network.AWS.Rekognition.Types.LabelDetectionSortBy
import Network.AWS.Rekognition.Types.Landmark
import Network.AWS.Rekognition.Types.LandmarkType
import Network.AWS.Rekognition.Types.ModerationLabel
import Network.AWS.Rekognition.Types.MouthOpen
import Network.AWS.Rekognition.Types.Mustache
import Network.AWS.Rekognition.Types.NotificationChannel
import Network.AWS.Rekognition.Types.OrientationCorrection
import Network.AWS.Rekognition.Types.OutputConfig
import Network.AWS.Rekognition.Types.Parent
import Network.AWS.Rekognition.Types.PersonDetail
import Network.AWS.Rekognition.Types.PersonDetection
import Network.AWS.Rekognition.Types.PersonMatch
import Network.AWS.Rekognition.Types.PersonTrackingSortBy
import Network.AWS.Rekognition.Types.Point
import Network.AWS.Rekognition.Types.Pose
import Network.AWS.Rekognition.Types.ProjectDescription
import Network.AWS.Rekognition.Types.ProjectStatus
import Network.AWS.Rekognition.Types.ProjectVersionDescription
import Network.AWS.Rekognition.Types.ProjectVersionStatus
import Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart
import Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson
import Network.AWS.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes
import Network.AWS.Rekognition.Types.ProtectiveEquipmentSummary
import Network.AWS.Rekognition.Types.ProtectiveEquipmentType
import Network.AWS.Rekognition.Types.QualityFilter
import Network.AWS.Rekognition.Types.Reason
import Network.AWS.Rekognition.Types.RegionOfInterest
import Network.AWS.Rekognition.Types.S3Object
import Network.AWS.Rekognition.Types.SegmentDetection
import Network.AWS.Rekognition.Types.SegmentType
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
import Network.AWS.Rekognition.Types.StreamProcessorStatus
import Network.AWS.Rekognition.Types.Summary
import Network.AWS.Rekognition.Types.Sunglasses
import Network.AWS.Rekognition.Types.TechnicalCueSegment
import Network.AWS.Rekognition.Types.TechnicalCueType
import Network.AWS.Rekognition.Types.TestingData
import Network.AWS.Rekognition.Types.TestingDataResult
import Network.AWS.Rekognition.Types.TextDetection
import Network.AWS.Rekognition.Types.TextDetectionResult
import Network.AWS.Rekognition.Types.TextTypes
import Network.AWS.Rekognition.Types.TrainingData
import Network.AWS.Rekognition.Types.TrainingDataResult
import Network.AWS.Rekognition.Types.UnindexedFace
import Network.AWS.Rekognition.Types.ValidationData
import Network.AWS.Rekognition.Types.Video
import Network.AWS.Rekognition.Types.VideoColorRange
import Network.AWS.Rekognition.Types.VideoJobStatus
import Network.AWS.Rekognition.Types.VideoMetadata
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-06-27@ of the Amazon Rekognition SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Rekognition",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "rekognition",
      Core._serviceSigningName = "rekognition",
      Core._serviceVersion = "2016-06-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "Rekognition",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You are not authorized to perform the action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The file size or duration of the supplied media is too large. The
-- maximum file size is 10GB. The maximum duration is 6 hours.
_VideoTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_VideoTooLargeException =
  Core._MatchServiceError
    defaultService
    "VideoTooLargeException"

-- | Input parameter violated a constraint. Validate your parameter before
-- calling the API operation again.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The provided image format is not supported.
_InvalidImageFormatException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidImageFormatException =
  Core._MatchServiceError
    defaultService
    "InvalidImageFormatException"

-- | A resource with the specified ID already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | Amazon Rekognition is unable to access the S3 object specified in the
-- request.
_InvalidS3ObjectException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidS3ObjectException =
  Core._MatchServiceError
    defaultService
    "InvalidS3ObjectException"

-- | The number of requests exceeded your throughput limit. If you want to
-- increase this limit, contact Amazon Rekognition.
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | The input image size exceeds the allowed limit. If you are calling
-- DetectProtectiveEquipment, the image size or resolution exceeds the
-- allowed limit. For more information, see Limits in Amazon Rekognition in
-- the Amazon Rekognition Developer Guide.
_ImageTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ImageTooLargeException =
  Core._MatchServiceError
    defaultService
    "ImageTooLargeException"

-- | The size of the collection exceeds the allowed limit. For more
-- information, see Limits in Amazon Rekognition in the Amazon Rekognition
-- Developer Guide.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | Amazon Rekognition is temporarily unable to process the request. Try
-- your call again.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | Amazon Rekognition experienced a service issue. Try your call again.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | A @ClientRequestToken@ input parameter was reused with an operation, but
-- at least one of the other input parameters is different from the
-- previous call to the operation.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | The requested resource isn\'t ready. For example, this exception occurs
-- when you call @DetectCustomLabels@ with a model version that isn\'t
-- deployed.
_ResourceNotReadyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"

-- | The resource specified in the request cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The number of in-progress human reviews you have has exceeded the number
-- allowed.
_HumanLoopQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HumanLoopQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "HumanLoopQuotaExceededException"

-- | Pagination token in the request is not valid.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"

-- | An Amazon Rekognition service limit was exceeded. For example, if you
-- start too many Amazon Rekognition Video jobs concurrently, calls to
-- start operations (@StartLabelDetection@, for example) will raise a
-- @LimitExceededException@ exception (HTTP status code: 400) until the
-- number of concurrently running jobs is below the Amazon Rekognition
-- service limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified resource is already being used.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
