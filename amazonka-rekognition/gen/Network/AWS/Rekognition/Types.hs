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
    _ServiceQuotaExceededException,
    _VideoTooLargeException,
    _InvalidPaginationTokenException,
    _HumanLoopQuotaExceededException,
    _ImageTooLargeException,
    _ResourceNotReadyException,
    _InvalidS3ObjectException,
    _ResourceAlreadyExistsException,
    _ThrottlingException,
    _InternalServerError,
    _InvalidParameterException,
    _AccessDeniedException,
    _ResourceInUseException,
    _LimitExceededException,
    _ProvisionedThroughputExceededException,
    _ResourceNotFoundException,
    _IdempotentParameterMismatchException,
    _InvalidImageFormatException,

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
    audioMetadata_sampleRate,
    audioMetadata_durationMillis,
    audioMetadata_numberOfChannels,

    -- * Beard
    Beard (..),
    newBeard,
    beard_confidence,
    beard_value,

    -- * BoundingBox
    BoundingBox (..),
    newBoundingBox,
    boundingBox_height,
    boundingBox_width,
    boundingBox_top,
    boundingBox_left,

    -- * Celebrity
    Celebrity (..),
    newCelebrity,
    celebrity_urls,
    celebrity_id,
    celebrity_matchConfidence,
    celebrity_name,
    celebrity_face,

    -- * CelebrityDetail
    CelebrityDetail (..),
    newCelebrityDetail,
    celebrityDetail_urls,
    celebrityDetail_id,
    celebrityDetail_boundingBox,
    celebrityDetail_name,
    celebrityDetail_confidence,
    celebrityDetail_face,

    -- * CelebrityRecognition
    CelebrityRecognition (..),
    newCelebrityRecognition,
    celebrityRecognition_timestamp,
    celebrityRecognition_celebrity,

    -- * CompareFacesMatch
    CompareFacesMatch (..),
    newCompareFacesMatch,
    compareFacesMatch_similarity,
    compareFacesMatch_face,

    -- * ComparedFace
    ComparedFace (..),
    newComparedFace,
    comparedFace_pose,
    comparedFace_landmarks,
    comparedFace_boundingBox,
    comparedFace_confidence,
    comparedFace_quality,

    -- * ComparedSourceImageFace
    ComparedSourceImageFace (..),
    newComparedSourceImageFace,
    comparedSourceImageFace_boundingBox,
    comparedSourceImageFace_confidence,

    -- * ContentModerationDetection
    ContentModerationDetection (..),
    newContentModerationDetection,
    contentModerationDetection_timestamp,
    contentModerationDetection_moderationLabel,

    -- * CoversBodyPart
    CoversBodyPart (..),
    newCoversBodyPart,
    coversBodyPart_confidence,
    coversBodyPart_value,

    -- * CustomLabel
    CustomLabel (..),
    newCustomLabel,
    customLabel_name,
    customLabel_confidence,
    customLabel_geometry,

    -- * DetectTextFilters
    DetectTextFilters (..),
    newDetectTextFilters,
    detectTextFilters_regionsOfInterest,
    detectTextFilters_wordFilter,

    -- * DetectionFilter
    DetectionFilter (..),
    newDetectionFilter,
    detectionFilter_minBoundingBoxWidth,
    detectionFilter_minConfidence,
    detectionFilter_minBoundingBoxHeight,

    -- * Emotion
    Emotion (..),
    newEmotion,
    emotion_confidence,
    emotion_type,

    -- * EquipmentDetection
    EquipmentDetection (..),
    newEquipmentDetection,
    equipmentDetection_coversBodyPart,
    equipmentDetection_boundingBox,
    equipmentDetection_confidence,
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
    face_faceId,
    face_imageId,
    face_externalImageId,
    face_boundingBox,
    face_confidence,

    -- * FaceDetail
    FaceDetail (..),
    newFaceDetail,
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

    -- * FaceDetection
    FaceDetection (..),
    newFaceDetection,
    faceDetection_face,
    faceDetection_timestamp,

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
    faceSearchSettings_collectionId,
    faceSearchSettings_faceMatchThreshold,

    -- * Gender
    Gender (..),
    newGender,
    gender_confidence,
    gender_value,

    -- * Geometry
    Geometry (..),
    newGeometry,
    geometry_polygon,
    geometry_boundingBox,

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

    -- * KinesisDataStream
    KinesisDataStream (..),
    newKinesisDataStream,
    kinesisDataStream_arn,

    -- * KinesisVideoStream
    KinesisVideoStream (..),
    newKinesisVideoStream,
    kinesisVideoStream_arn,

    -- * Label
    Label (..),
    newLabel,
    label_parents,
    label_instances,
    label_name,
    label_confidence,

    -- * LabelDetection
    LabelDetection (..),
    newLabelDetection,
    labelDetection_label,
    labelDetection_timestamp,

    -- * Landmark
    Landmark (..),
    newLandmark,
    landmark_y,
    landmark_x,
    landmark_type,

    -- * ModerationLabel
    ModerationLabel (..),
    newModerationLabel,
    moderationLabel_name,
    moderationLabel_confidence,
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
    personDetection_timestamp,
    personDetection_person,

    -- * PersonMatch
    PersonMatch (..),
    newPersonMatch,
    personMatch_faceMatches,
    personMatch_timestamp,
    personMatch_person,

    -- * Point
    Point (..),
    newPoint,
    point_y,
    point_x,

    -- * Pose
    Pose (..),
    newPose,
    pose_yaw,
    pose_pitch,
    pose_roll,

    -- * ProjectDescription
    ProjectDescription (..),
    newProjectDescription,
    projectDescription_creationTimestamp,
    projectDescription_status,
    projectDescription_projectArn,

    -- * ProjectVersionDescription
    ProjectVersionDescription (..),
    newProjectVersionDescription,
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

    -- * ProtectiveEquipmentBodyPart
    ProtectiveEquipmentBodyPart (..),
    newProtectiveEquipmentBodyPart,
    protectiveEquipmentBodyPart_equipmentDetections,
    protectiveEquipmentBodyPart_name,
    protectiveEquipmentBodyPart_confidence,

    -- * ProtectiveEquipmentPerson
    ProtectiveEquipmentPerson (..),
    newProtectiveEquipmentPerson,
    protectiveEquipmentPerson_id,
    protectiveEquipmentPerson_boundingBox,
    protectiveEquipmentPerson_bodyParts,
    protectiveEquipmentPerson_confidence,

    -- * ProtectiveEquipmentSummarizationAttributes
    ProtectiveEquipmentSummarizationAttributes (..),
    newProtectiveEquipmentSummarizationAttributes,
    protectiveEquipmentSummarizationAttributes_minConfidence,
    protectiveEquipmentSummarizationAttributes_requiredEquipmentTypes,

    -- * ProtectiveEquipmentSummary
    ProtectiveEquipmentSummary (..),
    newProtectiveEquipmentSummary,
    protectiveEquipmentSummary_personsWithRequiredEquipment,
    protectiveEquipmentSummary_personsIndeterminate,
    protectiveEquipmentSummary_personsWithoutRequiredEquipment,

    -- * RegionOfInterest
    RegionOfInterest (..),
    newRegionOfInterest,
    regionOfInterest_boundingBox,

    -- * S3Object
    S3Object (..),
    newS3Object,
    s3Object_version,
    s3Object_name,
    s3Object_bucket,

    -- * SegmentDetection
    SegmentDetection (..),
    newSegmentDetection,
    segmentDetection_shotSegment,
    segmentDetection_endTimestampMillis,
    segmentDetection_startTimecodeSMPTE,
    segmentDetection_durationSMPTE,
    segmentDetection_technicalCueSegment,
    segmentDetection_type,
    segmentDetection_durationMillis,
    segmentDetection_endTimecodeSMPTE,
    segmentDetection_startTimestampMillis,

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
    startSegmentDetectionFilters_technicalCueFilter,
    startSegmentDetectionFilters_shotFilter,

    -- * StartShotDetectionFilter
    StartShotDetectionFilter (..),
    newStartShotDetectionFilter,
    startShotDetectionFilter_minSegmentConfidence,

    -- * StartTechnicalCueDetectionFilter
    StartTechnicalCueDetectionFilter (..),
    newStartTechnicalCueDetectionFilter,
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
    testingData_autoCreate,
    testingData_assets,

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
    textDetection_id,
    textDetection_confidence,
    textDetection_parentId,
    textDetection_type,
    textDetection_geometry,

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
    videoMetadata_format,
    videoMetadata_frameHeight,
    videoMetadata_frameRate,
    videoMetadata_frameWidth,
    videoMetadata_durationMillis,
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The size of the collection exceeds the allowed limit. For more
-- information, see Limits in Amazon Rekognition in the Amazon Rekognition
-- Developer Guide.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The file size or duration of the supplied media is too large. The
-- maximum file size is 10GB. The maximum duration is 6 hours.
_VideoTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_VideoTooLargeException =
  Core._MatchServiceError
    defaultService
    "VideoTooLargeException"

-- | Pagination token in the request is not valid.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"

-- | The number of in-progress human reviews you have has exceeded the number
-- allowed.
_HumanLoopQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HumanLoopQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "HumanLoopQuotaExceededException"

-- | The input image size exceeds the allowed limit. For more information,
-- see Limits in Amazon Rekognition in the Amazon Rekognition Developer
-- Guide.
_ImageTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ImageTooLargeException =
  Core._MatchServiceError
    defaultService
    "ImageTooLargeException"

-- | The requested resource isn\'t ready. For example, this exception occurs
-- when you call @DetectCustomLabels@ with a model version that isn\'t
-- deployed.
_ResourceNotReadyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"

-- | Amazon Rekognition is unable to access the S3 object specified in the
-- request.
_InvalidS3ObjectException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidS3ObjectException =
  Core._MatchServiceError
    defaultService
    "InvalidS3ObjectException"

-- | A collection with the specified ID already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

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

-- | Input parameter violated a constraint. Validate your parameter before
-- calling the API operation again.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | You are not authorized to perform the action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The specified resource is already being used.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

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

-- | The number of requests exceeded your throughput limit. If you want to
-- increase this limit, contact Amazon Rekognition.
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | The collection specified in the request cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | A @ClientRequestToken@ input parameter was reused with an operation, but
-- at least one of the other input parameters is different from the
-- previous call to the operation.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | The provided image format is not supported.
_InvalidImageFormatException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidImageFormatException =
  Core._MatchServiceError
    defaultService
    "InvalidImageFormatException"
