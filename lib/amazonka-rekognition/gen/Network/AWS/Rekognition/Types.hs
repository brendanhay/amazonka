-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types
  ( -- * Service configuration
    rekognitionService,

    -- * Errors

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
    mkAgeRange,
    arLow,
    arHigh,

    -- * Asset
    Asset (..),
    mkAsset,
    aGroundTruthManifest,

    -- * AudioMetadata
    AudioMetadata (..),
    mkAudioMetadata,
    amCodec,
    amSampleRate,
    amNumberOfChannels,
    amDurationMillis,

    -- * Beard
    Beard (..),
    mkBeard,
    bValue,
    bConfidence,

    -- * BoundingBox
    BoundingBox (..),
    mkBoundingBox,
    bbHeight,
    bbLeft,
    bbWidth,
    bbTop,

    -- * Celebrity
    Celebrity (..),
    mkCelebrity,
    cMatchConfidence,
    cURLs,
    cName,
    cId,
    cFace,

    -- * CelebrityDetail
    CelebrityDetail (..),
    mkCelebrityDetail,
    cdBoundingBox,
    cdURLs,
    cdConfidence,
    cdName,
    cdId,
    cdFace,

    -- * CelebrityRecognition
    CelebrityRecognition (..),
    mkCelebrityRecognition,
    crCelebrity,
    crTimestamp,

    -- * CompareFacesMatch
    CompareFacesMatch (..),
    mkCompareFacesMatch,
    cfmSimilarity,
    cfmFace,

    -- * ComparedFace
    ComparedFace (..),
    mkComparedFace,
    cfBoundingBox,
    cfPose,
    cfConfidence,
    cfQuality,
    cfLandmarks,

    -- * ComparedSourceImageFace
    ComparedSourceImageFace (..),
    mkComparedSourceImageFace,
    csifBoundingBox,
    csifConfidence,

    -- * ContentModerationDetection
    ContentModerationDetection (..),
    mkContentModerationDetection,
    cmdModerationLabel,
    cmdTimestamp,

    -- * CoversBodyPart
    CoversBodyPart (..),
    mkCoversBodyPart,
    cbpValue,
    cbpConfidence,

    -- * CustomLabel
    CustomLabel (..),
    mkCustomLabel,
    clConfidence,
    clName,
    clGeometry,

    -- * DetectTextFilters
    DetectTextFilters (..),
    mkDetectTextFilters,
    dtfRegionsOfInterest,
    dtfWordFilter,

    -- * DetectionFilter
    DetectionFilter (..),
    mkDetectionFilter,
    dfMinBoundingBoxHeight,
    dfMinBoundingBoxWidth,
    dfMinConfidence,

    -- * Emotion
    Emotion (..),
    mkEmotion,
    eConfidence,
    eType,

    -- * EquipmentDetection
    EquipmentDetection (..),
    mkEquipmentDetection,
    edBoundingBox,
    edCoversBodyPart,
    edConfidence,
    edType,

    -- * EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
    erSummary,
    erF1Score,

    -- * EyeOpen
    EyeOpen (..),
    mkEyeOpen,
    eoValue,
    eoConfidence,

    -- * Eyeglasses
    Eyeglasses (..),
    mkEyeglasses,
    efValue,
    efConfidence,

    -- * Face
    Face (..),
    mkFace,
    fFaceId,
    fBoundingBox,
    fExternalImageId,
    fConfidence,
    fImageId,

    -- * FaceDetail
    FaceDetail (..),
    mkFaceDetail,
    fdAgeRange,
    fdSunglasses,
    fdMouthOpen,
    fdBoundingBox,
    fdEmotions,
    fdEyesOpen,
    fdPose,
    fdConfidence,
    fdGender,
    fdQuality,
    fdEyeglasses,
    fdBeard,
    fdMustache,
    fdSmile,
    fdLandmarks,

    -- * FaceDetection
    FaceDetection (..),
    mkFaceDetection,
    fdTimestamp,
    fdFace,

    -- * FaceMatch
    FaceMatch (..),
    mkFaceMatch,
    fmSimilarity,
    fmFace,

    -- * FaceRecord
    FaceRecord (..),
    mkFaceRecord,
    frFaceDetail,
    frFace,

    -- * FaceSearchSettings
    FaceSearchSettings (..),
    mkFaceSearchSettings,
    fssFaceMatchThreshold,
    fssCollectionId,

    -- * Gender
    Gender (..),
    mkGender,
    gValue,
    gConfidence,

    -- * Geometry
    Geometry (..),
    mkGeometry,
    gBoundingBox,
    gPolygon,

    -- * GroundTruthManifest
    GroundTruthManifest (..),
    mkGroundTruthManifest,
    gtmS3Object,

    -- * HumanLoopActivationOutput
    HumanLoopActivationOutput (..),
    mkHumanLoopActivationOutput,
    hlaoHumanLoopActivationReasons,
    hlaoHumanLoopARN,
    hlaoHumanLoopActivationConditionsEvaluationResults,

    -- * HumanLoopConfig
    HumanLoopConfig (..),
    mkHumanLoopConfig,
    hlcHumanLoopName,
    hlcDataAttributes,
    hlcFlowDefinitionARN,

    -- * HumanLoopDataAttributes
    HumanLoopDataAttributes (..),
    mkHumanLoopDataAttributes,
    hldaContentClassifiers,

    -- * Image
    Image (..),
    mkImage,
    iS3Object,
    iBytes,

    -- * ImageQuality
    ImageQuality (..),
    mkImageQuality,
    iqSharpness,
    iqBrightness,

    -- * Instance
    Instance (..),
    mkInstance,
    iBoundingBox,
    iConfidence,

    -- * KinesisDataStream
    KinesisDataStream (..),
    mkKinesisDataStream,
    kdsARN,

    -- * KinesisVideoStream
    KinesisVideoStream (..),
    mkKinesisVideoStream,
    kvsARN,

    -- * Label
    Label (..),
    mkLabel,
    lConfidence,
    lParents,
    lName,
    lInstances,

    -- * LabelDetection
    LabelDetection (..),
    mkLabelDetection,
    ldLabel,
    ldTimestamp,

    -- * Landmark
    Landmark (..),
    mkLandmark,
    lType,
    lX,
    lY,

    -- * ModerationLabel
    ModerationLabel (..),
    mkModerationLabel,
    mlConfidence,
    mlName,
    mlParentName,

    -- * MouthOpen
    MouthOpen (..),
    mkMouthOpen,
    moValue,
    moConfidence,

    -- * Mustache
    Mustache (..),
    mkMustache,
    mValue,
    mConfidence,

    -- * NotificationChannel
    NotificationChannel (..),
    mkNotificationChannel,
    ncSNSTopicARN,
    ncRoleARN,

    -- * OutputConfig
    OutputConfig (..),
    mkOutputConfig,
    ocS3KeyPrefix,
    ocS3Bucket,

    -- * Parent
    Parent (..),
    mkParent,
    pName,

    -- * PersonDetail
    PersonDetail (..),
    mkPersonDetail,
    pdBoundingBox,
    pdIndex,
    pdFace,

    -- * PersonDetection
    PersonDetection (..),
    mkPersonDetection,
    pdPerson,
    pdTimestamp,

    -- * PersonMatch
    PersonMatch (..),
    mkPersonMatch,
    pmFaceMatches,
    pmPerson,
    pmTimestamp,

    -- * Point
    Point (..),
    mkPoint,
    pX,
    pY,

    -- * Pose
    Pose (..),
    mkPose,
    pYaw,
    pRoll,
    pPitch,

    -- * ProjectDescription
    ProjectDescription (..),
    mkProjectDescription,
    pdStatus,
    pdCreationTimestamp,
    pdProjectARN,

    -- * ProjectVersionDescription
    ProjectVersionDescription (..),
    mkProjectVersionDescription,
    pvdMinInferenceUnits,
    pvdStatus,
    pvdEvaluationResult,
    pvdManifestSummary,
    pvdTestingDataResult,
    pvdStatusMessage,
    pvdCreationTimestamp,
    pvdProjectVersionARN,
    pvdOutputConfig,
    pvdBillableTrainingTimeInSeconds,
    pvdTrainingEndTimestamp,
    pvdTrainingDataResult,

    -- * ProtectiveEquipmentBodyPart
    ProtectiveEquipmentBodyPart (..),
    mkProtectiveEquipmentBodyPart,
    pebpEquipmentDetections,
    pebpConfidence,
    pebpName,

    -- * ProtectiveEquipmentPerson
    ProtectiveEquipmentPerson (..),
    mkProtectiveEquipmentPerson,
    pepBodyParts,
    pepBoundingBox,
    pepConfidence,
    pepId,

    -- * ProtectiveEquipmentSummarizationAttributes
    ProtectiveEquipmentSummarizationAttributes (..),
    mkProtectiveEquipmentSummarizationAttributes,
    pesaRequiredEquipmentTypes,
    pesaMinConfidence,

    -- * ProtectiveEquipmentSummary
    ProtectiveEquipmentSummary (..),
    mkProtectiveEquipmentSummary,
    pesPersonsWithRequiredEquipment,
    pesPersonsWithoutRequiredEquipment,
    pesPersonsIndeterminate,

    -- * RegionOfInterest
    RegionOfInterest (..),
    mkRegionOfInterest,
    roiBoundingBox,

    -- * S3Object
    S3Object (..),
    mkS3Object,
    soBucket,
    soName,
    soVersion,

    -- * SegmentDetection
    SegmentDetection (..),
    mkSegmentDetection,
    sdTechnicalCueSegment,
    sdDurationSMPTE,
    sdEndTimestampMillis,
    sdStartTimecodeSMPTE,
    sdEndTimecodeSMPTE,
    sdDurationMillis,
    sdStartTimestampMillis,
    sdType,
    sdShotSegment,

    -- * SegmentTypeInfo
    SegmentTypeInfo (..),
    mkSegmentTypeInfo,
    stiModelVersion,
    stiType,

    -- * ShotSegment
    ShotSegment (..),
    mkShotSegment,
    ssConfidence,
    ssIndex,

    -- * Smile
    Smile (..),
    mkSmile,
    sfValue,
    sfConfidence,

    -- * StartSegmentDetectionFilters
    StartSegmentDetectionFilters (..),
    mkStartSegmentDetectionFilters,
    ssdfTechnicalCueFilter,
    ssdfShotFilter,

    -- * StartShotDetectionFilter
    StartShotDetectionFilter (..),
    mkStartShotDetectionFilter,
    ssdfMinSegmentConfidence,

    -- * StartTechnicalCueDetectionFilter
    StartTechnicalCueDetectionFilter (..),
    mkStartTechnicalCueDetectionFilter,
    stcdfMinSegmentConfidence,

    -- * StartTextDetectionFilters
    StartTextDetectionFilters (..),
    mkStartTextDetectionFilters,
    stdfRegionsOfInterest,
    stdfWordFilter,

    -- * StreamProcessor
    StreamProcessor (..),
    mkStreamProcessor,
    spStatus,
    spName,

    -- * StreamProcessorInput
    StreamProcessorInput (..),
    mkStreamProcessorInput,
    spiKinesisVideoStream,

    -- * StreamProcessorOutput
    StreamProcessorOutput (..),
    mkStreamProcessorOutput,
    spoKinesisDataStream,

    -- * StreamProcessorSettings
    StreamProcessorSettings (..),
    mkStreamProcessorSettings,
    spsFaceSearch,

    -- * Summary
    Summary (..),
    mkSummary,
    sS3Object,

    -- * Sunglasses
    Sunglasses (..),
    mkSunglasses,
    sValue,
    sConfidence,

    -- * TechnicalCueSegment
    TechnicalCueSegment (..),
    mkTechnicalCueSegment,
    tcsConfidence,
    tcsType,

    -- * TestingData
    TestingData (..),
    mkTestingData,
    tdAssets,
    tdAutoCreate,

    -- * TestingDataResult
    TestingDataResult (..),
    mkTestingDataResult,
    tdrInput,
    tdrOutput,
    tdrValidation,

    -- * TextDetection
    TextDetection (..),
    mkTextDetection,
    tdDetectedText,
    tdConfidence,
    tdGeometry,
    tdId,
    tdType,
    tdParentId,

    -- * TextDetectionResult
    TextDetectionResult (..),
    mkTextDetectionResult,
    tdrTextDetection,
    tdrTimestamp,

    -- * TrainingData
    TrainingData (..),
    mkTrainingData,
    tAssets,

    -- * TrainingDataResult
    TrainingDataResult (..),
    mkTrainingDataResult,
    tInput,
    tOutput,
    tValidation,

    -- * UnindexedFace
    UnindexedFace (..),
    mkUnindexedFace,
    ufReasons,
    ufFaceDetail,

    -- * ValidationData
    ValidationData (..),
    mkValidationData,
    vdAssets,

    -- * Video
    Video (..),
    mkVideo,
    vS3Object,

    -- * VideoMetadata
    VideoMetadata (..),
    mkVideoMetadata,
    vmFrameRate,
    vmFormat,
    vmCodec,
    vmFrameHeight,
    vmDurationMillis,
    vmFrameWidth,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
rekognitionService :: Lude.Service
rekognitionService =
  Lude.Service
    { Lude._svcAbbrev = "Rekognition",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "rekognition",
      Lude._svcVersion = "2016-06-27",
      Lude._svcEndpoint = Lude.defaultEndpoint rekognitionService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Rekognition",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
