{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types
  ( -- * Service Configuration
    rekognition,

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
    AgeRange,
    ageRange,
    arLow,
    arHigh,

    -- * Asset
    Asset,
    asset,
    aGroundTruthManifest,

    -- * AudioMetadata
    AudioMetadata,
    audioMetadata,
    amCodec,
    amSampleRate,
    amNumberOfChannels,
    amDurationMillis,

    -- * Beard
    Beard,
    beard,
    bValue,
    bConfidence,

    -- * BoundingBox
    BoundingBox,
    boundingBox,
    bbHeight,
    bbLeft,
    bbWidth,
    bbTop,

    -- * Celebrity
    Celebrity,
    celebrity,
    cMatchConfidence,
    cURLs,
    cName,
    cId,
    cFace,

    -- * CelebrityDetail
    CelebrityDetail,
    celebrityDetail,
    cdBoundingBox,
    cdURLs,
    cdConfidence,
    cdName,
    cdId,
    cdFace,

    -- * CelebrityRecognition
    CelebrityRecognition,
    celebrityRecognition,
    crCelebrity,
    crTimestamp,

    -- * CompareFacesMatch
    CompareFacesMatch,
    compareFacesMatch,
    cfmSimilarity,
    cfmFace,

    -- * ComparedFace
    ComparedFace,
    comparedFace,
    cfBoundingBox,
    cfPose,
    cfConfidence,
    cfQuality,
    cfLandmarks,

    -- * ComparedSourceImageFace
    ComparedSourceImageFace,
    comparedSourceImageFace,
    csifBoundingBox,
    csifConfidence,

    -- * ContentModerationDetection
    ContentModerationDetection,
    contentModerationDetection,
    cmdModerationLabel,
    cmdTimestamp,

    -- * CoversBodyPart
    CoversBodyPart,
    coversBodyPart,
    cbpValue,
    cbpConfidence,

    -- * CustomLabel
    CustomLabel,
    customLabel,
    clConfidence,
    clName,
    clGeometry,

    -- * DetectTextFilters
    DetectTextFilters,
    detectTextFilters,
    dtfRegionsOfInterest,
    dtfWordFilter,

    -- * DetectionFilter
    DetectionFilter,
    detectionFilter,
    dfMinBoundingBoxHeight,
    dfMinBoundingBoxWidth,
    dfMinConfidence,

    -- * Emotion
    Emotion,
    emotion,
    eConfidence,
    eType,

    -- * EquipmentDetection
    EquipmentDetection,
    equipmentDetection,
    edBoundingBox,
    edCoversBodyPart,
    edConfidence,
    edType,

    -- * EvaluationResult
    EvaluationResult,
    evaluationResult,
    erSummary,
    erF1Score,

    -- * EyeOpen
    EyeOpen,
    eyeOpen,
    eoValue,
    eoConfidence,

    -- * Eyeglasses
    Eyeglasses,
    eyeglasses,
    eyeValue,
    eyeConfidence,

    -- * Face
    Face,
    face,
    fFaceId,
    fBoundingBox,
    fExternalImageId,
    fConfidence,
    fImageId,

    -- * FaceDetail
    FaceDetail,
    faceDetail,
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
    FaceDetection,
    faceDetection,
    fdTimestamp,
    fdFace,

    -- * FaceMatch
    FaceMatch,
    faceMatch,
    fmSimilarity,
    fmFace,

    -- * FaceRecord
    FaceRecord,
    faceRecord,
    frFaceDetail,
    frFace,

    -- * FaceSearchSettings
    FaceSearchSettings,
    faceSearchSettings,
    fssFaceMatchThreshold,
    fssCollectionId,

    -- * Gender
    Gender,
    gender,
    gValue,
    gConfidence,

    -- * Geometry
    Geometry,
    geometry,
    gBoundingBox,
    gPolygon,

    -- * GroundTruthManifest
    GroundTruthManifest,
    groundTruthManifest,
    gtmS3Object,

    -- * HumanLoopActivationOutput
    HumanLoopActivationOutput,
    humanLoopActivationOutput,
    hlaoHumanLoopActivationReasons,
    hlaoHumanLoopARN,
    hlaoHumanLoopActivationConditionsEvaluationResults,

    -- * HumanLoopConfig
    HumanLoopConfig,
    humanLoopConfig,
    hlcDataAttributes,
    hlcHumanLoopName,
    hlcFlowDefinitionARN,

    -- * HumanLoopDataAttributes
    HumanLoopDataAttributes,
    humanLoopDataAttributes,
    hldaContentClassifiers,

    -- * Image
    Image,
    image,
    iS3Object,
    iBytes,

    -- * ImageQuality
    ImageQuality,
    imageQuality,
    iqSharpness,
    iqBrightness,

    -- * Instance
    Instance,
    instance',
    iBoundingBox,
    iConfidence,

    -- * KinesisDataStream
    KinesisDataStream,
    kinesisDataStream,
    kdsARN,

    -- * KinesisVideoStream
    KinesisVideoStream,
    kinesisVideoStream,
    kvsARN,

    -- * Label
    Label,
    label,
    lConfidence,
    lParents,
    lName,
    lInstances,

    -- * LabelDetection
    LabelDetection,
    labelDetection,
    ldLabel,
    ldTimestamp,

    -- * Landmark
    Landmark,
    landmark,
    lType,
    lX,
    lY,

    -- * ModerationLabel
    ModerationLabel,
    moderationLabel,
    mlConfidence,
    mlName,
    mlParentName,

    -- * MouthOpen
    MouthOpen,
    mouthOpen,
    moValue,
    moConfidence,

    -- * Mustache
    Mustache,
    mustache,
    mValue,
    mConfidence,

    -- * NotificationChannel
    NotificationChannel,
    notificationChannel,
    ncSNSTopicARN,
    ncRoleARN,

    -- * OutputConfig
    OutputConfig,
    outputConfig,
    ocS3KeyPrefix,
    ocS3Bucket,

    -- * Parent
    Parent,
    parent,
    pName,

    -- * PersonDetail
    PersonDetail,
    personDetail,
    pdBoundingBox,
    pdIndex,
    pdFace,

    -- * PersonDetection
    PersonDetection,
    personDetection,
    pdPerson,
    pdTimestamp,

    -- * PersonMatch
    PersonMatch,
    personMatch,
    pmFaceMatches,
    pmPerson,
    pmTimestamp,

    -- * Point
    Point,
    point,
    pX,
    pY,

    -- * Pose
    Pose,
    pose,
    pYaw,
    pRoll,
    pPitch,

    -- * ProjectDescription
    ProjectDescription,
    projectDescription,
    pdStatus,
    pdCreationTimestamp,
    pdProjectARN,

    -- * ProjectVersionDescription
    ProjectVersionDescription,
    projectVersionDescription,
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
    ProtectiveEquipmentBodyPart,
    protectiveEquipmentBodyPart,
    pebpEquipmentDetections,
    pebpConfidence,
    pebpName,

    -- * ProtectiveEquipmentPerson
    ProtectiveEquipmentPerson,
    protectiveEquipmentPerson,
    pepBodyParts,
    pepBoundingBox,
    pepConfidence,
    pepId,

    -- * ProtectiveEquipmentSummarizationAttributes
    ProtectiveEquipmentSummarizationAttributes,
    protectiveEquipmentSummarizationAttributes,
    pesaMinConfidence,
    pesaRequiredEquipmentTypes,

    -- * ProtectiveEquipmentSummary
    ProtectiveEquipmentSummary,
    protectiveEquipmentSummary,
    pesPersonsWithRequiredEquipment,
    pesPersonsWithoutRequiredEquipment,
    pesPersonsIndeterminate,

    -- * RegionOfInterest
    RegionOfInterest,
    regionOfInterest,
    roiBoundingBox,

    -- * S3Object
    S3Object,
    s3Object,
    soBucket,
    soName,
    soVersion,

    -- * SegmentDetection
    SegmentDetection,
    segmentDetection,
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
    SegmentTypeInfo,
    segmentTypeInfo,
    stiModelVersion,
    stiType,

    -- * ShotSegment
    ShotSegment,
    shotSegment,
    ssConfidence,
    ssIndex,

    -- * Smile
    Smile,
    smile,
    smiValue,
    smiConfidence,

    -- * StartSegmentDetectionFilters
    StartSegmentDetectionFilters,
    startSegmentDetectionFilters,
    ssdfTechnicalCueFilter,
    ssdfShotFilter,

    -- * StartShotDetectionFilter
    StartShotDetectionFilter,
    startShotDetectionFilter,
    ssdfMinSegmentConfidence,

    -- * StartTechnicalCueDetectionFilter
    StartTechnicalCueDetectionFilter,
    startTechnicalCueDetectionFilter,
    stcdfMinSegmentConfidence,

    -- * StartTextDetectionFilters
    StartTextDetectionFilters,
    startTextDetectionFilters,
    stdfRegionsOfInterest,
    stdfWordFilter,

    -- * StreamProcessor
    StreamProcessor,
    streamProcessor,
    spStatus,
    spName,

    -- * StreamProcessorInput
    StreamProcessorInput,
    streamProcessorInput,
    spiKinesisVideoStream,

    -- * StreamProcessorOutput
    StreamProcessorOutput,
    streamProcessorOutput,
    spoKinesisDataStream,

    -- * StreamProcessorSettings
    StreamProcessorSettings,
    streamProcessorSettings,
    spsFaceSearch,

    -- * Summary
    Summary,
    summary,
    sS3Object,

    -- * Sunglasses
    Sunglasses,
    sunglasses,
    sValue,
    sConfidence,

    -- * TechnicalCueSegment
    TechnicalCueSegment,
    technicalCueSegment,
    tcsConfidence,
    tcsType,

    -- * TestingData
    TestingData,
    testingData,
    tdAssets,
    tdAutoCreate,

    -- * TestingDataResult
    TestingDataResult,
    testingDataResult,
    tdrInput,
    tdrOutput,
    tdrValidation,

    -- * TextDetection
    TextDetection,
    textDetection,
    tdDetectedText,
    tdConfidence,
    tdGeometry,
    tdId,
    tdType,
    tdParentId,

    -- * TextDetectionResult
    TextDetectionResult,
    textDetectionResult,
    tdrTextDetection,
    tdrTimestamp,

    -- * TrainingData
    TrainingData,
    trainingData,
    tAssets,

    -- * TrainingDataResult
    TrainingDataResult,
    trainingDataResult,
    tInput,
    tOutput,
    tValidation,

    -- * UnindexedFace
    UnindexedFace,
    unindexedFace,
    ufReasons,
    ufFaceDetail,

    -- * ValidationData
    ValidationData,
    validationData,
    vdAssets,

    -- * Video
    Video,
    video,
    vS3Object,

    -- * VideoMetadata
    VideoMetadata,
    videoMetadata,
    vmFrameRate,
    vmFormat,
    vmCodec,
    vmFrameHeight,
    vmDurationMillis,
    vmFrameWidth,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.Sign.V4

-- | API version @2016-06-27@ of the Amazon Rekognition SDK configuration.
rekognition :: Service
rekognition =
  Service
    { _svcAbbrev = "Rekognition",
      _svcSigner = v4,
      _svcPrefix = "rekognition",
      _svcVersion = "2016-06-27",
      _svcEndpoint = defaultEndpoint rekognition,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Rekognition",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
