{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the Amazon Rekognition API reference.
module Network.AWS.Rekognition
  ( -- * Service configuration
    rekognitionService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** ProjectVersionRunning
    mkProjectVersionRunning,

    -- ** ProjectVersionTrainingCompleted
    mkProjectVersionTrainingCompleted,

    -- * Operations
    -- $operations

    -- ** DetectProtectiveEquipment
    module Network.AWS.Rekognition.DetectProtectiveEquipment,

    -- ** DeleteProject
    module Network.AWS.Rekognition.DeleteProject,

    -- ** StartCelebrityRecognition
    module Network.AWS.Rekognition.StartCelebrityRecognition,

    -- ** GetPersonTracking
    module Network.AWS.Rekognition.GetPersonTracking,

    -- ** GetTextDetection
    module Network.AWS.Rekognition.GetTextDetection,

    -- ** StartSegmentDetection
    module Network.AWS.Rekognition.StartSegmentDetection,

    -- ** ListCollections (Paginated)
    module Network.AWS.Rekognition.ListCollections,

    -- ** StartProjectVersion
    module Network.AWS.Rekognition.StartProjectVersion,

    -- ** DeleteCollection
    module Network.AWS.Rekognition.DeleteCollection,

    -- ** CreateCollection
    module Network.AWS.Rekognition.CreateCollection,

    -- ** StopStreamProcessor
    module Network.AWS.Rekognition.StopStreamProcessor,

    -- ** DetectLabels
    module Network.AWS.Rekognition.DetectLabels,

    -- ** StartContentModeration
    module Network.AWS.Rekognition.StartContentModeration,

    -- ** SearchFacesByImage
    module Network.AWS.Rekognition.SearchFacesByImage,

    -- ** ListStreamProcessors (Paginated)
    module Network.AWS.Rekognition.ListStreamProcessors,

    -- ** DescribeCollection
    module Network.AWS.Rekognition.DescribeCollection,

    -- ** DeleteProjectVersion
    module Network.AWS.Rekognition.DeleteProjectVersion,

    -- ** DescribeProjectVersions (Paginated)
    module Network.AWS.Rekognition.DescribeProjectVersions,

    -- ** RecognizeCelebrities
    module Network.AWS.Rekognition.RecognizeCelebrities,

    -- ** DetectCustomLabels
    module Network.AWS.Rekognition.DetectCustomLabels,

    -- ** GetFaceSearch
    module Network.AWS.Rekognition.GetFaceSearch,

    -- ** StartLabelDetection
    module Network.AWS.Rekognition.StartLabelDetection,

    -- ** SearchFaces
    module Network.AWS.Rekognition.SearchFaces,

    -- ** IndexFaces
    module Network.AWS.Rekognition.IndexFaces,

    -- ** GetLabelDetection
    module Network.AWS.Rekognition.GetLabelDetection,

    -- ** StopProjectVersion
    module Network.AWS.Rekognition.StopProjectVersion,

    -- ** DescribeStreamProcessor
    module Network.AWS.Rekognition.DescribeStreamProcessor,

    -- ** StartFaceSearch
    module Network.AWS.Rekognition.StartFaceSearch,

    -- ** StartTextDetection
    module Network.AWS.Rekognition.StartTextDetection,

    -- ** StartPersonTracking
    module Network.AWS.Rekognition.StartPersonTracking,

    -- ** GetCelebrityRecognition
    module Network.AWS.Rekognition.GetCelebrityRecognition,

    -- ** StartStreamProcessor
    module Network.AWS.Rekognition.StartStreamProcessor,

    -- ** DetectText
    module Network.AWS.Rekognition.DetectText,

    -- ** GetSegmentDetection
    module Network.AWS.Rekognition.GetSegmentDetection,

    -- ** CompareFaces
    module Network.AWS.Rekognition.CompareFaces,

    -- ** DetectFaces
    module Network.AWS.Rekognition.DetectFaces,

    -- ** GetFaceDetection
    module Network.AWS.Rekognition.GetFaceDetection,

    -- ** ListFaces (Paginated)
    module Network.AWS.Rekognition.ListFaces,

    -- ** CreateProjectVersion
    module Network.AWS.Rekognition.CreateProjectVersion,

    -- ** DescribeProjects (Paginated)
    module Network.AWS.Rekognition.DescribeProjects,

    -- ** GetContentModeration
    module Network.AWS.Rekognition.GetContentModeration,

    -- ** DeleteFaces
    module Network.AWS.Rekognition.DeleteFaces,

    -- ** GetCelebrityInfo
    module Network.AWS.Rekognition.GetCelebrityInfo,

    -- ** DeleteStreamProcessor
    module Network.AWS.Rekognition.DeleteStreamProcessor,

    -- ** DetectModerationLabels
    module Network.AWS.Rekognition.DetectModerationLabels,

    -- ** CreateStreamProcessor
    module Network.AWS.Rekognition.CreateStreamProcessor,

    -- ** StartFaceDetection
    module Network.AWS.Rekognition.StartFaceDetection,

    -- ** CreateProject
    module Network.AWS.Rekognition.CreateProject,

    -- * Types

    -- ** Attribute
    Attribute (..),

    -- ** BodyPart
    BodyPart (..),

    -- ** CelebrityRecognitionSortBy
    CelebrityRecognitionSortBy (..),

    -- ** ContentClassifier
    ContentClassifier (..),

    -- ** ContentModerationSortBy
    ContentModerationSortBy (..),

    -- ** EmotionName
    EmotionName (..),

    -- ** FaceAttributes
    FaceAttributes (..),

    -- ** FaceSearchSortBy
    FaceSearchSortBy (..),

    -- ** GenderType
    GenderType (..),

    -- ** LabelDetectionSortBy
    LabelDetectionSortBy (..),

    -- ** LandmarkType
    LandmarkType (..),

    -- ** OrientationCorrection
    OrientationCorrection (..),

    -- ** PersonTrackingSortBy
    PersonTrackingSortBy (..),

    -- ** ProjectStatus
    ProjectStatus (..),

    -- ** ProjectVersionStatus
    ProjectVersionStatus (..),

    -- ** ProtectiveEquipmentType
    ProtectiveEquipmentType (..),

    -- ** QualityFilter
    QualityFilter (..),

    -- ** Reason
    Reason (..),

    -- ** SegmentType
    SegmentType (..),

    -- ** StreamProcessorStatus
    StreamProcessorStatus (..),

    -- ** TechnicalCueType
    TechnicalCueType (..),

    -- ** TextTypes
    TextTypes (..),

    -- ** VideoJobStatus
    VideoJobStatus (..),

    -- ** AgeRange
    AgeRange (..),
    mkAgeRange,
    arLow,
    arHigh,

    -- ** Asset
    Asset (..),
    mkAsset,
    aGroundTruthManifest,

    -- ** AudioMetadata
    AudioMetadata (..),
    mkAudioMetadata,
    amCodec,
    amSampleRate,
    amNumberOfChannels,
    amDurationMillis,

    -- ** Beard
    Beard (..),
    mkBeard,
    bValue,
    bConfidence,

    -- ** BoundingBox
    BoundingBox (..),
    mkBoundingBox,
    bbHeight,
    bbLeft,
    bbWidth,
    bbTop,

    -- ** Celebrity
    Celebrity (..),
    mkCelebrity,
    cMatchConfidence,
    cURLs,
    cName,
    cId,
    cFace,

    -- ** CelebrityDetail
    CelebrityDetail (..),
    mkCelebrityDetail,
    cdBoundingBox,
    cdURLs,
    cdConfidence,
    cdName,
    cdId,
    cdFace,

    -- ** CelebrityRecognition
    CelebrityRecognition (..),
    mkCelebrityRecognition,
    crCelebrity,
    crTimestamp,

    -- ** CompareFacesMatch
    CompareFacesMatch (..),
    mkCompareFacesMatch,
    cfmSimilarity,
    cfmFace,

    -- ** ComparedFace
    ComparedFace (..),
    mkComparedFace,
    cfBoundingBox,
    cfPose,
    cfConfidence,
    cfQuality,
    cfLandmarks,

    -- ** ComparedSourceImageFace
    ComparedSourceImageFace (..),
    mkComparedSourceImageFace,
    csifBoundingBox,
    csifConfidence,

    -- ** ContentModerationDetection
    ContentModerationDetection (..),
    mkContentModerationDetection,
    cmdModerationLabel,
    cmdTimestamp,

    -- ** CoversBodyPart
    CoversBodyPart (..),
    mkCoversBodyPart,
    cbpValue,
    cbpConfidence,

    -- ** CustomLabel
    CustomLabel (..),
    mkCustomLabel,
    clConfidence,
    clName,
    clGeometry,

    -- ** DetectTextFilters
    DetectTextFilters (..),
    mkDetectTextFilters,
    dtfRegionsOfInterest,
    dtfWordFilter,

    -- ** DetectionFilter
    DetectionFilter (..),
    mkDetectionFilter,
    dfMinBoundingBoxHeight,
    dfMinBoundingBoxWidth,
    dfMinConfidence,

    -- ** Emotion
    Emotion (..),
    mkEmotion,
    eConfidence,
    eType,

    -- ** EquipmentDetection
    EquipmentDetection (..),
    mkEquipmentDetection,
    edBoundingBox,
    edCoversBodyPart,
    edConfidence,
    edType,

    -- ** EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
    erSummary,
    erF1Score,

    -- ** EyeOpen
    EyeOpen (..),
    mkEyeOpen,
    eoValue,
    eoConfidence,

    -- ** Eyeglasses
    Eyeglasses (..),
    mkEyeglasses,
    eyeValue,
    eyeConfidence,

    -- ** Face
    Face (..),
    mkFace,
    fFaceId,
    fBoundingBox,
    fExternalImageId,
    fConfidence,
    fImageId,

    -- ** FaceDetail
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

    -- ** FaceDetection
    FaceDetection (..),
    mkFaceDetection,
    fdTimestamp,
    fdFace,

    -- ** FaceMatch
    FaceMatch (..),
    mkFaceMatch,
    fmSimilarity,
    fmFace,

    -- ** FaceRecord
    FaceRecord (..),
    mkFaceRecord,
    frFaceDetail,
    frFace,

    -- ** FaceSearchSettings
    FaceSearchSettings (..),
    mkFaceSearchSettings,
    fssFaceMatchThreshold,
    fssCollectionId,

    -- ** Gender
    Gender (..),
    mkGender,
    gValue,
    gConfidence,

    -- ** Geometry
    Geometry (..),
    mkGeometry,
    gBoundingBox,
    gPolygon,

    -- ** GroundTruthManifest
    GroundTruthManifest (..),
    mkGroundTruthManifest,
    gtmS3Object,

    -- ** HumanLoopActivationOutput
    HumanLoopActivationOutput (..),
    mkHumanLoopActivationOutput,
    hlaoHumanLoopActivationReasons,
    hlaoHumanLoopARN,
    hlaoHumanLoopActivationConditionsEvaluationResults,

    -- ** HumanLoopConfig
    HumanLoopConfig (..),
    mkHumanLoopConfig,
    hlcDataAttributes,
    hlcHumanLoopName,
    hlcFlowDefinitionARN,

    -- ** HumanLoopDataAttributes
    HumanLoopDataAttributes (..),
    mkHumanLoopDataAttributes,
    hldaContentClassifiers,

    -- ** Image
    Image (..),
    mkImage,
    iS3Object,
    iBytes,

    -- ** ImageQuality
    ImageQuality (..),
    mkImageQuality,
    iqSharpness,
    iqBrightness,

    -- ** Instance
    Instance (..),
    mkInstance,
    iBoundingBox,
    iConfidence,

    -- ** KinesisDataStream
    KinesisDataStream (..),
    mkKinesisDataStream,
    kdsARN,

    -- ** KinesisVideoStream
    KinesisVideoStream (..),
    mkKinesisVideoStream,
    kvsARN,

    -- ** Label
    Label (..),
    mkLabel,
    lConfidence,
    lParents,
    lName,
    lInstances,

    -- ** LabelDetection
    LabelDetection (..),
    mkLabelDetection,
    ldLabel,
    ldTimestamp,

    -- ** Landmark
    Landmark (..),
    mkLandmark,
    lType,
    lX,
    lY,

    -- ** ModerationLabel
    ModerationLabel (..),
    mkModerationLabel,
    mlConfidence,
    mlName,
    mlParentName,

    -- ** MouthOpen
    MouthOpen (..),
    mkMouthOpen,
    moValue,
    moConfidence,

    -- ** Mustache
    Mustache (..),
    mkMustache,
    mValue,
    mConfidence,

    -- ** NotificationChannel
    NotificationChannel (..),
    mkNotificationChannel,
    ncSNSTopicARN,
    ncRoleARN,

    -- ** OutputConfig
    OutputConfig (..),
    mkOutputConfig,
    ocS3KeyPrefix,
    ocS3Bucket,

    -- ** Parent
    Parent (..),
    mkParent,
    pName,

    -- ** PersonDetail
    PersonDetail (..),
    mkPersonDetail,
    pdBoundingBox,
    pdIndex,
    pdFace,

    -- ** PersonDetection
    PersonDetection (..),
    mkPersonDetection,
    pdPerson,
    pdTimestamp,

    -- ** PersonMatch
    PersonMatch (..),
    mkPersonMatch,
    pmFaceMatches,
    pmPerson,
    pmTimestamp,

    -- ** Point
    Point (..),
    mkPoint,
    pX,
    pY,

    -- ** Pose
    Pose (..),
    mkPose,
    pYaw,
    pRoll,
    pPitch,

    -- ** ProjectDescription
    ProjectDescription (..),
    mkProjectDescription,
    pdStatus,
    pdCreationTimestamp,
    pdProjectARN,

    -- ** ProjectVersionDescription
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

    -- ** ProtectiveEquipmentBodyPart
    ProtectiveEquipmentBodyPart (..),
    mkProtectiveEquipmentBodyPart,
    pebpEquipmentDetections,
    pebpConfidence,
    pebpName,

    -- ** ProtectiveEquipmentPerson
    ProtectiveEquipmentPerson (..),
    mkProtectiveEquipmentPerson,
    pepBodyParts,
    pepBoundingBox,
    pepConfidence,
    pepId,

    -- ** ProtectiveEquipmentSummarizationAttributes
    ProtectiveEquipmentSummarizationAttributes (..),
    mkProtectiveEquipmentSummarizationAttributes,
    pesaMinConfidence,
    pesaRequiredEquipmentTypes,

    -- ** ProtectiveEquipmentSummary
    ProtectiveEquipmentSummary (..),
    mkProtectiveEquipmentSummary,
    pesPersonsWithRequiredEquipment,
    pesPersonsWithoutRequiredEquipment,
    pesPersonsIndeterminate,

    -- ** RegionOfInterest
    RegionOfInterest (..),
    mkRegionOfInterest,
    roiBoundingBox,

    -- ** S3Object
    S3Object (..),
    mkS3Object,
    soBucket,
    soName,
    soVersion,

    -- ** SegmentDetection
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

    -- ** SegmentTypeInfo
    SegmentTypeInfo (..),
    mkSegmentTypeInfo,
    stiModelVersion,
    stiType,

    -- ** ShotSegment
    ShotSegment (..),
    mkShotSegment,
    ssConfidence,
    ssIndex,

    -- ** Smile
    Smile (..),
    mkSmile,
    smiValue,
    smiConfidence,

    -- ** StartSegmentDetectionFilters
    StartSegmentDetectionFilters (..),
    mkStartSegmentDetectionFilters,
    ssdfTechnicalCueFilter,
    ssdfShotFilter,

    -- ** StartShotDetectionFilter
    StartShotDetectionFilter (..),
    mkStartShotDetectionFilter,
    ssdfMinSegmentConfidence,

    -- ** StartTechnicalCueDetectionFilter
    StartTechnicalCueDetectionFilter (..),
    mkStartTechnicalCueDetectionFilter,
    stcdfMinSegmentConfidence,

    -- ** StartTextDetectionFilters
    StartTextDetectionFilters (..),
    mkStartTextDetectionFilters,
    stdfRegionsOfInterest,
    stdfWordFilter,

    -- ** StreamProcessor
    StreamProcessor (..),
    mkStreamProcessor,
    spStatus,
    spName,

    -- ** StreamProcessorInput
    StreamProcessorInput (..),
    mkStreamProcessorInput,
    spiKinesisVideoStream,

    -- ** StreamProcessorOutput
    StreamProcessorOutput (..),
    mkStreamProcessorOutput,
    spoKinesisDataStream,

    -- ** StreamProcessorSettings
    StreamProcessorSettings (..),
    mkStreamProcessorSettings,
    spsFaceSearch,

    -- ** Summary
    Summary (..),
    mkSummary,
    sS3Object,

    -- ** Sunglasses
    Sunglasses (..),
    mkSunglasses,
    sValue,
    sConfidence,

    -- ** TechnicalCueSegment
    TechnicalCueSegment (..),
    mkTechnicalCueSegment,
    tcsConfidence,
    tcsType,

    -- ** TestingData
    TestingData (..),
    mkTestingData,
    tdAssets,
    tdAutoCreate,

    -- ** TestingDataResult
    TestingDataResult (..),
    mkTestingDataResult,
    tdrInput,
    tdrOutput,
    tdrValidation,

    -- ** TextDetection
    TextDetection (..),
    mkTextDetection,
    tdDetectedText,
    tdConfidence,
    tdGeometry,
    tdId,
    tdType,
    tdParentId,

    -- ** TextDetectionResult
    TextDetectionResult (..),
    mkTextDetectionResult,
    tdrTextDetection,
    tdrTimestamp,

    -- ** TrainingData
    TrainingData (..),
    mkTrainingData,
    tAssets,

    -- ** TrainingDataResult
    TrainingDataResult (..),
    mkTrainingDataResult,
    tInput,
    tOutput,
    tValidation,

    -- ** UnindexedFace
    UnindexedFace (..),
    mkUnindexedFace,
    ufReasons,
    ufFaceDetail,

    -- ** ValidationData
    ValidationData (..),
    mkValidationData,
    vdAssets,

    -- ** Video
    Video (..),
    mkVideo,
    vS3Object,

    -- ** VideoMetadata
    VideoMetadata (..),
    mkVideoMetadata,
    vmFrameRate,
    vmFormat,
    vmCodec,
    vmFrameHeight,
    vmDurationMillis,
    vmFrameWidth,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
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
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Rekognition'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
