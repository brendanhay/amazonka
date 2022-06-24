{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Rekognition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-06-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the Amazon Rekognition API reference.
module Amazonka.Rekognition
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotReadyException
    _ResourceNotReadyException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ImageTooLargeException
    _ImageTooLargeException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** HumanLoopQuotaExceededException
    _HumanLoopQuotaExceededException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** VideoTooLargeException
    _VideoTooLargeException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidImageFormatException
    _InvalidImageFormatException,

    -- ** InvalidS3ObjectException
    _InvalidS3ObjectException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- * Waiters
    -- $waiters

    -- ** ProjectVersionTrainingCompleted
    newProjectVersionTrainingCompleted,

    -- ** ProjectVersionRunning
    newProjectVersionRunning,

    -- * Operations
    -- $operations

    -- ** CompareFaces
    CompareFaces (CompareFaces'),
    newCompareFaces,
    CompareFacesResponse (CompareFacesResponse'),
    newCompareFacesResponse,

    -- ** CreateCollection
    CreateCollection (CreateCollection'),
    newCreateCollection,
    CreateCollectionResponse (CreateCollectionResponse'),
    newCreateCollectionResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** CreateProjectVersion
    CreateProjectVersion (CreateProjectVersion'),
    newCreateProjectVersion,
    CreateProjectVersionResponse (CreateProjectVersionResponse'),
    newCreateProjectVersionResponse,

    -- ** CreateStreamProcessor
    CreateStreamProcessor (CreateStreamProcessor'),
    newCreateStreamProcessor,
    CreateStreamProcessorResponse (CreateStreamProcessorResponse'),
    newCreateStreamProcessorResponse,

    -- ** DeleteCollection
    DeleteCollection (DeleteCollection'),
    newDeleteCollection,
    DeleteCollectionResponse (DeleteCollectionResponse'),
    newDeleteCollectionResponse,

    -- ** DeleteFaces
    DeleteFaces (DeleteFaces'),
    newDeleteFaces,
    DeleteFacesResponse (DeleteFacesResponse'),
    newDeleteFacesResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DeleteProjectVersion
    DeleteProjectVersion (DeleteProjectVersion'),
    newDeleteProjectVersion,
    DeleteProjectVersionResponse (DeleteProjectVersionResponse'),
    newDeleteProjectVersionResponse,

    -- ** DeleteStreamProcessor
    DeleteStreamProcessor (DeleteStreamProcessor'),
    newDeleteStreamProcessor,
    DeleteStreamProcessorResponse (DeleteStreamProcessorResponse'),
    newDeleteStreamProcessorResponse,

    -- ** DescribeCollection
    DescribeCollection (DescribeCollection'),
    newDescribeCollection,
    DescribeCollectionResponse (DescribeCollectionResponse'),
    newDescribeCollectionResponse,

    -- ** DescribeProjectVersions (Paginated)
    DescribeProjectVersions (DescribeProjectVersions'),
    newDescribeProjectVersions,
    DescribeProjectVersionsResponse (DescribeProjectVersionsResponse'),
    newDescribeProjectVersionsResponse,

    -- ** DescribeProjects (Paginated)
    DescribeProjects (DescribeProjects'),
    newDescribeProjects,
    DescribeProjectsResponse (DescribeProjectsResponse'),
    newDescribeProjectsResponse,

    -- ** DescribeStreamProcessor
    DescribeStreamProcessor (DescribeStreamProcessor'),
    newDescribeStreamProcessor,
    DescribeStreamProcessorResponse (DescribeStreamProcessorResponse'),
    newDescribeStreamProcessorResponse,

    -- ** DetectCustomLabels
    DetectCustomLabels (DetectCustomLabels'),
    newDetectCustomLabels,
    DetectCustomLabelsResponse (DetectCustomLabelsResponse'),
    newDetectCustomLabelsResponse,

    -- ** DetectFaces
    DetectFaces (DetectFaces'),
    newDetectFaces,
    DetectFacesResponse (DetectFacesResponse'),
    newDetectFacesResponse,

    -- ** DetectLabels
    DetectLabels (DetectLabels'),
    newDetectLabels,
    DetectLabelsResponse (DetectLabelsResponse'),
    newDetectLabelsResponse,

    -- ** DetectModerationLabels
    DetectModerationLabels (DetectModerationLabels'),
    newDetectModerationLabels,
    DetectModerationLabelsResponse (DetectModerationLabelsResponse'),
    newDetectModerationLabelsResponse,

    -- ** DetectProtectiveEquipment
    DetectProtectiveEquipment (DetectProtectiveEquipment'),
    newDetectProtectiveEquipment,
    DetectProtectiveEquipmentResponse (DetectProtectiveEquipmentResponse'),
    newDetectProtectiveEquipmentResponse,

    -- ** DetectText
    DetectText (DetectText'),
    newDetectText,
    DetectTextResponse (DetectTextResponse'),
    newDetectTextResponse,

    -- ** GetCelebrityInfo
    GetCelebrityInfo (GetCelebrityInfo'),
    newGetCelebrityInfo,
    GetCelebrityInfoResponse (GetCelebrityInfoResponse'),
    newGetCelebrityInfoResponse,

    -- ** GetCelebrityRecognition
    GetCelebrityRecognition (GetCelebrityRecognition'),
    newGetCelebrityRecognition,
    GetCelebrityRecognitionResponse (GetCelebrityRecognitionResponse'),
    newGetCelebrityRecognitionResponse,

    -- ** GetContentModeration
    GetContentModeration (GetContentModeration'),
    newGetContentModeration,
    GetContentModerationResponse (GetContentModerationResponse'),
    newGetContentModerationResponse,

    -- ** GetFaceDetection
    GetFaceDetection (GetFaceDetection'),
    newGetFaceDetection,
    GetFaceDetectionResponse (GetFaceDetectionResponse'),
    newGetFaceDetectionResponse,

    -- ** GetFaceSearch
    GetFaceSearch (GetFaceSearch'),
    newGetFaceSearch,
    GetFaceSearchResponse (GetFaceSearchResponse'),
    newGetFaceSearchResponse,

    -- ** GetLabelDetection
    GetLabelDetection (GetLabelDetection'),
    newGetLabelDetection,
    GetLabelDetectionResponse (GetLabelDetectionResponse'),
    newGetLabelDetectionResponse,

    -- ** GetPersonTracking
    GetPersonTracking (GetPersonTracking'),
    newGetPersonTracking,
    GetPersonTrackingResponse (GetPersonTrackingResponse'),
    newGetPersonTrackingResponse,

    -- ** GetSegmentDetection
    GetSegmentDetection (GetSegmentDetection'),
    newGetSegmentDetection,
    GetSegmentDetectionResponse (GetSegmentDetectionResponse'),
    newGetSegmentDetectionResponse,

    -- ** GetTextDetection
    GetTextDetection (GetTextDetection'),
    newGetTextDetection,
    GetTextDetectionResponse (GetTextDetectionResponse'),
    newGetTextDetectionResponse,

    -- ** IndexFaces
    IndexFaces (IndexFaces'),
    newIndexFaces,
    IndexFacesResponse (IndexFacesResponse'),
    newIndexFacesResponse,

    -- ** ListCollections (Paginated)
    ListCollections (ListCollections'),
    newListCollections,
    ListCollectionsResponse (ListCollectionsResponse'),
    newListCollectionsResponse,

    -- ** ListFaces (Paginated)
    ListFaces (ListFaces'),
    newListFaces,
    ListFacesResponse (ListFacesResponse'),
    newListFacesResponse,

    -- ** ListStreamProcessors (Paginated)
    ListStreamProcessors (ListStreamProcessors'),
    newListStreamProcessors,
    ListStreamProcessorsResponse (ListStreamProcessorsResponse'),
    newListStreamProcessorsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RecognizeCelebrities
    RecognizeCelebrities (RecognizeCelebrities'),
    newRecognizeCelebrities,
    RecognizeCelebritiesResponse (RecognizeCelebritiesResponse'),
    newRecognizeCelebritiesResponse,

    -- ** SearchFaces
    SearchFaces (SearchFaces'),
    newSearchFaces,
    SearchFacesResponse (SearchFacesResponse'),
    newSearchFacesResponse,

    -- ** SearchFacesByImage
    SearchFacesByImage (SearchFacesByImage'),
    newSearchFacesByImage,
    SearchFacesByImageResponse (SearchFacesByImageResponse'),
    newSearchFacesByImageResponse,

    -- ** StartCelebrityRecognition
    StartCelebrityRecognition (StartCelebrityRecognition'),
    newStartCelebrityRecognition,
    StartCelebrityRecognitionResponse (StartCelebrityRecognitionResponse'),
    newStartCelebrityRecognitionResponse,

    -- ** StartContentModeration
    StartContentModeration (StartContentModeration'),
    newStartContentModeration,
    StartContentModerationResponse (StartContentModerationResponse'),
    newStartContentModerationResponse,

    -- ** StartFaceDetection
    StartFaceDetection (StartFaceDetection'),
    newStartFaceDetection,
    StartFaceDetectionResponse (StartFaceDetectionResponse'),
    newStartFaceDetectionResponse,

    -- ** StartFaceSearch
    StartFaceSearch (StartFaceSearch'),
    newStartFaceSearch,
    StartFaceSearchResponse (StartFaceSearchResponse'),
    newStartFaceSearchResponse,

    -- ** StartLabelDetection
    StartLabelDetection (StartLabelDetection'),
    newStartLabelDetection,
    StartLabelDetectionResponse (StartLabelDetectionResponse'),
    newStartLabelDetectionResponse,

    -- ** StartPersonTracking
    StartPersonTracking (StartPersonTracking'),
    newStartPersonTracking,
    StartPersonTrackingResponse (StartPersonTrackingResponse'),
    newStartPersonTrackingResponse,

    -- ** StartProjectVersion
    StartProjectVersion (StartProjectVersion'),
    newStartProjectVersion,
    StartProjectVersionResponse (StartProjectVersionResponse'),
    newStartProjectVersionResponse,

    -- ** StartSegmentDetection
    StartSegmentDetection (StartSegmentDetection'),
    newStartSegmentDetection,
    StartSegmentDetectionResponse (StartSegmentDetectionResponse'),
    newStartSegmentDetectionResponse,

    -- ** StartStreamProcessor
    StartStreamProcessor (StartStreamProcessor'),
    newStartStreamProcessor,
    StartStreamProcessorResponse (StartStreamProcessorResponse'),
    newStartStreamProcessorResponse,

    -- ** StartTextDetection
    StartTextDetection (StartTextDetection'),
    newStartTextDetection,
    StartTextDetectionResponse (StartTextDetectionResponse'),
    newStartTextDetectionResponse,

    -- ** StopProjectVersion
    StopProjectVersion (StopProjectVersion'),
    newStopProjectVersion,
    StopProjectVersionResponse (StopProjectVersionResponse'),
    newStopProjectVersionResponse,

    -- ** StopStreamProcessor
    StopStreamProcessor (StopStreamProcessor'),
    newStopStreamProcessor,
    StopStreamProcessorResponse (StopStreamProcessorResponse'),
    newStopStreamProcessorResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

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

    -- ** KnownGenderType
    KnownGenderType (..),

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

    -- ** VideoColorRange
    VideoColorRange (..),

    -- ** VideoJobStatus
    VideoJobStatus (..),

    -- ** AgeRange
    AgeRange (AgeRange'),
    newAgeRange,

    -- ** Asset
    Asset (Asset'),
    newAsset,

    -- ** AudioMetadata
    AudioMetadata (AudioMetadata'),
    newAudioMetadata,

    -- ** Beard
    Beard (Beard'),
    newBeard,

    -- ** BlackFrame
    BlackFrame (BlackFrame'),
    newBlackFrame,

    -- ** BoundingBox
    BoundingBox (BoundingBox'),
    newBoundingBox,

    -- ** Celebrity
    Celebrity (Celebrity'),
    newCelebrity,

    -- ** CelebrityDetail
    CelebrityDetail (CelebrityDetail'),
    newCelebrityDetail,

    -- ** CelebrityRecognition
    CelebrityRecognition (CelebrityRecognition'),
    newCelebrityRecognition,

    -- ** CompareFacesMatch
    CompareFacesMatch (CompareFacesMatch'),
    newCompareFacesMatch,

    -- ** ComparedFace
    ComparedFace (ComparedFace'),
    newComparedFace,

    -- ** ComparedSourceImageFace
    ComparedSourceImageFace (ComparedSourceImageFace'),
    newComparedSourceImageFace,

    -- ** ContentModerationDetection
    ContentModerationDetection (ContentModerationDetection'),
    newContentModerationDetection,

    -- ** CoversBodyPart
    CoversBodyPart (CoversBodyPart'),
    newCoversBodyPart,

    -- ** CustomLabel
    CustomLabel (CustomLabel'),
    newCustomLabel,

    -- ** DetectTextFilters
    DetectTextFilters (DetectTextFilters'),
    newDetectTextFilters,

    -- ** DetectionFilter
    DetectionFilter (DetectionFilter'),
    newDetectionFilter,

    -- ** Emotion
    Emotion (Emotion'),
    newEmotion,

    -- ** EquipmentDetection
    EquipmentDetection (EquipmentDetection'),
    newEquipmentDetection,

    -- ** EvaluationResult
    EvaluationResult (EvaluationResult'),
    newEvaluationResult,

    -- ** EyeOpen
    EyeOpen (EyeOpen'),
    newEyeOpen,

    -- ** Eyeglasses
    Eyeglasses (Eyeglasses'),
    newEyeglasses,

    -- ** Face
    Face (Face'),
    newFace,

    -- ** FaceDetail
    FaceDetail (FaceDetail'),
    newFaceDetail,

    -- ** FaceDetection
    FaceDetection (FaceDetection'),
    newFaceDetection,

    -- ** FaceMatch
    FaceMatch (FaceMatch'),
    newFaceMatch,

    -- ** FaceRecord
    FaceRecord (FaceRecord'),
    newFaceRecord,

    -- ** FaceSearchSettings
    FaceSearchSettings (FaceSearchSettings'),
    newFaceSearchSettings,

    -- ** Gender
    Gender (Gender'),
    newGender,

    -- ** Geometry
    Geometry (Geometry'),
    newGeometry,

    -- ** GroundTruthManifest
    GroundTruthManifest (GroundTruthManifest'),
    newGroundTruthManifest,

    -- ** HumanLoopActivationOutput
    HumanLoopActivationOutput (HumanLoopActivationOutput'),
    newHumanLoopActivationOutput,

    -- ** HumanLoopConfig
    HumanLoopConfig (HumanLoopConfig'),
    newHumanLoopConfig,

    -- ** HumanLoopDataAttributes
    HumanLoopDataAttributes (HumanLoopDataAttributes'),
    newHumanLoopDataAttributes,

    -- ** Image
    Image (Image'),
    newImage,

    -- ** ImageQuality
    ImageQuality (ImageQuality'),
    newImageQuality,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** KinesisDataStream
    KinesisDataStream (KinesisDataStream'),
    newKinesisDataStream,

    -- ** KinesisVideoStream
    KinesisVideoStream (KinesisVideoStream'),
    newKinesisVideoStream,

    -- ** KnownGender
    KnownGender (KnownGender'),
    newKnownGender,

    -- ** Label
    Label (Label'),
    newLabel,

    -- ** LabelDetection
    LabelDetection (LabelDetection'),
    newLabelDetection,

    -- ** Landmark
    Landmark (Landmark'),
    newLandmark,

    -- ** ModerationLabel
    ModerationLabel (ModerationLabel'),
    newModerationLabel,

    -- ** MouthOpen
    MouthOpen (MouthOpen'),
    newMouthOpen,

    -- ** Mustache
    Mustache (Mustache'),
    newMustache,

    -- ** NotificationChannel
    NotificationChannel (NotificationChannel'),
    newNotificationChannel,

    -- ** OutputConfig
    OutputConfig (OutputConfig'),
    newOutputConfig,

    -- ** Parent
    Parent (Parent'),
    newParent,

    -- ** PersonDetail
    PersonDetail (PersonDetail'),
    newPersonDetail,

    -- ** PersonDetection
    PersonDetection (PersonDetection'),
    newPersonDetection,

    -- ** PersonMatch
    PersonMatch (PersonMatch'),
    newPersonMatch,

    -- ** Point
    Point (Point'),
    newPoint,

    -- ** Pose
    Pose (Pose'),
    newPose,

    -- ** ProjectDescription
    ProjectDescription (ProjectDescription'),
    newProjectDescription,

    -- ** ProjectVersionDescription
    ProjectVersionDescription (ProjectVersionDescription'),
    newProjectVersionDescription,

    -- ** ProtectiveEquipmentBodyPart
    ProtectiveEquipmentBodyPart (ProtectiveEquipmentBodyPart'),
    newProtectiveEquipmentBodyPart,

    -- ** ProtectiveEquipmentPerson
    ProtectiveEquipmentPerson (ProtectiveEquipmentPerson'),
    newProtectiveEquipmentPerson,

    -- ** ProtectiveEquipmentSummarizationAttributes
    ProtectiveEquipmentSummarizationAttributes (ProtectiveEquipmentSummarizationAttributes'),
    newProtectiveEquipmentSummarizationAttributes,

    -- ** ProtectiveEquipmentSummary
    ProtectiveEquipmentSummary (ProtectiveEquipmentSummary'),
    newProtectiveEquipmentSummary,

    -- ** RegionOfInterest
    RegionOfInterest (RegionOfInterest'),
    newRegionOfInterest,

    -- ** S3Object
    S3Object (S3Object'),
    newS3Object,

    -- ** SegmentDetection
    SegmentDetection (SegmentDetection'),
    newSegmentDetection,

    -- ** SegmentTypeInfo
    SegmentTypeInfo (SegmentTypeInfo'),
    newSegmentTypeInfo,

    -- ** ShotSegment
    ShotSegment (ShotSegment'),
    newShotSegment,

    -- ** Smile
    Smile (Smile'),
    newSmile,

    -- ** StartSegmentDetectionFilters
    StartSegmentDetectionFilters (StartSegmentDetectionFilters'),
    newStartSegmentDetectionFilters,

    -- ** StartShotDetectionFilter
    StartShotDetectionFilter (StartShotDetectionFilter'),
    newStartShotDetectionFilter,

    -- ** StartTechnicalCueDetectionFilter
    StartTechnicalCueDetectionFilter (StartTechnicalCueDetectionFilter'),
    newStartTechnicalCueDetectionFilter,

    -- ** StartTextDetectionFilters
    StartTextDetectionFilters (StartTextDetectionFilters'),
    newStartTextDetectionFilters,

    -- ** StreamProcessor
    StreamProcessor (StreamProcessor'),
    newStreamProcessor,

    -- ** StreamProcessorInput
    StreamProcessorInput (StreamProcessorInput'),
    newStreamProcessorInput,

    -- ** StreamProcessorOutput
    StreamProcessorOutput (StreamProcessorOutput'),
    newStreamProcessorOutput,

    -- ** StreamProcessorSettings
    StreamProcessorSettings (StreamProcessorSettings'),
    newStreamProcessorSettings,

    -- ** Summary
    Summary (Summary'),
    newSummary,

    -- ** Sunglasses
    Sunglasses (Sunglasses'),
    newSunglasses,

    -- ** TechnicalCueSegment
    TechnicalCueSegment (TechnicalCueSegment'),
    newTechnicalCueSegment,

    -- ** TestingData
    TestingData (TestingData'),
    newTestingData,

    -- ** TestingDataResult
    TestingDataResult (TestingDataResult'),
    newTestingDataResult,

    -- ** TextDetection
    TextDetection (TextDetection'),
    newTextDetection,

    -- ** TextDetectionResult
    TextDetectionResult (TextDetectionResult'),
    newTextDetectionResult,

    -- ** TrainingData
    TrainingData (TrainingData'),
    newTrainingData,

    -- ** TrainingDataResult
    TrainingDataResult (TrainingDataResult'),
    newTrainingDataResult,

    -- ** UnindexedFace
    UnindexedFace (UnindexedFace'),
    newUnindexedFace,

    -- ** ValidationData
    ValidationData (ValidationData'),
    newValidationData,

    -- ** Video
    Video (Video'),
    newVideo,

    -- ** VideoMetadata
    VideoMetadata (VideoMetadata'),
    newVideoMetadata,
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
import Amazonka.Rekognition.Lens
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
import Amazonka.Rekognition.Types
import Amazonka.Rekognition.UntagResource
import Amazonka.Rekognition.Waiters

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
