{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Rekognition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-06-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the Amazon Rekognition API reference.
module Network.AWS.Rekognition
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** VideoTooLargeException
    _VideoTooLargeException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidImageFormatException
    _InvalidImageFormatException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** InvalidS3ObjectException
    _InvalidS3ObjectException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** ImageTooLargeException
    _ImageTooLargeException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** ResourceNotReadyException
    _ResourceNotReadyException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** HumanLoopQuotaExceededException
    _HumanLoopQuotaExceededException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- ** ProjectVersionRunning
    newProjectVersionRunning,

    -- ** ProjectVersionTrainingCompleted
    newProjectVersionTrainingCompleted,

    -- * Operations
    -- $operations

    -- ** DetectProtectiveEquipment
    DetectProtectiveEquipment (DetectProtectiveEquipment'),
    newDetectProtectiveEquipment,
    DetectProtectiveEquipmentResponse (DetectProtectiveEquipmentResponse'),
    newDetectProtectiveEquipmentResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** StartCelebrityRecognition
    StartCelebrityRecognition (StartCelebrityRecognition'),
    newStartCelebrityRecognition,
    StartCelebrityRecognitionResponse (StartCelebrityRecognitionResponse'),
    newStartCelebrityRecognitionResponse,

    -- ** GetPersonTracking
    GetPersonTracking (GetPersonTracking'),
    newGetPersonTracking,
    GetPersonTrackingResponse (GetPersonTrackingResponse'),
    newGetPersonTrackingResponse,

    -- ** GetTextDetection
    GetTextDetection (GetTextDetection'),
    newGetTextDetection,
    GetTextDetectionResponse (GetTextDetectionResponse'),
    newGetTextDetectionResponse,

    -- ** StartSegmentDetection
    StartSegmentDetection (StartSegmentDetection'),
    newStartSegmentDetection,
    StartSegmentDetectionResponse (StartSegmentDetectionResponse'),
    newStartSegmentDetectionResponse,

    -- ** ListCollections (Paginated)
    ListCollections (ListCollections'),
    newListCollections,
    ListCollectionsResponse (ListCollectionsResponse'),
    newListCollectionsResponse,

    -- ** StartProjectVersion
    StartProjectVersion (StartProjectVersion'),
    newStartProjectVersion,
    StartProjectVersionResponse (StartProjectVersionResponse'),
    newStartProjectVersionResponse,

    -- ** DeleteCollection
    DeleteCollection (DeleteCollection'),
    newDeleteCollection,
    DeleteCollectionResponse (DeleteCollectionResponse'),
    newDeleteCollectionResponse,

    -- ** CreateCollection
    CreateCollection (CreateCollection'),
    newCreateCollection,
    CreateCollectionResponse (CreateCollectionResponse'),
    newCreateCollectionResponse,

    -- ** StopStreamProcessor
    StopStreamProcessor (StopStreamProcessor'),
    newStopStreamProcessor,
    StopStreamProcessorResponse (StopStreamProcessorResponse'),
    newStopStreamProcessorResponse,

    -- ** DetectLabels
    DetectLabels (DetectLabels'),
    newDetectLabels,
    DetectLabelsResponse (DetectLabelsResponse'),
    newDetectLabelsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartContentModeration
    StartContentModeration (StartContentModeration'),
    newStartContentModeration,
    StartContentModerationResponse (StartContentModerationResponse'),
    newStartContentModerationResponse,

    -- ** SearchFacesByImage
    SearchFacesByImage (SearchFacesByImage'),
    newSearchFacesByImage,
    SearchFacesByImageResponse (SearchFacesByImageResponse'),
    newSearchFacesByImageResponse,

    -- ** ListStreamProcessors (Paginated)
    ListStreamProcessors (ListStreamProcessors'),
    newListStreamProcessors,
    ListStreamProcessorsResponse (ListStreamProcessorsResponse'),
    newListStreamProcessorsResponse,

    -- ** DescribeCollection
    DescribeCollection (DescribeCollection'),
    newDescribeCollection,
    DescribeCollectionResponse (DescribeCollectionResponse'),
    newDescribeCollectionResponse,

    -- ** DeleteProjectVersion
    DeleteProjectVersion (DeleteProjectVersion'),
    newDeleteProjectVersion,
    DeleteProjectVersionResponse (DeleteProjectVersionResponse'),
    newDeleteProjectVersionResponse,

    -- ** DescribeProjectVersions (Paginated)
    DescribeProjectVersions (DescribeProjectVersions'),
    newDescribeProjectVersions,
    DescribeProjectVersionsResponse (DescribeProjectVersionsResponse'),
    newDescribeProjectVersionsResponse,

    -- ** RecognizeCelebrities
    RecognizeCelebrities (RecognizeCelebrities'),
    newRecognizeCelebrities,
    RecognizeCelebritiesResponse (RecognizeCelebritiesResponse'),
    newRecognizeCelebritiesResponse,

    -- ** DetectCustomLabels
    DetectCustomLabels (DetectCustomLabels'),
    newDetectCustomLabels,
    DetectCustomLabelsResponse (DetectCustomLabelsResponse'),
    newDetectCustomLabelsResponse,

    -- ** GetFaceSearch
    GetFaceSearch (GetFaceSearch'),
    newGetFaceSearch,
    GetFaceSearchResponse (GetFaceSearchResponse'),
    newGetFaceSearchResponse,

    -- ** StartLabelDetection
    StartLabelDetection (StartLabelDetection'),
    newStartLabelDetection,
    StartLabelDetectionResponse (StartLabelDetectionResponse'),
    newStartLabelDetectionResponse,

    -- ** SearchFaces
    SearchFaces (SearchFaces'),
    newSearchFaces,
    SearchFacesResponse (SearchFacesResponse'),
    newSearchFacesResponse,

    -- ** IndexFaces
    IndexFaces (IndexFaces'),
    newIndexFaces,
    IndexFacesResponse (IndexFacesResponse'),
    newIndexFacesResponse,

    -- ** GetLabelDetection
    GetLabelDetection (GetLabelDetection'),
    newGetLabelDetection,
    GetLabelDetectionResponse (GetLabelDetectionResponse'),
    newGetLabelDetectionResponse,

    -- ** StopProjectVersion
    StopProjectVersion (StopProjectVersion'),
    newStopProjectVersion,
    StopProjectVersionResponse (StopProjectVersionResponse'),
    newStopProjectVersionResponse,

    -- ** DescribeStreamProcessor
    DescribeStreamProcessor (DescribeStreamProcessor'),
    newDescribeStreamProcessor,
    DescribeStreamProcessorResponse (DescribeStreamProcessorResponse'),
    newDescribeStreamProcessorResponse,

    -- ** StartFaceSearch
    StartFaceSearch (StartFaceSearch'),
    newStartFaceSearch,
    StartFaceSearchResponse (StartFaceSearchResponse'),
    newStartFaceSearchResponse,

    -- ** StartTextDetection
    StartTextDetection (StartTextDetection'),
    newStartTextDetection,
    StartTextDetectionResponse (StartTextDetectionResponse'),
    newStartTextDetectionResponse,

    -- ** StartPersonTracking
    StartPersonTracking (StartPersonTracking'),
    newStartPersonTracking,
    StartPersonTrackingResponse (StartPersonTrackingResponse'),
    newStartPersonTrackingResponse,

    -- ** GetCelebrityRecognition
    GetCelebrityRecognition (GetCelebrityRecognition'),
    newGetCelebrityRecognition,
    GetCelebrityRecognitionResponse (GetCelebrityRecognitionResponse'),
    newGetCelebrityRecognitionResponse,

    -- ** StartStreamProcessor
    StartStreamProcessor (StartStreamProcessor'),
    newStartStreamProcessor,
    StartStreamProcessorResponse (StartStreamProcessorResponse'),
    newStartStreamProcessorResponse,

    -- ** DetectText
    DetectText (DetectText'),
    newDetectText,
    DetectTextResponse (DetectTextResponse'),
    newDetectTextResponse,

    -- ** GetSegmentDetection
    GetSegmentDetection (GetSegmentDetection'),
    newGetSegmentDetection,
    GetSegmentDetectionResponse (GetSegmentDetectionResponse'),
    newGetSegmentDetectionResponse,

    -- ** CompareFaces
    CompareFaces (CompareFaces'),
    newCompareFaces,
    CompareFacesResponse (CompareFacesResponse'),
    newCompareFacesResponse,

    -- ** DetectFaces
    DetectFaces (DetectFaces'),
    newDetectFaces,
    DetectFacesResponse (DetectFacesResponse'),
    newDetectFacesResponse,

    -- ** GetFaceDetection
    GetFaceDetection (GetFaceDetection'),
    newGetFaceDetection,
    GetFaceDetectionResponse (GetFaceDetectionResponse'),
    newGetFaceDetectionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListFaces (Paginated)
    ListFaces (ListFaces'),
    newListFaces,
    ListFacesResponse (ListFacesResponse'),
    newListFacesResponse,

    -- ** CreateProjectVersion
    CreateProjectVersion (CreateProjectVersion'),
    newCreateProjectVersion,
    CreateProjectVersionResponse (CreateProjectVersionResponse'),
    newCreateProjectVersionResponse,

    -- ** DescribeProjects (Paginated)
    DescribeProjects (DescribeProjects'),
    newDescribeProjects,
    DescribeProjectsResponse (DescribeProjectsResponse'),
    newDescribeProjectsResponse,

    -- ** GetContentModeration
    GetContentModeration (GetContentModeration'),
    newGetContentModeration,
    GetContentModerationResponse (GetContentModerationResponse'),
    newGetContentModerationResponse,

    -- ** DeleteFaces
    DeleteFaces (DeleteFaces'),
    newDeleteFaces,
    DeleteFacesResponse (DeleteFacesResponse'),
    newDeleteFacesResponse,

    -- ** GetCelebrityInfo
    GetCelebrityInfo (GetCelebrityInfo'),
    newGetCelebrityInfo,
    GetCelebrityInfoResponse (GetCelebrityInfoResponse'),
    newGetCelebrityInfoResponse,

    -- ** DeleteStreamProcessor
    DeleteStreamProcessor (DeleteStreamProcessor'),
    newDeleteStreamProcessor,
    DeleteStreamProcessorResponse (DeleteStreamProcessorResponse'),
    newDeleteStreamProcessorResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DetectModerationLabels
    DetectModerationLabels (DetectModerationLabels'),
    newDetectModerationLabels,
    DetectModerationLabelsResponse (DetectModerationLabelsResponse'),
    newDetectModerationLabelsResponse,

    -- ** CreateStreamProcessor
    CreateStreamProcessor (CreateStreamProcessor'),
    newCreateStreamProcessor,
    CreateStreamProcessorResponse (CreateStreamProcessorResponse'),
    newCreateStreamProcessorResponse,

    -- ** StartFaceDetection
    StartFaceDetection (StartFaceDetection'),
    newStartFaceDetection,
    StartFaceDetectionResponse (StartFaceDetectionResponse'),
    newStartFaceDetectionResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

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
import Network.AWS.Rekognition.Lens
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
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.UntagResource
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
