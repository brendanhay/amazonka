{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Rekognition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-06-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the API Reference for
-- <https://docs.aws.amazon.com/rekognition/latest/dg/images.html Amazon Rekognition Image>,
-- <https://docs.aws.amazon.com/rekognition/latest/customlabels-dg/what-is.html Amazon Rekognition Custom Labels>,
-- <https://docs.aws.amazon.com/rekognition/latest/dg/video.html Amazon Rekognition Stored Video>,
-- <https://docs.aws.amazon.com/rekognition/latest/dg/streaming-video.html Amazon Rekognition Streaming Video>.
-- It provides descriptions of actions, data types, common parameters, and
-- common errors.
--
-- __Amazon Rekognition Image__
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_CompareFaces.html CompareFaces>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_CreateCollection.html CreateCollection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DeleteCollection.html DeleteCollection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DeleteFaces.html DeleteFaces>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DescribeCollection.html DescribeCollection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DetectFaces.html DetectFaces>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DetectLabels.html DetectLabels>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DetectModerationLabels.html DetectModerationLabels>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DetectProtectiveEquipment.html DetectProtectiveEquipment>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DetectText.html DetectText>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_GetCelebrityInfo.html GetCelebrityInfo>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_IndexFaces.html IndexFaces>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_ListCollections.html ListCollections>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_ListFaces.html ListFaces>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_RecognizeCelebrities.html RecognizeCelebrities>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_SearchFaces.html SearchFaces>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_SearchFacesByImage.html SearchFacesByImage>
--
-- __Amazon Rekognition Custom Labels__
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_CopyProjectVersion.html CopyProjectVersion>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_CreateDataset.html CreateDataset>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_CreateProject.html CreateProject>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_CreateProjectVersion.html CreateProjectVersion>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DeleteDataset.html DeleteDataset>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DeleteProject.html DeleteProject>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DeleteProjectPolicy.html DeleteProjectPolicy>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DeleteProjectVersion.html DeleteProjectVersion>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DescribeDataset.html DescribeDataset>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DescribeProjects.html DescribeProjects>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DescribeProjectVersions.html DescribeProjectVersions>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DetectCustomLabels.html DetectCustomLabels>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DistributeDatasetEntries.html DistributeDatasetEntries>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_ListDatasetEntries.html ListDatasetEntries>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_ListDatasetLabels.html ListDatasetLabels>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_ListProjectPolicies.html ListProjectPolicies>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_PutProjectPolicy.html PutProjectPolicy>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartProjectVersion.html StartProjectVersion>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StopProjectVersion.html StopProjectVersion>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_UpdateDatasetEntries.html UpdateDatasetEntries>
--
-- __Amazon Rekognition Video Stored Video__
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_GetCelebrityRecognition.html GetCelebrityRecognition>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_GetContentModeration.html GetContentModeration>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_GetFaceDetection.html GetFaceDetection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_GetFaceSearch.html GetFaceSearch>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_GetLabelDetection.html GetLabelDetection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_GetPersonTracking.html GetPersonTracking>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_GetSegmentDetection.html GetSegmentDetection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_GetTextDetection.html GetTextDetection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartCelebrityRecognition.html StartCelebrityRecognition>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartContentModeration.html StartContentModeration>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartFaceDetection.html StartFaceDetection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartFaceSearch.html StartFaceSearch>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartLabelDetection.html StartLabelDetection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartPersonTracking.html StartPersonTracking>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartSegmentDetection.html StartSegmentDetection>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartTextDetection.html StartTextDetection>
--
-- __Amazon Rekognition Video Streaming Video__
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_CreateStreamProcessor.html CreateStreamProcessor>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DeleteStreamProcessor.html DeleteStreamProcessor>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_DescribeStreamProcessor.html DescribeStreamProcessor>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_ListStreamProcessors.html ListStreamProcessors>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StartStreamProcessor.html StartStreamProcessor>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_StopStreamProcessor.html StopStreamProcessor>
--
-- -   <https://docs.aws.amazon.com/rekognition/latest/APIReference/API_UpdateStreamProcessor.html UpdateStreamProcessor>
module Amazonka.Rekognition
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** HumanLoopQuotaExceededException
    _HumanLoopQuotaExceededException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** ImageTooLargeException
    _ImageTooLargeException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidImageFormatException
    _InvalidImageFormatException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidPolicyRevisionIdException
    _InvalidPolicyRevisionIdException,

    -- ** InvalidS3ObjectException
    _InvalidS3ObjectException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceNotReadyException
    _ResourceNotReadyException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** VideoTooLargeException
    _VideoTooLargeException,

    -- * Waiters
    -- $waiters

    -- ** ProjectVersionRunning
    newProjectVersionRunning,

    -- ** ProjectVersionTrainingCompleted
    newProjectVersionTrainingCompleted,

    -- * Operations
    -- $operations

    -- ** CompareFaces
    CompareFaces (CompareFaces'),
    newCompareFaces,
    CompareFacesResponse (CompareFacesResponse'),
    newCompareFacesResponse,

    -- ** CopyProjectVersion
    CopyProjectVersion (CopyProjectVersion'),
    newCopyProjectVersion,
    CopyProjectVersionResponse (CopyProjectVersionResponse'),
    newCopyProjectVersionResponse,

    -- ** CreateCollection
    CreateCollection (CreateCollection'),
    newCreateCollection,
    CreateCollectionResponse (CreateCollectionResponse'),
    newCreateCollectionResponse,

    -- ** CreateDataset
    CreateDataset (CreateDataset'),
    newCreateDataset,
    CreateDatasetResponse (CreateDatasetResponse'),
    newCreateDatasetResponse,

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

    -- ** DeleteDataset
    DeleteDataset (DeleteDataset'),
    newDeleteDataset,
    DeleteDatasetResponse (DeleteDatasetResponse'),
    newDeleteDatasetResponse,

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

    -- ** DeleteProjectPolicy
    DeleteProjectPolicy (DeleteProjectPolicy'),
    newDeleteProjectPolicy,
    DeleteProjectPolicyResponse (DeleteProjectPolicyResponse'),
    newDeleteProjectPolicyResponse,

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

    -- ** DescribeDataset
    DescribeDataset (DescribeDataset'),
    newDescribeDataset,
    DescribeDatasetResponse (DescribeDatasetResponse'),
    newDescribeDatasetResponse,

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

    -- ** DistributeDatasetEntries
    DistributeDatasetEntries (DistributeDatasetEntries'),
    newDistributeDatasetEntries,
    DistributeDatasetEntriesResponse (DistributeDatasetEntriesResponse'),
    newDistributeDatasetEntriesResponse,

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

    -- ** ListDatasetEntries (Paginated)
    ListDatasetEntries (ListDatasetEntries'),
    newListDatasetEntries,
    ListDatasetEntriesResponse (ListDatasetEntriesResponse'),
    newListDatasetEntriesResponse,

    -- ** ListDatasetLabels (Paginated)
    ListDatasetLabels (ListDatasetLabels'),
    newListDatasetLabels,
    ListDatasetLabelsResponse (ListDatasetLabelsResponse'),
    newListDatasetLabelsResponse,

    -- ** ListFaces (Paginated)
    ListFaces (ListFaces'),
    newListFaces,
    ListFacesResponse (ListFacesResponse'),
    newListFacesResponse,

    -- ** ListProjectPolicies (Paginated)
    ListProjectPolicies (ListProjectPolicies'),
    newListProjectPolicies,
    ListProjectPoliciesResponse (ListProjectPoliciesResponse'),
    newListProjectPoliciesResponse,

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

    -- ** PutProjectPolicy
    PutProjectPolicy (PutProjectPolicy'),
    newPutProjectPolicy,
    PutProjectPolicyResponse (PutProjectPolicyResponse'),
    newPutProjectPolicyResponse,

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

    -- ** UpdateDatasetEntries
    UpdateDatasetEntries (UpdateDatasetEntries'),
    newUpdateDatasetEntries,
    UpdateDatasetEntriesResponse (UpdateDatasetEntriesResponse'),
    newUpdateDatasetEntriesResponse,

    -- ** UpdateStreamProcessor
    UpdateStreamProcessor (UpdateStreamProcessor'),
    newUpdateStreamProcessor,
    UpdateStreamProcessorResponse (UpdateStreamProcessorResponse'),
    newUpdateStreamProcessorResponse,

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

    -- ** DatasetStatus
    DatasetStatus (..),

    -- ** DatasetStatusMessageCode
    DatasetStatusMessageCode (..),

    -- ** DatasetType
    DatasetType (..),

    -- ** DetectLabelsFeatureName
    DetectLabelsFeatureName (..),

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

    -- ** LabelDetectionAggregateBy
    LabelDetectionAggregateBy (..),

    -- ** LabelDetectionFeatureName
    LabelDetectionFeatureName (..),

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

    -- ** StreamProcessorParameterToDelete
    StreamProcessorParameterToDelete (..),

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

    -- ** ConnectedHomeSettings
    ConnectedHomeSettings (ConnectedHomeSettings'),
    newConnectedHomeSettings,

    -- ** ConnectedHomeSettingsForUpdate
    ConnectedHomeSettingsForUpdate (ConnectedHomeSettingsForUpdate'),
    newConnectedHomeSettingsForUpdate,

    -- ** ContentModerationDetection
    ContentModerationDetection (ContentModerationDetection'),
    newContentModerationDetection,

    -- ** CoversBodyPart
    CoversBodyPart (CoversBodyPart'),
    newCoversBodyPart,

    -- ** CustomLabel
    CustomLabel (CustomLabel'),
    newCustomLabel,

    -- ** DatasetChanges
    DatasetChanges (DatasetChanges'),
    newDatasetChanges,

    -- ** DatasetDescription
    DatasetDescription (DatasetDescription'),
    newDatasetDescription,

    -- ** DatasetLabelDescription
    DatasetLabelDescription (DatasetLabelDescription'),
    newDatasetLabelDescription,

    -- ** DatasetLabelStats
    DatasetLabelStats (DatasetLabelStats'),
    newDatasetLabelStats,

    -- ** DatasetMetadata
    DatasetMetadata (DatasetMetadata'),
    newDatasetMetadata,

    -- ** DatasetSource
    DatasetSource (DatasetSource'),
    newDatasetSource,

    -- ** DatasetStats
    DatasetStats (DatasetStats'),
    newDatasetStats,

    -- ** DetectLabelsImageBackground
    DetectLabelsImageBackground (DetectLabelsImageBackground'),
    newDetectLabelsImageBackground,

    -- ** DetectLabelsImageForeground
    DetectLabelsImageForeground (DetectLabelsImageForeground'),
    newDetectLabelsImageForeground,

    -- ** DetectLabelsImageProperties
    DetectLabelsImageProperties (DetectLabelsImageProperties'),
    newDetectLabelsImageProperties,

    -- ** DetectLabelsImagePropertiesSettings
    DetectLabelsImagePropertiesSettings (DetectLabelsImagePropertiesSettings'),
    newDetectLabelsImagePropertiesSettings,

    -- ** DetectLabelsImageQuality
    DetectLabelsImageQuality (DetectLabelsImageQuality'),
    newDetectLabelsImageQuality,

    -- ** DetectLabelsSettings
    DetectLabelsSettings (DetectLabelsSettings'),
    newDetectLabelsSettings,

    -- ** DetectTextFilters
    DetectTextFilters (DetectTextFilters'),
    newDetectTextFilters,

    -- ** DetectionFilter
    DetectionFilter (DetectionFilter'),
    newDetectionFilter,

    -- ** DistributeDataset
    DistributeDataset (DistributeDataset'),
    newDistributeDataset,

    -- ** DominantColor
    DominantColor (DominantColor'),
    newDominantColor,

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

    -- ** GeneralLabelsSettings
    GeneralLabelsSettings (GeneralLabelsSettings'),
    newGeneralLabelsSettings,

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

    -- ** KinesisVideoStreamStartSelector
    KinesisVideoStreamStartSelector (KinesisVideoStreamStartSelector'),
    newKinesisVideoStreamStartSelector,

    -- ** KnownGender
    KnownGender (KnownGender'),
    newKnownGender,

    -- ** Label
    Label (Label'),
    newLabel,

    -- ** LabelAlias
    LabelAlias (LabelAlias'),
    newLabelAlias,

    -- ** LabelCategory
    LabelCategory (LabelCategory'),
    newLabelCategory,

    -- ** LabelDetection
    LabelDetection (LabelDetection'),
    newLabelDetection,

    -- ** LabelDetectionSettings
    LabelDetectionSettings (LabelDetectionSettings'),
    newLabelDetectionSettings,

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

    -- ** ProjectPolicy
    ProjectPolicy (ProjectPolicy'),
    newProjectPolicy,

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

    -- ** S3Destination
    S3Destination (S3Destination'),
    newS3Destination,

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

    -- ** StreamProcessingStartSelector
    StreamProcessingStartSelector (StreamProcessingStartSelector'),
    newStreamProcessingStartSelector,

    -- ** StreamProcessingStopSelector
    StreamProcessingStopSelector (StreamProcessingStopSelector'),
    newStreamProcessingStopSelector,

    -- ** StreamProcessor
    StreamProcessor (StreamProcessor'),
    newStreamProcessor,

    -- ** StreamProcessorDataSharingPreference
    StreamProcessorDataSharingPreference (StreamProcessorDataSharingPreference'),
    newStreamProcessorDataSharingPreference,

    -- ** StreamProcessorInput
    StreamProcessorInput (StreamProcessorInput'),
    newStreamProcessorInput,

    -- ** StreamProcessorNotificationChannel
    StreamProcessorNotificationChannel (StreamProcessorNotificationChannel'),
    newStreamProcessorNotificationChannel,

    -- ** StreamProcessorOutput
    StreamProcessorOutput (StreamProcessorOutput'),
    newStreamProcessorOutput,

    -- ** StreamProcessorSettings
    StreamProcessorSettings (StreamProcessorSettings'),
    newStreamProcessorSettings,

    -- ** StreamProcessorSettingsForUpdate
    StreamProcessorSettingsForUpdate (StreamProcessorSettingsForUpdate'),
    newStreamProcessorSettingsForUpdate,

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
import Amazonka.Rekognition.CopyProjectVersion
import Amazonka.Rekognition.CreateCollection
import Amazonka.Rekognition.CreateDataset
import Amazonka.Rekognition.CreateProject
import Amazonka.Rekognition.CreateProjectVersion
import Amazonka.Rekognition.CreateStreamProcessor
import Amazonka.Rekognition.DeleteCollection
import Amazonka.Rekognition.DeleteDataset
import Amazonka.Rekognition.DeleteFaces
import Amazonka.Rekognition.DeleteProject
import Amazonka.Rekognition.DeleteProjectPolicy
import Amazonka.Rekognition.DeleteProjectVersion
import Amazonka.Rekognition.DeleteStreamProcessor
import Amazonka.Rekognition.DescribeCollection
import Amazonka.Rekognition.DescribeDataset
import Amazonka.Rekognition.DescribeProjectVersions
import Amazonka.Rekognition.DescribeProjects
import Amazonka.Rekognition.DescribeStreamProcessor
import Amazonka.Rekognition.DetectCustomLabels
import Amazonka.Rekognition.DetectFaces
import Amazonka.Rekognition.DetectLabels
import Amazonka.Rekognition.DetectModerationLabels
import Amazonka.Rekognition.DetectProtectiveEquipment
import Amazonka.Rekognition.DetectText
import Amazonka.Rekognition.DistributeDatasetEntries
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
import Amazonka.Rekognition.ListDatasetEntries
import Amazonka.Rekognition.ListDatasetLabels
import Amazonka.Rekognition.ListFaces
import Amazonka.Rekognition.ListProjectPolicies
import Amazonka.Rekognition.ListStreamProcessors
import Amazonka.Rekognition.ListTagsForResource
import Amazonka.Rekognition.PutProjectPolicy
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
import Amazonka.Rekognition.UpdateDatasetEntries
import Amazonka.Rekognition.UpdateStreamProcessor
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
