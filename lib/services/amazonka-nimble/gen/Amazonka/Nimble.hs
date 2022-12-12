{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Nimble
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Nimble Studio empowers creative studios to produce visual
-- effects, animation, and interactive content entirely in the cloud, from
-- storyboard sketch to final deliverable. Rapidly onboard and collaborate
-- with artists globally and create content faster with access to virtual
-- workstations, high-speed storage, and scalable rendering across AWS’s
-- global infrastructure.
module Amazonka.Nimble
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** LaunchProfileDeleted
    newLaunchProfileDeleted,

    -- ** LaunchProfileReady
    newLaunchProfileReady,

    -- ** StreamingImageDeleted
    newStreamingImageDeleted,

    -- ** StreamingImageReady
    newStreamingImageReady,

    -- ** StreamingSessionDeleted
    newStreamingSessionDeleted,

    -- ** StreamingSessionReady
    newStreamingSessionReady,

    -- ** StreamingSessionStopped
    newStreamingSessionStopped,

    -- ** StreamingSessionStreamReady
    newStreamingSessionStreamReady,

    -- ** StudioComponentDeleted
    newStudioComponentDeleted,

    -- ** StudioComponentReady
    newStudioComponentReady,

    -- ** StudioDeleted
    newStudioDeleted,

    -- ** StudioReady
    newStudioReady,

    -- * Operations
    -- $operations

    -- ** AcceptEulas
    AcceptEulas (AcceptEulas'),
    newAcceptEulas,
    AcceptEulasResponse (AcceptEulasResponse'),
    newAcceptEulasResponse,

    -- ** CreateLaunchProfile
    CreateLaunchProfile (CreateLaunchProfile'),
    newCreateLaunchProfile,
    CreateLaunchProfileResponse (CreateLaunchProfileResponse'),
    newCreateLaunchProfileResponse,

    -- ** CreateStreamingImage
    CreateStreamingImage (CreateStreamingImage'),
    newCreateStreamingImage,
    CreateStreamingImageResponse (CreateStreamingImageResponse'),
    newCreateStreamingImageResponse,

    -- ** CreateStreamingSession
    CreateStreamingSession (CreateStreamingSession'),
    newCreateStreamingSession,
    CreateStreamingSessionResponse (CreateStreamingSessionResponse'),
    newCreateStreamingSessionResponse,

    -- ** CreateStreamingSessionStream
    CreateStreamingSessionStream (CreateStreamingSessionStream'),
    newCreateStreamingSessionStream,
    CreateStreamingSessionStreamResponse (CreateStreamingSessionStreamResponse'),
    newCreateStreamingSessionStreamResponse,

    -- ** CreateStudio
    CreateStudio (CreateStudio'),
    newCreateStudio,
    CreateStudioResponse (CreateStudioResponse'),
    newCreateStudioResponse,

    -- ** CreateStudioComponent
    CreateStudioComponent (CreateStudioComponent'),
    newCreateStudioComponent,
    CreateStudioComponentResponse (CreateStudioComponentResponse'),
    newCreateStudioComponentResponse,

    -- ** DeleteLaunchProfile
    DeleteLaunchProfile (DeleteLaunchProfile'),
    newDeleteLaunchProfile,
    DeleteLaunchProfileResponse (DeleteLaunchProfileResponse'),
    newDeleteLaunchProfileResponse,

    -- ** DeleteLaunchProfileMember
    DeleteLaunchProfileMember (DeleteLaunchProfileMember'),
    newDeleteLaunchProfileMember,
    DeleteLaunchProfileMemberResponse (DeleteLaunchProfileMemberResponse'),
    newDeleteLaunchProfileMemberResponse,

    -- ** DeleteStreamingImage
    DeleteStreamingImage (DeleteStreamingImage'),
    newDeleteStreamingImage,
    DeleteStreamingImageResponse (DeleteStreamingImageResponse'),
    newDeleteStreamingImageResponse,

    -- ** DeleteStreamingSession
    DeleteStreamingSession (DeleteStreamingSession'),
    newDeleteStreamingSession,
    DeleteStreamingSessionResponse (DeleteStreamingSessionResponse'),
    newDeleteStreamingSessionResponse,

    -- ** DeleteStudio
    DeleteStudio (DeleteStudio'),
    newDeleteStudio,
    DeleteStudioResponse (DeleteStudioResponse'),
    newDeleteStudioResponse,

    -- ** DeleteStudioComponent
    DeleteStudioComponent (DeleteStudioComponent'),
    newDeleteStudioComponent,
    DeleteStudioComponentResponse (DeleteStudioComponentResponse'),
    newDeleteStudioComponentResponse,

    -- ** DeleteStudioMember
    DeleteStudioMember (DeleteStudioMember'),
    newDeleteStudioMember,
    DeleteStudioMemberResponse (DeleteStudioMemberResponse'),
    newDeleteStudioMemberResponse,

    -- ** GetEula
    GetEula (GetEula'),
    newGetEula,
    GetEulaResponse (GetEulaResponse'),
    newGetEulaResponse,

    -- ** GetLaunchProfile
    GetLaunchProfile (GetLaunchProfile'),
    newGetLaunchProfile,
    GetLaunchProfileResponse (GetLaunchProfileResponse'),
    newGetLaunchProfileResponse,

    -- ** GetLaunchProfileDetails
    GetLaunchProfileDetails (GetLaunchProfileDetails'),
    newGetLaunchProfileDetails,
    GetLaunchProfileDetailsResponse (GetLaunchProfileDetailsResponse'),
    newGetLaunchProfileDetailsResponse,

    -- ** GetLaunchProfileInitialization
    GetLaunchProfileInitialization (GetLaunchProfileInitialization'),
    newGetLaunchProfileInitialization,
    GetLaunchProfileInitializationResponse (GetLaunchProfileInitializationResponse'),
    newGetLaunchProfileInitializationResponse,

    -- ** GetLaunchProfileMember
    GetLaunchProfileMember (GetLaunchProfileMember'),
    newGetLaunchProfileMember,
    GetLaunchProfileMemberResponse (GetLaunchProfileMemberResponse'),
    newGetLaunchProfileMemberResponse,

    -- ** GetStreamingImage
    GetStreamingImage (GetStreamingImage'),
    newGetStreamingImage,
    GetStreamingImageResponse (GetStreamingImageResponse'),
    newGetStreamingImageResponse,

    -- ** GetStreamingSession
    GetStreamingSession (GetStreamingSession'),
    newGetStreamingSession,
    GetStreamingSessionResponse (GetStreamingSessionResponse'),
    newGetStreamingSessionResponse,

    -- ** GetStreamingSessionStream
    GetStreamingSessionStream (GetStreamingSessionStream'),
    newGetStreamingSessionStream,
    GetStreamingSessionStreamResponse (GetStreamingSessionStreamResponse'),
    newGetStreamingSessionStreamResponse,

    -- ** GetStudio
    GetStudio (GetStudio'),
    newGetStudio,
    GetStudioResponse (GetStudioResponse'),
    newGetStudioResponse,

    -- ** GetStudioComponent
    GetStudioComponent (GetStudioComponent'),
    newGetStudioComponent,
    GetStudioComponentResponse (GetStudioComponentResponse'),
    newGetStudioComponentResponse,

    -- ** GetStudioMember
    GetStudioMember (GetStudioMember'),
    newGetStudioMember,
    GetStudioMemberResponse (GetStudioMemberResponse'),
    newGetStudioMemberResponse,

    -- ** ListEulaAcceptances (Paginated)
    ListEulaAcceptances (ListEulaAcceptances'),
    newListEulaAcceptances,
    ListEulaAcceptancesResponse (ListEulaAcceptancesResponse'),
    newListEulaAcceptancesResponse,

    -- ** ListEulas (Paginated)
    ListEulas (ListEulas'),
    newListEulas,
    ListEulasResponse (ListEulasResponse'),
    newListEulasResponse,

    -- ** ListLaunchProfileMembers (Paginated)
    ListLaunchProfileMembers (ListLaunchProfileMembers'),
    newListLaunchProfileMembers,
    ListLaunchProfileMembersResponse (ListLaunchProfileMembersResponse'),
    newListLaunchProfileMembersResponse,

    -- ** ListLaunchProfiles (Paginated)
    ListLaunchProfiles (ListLaunchProfiles'),
    newListLaunchProfiles,
    ListLaunchProfilesResponse (ListLaunchProfilesResponse'),
    newListLaunchProfilesResponse,

    -- ** ListStreamingImages (Paginated)
    ListStreamingImages (ListStreamingImages'),
    newListStreamingImages,
    ListStreamingImagesResponse (ListStreamingImagesResponse'),
    newListStreamingImagesResponse,

    -- ** ListStreamingSessions (Paginated)
    ListStreamingSessions (ListStreamingSessions'),
    newListStreamingSessions,
    ListStreamingSessionsResponse (ListStreamingSessionsResponse'),
    newListStreamingSessionsResponse,

    -- ** ListStudioComponents (Paginated)
    ListStudioComponents (ListStudioComponents'),
    newListStudioComponents,
    ListStudioComponentsResponse (ListStudioComponentsResponse'),
    newListStudioComponentsResponse,

    -- ** ListStudioMembers (Paginated)
    ListStudioMembers (ListStudioMembers'),
    newListStudioMembers,
    ListStudioMembersResponse (ListStudioMembersResponse'),
    newListStudioMembersResponse,

    -- ** ListStudios (Paginated)
    ListStudios (ListStudios'),
    newListStudios,
    ListStudiosResponse (ListStudiosResponse'),
    newListStudiosResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutLaunchProfileMembers
    PutLaunchProfileMembers (PutLaunchProfileMembers'),
    newPutLaunchProfileMembers,
    PutLaunchProfileMembersResponse (PutLaunchProfileMembersResponse'),
    newPutLaunchProfileMembersResponse,

    -- ** PutStudioMembers
    PutStudioMembers (PutStudioMembers'),
    newPutStudioMembers,
    PutStudioMembersResponse (PutStudioMembersResponse'),
    newPutStudioMembersResponse,

    -- ** StartStreamingSession
    StartStreamingSession (StartStreamingSession'),
    newStartStreamingSession,
    StartStreamingSessionResponse (StartStreamingSessionResponse'),
    newStartStreamingSessionResponse,

    -- ** StartStudioSSOConfigurationRepair
    StartStudioSSOConfigurationRepair (StartStudioSSOConfigurationRepair'),
    newStartStudioSSOConfigurationRepair,
    StartStudioSSOConfigurationRepairResponse (StartStudioSSOConfigurationRepairResponse'),
    newStartStudioSSOConfigurationRepairResponse,

    -- ** StopStreamingSession
    StopStreamingSession (StopStreamingSession'),
    newStopStreamingSession,
    StopStreamingSessionResponse (StopStreamingSessionResponse'),
    newStopStreamingSessionResponse,

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

    -- ** UpdateLaunchProfile
    UpdateLaunchProfile (UpdateLaunchProfile'),
    newUpdateLaunchProfile,
    UpdateLaunchProfileResponse (UpdateLaunchProfileResponse'),
    newUpdateLaunchProfileResponse,

    -- ** UpdateLaunchProfileMember
    UpdateLaunchProfileMember (UpdateLaunchProfileMember'),
    newUpdateLaunchProfileMember,
    UpdateLaunchProfileMemberResponse (UpdateLaunchProfileMemberResponse'),
    newUpdateLaunchProfileMemberResponse,

    -- ** UpdateStreamingImage
    UpdateStreamingImage (UpdateStreamingImage'),
    newUpdateStreamingImage,
    UpdateStreamingImageResponse (UpdateStreamingImageResponse'),
    newUpdateStreamingImageResponse,

    -- ** UpdateStudio
    UpdateStudio (UpdateStudio'),
    newUpdateStudio,
    UpdateStudioResponse (UpdateStudioResponse'),
    newUpdateStudioResponse,

    -- ** UpdateStudioComponent
    UpdateStudioComponent (UpdateStudioComponent'),
    newUpdateStudioComponent,
    UpdateStudioComponentResponse (UpdateStudioComponentResponse'),
    newUpdateStudioComponentResponse,

    -- * Types

    -- ** LaunchProfilePersona
    LaunchProfilePersona (..),

    -- ** LaunchProfilePlatform
    LaunchProfilePlatform (..),

    -- ** LaunchProfileState
    LaunchProfileState (..),

    -- ** LaunchProfileStatusCode
    LaunchProfileStatusCode (..),

    -- ** LaunchProfileValidationState
    LaunchProfileValidationState (..),

    -- ** LaunchProfileValidationStatusCode
    LaunchProfileValidationStatusCode (..),

    -- ** LaunchProfileValidationType
    LaunchProfileValidationType (..),

    -- ** StreamingClipboardMode
    StreamingClipboardMode (..),

    -- ** StreamingImageEncryptionConfigurationKeyType
    StreamingImageEncryptionConfigurationKeyType (..),

    -- ** StreamingImageState
    StreamingImageState (..),

    -- ** StreamingImageStatusCode
    StreamingImageStatusCode (..),

    -- ** StreamingInstanceType
    StreamingInstanceType (..),

    -- ** StreamingSessionState
    StreamingSessionState (..),

    -- ** StreamingSessionStatusCode
    StreamingSessionStatusCode (..),

    -- ** StreamingSessionStorageMode
    StreamingSessionStorageMode (..),

    -- ** StreamingSessionStreamState
    StreamingSessionStreamState (..),

    -- ** StreamingSessionStreamStatusCode
    StreamingSessionStreamStatusCode (..),

    -- ** StudioComponentInitializationScriptRunContext
    StudioComponentInitializationScriptRunContext (..),

    -- ** StudioComponentState
    StudioComponentState (..),

    -- ** StudioComponentStatusCode
    StudioComponentStatusCode (..),

    -- ** StudioComponentSubtype
    StudioComponentSubtype (..),

    -- ** StudioComponentType
    StudioComponentType (..),

    -- ** StudioEncryptionConfigurationKeyType
    StudioEncryptionConfigurationKeyType (..),

    -- ** StudioPersona
    StudioPersona (..),

    -- ** StudioState
    StudioState (..),

    -- ** StudioStatusCode
    StudioStatusCode (..),

    -- ** ActiveDirectoryComputerAttribute
    ActiveDirectoryComputerAttribute (ActiveDirectoryComputerAttribute'),
    newActiveDirectoryComputerAttribute,

    -- ** ActiveDirectoryConfiguration
    ActiveDirectoryConfiguration (ActiveDirectoryConfiguration'),
    newActiveDirectoryConfiguration,

    -- ** ComputeFarmConfiguration
    ComputeFarmConfiguration (ComputeFarmConfiguration'),
    newComputeFarmConfiguration,

    -- ** Eula
    Eula (Eula'),
    newEula,

    -- ** EulaAcceptance
    EulaAcceptance (EulaAcceptance'),
    newEulaAcceptance,

    -- ** LaunchProfile
    LaunchProfile (LaunchProfile'),
    newLaunchProfile,

    -- ** LaunchProfileInitialization
    LaunchProfileInitialization (LaunchProfileInitialization'),
    newLaunchProfileInitialization,

    -- ** LaunchProfileInitializationActiveDirectory
    LaunchProfileInitializationActiveDirectory (LaunchProfileInitializationActiveDirectory'),
    newLaunchProfileInitializationActiveDirectory,

    -- ** LaunchProfileInitializationScript
    LaunchProfileInitializationScript (LaunchProfileInitializationScript'),
    newLaunchProfileInitializationScript,

    -- ** LaunchProfileMembership
    LaunchProfileMembership (LaunchProfileMembership'),
    newLaunchProfileMembership,

    -- ** LicenseServiceConfiguration
    LicenseServiceConfiguration (LicenseServiceConfiguration'),
    newLicenseServiceConfiguration,

    -- ** NewLaunchProfileMember
    NewLaunchProfileMember (NewLaunchProfileMember'),
    newNewLaunchProfileMember,

    -- ** NewStudioMember
    NewStudioMember (NewStudioMember'),
    newNewStudioMember,

    -- ** ScriptParameterKeyValue
    ScriptParameterKeyValue (ScriptParameterKeyValue'),
    newScriptParameterKeyValue,

    -- ** SharedFileSystemConfiguration
    SharedFileSystemConfiguration (SharedFileSystemConfiguration'),
    newSharedFileSystemConfiguration,

    -- ** StreamConfiguration
    StreamConfiguration (StreamConfiguration'),
    newStreamConfiguration,

    -- ** StreamConfigurationCreate
    StreamConfigurationCreate (StreamConfigurationCreate'),
    newStreamConfigurationCreate,

    -- ** StreamConfigurationSessionStorage
    StreamConfigurationSessionStorage (StreamConfigurationSessionStorage'),
    newStreamConfigurationSessionStorage,

    -- ** StreamingImage
    StreamingImage (StreamingImage'),
    newStreamingImage,

    -- ** StreamingImageEncryptionConfiguration
    StreamingImageEncryptionConfiguration (StreamingImageEncryptionConfiguration'),
    newStreamingImageEncryptionConfiguration,

    -- ** StreamingSession
    StreamingSession (StreamingSession'),
    newStreamingSession,

    -- ** StreamingSessionStorageRoot
    StreamingSessionStorageRoot (StreamingSessionStorageRoot'),
    newStreamingSessionStorageRoot,

    -- ** StreamingSessionStream
    StreamingSessionStream (StreamingSessionStream'),
    newStreamingSessionStream,

    -- ** Studio
    Studio (Studio'),
    newStudio,

    -- ** StudioComponent
    StudioComponent (StudioComponent'),
    newStudioComponent,

    -- ** StudioComponentConfiguration
    StudioComponentConfiguration (StudioComponentConfiguration'),
    newStudioComponentConfiguration,

    -- ** StudioComponentInitializationScript
    StudioComponentInitializationScript (StudioComponentInitializationScript'),
    newStudioComponentInitializationScript,

    -- ** StudioComponentSummary
    StudioComponentSummary (StudioComponentSummary'),
    newStudioComponentSummary,

    -- ** StudioEncryptionConfiguration
    StudioEncryptionConfiguration (StudioEncryptionConfiguration'),
    newStudioEncryptionConfiguration,

    -- ** StudioMembership
    StudioMembership (StudioMembership'),
    newStudioMembership,

    -- ** ValidationResult
    ValidationResult (ValidationResult'),
    newValidationResult,
  )
where

import Amazonka.Nimble.AcceptEulas
import Amazonka.Nimble.CreateLaunchProfile
import Amazonka.Nimble.CreateStreamingImage
import Amazonka.Nimble.CreateStreamingSession
import Amazonka.Nimble.CreateStreamingSessionStream
import Amazonka.Nimble.CreateStudio
import Amazonka.Nimble.CreateStudioComponent
import Amazonka.Nimble.DeleteLaunchProfile
import Amazonka.Nimble.DeleteLaunchProfileMember
import Amazonka.Nimble.DeleteStreamingImage
import Amazonka.Nimble.DeleteStreamingSession
import Amazonka.Nimble.DeleteStudio
import Amazonka.Nimble.DeleteStudioComponent
import Amazonka.Nimble.DeleteStudioMember
import Amazonka.Nimble.GetEula
import Amazonka.Nimble.GetLaunchProfile
import Amazonka.Nimble.GetLaunchProfileDetails
import Amazonka.Nimble.GetLaunchProfileInitialization
import Amazonka.Nimble.GetLaunchProfileMember
import Amazonka.Nimble.GetStreamingImage
import Amazonka.Nimble.GetStreamingSession
import Amazonka.Nimble.GetStreamingSessionStream
import Amazonka.Nimble.GetStudio
import Amazonka.Nimble.GetStudioComponent
import Amazonka.Nimble.GetStudioMember
import Amazonka.Nimble.Lens
import Amazonka.Nimble.ListEulaAcceptances
import Amazonka.Nimble.ListEulas
import Amazonka.Nimble.ListLaunchProfileMembers
import Amazonka.Nimble.ListLaunchProfiles
import Amazonka.Nimble.ListStreamingImages
import Amazonka.Nimble.ListStreamingSessions
import Amazonka.Nimble.ListStudioComponents
import Amazonka.Nimble.ListStudioMembers
import Amazonka.Nimble.ListStudios
import Amazonka.Nimble.ListTagsForResource
import Amazonka.Nimble.PutLaunchProfileMembers
import Amazonka.Nimble.PutStudioMembers
import Amazonka.Nimble.StartStreamingSession
import Amazonka.Nimble.StartStudioSSOConfigurationRepair
import Amazonka.Nimble.StopStreamingSession
import Amazonka.Nimble.TagResource
import Amazonka.Nimble.Types
import Amazonka.Nimble.UntagResource
import Amazonka.Nimble.UpdateLaunchProfile
import Amazonka.Nimble.UpdateLaunchProfileMember
import Amazonka.Nimble.UpdateStreamingImage
import Amazonka.Nimble.UpdateStudio
import Amazonka.Nimble.UpdateStudioComponent
import Amazonka.Nimble.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Nimble'.

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
