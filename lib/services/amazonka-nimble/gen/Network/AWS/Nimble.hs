{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Nimble
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Nimble
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutLaunchProfileMembers
    PutLaunchProfileMembers (PutLaunchProfileMembers'),
    newPutLaunchProfileMembers,
    PutLaunchProfileMembersResponse (PutLaunchProfileMembersResponse'),
    newPutLaunchProfileMembersResponse,

    -- ** CreateStudio
    CreateStudio (CreateStudio'),
    newCreateStudio,
    CreateStudioResponse (CreateStudioResponse'),
    newCreateStudioResponse,

    -- ** UpdateLaunchProfileMember
    UpdateLaunchProfileMember (UpdateLaunchProfileMember'),
    newUpdateLaunchProfileMember,
    UpdateLaunchProfileMemberResponse (UpdateLaunchProfileMemberResponse'),
    newUpdateLaunchProfileMemberResponse,

    -- ** DeleteLaunchProfileMember
    DeleteLaunchProfileMember (DeleteLaunchProfileMember'),
    newDeleteLaunchProfileMember,
    DeleteLaunchProfileMemberResponse (DeleteLaunchProfileMemberResponse'),
    newDeleteLaunchProfileMemberResponse,

    -- ** ListLaunchProfiles (Paginated)
    ListLaunchProfiles (ListLaunchProfiles'),
    newListLaunchProfiles,
    ListLaunchProfilesResponse (ListLaunchProfilesResponse'),
    newListLaunchProfilesResponse,

    -- ** CreateLaunchProfile
    CreateLaunchProfile (CreateLaunchProfile'),
    newCreateLaunchProfile,
    CreateLaunchProfileResponse (CreateLaunchProfileResponse'),
    newCreateLaunchProfileResponse,

    -- ** ListStreamingImages (Paginated)
    ListStreamingImages (ListStreamingImages'),
    newListStreamingImages,
    ListStreamingImagesResponse (ListStreamingImagesResponse'),
    newListStreamingImagesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartStudioSSOConfigurationRepair
    StartStudioSSOConfigurationRepair (StartStudioSSOConfigurationRepair'),
    newStartStudioSSOConfigurationRepair,
    StartStudioSSOConfigurationRepairResponse (StartStudioSSOConfigurationRepairResponse'),
    newStartStudioSSOConfigurationRepairResponse,

    -- ** GetLaunchProfileInitialization
    GetLaunchProfileInitialization (GetLaunchProfileInitialization'),
    newGetLaunchProfileInitialization,
    GetLaunchProfileInitializationResponse (GetLaunchProfileInitializationResponse'),
    newGetLaunchProfileInitializationResponse,

    -- ** GetLaunchProfile
    GetLaunchProfile (GetLaunchProfile'),
    newGetLaunchProfile,
    GetLaunchProfileResponse (GetLaunchProfileResponse'),
    newGetLaunchProfileResponse,

    -- ** CreateStudioComponent
    CreateStudioComponent (CreateStudioComponent'),
    newCreateStudioComponent,
    CreateStudioComponentResponse (CreateStudioComponentResponse'),
    newCreateStudioComponentResponse,

    -- ** GetEula
    GetEula (GetEula'),
    newGetEula,
    GetEulaResponse (GetEulaResponse'),
    newGetEulaResponse,

    -- ** ListStudioComponents (Paginated)
    ListStudioComponents (ListStudioComponents'),
    newListStudioComponents,
    ListStudioComponentsResponse (ListStudioComponentsResponse'),
    newListStudioComponentsResponse,

    -- ** AcceptEulas
    AcceptEulas (AcceptEulas'),
    newAcceptEulas,
    AcceptEulasResponse (AcceptEulasResponse'),
    newAcceptEulasResponse,

    -- ** CreateStreamingSession
    CreateStreamingSession (CreateStreamingSession'),
    newCreateStreamingSession,
    CreateStreamingSessionResponse (CreateStreamingSessionResponse'),
    newCreateStreamingSessionResponse,

    -- ** DeleteStudioComponent
    DeleteStudioComponent (DeleteStudioComponent'),
    newDeleteStudioComponent,
    DeleteStudioComponentResponse (DeleteStudioComponentResponse'),
    newDeleteStudioComponentResponse,

    -- ** UpdateStudioComponent
    UpdateStudioComponent (UpdateStudioComponent'),
    newUpdateStudioComponent,
    UpdateStudioComponentResponse (UpdateStudioComponentResponse'),
    newUpdateStudioComponentResponse,

    -- ** GetStudioMember
    GetStudioMember (GetStudioMember'),
    newGetStudioMember,
    GetStudioMemberResponse (GetStudioMemberResponse'),
    newGetStudioMemberResponse,

    -- ** DeleteStudio
    DeleteStudio (DeleteStudio'),
    newDeleteStudio,
    DeleteStudioResponse (DeleteStudioResponse'),
    newDeleteStudioResponse,

    -- ** UpdateStudio
    UpdateStudio (UpdateStudio'),
    newUpdateStudio,
    UpdateStudioResponse (UpdateStudioResponse'),
    newUpdateStudioResponse,

    -- ** ListStudios (Paginated)
    ListStudios (ListStudios'),
    newListStudios,
    ListStudiosResponse (ListStudiosResponse'),
    newListStudiosResponse,

    -- ** GetStudioComponent
    GetStudioComponent (GetStudioComponent'),
    newGetStudioComponent,
    GetStudioComponentResponse (GetStudioComponentResponse'),
    newGetStudioComponentResponse,

    -- ** ListEulas (Paginated)
    ListEulas (ListEulas'),
    newListEulas,
    ListEulasResponse (ListEulasResponse'),
    newListEulasResponse,

    -- ** GetStreamingSession
    GetStreamingSession (GetStreamingSession'),
    newGetStreamingSession,
    GetStreamingSessionResponse (GetStreamingSessionResponse'),
    newGetStreamingSessionResponse,

    -- ** ListLaunchProfileMembers (Paginated)
    ListLaunchProfileMembers (ListLaunchProfileMembers'),
    newListLaunchProfileMembers,
    ListLaunchProfileMembersResponse (ListLaunchProfileMembersResponse'),
    newListLaunchProfileMembersResponse,

    -- ** DeleteLaunchProfile
    DeleteLaunchProfile (DeleteLaunchProfile'),
    newDeleteLaunchProfile,
    DeleteLaunchProfileResponse (DeleteLaunchProfileResponse'),
    newDeleteLaunchProfileResponse,

    -- ** UpdateLaunchProfile
    UpdateLaunchProfile (UpdateLaunchProfile'),
    newUpdateLaunchProfile,
    UpdateLaunchProfileResponse (UpdateLaunchProfileResponse'),
    newUpdateLaunchProfileResponse,

    -- ** CreateStreamingImage
    CreateStreamingImage (CreateStreamingImage'),
    newCreateStreamingImage,
    CreateStreamingImageResponse (CreateStreamingImageResponse'),
    newCreateStreamingImageResponse,

    -- ** CreateStreamingSessionStream
    CreateStreamingSessionStream (CreateStreamingSessionStream'),
    newCreateStreamingSessionStream,
    CreateStreamingSessionStreamResponse (CreateStreamingSessionStreamResponse'),
    newCreateStreamingSessionStreamResponse,

    -- ** GetLaunchProfileDetails
    GetLaunchProfileDetails (GetLaunchProfileDetails'),
    newGetLaunchProfileDetails,
    GetLaunchProfileDetailsResponse (GetLaunchProfileDetailsResponse'),
    newGetLaunchProfileDetailsResponse,

    -- ** PutStudioMembers
    PutStudioMembers (PutStudioMembers'),
    newPutStudioMembers,
    PutStudioMembersResponse (PutStudioMembersResponse'),
    newPutStudioMembersResponse,

    -- ** DeleteStreamingImage
    DeleteStreamingImage (DeleteStreamingImage'),
    newDeleteStreamingImage,
    DeleteStreamingImageResponse (DeleteStreamingImageResponse'),
    newDeleteStreamingImageResponse,

    -- ** UpdateStreamingImage
    UpdateStreamingImage (UpdateStreamingImage'),
    newUpdateStreamingImage,
    UpdateStreamingImageResponse (UpdateStreamingImageResponse'),
    newUpdateStreamingImageResponse,

    -- ** GetStreamingImage
    GetStreamingImage (GetStreamingImage'),
    newGetStreamingImage,
    GetStreamingImageResponse (GetStreamingImageResponse'),
    newGetStreamingImageResponse,

    -- ** ListEulaAcceptances (Paginated)
    ListEulaAcceptances (ListEulaAcceptances'),
    newListEulaAcceptances,
    ListEulaAcceptancesResponse (ListEulaAcceptancesResponse'),
    newListEulaAcceptancesResponse,

    -- ** GetStreamingSessionStream
    GetStreamingSessionStream (GetStreamingSessionStream'),
    newGetStreamingSessionStream,
    GetStreamingSessionStreamResponse (GetStreamingSessionStreamResponse'),
    newGetStreamingSessionStreamResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetLaunchProfileMember
    GetLaunchProfileMember (GetLaunchProfileMember'),
    newGetLaunchProfileMember,
    GetLaunchProfileMemberResponse (GetLaunchProfileMemberResponse'),
    newGetLaunchProfileMemberResponse,

    -- ** DeleteStreamingSession
    DeleteStreamingSession (DeleteStreamingSession'),
    newDeleteStreamingSession,
    DeleteStreamingSessionResponse (DeleteStreamingSessionResponse'),
    newDeleteStreamingSessionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListStreamingSessions (Paginated)
    ListStreamingSessions (ListStreamingSessions'),
    newListStreamingSessions,
    ListStreamingSessionsResponse (ListStreamingSessionsResponse'),
    newListStreamingSessionsResponse,

    -- ** GetStudio
    GetStudio (GetStudio'),
    newGetStudio,
    GetStudioResponse (GetStudioResponse'),
    newGetStudioResponse,

    -- ** ListStudioMembers (Paginated)
    ListStudioMembers (ListStudioMembers'),
    newListStudioMembers,
    ListStudioMembersResponse (ListStudioMembersResponse'),
    newListStudioMembersResponse,

    -- ** DeleteStudioMember
    DeleteStudioMember (DeleteStudioMember'),
    newDeleteStudioMember,
    DeleteStudioMemberResponse (DeleteStudioMemberResponse'),
    newDeleteStudioMemberResponse,

    -- * Types

    -- ** LaunchProfilePersona
    LaunchProfilePersona (..),

    -- ** LaunchProfilePlatform
    LaunchProfilePlatform (..),

    -- ** LaunchProfileState
    LaunchProfileState (..),

    -- ** LaunchProfileStatusCode
    LaunchProfileStatusCode (..),

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

    -- ** StreamingImage
    StreamingImage (StreamingImage'),
    newStreamingImage,

    -- ** StreamingImageEncryptionConfiguration
    StreamingImageEncryptionConfiguration (StreamingImageEncryptionConfiguration'),
    newStreamingImageEncryptionConfiguration,

    -- ** StreamingSession
    StreamingSession (StreamingSession'),
    newStreamingSession,

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
  )
where

import Network.AWS.Nimble.AcceptEulas
import Network.AWS.Nimble.CreateLaunchProfile
import Network.AWS.Nimble.CreateStreamingImage
import Network.AWS.Nimble.CreateStreamingSession
import Network.AWS.Nimble.CreateStreamingSessionStream
import Network.AWS.Nimble.CreateStudio
import Network.AWS.Nimble.CreateStudioComponent
import Network.AWS.Nimble.DeleteLaunchProfile
import Network.AWS.Nimble.DeleteLaunchProfileMember
import Network.AWS.Nimble.DeleteStreamingImage
import Network.AWS.Nimble.DeleteStreamingSession
import Network.AWS.Nimble.DeleteStudio
import Network.AWS.Nimble.DeleteStudioComponent
import Network.AWS.Nimble.DeleteStudioMember
import Network.AWS.Nimble.GetEula
import Network.AWS.Nimble.GetLaunchProfile
import Network.AWS.Nimble.GetLaunchProfileDetails
import Network.AWS.Nimble.GetLaunchProfileInitialization
import Network.AWS.Nimble.GetLaunchProfileMember
import Network.AWS.Nimble.GetStreamingImage
import Network.AWS.Nimble.GetStreamingSession
import Network.AWS.Nimble.GetStreamingSessionStream
import Network.AWS.Nimble.GetStudio
import Network.AWS.Nimble.GetStudioComponent
import Network.AWS.Nimble.GetStudioMember
import Network.AWS.Nimble.Lens
import Network.AWS.Nimble.ListEulaAcceptances
import Network.AWS.Nimble.ListEulas
import Network.AWS.Nimble.ListLaunchProfileMembers
import Network.AWS.Nimble.ListLaunchProfiles
import Network.AWS.Nimble.ListStreamingImages
import Network.AWS.Nimble.ListStreamingSessions
import Network.AWS.Nimble.ListStudioComponents
import Network.AWS.Nimble.ListStudioMembers
import Network.AWS.Nimble.ListStudios
import Network.AWS.Nimble.ListTagsForResource
import Network.AWS.Nimble.PutLaunchProfileMembers
import Network.AWS.Nimble.PutStudioMembers
import Network.AWS.Nimble.StartStudioSSOConfigurationRepair
import Network.AWS.Nimble.TagResource
import Network.AWS.Nimble.Types
import Network.AWS.Nimble.UntagResource
import Network.AWS.Nimble.UpdateLaunchProfile
import Network.AWS.Nimble.UpdateLaunchProfileMember
import Network.AWS.Nimble.UpdateStreamingImage
import Network.AWS.Nimble.UpdateStudio
import Network.AWS.Nimble.UpdateStudioComponent
import Network.AWS.Nimble.Waiters

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
