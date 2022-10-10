{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Nimble.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Lens
  ( -- * Operations

    -- ** AcceptEulas
    acceptEulas_clientToken,
    acceptEulas_eulaIds,
    acceptEulas_studioId,
    acceptEulasResponse_eulaAcceptances,
    acceptEulasResponse_httpStatus,

    -- ** CreateLaunchProfile
    createLaunchProfile_tags,
    createLaunchProfile_clientToken,
    createLaunchProfile_description,
    createLaunchProfile_ec2SubnetIds,
    createLaunchProfile_launchProfileProtocolVersions,
    createLaunchProfile_name,
    createLaunchProfile_streamConfiguration,
    createLaunchProfile_studioComponentIds,
    createLaunchProfile_studioId,
    createLaunchProfileResponse_launchProfile,
    createLaunchProfileResponse_httpStatus,

    -- ** CreateStreamingImage
    createStreamingImage_tags,
    createStreamingImage_clientToken,
    createStreamingImage_description,
    createStreamingImage_ec2ImageId,
    createStreamingImage_name,
    createStreamingImage_studioId,
    createStreamingImageResponse_streamingImage,
    createStreamingImageResponse_httpStatus,

    -- ** CreateStreamingSession
    createStreamingSession_tags,
    createStreamingSession_launchProfileId,
    createStreamingSession_streamingImageId,
    createStreamingSession_clientToken,
    createStreamingSession_ec2InstanceType,
    createStreamingSession_ownedBy,
    createStreamingSession_studioId,
    createStreamingSessionResponse_session,
    createStreamingSessionResponse_httpStatus,

    -- ** CreateStreamingSessionStream
    createStreamingSessionStream_clientToken,
    createStreamingSessionStream_expirationInSeconds,
    createStreamingSessionStream_sessionId,
    createStreamingSessionStream_studioId,
    createStreamingSessionStreamResponse_stream,
    createStreamingSessionStreamResponse_httpStatus,

    -- ** CreateStudio
    createStudio_tags,
    createStudio_clientToken,
    createStudio_studioEncryptionConfiguration,
    createStudio_adminRoleArn,
    createStudio_displayName,
    createStudio_studioName,
    createStudio_userRoleArn,
    createStudioResponse_studio,
    createStudioResponse_httpStatus,

    -- ** CreateStudioComponent
    createStudioComponent_tags,
    createStudioComponent_scriptParameters,
    createStudioComponent_clientToken,
    createStudioComponent_initializationScripts,
    createStudioComponent_configuration,
    createStudioComponent_description,
    createStudioComponent_secureInitializationRoleArn,
    createStudioComponent_subtype,
    createStudioComponent_runtimeRoleArn,
    createStudioComponent_ec2SecurityGroupIds,
    createStudioComponent_name,
    createStudioComponent_studioId,
    createStudioComponent_type,
    createStudioComponentResponse_studioComponent,
    createStudioComponentResponse_httpStatus,

    -- ** DeleteLaunchProfile
    deleteLaunchProfile_clientToken,
    deleteLaunchProfile_launchProfileId,
    deleteLaunchProfile_studioId,
    deleteLaunchProfileResponse_launchProfile,
    deleteLaunchProfileResponse_httpStatus,

    -- ** DeleteLaunchProfileMember
    deleteLaunchProfileMember_clientToken,
    deleteLaunchProfileMember_launchProfileId,
    deleteLaunchProfileMember_principalId,
    deleteLaunchProfileMember_studioId,
    deleteLaunchProfileMemberResponse_httpStatus,

    -- ** DeleteStreamingImage
    deleteStreamingImage_clientToken,
    deleteStreamingImage_streamingImageId,
    deleteStreamingImage_studioId,
    deleteStreamingImageResponse_streamingImage,
    deleteStreamingImageResponse_httpStatus,

    -- ** DeleteStreamingSession
    deleteStreamingSession_clientToken,
    deleteStreamingSession_sessionId,
    deleteStreamingSession_studioId,
    deleteStreamingSessionResponse_session,
    deleteStreamingSessionResponse_httpStatus,

    -- ** DeleteStudio
    deleteStudio_clientToken,
    deleteStudio_studioId,
    deleteStudioResponse_httpStatus,
    deleteStudioResponse_studio,

    -- ** DeleteStudioComponent
    deleteStudioComponent_clientToken,
    deleteStudioComponent_studioComponentId,
    deleteStudioComponent_studioId,
    deleteStudioComponentResponse_studioComponent,
    deleteStudioComponentResponse_httpStatus,

    -- ** DeleteStudioMember
    deleteStudioMember_clientToken,
    deleteStudioMember_principalId,
    deleteStudioMember_studioId,
    deleteStudioMemberResponse_httpStatus,

    -- ** GetEula
    getEula_eulaId,
    getEulaResponse_eula,
    getEulaResponse_httpStatus,

    -- ** GetLaunchProfile
    getLaunchProfile_launchProfileId,
    getLaunchProfile_studioId,
    getLaunchProfileResponse_launchProfile,
    getLaunchProfileResponse_httpStatus,

    -- ** GetLaunchProfileDetails
    getLaunchProfileDetails_launchProfileId,
    getLaunchProfileDetails_studioId,
    getLaunchProfileDetailsResponse_launchProfile,
    getLaunchProfileDetailsResponse_studioComponentSummaries,
    getLaunchProfileDetailsResponse_streamingImages,
    getLaunchProfileDetailsResponse_httpStatus,

    -- ** GetLaunchProfileInitialization
    getLaunchProfileInitialization_launchProfileId,
    getLaunchProfileInitialization_launchProfileProtocolVersions,
    getLaunchProfileInitialization_launchPurpose,
    getLaunchProfileInitialization_platform,
    getLaunchProfileInitialization_studioId,
    getLaunchProfileInitializationResponse_launchProfileInitialization,
    getLaunchProfileInitializationResponse_httpStatus,

    -- ** GetLaunchProfileMember
    getLaunchProfileMember_launchProfileId,
    getLaunchProfileMember_principalId,
    getLaunchProfileMember_studioId,
    getLaunchProfileMemberResponse_member,
    getLaunchProfileMemberResponse_httpStatus,

    -- ** GetStreamingImage
    getStreamingImage_streamingImageId,
    getStreamingImage_studioId,
    getStreamingImageResponse_streamingImage,
    getStreamingImageResponse_httpStatus,

    -- ** GetStreamingSession
    getStreamingSession_sessionId,
    getStreamingSession_studioId,
    getStreamingSessionResponse_session,
    getStreamingSessionResponse_httpStatus,

    -- ** GetStreamingSessionStream
    getStreamingSessionStream_sessionId,
    getStreamingSessionStream_streamId,
    getStreamingSessionStream_studioId,
    getStreamingSessionStreamResponse_stream,
    getStreamingSessionStreamResponse_httpStatus,

    -- ** GetStudio
    getStudio_studioId,
    getStudioResponse_httpStatus,
    getStudioResponse_studio,

    -- ** GetStudioComponent
    getStudioComponent_studioComponentId,
    getStudioComponent_studioId,
    getStudioComponentResponse_studioComponent,
    getStudioComponentResponse_httpStatus,

    -- ** GetStudioMember
    getStudioMember_principalId,
    getStudioMember_studioId,
    getStudioMemberResponse_member,
    getStudioMemberResponse_httpStatus,

    -- ** ListEulaAcceptances
    listEulaAcceptances_nextToken,
    listEulaAcceptances_eulaIds,
    listEulaAcceptances_studioId,
    listEulaAcceptancesResponse_nextToken,
    listEulaAcceptancesResponse_eulaAcceptances,
    listEulaAcceptancesResponse_httpStatus,

    -- ** ListEulas
    listEulas_nextToken,
    listEulas_eulaIds,
    listEulasResponse_eulas,
    listEulasResponse_nextToken,
    listEulasResponse_httpStatus,

    -- ** ListLaunchProfileMembers
    listLaunchProfileMembers_nextToken,
    listLaunchProfileMembers_maxResults,
    listLaunchProfileMembers_launchProfileId,
    listLaunchProfileMembers_studioId,
    listLaunchProfileMembersResponse_nextToken,
    listLaunchProfileMembersResponse_members,
    listLaunchProfileMembersResponse_httpStatus,

    -- ** ListLaunchProfiles
    listLaunchProfiles_principalId,
    listLaunchProfiles_nextToken,
    listLaunchProfiles_maxResults,
    listLaunchProfiles_states,
    listLaunchProfiles_studioId,
    listLaunchProfilesResponse_nextToken,
    listLaunchProfilesResponse_launchProfiles,
    listLaunchProfilesResponse_httpStatus,

    -- ** ListStreamingImages
    listStreamingImages_nextToken,
    listStreamingImages_owner,
    listStreamingImages_studioId,
    listStreamingImagesResponse_nextToken,
    listStreamingImagesResponse_streamingImages,
    listStreamingImagesResponse_httpStatus,

    -- ** ListStreamingSessions
    listStreamingSessions_nextToken,
    listStreamingSessions_sessionIds,
    listStreamingSessions_createdBy,
    listStreamingSessions_ownedBy,
    listStreamingSessions_studioId,
    listStreamingSessionsResponse_nextToken,
    listStreamingSessionsResponse_sessions,
    listStreamingSessionsResponse_httpStatus,

    -- ** ListStudioComponents
    listStudioComponents_nextToken,
    listStudioComponents_types,
    listStudioComponents_maxResults,
    listStudioComponents_states,
    listStudioComponents_studioId,
    listStudioComponentsResponse_nextToken,
    listStudioComponentsResponse_studioComponents,
    listStudioComponentsResponse_httpStatus,

    -- ** ListStudioMembers
    listStudioMembers_nextToken,
    listStudioMembers_maxResults,
    listStudioMembers_studioId,
    listStudioMembersResponse_nextToken,
    listStudioMembersResponse_members,
    listStudioMembersResponse_httpStatus,

    -- ** ListStudios
    listStudios_nextToken,
    listStudiosResponse_nextToken,
    listStudiosResponse_httpStatus,
    listStudiosResponse_studios,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutLaunchProfileMembers
    putLaunchProfileMembers_clientToken,
    putLaunchProfileMembers_identityStoreId,
    putLaunchProfileMembers_launchProfileId,
    putLaunchProfileMembers_members,
    putLaunchProfileMembers_studioId,
    putLaunchProfileMembersResponse_httpStatus,

    -- ** PutStudioMembers
    putStudioMembers_clientToken,
    putStudioMembers_identityStoreId,
    putStudioMembers_members,
    putStudioMembers_studioId,
    putStudioMembersResponse_httpStatus,

    -- ** StartStreamingSession
    startStreamingSession_clientToken,
    startStreamingSession_sessionId,
    startStreamingSession_studioId,
    startStreamingSessionResponse_session,
    startStreamingSessionResponse_httpStatus,

    -- ** StartStudioSSOConfigurationRepair
    startStudioSSOConfigurationRepair_clientToken,
    startStudioSSOConfigurationRepair_studioId,
    startStudioSSOConfigurationRepairResponse_httpStatus,
    startStudioSSOConfigurationRepairResponse_studio,

    -- ** StopStreamingSession
    stopStreamingSession_clientToken,
    stopStreamingSession_sessionId,
    stopStreamingSession_studioId,
    stopStreamingSessionResponse_session,
    stopStreamingSessionResponse_httpStatus,

    -- ** TagResource
    tagResource_tags,
    tagResource_resourceArn,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateLaunchProfile
    updateLaunchProfile_studioComponentIds,
    updateLaunchProfile_name,
    updateLaunchProfile_clientToken,
    updateLaunchProfile_description,
    updateLaunchProfile_streamConfiguration,
    updateLaunchProfile_launchProfileProtocolVersions,
    updateLaunchProfile_launchProfileId,
    updateLaunchProfile_studioId,
    updateLaunchProfileResponse_launchProfile,
    updateLaunchProfileResponse_httpStatus,

    -- ** UpdateLaunchProfileMember
    updateLaunchProfileMember_clientToken,
    updateLaunchProfileMember_launchProfileId,
    updateLaunchProfileMember_persona,
    updateLaunchProfileMember_principalId,
    updateLaunchProfileMember_studioId,
    updateLaunchProfileMemberResponse_member,
    updateLaunchProfileMemberResponse_httpStatus,

    -- ** UpdateStreamingImage
    updateStreamingImage_name,
    updateStreamingImage_clientToken,
    updateStreamingImage_description,
    updateStreamingImage_streamingImageId,
    updateStreamingImage_studioId,
    updateStreamingImageResponse_streamingImage,
    updateStreamingImageResponse_httpStatus,

    -- ** UpdateStudio
    updateStudio_clientToken,
    updateStudio_adminRoleArn,
    updateStudio_displayName,
    updateStudio_userRoleArn,
    updateStudio_studioId,
    updateStudioResponse_httpStatus,
    updateStudioResponse_studio,

    -- ** UpdateStudioComponent
    updateStudioComponent_scriptParameters,
    updateStudioComponent_name,
    updateStudioComponent_clientToken,
    updateStudioComponent_type,
    updateStudioComponent_initializationScripts,
    updateStudioComponent_configuration,
    updateStudioComponent_description,
    updateStudioComponent_secureInitializationRoleArn,
    updateStudioComponent_subtype,
    updateStudioComponent_runtimeRoleArn,
    updateStudioComponent_ec2SecurityGroupIds,
    updateStudioComponent_studioComponentId,
    updateStudioComponent_studioId,
    updateStudioComponentResponse_studioComponent,
    updateStudioComponentResponse_httpStatus,

    -- * Types

    -- ** ActiveDirectoryComputerAttribute
    activeDirectoryComputerAttribute_name,
    activeDirectoryComputerAttribute_value,

    -- ** ActiveDirectoryConfiguration
    activeDirectoryConfiguration_directoryId,
    activeDirectoryConfiguration_computerAttributes,
    activeDirectoryConfiguration_organizationalUnitDistinguishedName,

    -- ** ComputeFarmConfiguration
    computeFarmConfiguration_activeDirectoryUser,
    computeFarmConfiguration_endpoint,

    -- ** Eula
    eula_name,
    eula_eulaId,
    eula_content,
    eula_createdAt,
    eula_updatedAt,

    -- ** EulaAcceptance
    eulaAcceptance_eulaAcceptanceId,
    eulaAcceptance_accepteeId,
    eulaAcceptance_eulaId,
    eulaAcceptance_acceptedAt,
    eulaAcceptance_acceptedBy,

    -- ** LaunchProfile
    launchProfile_ec2SubnetIds,
    launchProfile_tags,
    launchProfile_launchProfileId,
    launchProfile_studioComponentIds,
    launchProfile_name,
    launchProfile_updatedBy,
    launchProfile_arn,
    launchProfile_state,
    launchProfile_description,
    launchProfile_streamConfiguration,
    launchProfile_validationResults,
    launchProfile_launchProfileProtocolVersions,
    launchProfile_statusCode,
    launchProfile_createdBy,
    launchProfile_statusMessage,
    launchProfile_createdAt,
    launchProfile_updatedAt,

    -- ** LaunchProfileInitialization
    launchProfileInitialization_launchProfileId,
    launchProfileInitialization_name,
    launchProfileInitialization_platform,
    launchProfileInitialization_userInitializationScripts,
    launchProfileInitialization_activeDirectory,
    launchProfileInitialization_launchPurpose,
    launchProfileInitialization_launchProfileProtocolVersion,
    launchProfileInitialization_ec2SecurityGroupIds,
    launchProfileInitialization_systemInitializationScripts,

    -- ** LaunchProfileInitializationActiveDirectory
    launchProfileInitializationActiveDirectory_directoryId,
    launchProfileInitializationActiveDirectory_directoryName,
    launchProfileInitializationActiveDirectory_computerAttributes,
    launchProfileInitializationActiveDirectory_studioComponentId,
    launchProfileInitializationActiveDirectory_organizationalUnitDistinguishedName,
    launchProfileInitializationActiveDirectory_dnsIpAddresses,
    launchProfileInitializationActiveDirectory_studioComponentName,

    -- ** LaunchProfileInitializationScript
    launchProfileInitializationScript_secureInitializationRoleArn,
    launchProfileInitializationScript_studioComponentId,
    launchProfileInitializationScript_runtimeRoleArn,
    launchProfileInitializationScript_studioComponentName,
    launchProfileInitializationScript_script,

    -- ** LaunchProfileMembership
    launchProfileMembership_principalId,
    launchProfileMembership_persona,
    launchProfileMembership_sid,
    launchProfileMembership_identityStoreId,

    -- ** LicenseServiceConfiguration
    licenseServiceConfiguration_endpoint,

    -- ** NewLaunchProfileMember
    newLaunchProfileMember_persona,
    newLaunchProfileMember_principalId,

    -- ** NewStudioMember
    newStudioMember_persona,
    newStudioMember_principalId,

    -- ** ScriptParameterKeyValue
    scriptParameterKeyValue_key,
    scriptParameterKeyValue_value,

    -- ** SharedFileSystemConfiguration
    sharedFileSystemConfiguration_windowsMountDrive,
    sharedFileSystemConfiguration_linuxMountPoint,
    sharedFileSystemConfiguration_fileSystemId,
    sharedFileSystemConfiguration_shareName,
    sharedFileSystemConfiguration_endpoint,

    -- ** StreamConfiguration
    streamConfiguration_maxStoppedSessionLengthInMinutes,
    streamConfiguration_maxSessionLengthInMinutes,
    streamConfiguration_sessionStorage,
    streamConfiguration_clipboardMode,
    streamConfiguration_ec2InstanceTypes,
    streamConfiguration_streamingImageIds,

    -- ** StreamConfigurationCreate
    streamConfigurationCreate_maxStoppedSessionLengthInMinutes,
    streamConfigurationCreate_maxSessionLengthInMinutes,
    streamConfigurationCreate_sessionStorage,
    streamConfigurationCreate_clipboardMode,
    streamConfigurationCreate_ec2InstanceTypes,
    streamConfigurationCreate_streamingImageIds,

    -- ** StreamConfigurationSessionStorage
    streamConfigurationSessionStorage_root,
    streamConfigurationSessionStorage_mode,

    -- ** StreamingImage
    streamingImage_tags,
    streamingImage_streamingImageId,
    streamingImage_name,
    streamingImage_ec2ImageId,
    streamingImage_arn,
    streamingImage_state,
    streamingImage_owner,
    streamingImage_description,
    streamingImage_platform,
    streamingImage_eulaIds,
    streamingImage_encryptionConfiguration,
    streamingImage_statusCode,
    streamingImage_statusMessage,

    -- ** StreamingImageEncryptionConfiguration
    streamingImageEncryptionConfiguration_keyArn,
    streamingImageEncryptionConfiguration_keyType,

    -- ** StreamingSession
    streamingSession_tags,
    streamingSession_launchProfileId,
    streamingSession_streamingImageId,
    streamingSession_stopAt,
    streamingSession_updatedBy,
    streamingSession_ec2InstanceType,
    streamingSession_terminateAt,
    streamingSession_stoppedBy,
    streamingSession_arn,
    streamingSession_state,
    streamingSession_startedBy,
    streamingSession_startedAt,
    streamingSession_sessionId,
    streamingSession_stoppedAt,
    streamingSession_statusCode,
    streamingSession_createdBy,
    streamingSession_statusMessage,
    streamingSession_ownedBy,
    streamingSession_createdAt,
    streamingSession_updatedAt,

    -- ** StreamingSessionStorageRoot
    streamingSessionStorageRoot_windows,
    streamingSessionStorageRoot_linux,

    -- ** StreamingSessionStream
    streamingSessionStream_state,
    streamingSessionStream_streamId,
    streamingSessionStream_url,
    streamingSessionStream_expiresAt,
    streamingSessionStream_statusCode,
    streamingSessionStream_createdBy,
    streamingSessionStream_ownedBy,
    streamingSessionStream_createdAt,

    -- ** Studio
    studio_studioId,
    studio_tags,
    studio_studioName,
    studio_ssoClientId,
    studio_adminRoleArn,
    studio_arn,
    studio_displayName,
    studio_state,
    studio_studioEncryptionConfiguration,
    studio_studioUrl,
    studio_userRoleArn,
    studio_homeRegion,
    studio_statusCode,
    studio_statusMessage,
    studio_createdAt,
    studio_updatedAt,

    -- ** StudioComponent
    studioComponent_tags,
    studioComponent_scriptParameters,
    studioComponent_name,
    studioComponent_type,
    studioComponent_updatedBy,
    studioComponent_initializationScripts,
    studioComponent_configuration,
    studioComponent_arn,
    studioComponent_state,
    studioComponent_description,
    studioComponent_secureInitializationRoleArn,
    studioComponent_studioComponentId,
    studioComponent_subtype,
    studioComponent_runtimeRoleArn,
    studioComponent_statusCode,
    studioComponent_createdBy,
    studioComponent_statusMessage,
    studioComponent_ec2SecurityGroupIds,
    studioComponent_createdAt,
    studioComponent_updatedAt,

    -- ** StudioComponentConfiguration
    studioComponentConfiguration_activeDirectoryConfiguration,
    studioComponentConfiguration_licenseServiceConfiguration,
    studioComponentConfiguration_computeFarmConfiguration,
    studioComponentConfiguration_sharedFileSystemConfiguration,

    -- ** StudioComponentInitializationScript
    studioComponentInitializationScript_platform,
    studioComponentInitializationScript_launchProfileProtocolVersion,
    studioComponentInitializationScript_runContext,
    studioComponentInitializationScript_script,

    -- ** StudioComponentSummary
    studioComponentSummary_name,
    studioComponentSummary_type,
    studioComponentSummary_updatedBy,
    studioComponentSummary_description,
    studioComponentSummary_studioComponentId,
    studioComponentSummary_subtype,
    studioComponentSummary_createdBy,
    studioComponentSummary_createdAt,
    studioComponentSummary_updatedAt,

    -- ** StudioEncryptionConfiguration
    studioEncryptionConfiguration_keyArn,
    studioEncryptionConfiguration_keyType,

    -- ** StudioMembership
    studioMembership_principalId,
    studioMembership_persona,
    studioMembership_sid,
    studioMembership_identityStoreId,

    -- ** ValidationResult
    validationResult_state,
    validationResult_statusCode,
    validationResult_statusMessage,
    validationResult_type,
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
import Amazonka.Nimble.Types.ActiveDirectoryComputerAttribute
import Amazonka.Nimble.Types.ActiveDirectoryConfiguration
import Amazonka.Nimble.Types.ComputeFarmConfiguration
import Amazonka.Nimble.Types.Eula
import Amazonka.Nimble.Types.EulaAcceptance
import Amazonka.Nimble.Types.LaunchProfile
import Amazonka.Nimble.Types.LaunchProfileInitialization
import Amazonka.Nimble.Types.LaunchProfileInitializationActiveDirectory
import Amazonka.Nimble.Types.LaunchProfileInitializationScript
import Amazonka.Nimble.Types.LaunchProfileMembership
import Amazonka.Nimble.Types.LicenseServiceConfiguration
import Amazonka.Nimble.Types.NewLaunchProfileMember
import Amazonka.Nimble.Types.NewStudioMember
import Amazonka.Nimble.Types.ScriptParameterKeyValue
import Amazonka.Nimble.Types.SharedFileSystemConfiguration
import Amazonka.Nimble.Types.StreamConfiguration
import Amazonka.Nimble.Types.StreamConfigurationCreate
import Amazonka.Nimble.Types.StreamConfigurationSessionStorage
import Amazonka.Nimble.Types.StreamingImage
import Amazonka.Nimble.Types.StreamingImageEncryptionConfiguration
import Amazonka.Nimble.Types.StreamingSession
import Amazonka.Nimble.Types.StreamingSessionStorageRoot
import Amazonka.Nimble.Types.StreamingSessionStream
import Amazonka.Nimble.Types.Studio
import Amazonka.Nimble.Types.StudioComponent
import Amazonka.Nimble.Types.StudioComponentConfiguration
import Amazonka.Nimble.Types.StudioComponentInitializationScript
import Amazonka.Nimble.Types.StudioComponentSummary
import Amazonka.Nimble.Types.StudioEncryptionConfiguration
import Amazonka.Nimble.Types.StudioMembership
import Amazonka.Nimble.Types.ValidationResult
import Amazonka.Nimble.UntagResource
import Amazonka.Nimble.UpdateLaunchProfile
import Amazonka.Nimble.UpdateLaunchProfileMember
import Amazonka.Nimble.UpdateStreamingImage
import Amazonka.Nimble.UpdateStudio
import Amazonka.Nimble.UpdateStudioComponent
