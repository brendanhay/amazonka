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
    createLaunchProfile_clientToken,
    createLaunchProfile_description,
    createLaunchProfile_tags,
    createLaunchProfile_ec2SubnetIds,
    createLaunchProfile_launchProfileProtocolVersions,
    createLaunchProfile_name,
    createLaunchProfile_streamConfiguration,
    createLaunchProfile_studioComponentIds,
    createLaunchProfile_studioId,
    createLaunchProfileResponse_launchProfile,
    createLaunchProfileResponse_httpStatus,

    -- ** CreateStreamingImage
    createStreamingImage_clientToken,
    createStreamingImage_description,
    createStreamingImage_tags,
    createStreamingImage_ec2ImageId,
    createStreamingImage_name,
    createStreamingImage_studioId,
    createStreamingImageResponse_streamingImage,
    createStreamingImageResponse_httpStatus,

    -- ** CreateStreamingSession
    createStreamingSession_clientToken,
    createStreamingSession_ec2InstanceType,
    createStreamingSession_launchProfileId,
    createStreamingSession_ownedBy,
    createStreamingSession_streamingImageId,
    createStreamingSession_tags,
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
    createStudio_clientToken,
    createStudio_studioEncryptionConfiguration,
    createStudio_tags,
    createStudio_adminRoleArn,
    createStudio_displayName,
    createStudio_studioName,
    createStudio_userRoleArn,
    createStudioResponse_studio,
    createStudioResponse_httpStatus,

    -- ** CreateStudioComponent
    createStudioComponent_clientToken,
    createStudioComponent_configuration,
    createStudioComponent_description,
    createStudioComponent_ec2SecurityGroupIds,
    createStudioComponent_initializationScripts,
    createStudioComponent_runtimeRoleArn,
    createStudioComponent_scriptParameters,
    createStudioComponent_secureInitializationRoleArn,
    createStudioComponent_subtype,
    createStudioComponent_tags,
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
    getLaunchProfileDetailsResponse_streamingImages,
    getLaunchProfileDetailsResponse_studioComponentSummaries,
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
    listEulaAcceptances_eulaIds,
    listEulaAcceptances_nextToken,
    listEulaAcceptances_studioId,
    listEulaAcceptancesResponse_eulaAcceptances,
    listEulaAcceptancesResponse_nextToken,
    listEulaAcceptancesResponse_httpStatus,

    -- ** ListEulas
    listEulas_eulaIds,
    listEulas_nextToken,
    listEulasResponse_eulas,
    listEulasResponse_nextToken,
    listEulasResponse_httpStatus,

    -- ** ListLaunchProfileMembers
    listLaunchProfileMembers_maxResults,
    listLaunchProfileMembers_nextToken,
    listLaunchProfileMembers_launchProfileId,
    listLaunchProfileMembers_studioId,
    listLaunchProfileMembersResponse_members,
    listLaunchProfileMembersResponse_nextToken,
    listLaunchProfileMembersResponse_httpStatus,

    -- ** ListLaunchProfiles
    listLaunchProfiles_maxResults,
    listLaunchProfiles_nextToken,
    listLaunchProfiles_principalId,
    listLaunchProfiles_states,
    listLaunchProfiles_studioId,
    listLaunchProfilesResponse_launchProfiles,
    listLaunchProfilesResponse_nextToken,
    listLaunchProfilesResponse_httpStatus,

    -- ** ListStreamingImages
    listStreamingImages_nextToken,
    listStreamingImages_owner,
    listStreamingImages_studioId,
    listStreamingImagesResponse_nextToken,
    listStreamingImagesResponse_streamingImages,
    listStreamingImagesResponse_httpStatus,

    -- ** ListStreamingSessions
    listStreamingSessions_createdBy,
    listStreamingSessions_nextToken,
    listStreamingSessions_ownedBy,
    listStreamingSessions_sessionIds,
    listStreamingSessions_studioId,
    listStreamingSessionsResponse_nextToken,
    listStreamingSessionsResponse_sessions,
    listStreamingSessionsResponse_httpStatus,

    -- ** ListStudioComponents
    listStudioComponents_maxResults,
    listStudioComponents_nextToken,
    listStudioComponents_states,
    listStudioComponents_types,
    listStudioComponents_studioId,
    listStudioComponentsResponse_nextToken,
    listStudioComponentsResponse_studioComponents,
    listStudioComponentsResponse_httpStatus,

    -- ** ListStudioMembers
    listStudioMembers_maxResults,
    listStudioMembers_nextToken,
    listStudioMembers_studioId,
    listStudioMembersResponse_members,
    listStudioMembersResponse_nextToken,
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
    updateLaunchProfile_clientToken,
    updateLaunchProfile_description,
    updateLaunchProfile_launchProfileProtocolVersions,
    updateLaunchProfile_name,
    updateLaunchProfile_streamConfiguration,
    updateLaunchProfile_studioComponentIds,
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
    updateStreamingImage_clientToken,
    updateStreamingImage_description,
    updateStreamingImage_name,
    updateStreamingImage_streamingImageId,
    updateStreamingImage_studioId,
    updateStreamingImageResponse_streamingImage,
    updateStreamingImageResponse_httpStatus,

    -- ** UpdateStudio
    updateStudio_adminRoleArn,
    updateStudio_clientToken,
    updateStudio_displayName,
    updateStudio_userRoleArn,
    updateStudio_studioId,
    updateStudioResponse_httpStatus,
    updateStudioResponse_studio,

    -- ** UpdateStudioComponent
    updateStudioComponent_clientToken,
    updateStudioComponent_configuration,
    updateStudioComponent_description,
    updateStudioComponent_ec2SecurityGroupIds,
    updateStudioComponent_initializationScripts,
    updateStudioComponent_name,
    updateStudioComponent_runtimeRoleArn,
    updateStudioComponent_scriptParameters,
    updateStudioComponent_secureInitializationRoleArn,
    updateStudioComponent_subtype,
    updateStudioComponent_type,
    updateStudioComponent_studioComponentId,
    updateStudioComponent_studioId,
    updateStudioComponentResponse_studioComponent,
    updateStudioComponentResponse_httpStatus,

    -- * Types

    -- ** ActiveDirectoryComputerAttribute
    activeDirectoryComputerAttribute_name,
    activeDirectoryComputerAttribute_value,

    -- ** ActiveDirectoryConfiguration
    activeDirectoryConfiguration_computerAttributes,
    activeDirectoryConfiguration_directoryId,
    activeDirectoryConfiguration_organizationalUnitDistinguishedName,

    -- ** ComputeFarmConfiguration
    computeFarmConfiguration_activeDirectoryUser,
    computeFarmConfiguration_endpoint,

    -- ** Eula
    eula_content,
    eula_createdAt,
    eula_eulaId,
    eula_name,
    eula_updatedAt,

    -- ** EulaAcceptance
    eulaAcceptance_acceptedAt,
    eulaAcceptance_acceptedBy,
    eulaAcceptance_accepteeId,
    eulaAcceptance_eulaAcceptanceId,
    eulaAcceptance_eulaId,

    -- ** LaunchProfile
    launchProfile_arn,
    launchProfile_createdAt,
    launchProfile_createdBy,
    launchProfile_description,
    launchProfile_ec2SubnetIds,
    launchProfile_launchProfileId,
    launchProfile_launchProfileProtocolVersions,
    launchProfile_name,
    launchProfile_state,
    launchProfile_statusCode,
    launchProfile_statusMessage,
    launchProfile_streamConfiguration,
    launchProfile_studioComponentIds,
    launchProfile_tags,
    launchProfile_updatedAt,
    launchProfile_updatedBy,
    launchProfile_validationResults,

    -- ** LaunchProfileInitialization
    launchProfileInitialization_activeDirectory,
    launchProfileInitialization_ec2SecurityGroupIds,
    launchProfileInitialization_launchProfileId,
    launchProfileInitialization_launchProfileProtocolVersion,
    launchProfileInitialization_launchPurpose,
    launchProfileInitialization_name,
    launchProfileInitialization_platform,
    launchProfileInitialization_systemInitializationScripts,
    launchProfileInitialization_userInitializationScripts,

    -- ** LaunchProfileInitializationActiveDirectory
    launchProfileInitializationActiveDirectory_computerAttributes,
    launchProfileInitializationActiveDirectory_directoryId,
    launchProfileInitializationActiveDirectory_directoryName,
    launchProfileInitializationActiveDirectory_dnsIpAddresses,
    launchProfileInitializationActiveDirectory_organizationalUnitDistinguishedName,
    launchProfileInitializationActiveDirectory_studioComponentId,
    launchProfileInitializationActiveDirectory_studioComponentName,

    -- ** LaunchProfileInitializationScript
    launchProfileInitializationScript_runtimeRoleArn,
    launchProfileInitializationScript_script,
    launchProfileInitializationScript_secureInitializationRoleArn,
    launchProfileInitializationScript_studioComponentId,
    launchProfileInitializationScript_studioComponentName,

    -- ** LaunchProfileMembership
    launchProfileMembership_identityStoreId,
    launchProfileMembership_persona,
    launchProfileMembership_principalId,
    launchProfileMembership_sid,

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
    sharedFileSystemConfiguration_endpoint,
    sharedFileSystemConfiguration_fileSystemId,
    sharedFileSystemConfiguration_linuxMountPoint,
    sharedFileSystemConfiguration_shareName,
    sharedFileSystemConfiguration_windowsMountDrive,

    -- ** StreamConfiguration
    streamConfiguration_maxSessionLengthInMinutes,
    streamConfiguration_maxStoppedSessionLengthInMinutes,
    streamConfiguration_sessionStorage,
    streamConfiguration_clipboardMode,
    streamConfiguration_ec2InstanceTypes,
    streamConfiguration_streamingImageIds,

    -- ** StreamConfigurationCreate
    streamConfigurationCreate_maxSessionLengthInMinutes,
    streamConfigurationCreate_maxStoppedSessionLengthInMinutes,
    streamConfigurationCreate_sessionStorage,
    streamConfigurationCreate_clipboardMode,
    streamConfigurationCreate_ec2InstanceTypes,
    streamConfigurationCreate_streamingImageIds,

    -- ** StreamConfigurationSessionStorage
    streamConfigurationSessionStorage_root,
    streamConfigurationSessionStorage_mode,

    -- ** StreamingImage
    streamingImage_arn,
    streamingImage_description,
    streamingImage_ec2ImageId,
    streamingImage_encryptionConfiguration,
    streamingImage_eulaIds,
    streamingImage_name,
    streamingImage_owner,
    streamingImage_platform,
    streamingImage_state,
    streamingImage_statusCode,
    streamingImage_statusMessage,
    streamingImage_streamingImageId,
    streamingImage_tags,

    -- ** StreamingImageEncryptionConfiguration
    streamingImageEncryptionConfiguration_keyArn,
    streamingImageEncryptionConfiguration_keyType,

    -- ** StreamingSession
    streamingSession_arn,
    streamingSession_createdAt,
    streamingSession_createdBy,
    streamingSession_ec2InstanceType,
    streamingSession_launchProfileId,
    streamingSession_ownedBy,
    streamingSession_sessionId,
    streamingSession_startedAt,
    streamingSession_startedBy,
    streamingSession_state,
    streamingSession_statusCode,
    streamingSession_statusMessage,
    streamingSession_stopAt,
    streamingSession_stoppedAt,
    streamingSession_stoppedBy,
    streamingSession_streamingImageId,
    streamingSession_tags,
    streamingSession_terminateAt,
    streamingSession_updatedAt,
    streamingSession_updatedBy,

    -- ** StreamingSessionStorageRoot
    streamingSessionStorageRoot_linux,
    streamingSessionStorageRoot_windows,

    -- ** StreamingSessionStream
    streamingSessionStream_createdAt,
    streamingSessionStream_createdBy,
    streamingSessionStream_expiresAt,
    streamingSessionStream_ownedBy,
    streamingSessionStream_state,
    streamingSessionStream_statusCode,
    streamingSessionStream_streamId,
    streamingSessionStream_url,

    -- ** Studio
    studio_adminRoleArn,
    studio_arn,
    studio_createdAt,
    studio_displayName,
    studio_homeRegion,
    studio_ssoClientId,
    studio_state,
    studio_statusCode,
    studio_statusMessage,
    studio_studioEncryptionConfiguration,
    studio_studioId,
    studio_studioName,
    studio_studioUrl,
    studio_tags,
    studio_updatedAt,
    studio_userRoleArn,

    -- ** StudioComponent
    studioComponent_arn,
    studioComponent_configuration,
    studioComponent_createdAt,
    studioComponent_createdBy,
    studioComponent_description,
    studioComponent_ec2SecurityGroupIds,
    studioComponent_initializationScripts,
    studioComponent_name,
    studioComponent_runtimeRoleArn,
    studioComponent_scriptParameters,
    studioComponent_secureInitializationRoleArn,
    studioComponent_state,
    studioComponent_statusCode,
    studioComponent_statusMessage,
    studioComponent_studioComponentId,
    studioComponent_subtype,
    studioComponent_tags,
    studioComponent_type,
    studioComponent_updatedAt,
    studioComponent_updatedBy,

    -- ** StudioComponentConfiguration
    studioComponentConfiguration_activeDirectoryConfiguration,
    studioComponentConfiguration_computeFarmConfiguration,
    studioComponentConfiguration_licenseServiceConfiguration,
    studioComponentConfiguration_sharedFileSystemConfiguration,

    -- ** StudioComponentInitializationScript
    studioComponentInitializationScript_launchProfileProtocolVersion,
    studioComponentInitializationScript_platform,
    studioComponentInitializationScript_runContext,
    studioComponentInitializationScript_script,

    -- ** StudioComponentSummary
    studioComponentSummary_createdAt,
    studioComponentSummary_createdBy,
    studioComponentSummary_description,
    studioComponentSummary_name,
    studioComponentSummary_studioComponentId,
    studioComponentSummary_subtype,
    studioComponentSummary_type,
    studioComponentSummary_updatedAt,
    studioComponentSummary_updatedBy,

    -- ** StudioEncryptionConfiguration
    studioEncryptionConfiguration_keyArn,
    studioEncryptionConfiguration_keyType,

    -- ** StudioMembership
    studioMembership_identityStoreId,
    studioMembership_persona,
    studioMembership_principalId,
    studioMembership_sid,

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
