{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Nimble.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Nimble.Lens
  ( -- * Operations

    -- ** PutLaunchProfileMembers
    putLaunchProfileMembers_clientToken,
    putLaunchProfileMembers_studioId,
    putLaunchProfileMembers_members,
    putLaunchProfileMembers_launchProfileId,
    putLaunchProfileMembers_identityStoreId,
    putLaunchProfileMembersResponse_httpStatus,

    -- ** CreateStudio
    createStudio_studioEncryptionConfiguration,
    createStudio_clientToken,
    createStudio_tags,
    createStudio_displayName,
    createStudio_studioName,
    createStudio_userRoleArn,
    createStudio_adminRoleArn,
    createStudioResponse_studio,
    createStudioResponse_httpStatus,

    -- ** UpdateLaunchProfileMember
    updateLaunchProfileMember_clientToken,
    updateLaunchProfileMember_studioId,
    updateLaunchProfileMember_persona,
    updateLaunchProfileMember_principalId,
    updateLaunchProfileMember_launchProfileId,
    updateLaunchProfileMemberResponse_member,
    updateLaunchProfileMemberResponse_httpStatus,

    -- ** DeleteLaunchProfileMember
    deleteLaunchProfileMember_clientToken,
    deleteLaunchProfileMember_studioId,
    deleteLaunchProfileMember_principalId,
    deleteLaunchProfileMember_launchProfileId,
    deleteLaunchProfileMemberResponse_httpStatus,

    -- ** ListLaunchProfiles
    listLaunchProfiles_states,
    listLaunchProfiles_principalId,
    listLaunchProfiles_nextToken,
    listLaunchProfiles_maxResults,
    listLaunchProfiles_studioId,
    listLaunchProfilesResponse_launchProfiles,
    listLaunchProfilesResponse_nextToken,
    listLaunchProfilesResponse_httpStatus,

    -- ** CreateLaunchProfile
    createLaunchProfile_clientToken,
    createLaunchProfile_description,
    createLaunchProfile_tags,
    createLaunchProfile_ec2SubnetIds,
    createLaunchProfile_studioComponentIds,
    createLaunchProfile_studioId,
    createLaunchProfile_launchProfileProtocolVersions,
    createLaunchProfile_name,
    createLaunchProfile_streamConfiguration,
    createLaunchProfileResponse_launchProfile,
    createLaunchProfileResponse_httpStatus,

    -- ** ListStreamingImages
    listStreamingImages_owner,
    listStreamingImages_nextToken,
    listStreamingImages_studioId,
    listStreamingImagesResponse_streamingImages,
    listStreamingImagesResponse_nextToken,
    listStreamingImagesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartStudioSSOConfigurationRepair
    startStudioSSOConfigurationRepair_clientToken,
    startStudioSSOConfigurationRepair_studioId,
    startStudioSSOConfigurationRepairResponse_studio,
    startStudioSSOConfigurationRepairResponse_httpStatus,

    -- ** GetLaunchProfileInitialization
    getLaunchProfileInitialization_studioId,
    getLaunchProfileInitialization_launchProfileProtocolVersions,
    getLaunchProfileInitialization_launchPurpose,
    getLaunchProfileInitialization_launchProfileId,
    getLaunchProfileInitialization_platform,
    getLaunchProfileInitializationResponse_launchProfileInitialization,
    getLaunchProfileInitializationResponse_httpStatus,

    -- ** GetLaunchProfile
    getLaunchProfile_studioId,
    getLaunchProfile_launchProfileId,
    getLaunchProfileResponse_launchProfile,
    getLaunchProfileResponse_httpStatus,

    -- ** CreateStudioComponent
    createStudioComponent_initializationScripts,
    createStudioComponent_clientToken,
    createStudioComponent_ec2SecurityGroupIds,
    createStudioComponent_subtype,
    createStudioComponent_scriptParameters,
    createStudioComponent_configuration,
    createStudioComponent_description,
    createStudioComponent_tags,
    createStudioComponent_studioId,
    createStudioComponent_name,
    createStudioComponent_type,
    createStudioComponentResponse_studioComponent,
    createStudioComponentResponse_httpStatus,

    -- ** GetEula
    getEula_eulaId,
    getEulaResponse_eula,
    getEulaResponse_httpStatus,

    -- ** ListStudioComponents
    listStudioComponents_states,
    listStudioComponents_types,
    listStudioComponents_nextToken,
    listStudioComponents_maxResults,
    listStudioComponents_studioId,
    listStudioComponentsResponse_studioComponents,
    listStudioComponentsResponse_nextToken,
    listStudioComponentsResponse_httpStatus,

    -- ** AcceptEulas
    acceptEulas_clientToken,
    acceptEulas_eulaIds,
    acceptEulas_studioId,
    acceptEulasResponse_eulaAcceptances,
    acceptEulasResponse_httpStatus,

    -- ** CreateStreamingSession
    createStreamingSession_ownedBy,
    createStreamingSession_clientToken,
    createStreamingSession_ec2InstanceType,
    createStreamingSession_launchProfileId,
    createStreamingSession_streamingImageId,
    createStreamingSession_tags,
    createStreamingSession_studioId,
    createStreamingSessionResponse_session,
    createStreamingSessionResponse_httpStatus,

    -- ** DeleteStudioComponent
    deleteStudioComponent_clientToken,
    deleteStudioComponent_studioId,
    deleteStudioComponent_studioComponentId,
    deleteStudioComponentResponse_studioComponent,
    deleteStudioComponentResponse_httpStatus,

    -- ** UpdateStudioComponent
    updateStudioComponent_initializationScripts,
    updateStudioComponent_clientToken,
    updateStudioComponent_ec2SecurityGroupIds,
    updateStudioComponent_subtype,
    updateStudioComponent_name,
    updateStudioComponent_scriptParameters,
    updateStudioComponent_type,
    updateStudioComponent_configuration,
    updateStudioComponent_description,
    updateStudioComponent_studioId,
    updateStudioComponent_studioComponentId,
    updateStudioComponentResponse_studioComponent,
    updateStudioComponentResponse_httpStatus,

    -- ** GetStudioMember
    getStudioMember_studioId,
    getStudioMember_principalId,
    getStudioMemberResponse_member,
    getStudioMemberResponse_httpStatus,

    -- ** DeleteStudio
    deleteStudio_clientToken,
    deleteStudio_studioId,
    deleteStudioResponse_studio,
    deleteStudioResponse_httpStatus,

    -- ** UpdateStudio
    updateStudio_clientToken,
    updateStudio_userRoleArn,
    updateStudio_displayName,
    updateStudio_adminRoleArn,
    updateStudio_studioId,
    updateStudioResponse_studio,
    updateStudioResponse_httpStatus,

    -- ** ListStudios
    listStudios_nextToken,
    listStudiosResponse_studios,
    listStudiosResponse_nextToken,
    listStudiosResponse_httpStatus,

    -- ** GetStudioComponent
    getStudioComponent_studioId,
    getStudioComponent_studioComponentId,
    getStudioComponentResponse_studioComponent,
    getStudioComponentResponse_httpStatus,

    -- ** ListEulas
    listEulas_nextToken,
    listEulas_eulaIds,
    listEulasResponse_eulas,
    listEulasResponse_nextToken,
    listEulasResponse_httpStatus,

    -- ** GetStreamingSession
    getStreamingSession_studioId,
    getStreamingSession_sessionId,
    getStreamingSessionResponse_session,
    getStreamingSessionResponse_httpStatus,

    -- ** ListLaunchProfileMembers
    listLaunchProfileMembers_nextToken,
    listLaunchProfileMembers_maxResults,
    listLaunchProfileMembers_studioId,
    listLaunchProfileMembers_launchProfileId,
    listLaunchProfileMembersResponse_members,
    listLaunchProfileMembersResponse_nextToken,
    listLaunchProfileMembersResponse_httpStatus,

    -- ** DeleteLaunchProfile
    deleteLaunchProfile_clientToken,
    deleteLaunchProfile_studioId,
    deleteLaunchProfile_launchProfileId,
    deleteLaunchProfileResponse_launchProfile,
    deleteLaunchProfileResponse_httpStatus,

    -- ** UpdateLaunchProfile
    updateLaunchProfile_clientToken,
    updateLaunchProfile_launchProfileProtocolVersions,
    updateLaunchProfile_streamConfiguration,
    updateLaunchProfile_name,
    updateLaunchProfile_description,
    updateLaunchProfile_studioComponentIds,
    updateLaunchProfile_studioId,
    updateLaunchProfile_launchProfileId,
    updateLaunchProfileResponse_launchProfile,
    updateLaunchProfileResponse_httpStatus,

    -- ** CreateStreamingImage
    createStreamingImage_clientToken,
    createStreamingImage_description,
    createStreamingImage_tags,
    createStreamingImage_studioId,
    createStreamingImage_name,
    createStreamingImage_ec2ImageId,
    createStreamingImageResponse_streamingImage,
    createStreamingImageResponse_httpStatus,

    -- ** CreateStreamingSessionStream
    createStreamingSessionStream_expirationInSeconds,
    createStreamingSessionStream_clientToken,
    createStreamingSessionStream_studioId,
    createStreamingSessionStream_sessionId,
    createStreamingSessionStreamResponse_stream,
    createStreamingSessionStreamResponse_httpStatus,

    -- ** GetLaunchProfileDetails
    getLaunchProfileDetails_studioId,
    getLaunchProfileDetails_launchProfileId,
    getLaunchProfileDetailsResponse_streamingImages,
    getLaunchProfileDetailsResponse_launchProfile,
    getLaunchProfileDetailsResponse_studioComponentSummaries,
    getLaunchProfileDetailsResponse_httpStatus,

    -- ** PutStudioMembers
    putStudioMembers_clientToken,
    putStudioMembers_studioId,
    putStudioMembers_members,
    putStudioMembers_identityStoreId,
    putStudioMembersResponse_httpStatus,

    -- ** DeleteStreamingImage
    deleteStreamingImage_clientToken,
    deleteStreamingImage_studioId,
    deleteStreamingImage_streamingImageId,
    deleteStreamingImageResponse_streamingImage,
    deleteStreamingImageResponse_httpStatus,

    -- ** UpdateStreamingImage
    updateStreamingImage_clientToken,
    updateStreamingImage_name,
    updateStreamingImage_description,
    updateStreamingImage_studioId,
    updateStreamingImage_streamingImageId,
    updateStreamingImageResponse_streamingImage,
    updateStreamingImageResponse_httpStatus,

    -- ** GetStreamingImage
    getStreamingImage_studioId,
    getStreamingImage_streamingImageId,
    getStreamingImageResponse_streamingImage,
    getStreamingImageResponse_httpStatus,

    -- ** ListEulaAcceptances
    listEulaAcceptances_nextToken,
    listEulaAcceptances_eulaIds,
    listEulaAcceptances_studioId,
    listEulaAcceptancesResponse_nextToken,
    listEulaAcceptancesResponse_eulaAcceptances,
    listEulaAcceptancesResponse_httpStatus,

    -- ** GetStreamingSessionStream
    getStreamingSessionStream_studioId,
    getStreamingSessionStream_streamId,
    getStreamingSessionStream_sessionId,
    getStreamingSessionStreamResponse_stream,
    getStreamingSessionStreamResponse_httpStatus,

    -- ** TagResource
    tagResource_tags,
    tagResource_resourceArn,
    tagResourceResponse_httpStatus,

    -- ** GetLaunchProfileMember
    getLaunchProfileMember_studioId,
    getLaunchProfileMember_principalId,
    getLaunchProfileMember_launchProfileId,
    getLaunchProfileMemberResponse_member,
    getLaunchProfileMemberResponse_httpStatus,

    -- ** DeleteStreamingSession
    deleteStreamingSession_clientToken,
    deleteStreamingSession_studioId,
    deleteStreamingSession_sessionId,
    deleteStreamingSessionResponse_session,
    deleteStreamingSessionResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,
    untagResourceResponse_httpStatus,

    -- ** ListStreamingSessions
    listStreamingSessions_ownedBy,
    listStreamingSessions_createdBy,
    listStreamingSessions_nextToken,
    listStreamingSessions_sessionIds,
    listStreamingSessions_studioId,
    listStreamingSessionsResponse_nextToken,
    listStreamingSessionsResponse_sessions,
    listStreamingSessionsResponse_httpStatus,

    -- ** GetStudio
    getStudio_studioId,
    getStudioResponse_studio,
    getStudioResponse_httpStatus,

    -- ** ListStudioMembers
    listStudioMembers_nextToken,
    listStudioMembers_maxResults,
    listStudioMembers_studioId,
    listStudioMembersResponse_members,
    listStudioMembersResponse_nextToken,
    listStudioMembersResponse_httpStatus,

    -- ** DeleteStudioMember
    deleteStudioMember_clientToken,
    deleteStudioMember_studioId,
    deleteStudioMember_principalId,
    deleteStudioMemberResponse_httpStatus,

    -- * Types

    -- ** ActiveDirectoryComputerAttribute
    activeDirectoryComputerAttribute_value,
    activeDirectoryComputerAttribute_name,

    -- ** ActiveDirectoryConfiguration
    activeDirectoryConfiguration_directoryId,
    activeDirectoryConfiguration_computerAttributes,
    activeDirectoryConfiguration_organizationalUnitDistinguishedName,

    -- ** ComputeFarmConfiguration
    computeFarmConfiguration_activeDirectoryUser,
    computeFarmConfiguration_endpoint,

    -- ** Eula
    eula_createdAt,
    eula_eulaId,
    eula_content,
    eula_name,
    eula_updatedAt,

    -- ** EulaAcceptance
    eulaAcceptance_accepteeId,
    eulaAcceptance_eulaId,
    eulaAcceptance_acceptedAt,
    eulaAcceptance_acceptedBy,
    eulaAcceptance_eulaAcceptanceId,

    -- ** LaunchProfile
    launchProfile_state,
    launchProfile_arn,
    launchProfile_createdAt,
    launchProfile_createdBy,
    launchProfile_launchProfileId,
    launchProfile_updatedBy,
    launchProfile_launchProfileProtocolVersions,
    launchProfile_ec2SubnetIds,
    launchProfile_streamConfiguration,
    launchProfile_name,
    launchProfile_statusMessage,
    launchProfile_updatedAt,
    launchProfile_description,
    launchProfile_tags,
    launchProfile_statusCode,
    launchProfile_studioComponentIds,

    -- ** LaunchProfileInitialization
    launchProfileInitialization_platform,
    launchProfileInitialization_activeDirectory,
    launchProfileInitialization_launchPurpose,
    launchProfileInitialization_launchProfileId,
    launchProfileInitialization_ec2SecurityGroupIds,
    launchProfileInitialization_name,
    launchProfileInitialization_launchProfileProtocolVersion,
    launchProfileInitialization_userInitializationScripts,
    launchProfileInitialization_systemInitializationScripts,

    -- ** LaunchProfileInitializationActiveDirectory
    launchProfileInitializationActiveDirectory_directoryId,
    launchProfileInitializationActiveDirectory_studioComponentId,
    launchProfileInitializationActiveDirectory_studioComponentName,
    launchProfileInitializationActiveDirectory_dnsIpAddresses,
    launchProfileInitializationActiveDirectory_computerAttributes,
    launchProfileInitializationActiveDirectory_organizationalUnitDistinguishedName,
    launchProfileInitializationActiveDirectory_directoryName,

    -- ** LaunchProfileInitializationScript
    launchProfileInitializationScript_script,
    launchProfileInitializationScript_studioComponentId,
    launchProfileInitializationScript_studioComponentName,

    -- ** LaunchProfileMembership
    launchProfileMembership_identityStoreId,
    launchProfileMembership_principalId,
    launchProfileMembership_persona,

    -- ** LicenseServiceConfiguration
    licenseServiceConfiguration_endpoint,

    -- ** NewLaunchProfileMember
    newLaunchProfileMember_persona,
    newLaunchProfileMember_principalId,

    -- ** NewStudioMember
    newStudioMember_persona,
    newStudioMember_principalId,

    -- ** ScriptParameterKeyValue
    scriptParameterKeyValue_value,
    scriptParameterKeyValue_key,

    -- ** SharedFileSystemConfiguration
    sharedFileSystemConfiguration_fileSystemId,
    sharedFileSystemConfiguration_windowsMountDrive,
    sharedFileSystemConfiguration_linuxMountPoint,
    sharedFileSystemConfiguration_shareName,
    sharedFileSystemConfiguration_endpoint,

    -- ** StreamConfiguration
    streamConfiguration_streamingImageIds,
    streamConfiguration_maxSessionLengthInMinutes,
    streamConfiguration_clipboardMode,
    streamConfiguration_ec2InstanceTypes,

    -- ** StreamConfigurationCreate
    streamConfigurationCreate_maxSessionLengthInMinutes,
    streamConfigurationCreate_clipboardMode,
    streamConfigurationCreate_streamingImageIds,
    streamConfigurationCreate_ec2InstanceTypes,

    -- ** StreamingImage
    streamingImage_state,
    streamingImage_platform,
    streamingImage_arn,
    streamingImage_streamingImageId,
    streamingImage_ec2ImageId,
    streamingImage_owner,
    streamingImage_name,
    streamingImage_encryptionConfiguration,
    streamingImage_statusMessage,
    streamingImage_eulaIds,
    streamingImage_description,
    streamingImage_tags,
    streamingImage_statusCode,

    -- ** StreamingImageEncryptionConfiguration
    streamingImageEncryptionConfiguration_keyArn,
    streamingImageEncryptionConfiguration_keyType,

    -- ** StreamingSession
    streamingSession_ownedBy,
    streamingSession_state,
    streamingSession_arn,
    streamingSession_createdAt,
    streamingSession_ec2InstanceType,
    streamingSession_createdBy,
    streamingSession_launchProfileId,
    streamingSession_streamingImageId,
    streamingSession_updatedBy,
    streamingSession_terminateAt,
    streamingSession_statusMessage,
    streamingSession_updatedAt,
    streamingSession_sessionId,
    streamingSession_tags,
    streamingSession_statusCode,

    -- ** StreamingSessionStream
    streamingSessionStream_ownedBy,
    streamingSessionStream_state,
    streamingSessionStream_createdAt,
    streamingSessionStream_expiresAt,
    streamingSessionStream_url,
    streamingSessionStream_createdBy,
    streamingSessionStream_streamId,
    streamingSessionStream_statusCode,

    -- ** Studio
    studio_studioEncryptionConfiguration,
    studio_state,
    studio_studioName,
    studio_arn,
    studio_createdAt,
    studio_studioId,
    studio_userRoleArn,
    studio_ssoClientId,
    studio_homeRegion,
    studio_statusMessage,
    studio_displayName,
    studio_updatedAt,
    studio_studioUrl,
    studio_adminRoleArn,
    studio_tags,
    studio_statusCode,

    -- ** StudioComponent
    studioComponent_initializationScripts,
    studioComponent_state,
    studioComponent_studioComponentId,
    studioComponent_arn,
    studioComponent_createdAt,
    studioComponent_createdBy,
    studioComponent_ec2SecurityGroupIds,
    studioComponent_updatedBy,
    studioComponent_subtype,
    studioComponent_name,
    studioComponent_statusMessage,
    studioComponent_scriptParameters,
    studioComponent_updatedAt,
    studioComponent_type,
    studioComponent_configuration,
    studioComponent_description,
    studioComponent_tags,
    studioComponent_statusCode,

    -- ** StudioComponentConfiguration
    studioComponentConfiguration_activeDirectoryConfiguration,
    studioComponentConfiguration_licenseServiceConfiguration,
    studioComponentConfiguration_sharedFileSystemConfiguration,
    studioComponentConfiguration_computeFarmConfiguration,

    -- ** StudioComponentInitializationScript
    studioComponentInitializationScript_script,
    studioComponentInitializationScript_platform,
    studioComponentInitializationScript_runContext,
    studioComponentInitializationScript_launchProfileProtocolVersion,

    -- ** StudioComponentSummary
    studioComponentSummary_studioComponentId,
    studioComponentSummary_createdAt,
    studioComponentSummary_createdBy,
    studioComponentSummary_updatedBy,
    studioComponentSummary_subtype,
    studioComponentSummary_name,
    studioComponentSummary_updatedAt,
    studioComponentSummary_type,
    studioComponentSummary_description,

    -- ** StudioEncryptionConfiguration
    studioEncryptionConfiguration_keyArn,
    studioEncryptionConfiguration_keyType,

    -- ** StudioMembership
    studioMembership_identityStoreId,
    studioMembership_principalId,
    studioMembership_persona,
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
import Network.AWS.Nimble.Types.ActiveDirectoryComputerAttribute
import Network.AWS.Nimble.Types.ActiveDirectoryConfiguration
import Network.AWS.Nimble.Types.ComputeFarmConfiguration
import Network.AWS.Nimble.Types.Eula
import Network.AWS.Nimble.Types.EulaAcceptance
import Network.AWS.Nimble.Types.LaunchProfile
import Network.AWS.Nimble.Types.LaunchProfileInitialization
import Network.AWS.Nimble.Types.LaunchProfileInitializationActiveDirectory
import Network.AWS.Nimble.Types.LaunchProfileInitializationScript
import Network.AWS.Nimble.Types.LaunchProfileMembership
import Network.AWS.Nimble.Types.LicenseServiceConfiguration
import Network.AWS.Nimble.Types.NewLaunchProfileMember
import Network.AWS.Nimble.Types.NewStudioMember
import Network.AWS.Nimble.Types.ScriptParameterKeyValue
import Network.AWS.Nimble.Types.SharedFileSystemConfiguration
import Network.AWS.Nimble.Types.StreamConfiguration
import Network.AWS.Nimble.Types.StreamConfigurationCreate
import Network.AWS.Nimble.Types.StreamingImage
import Network.AWS.Nimble.Types.StreamingImageEncryptionConfiguration
import Network.AWS.Nimble.Types.StreamingSession
import Network.AWS.Nimble.Types.StreamingSessionStream
import Network.AWS.Nimble.Types.Studio
import Network.AWS.Nimble.Types.StudioComponent
import Network.AWS.Nimble.Types.StudioComponentConfiguration
import Network.AWS.Nimble.Types.StudioComponentInitializationScript
import Network.AWS.Nimble.Types.StudioComponentSummary
import Network.AWS.Nimble.Types.StudioEncryptionConfiguration
import Network.AWS.Nimble.Types.StudioMembership
import Network.AWS.Nimble.UntagResource
import Network.AWS.Nimble.UpdateLaunchProfile
import Network.AWS.Nimble.UpdateLaunchProfileMember
import Network.AWS.Nimble.UpdateStreamingImage
import Network.AWS.Nimble.UpdateStudio
import Network.AWS.Nimble.UpdateStudioComponent
