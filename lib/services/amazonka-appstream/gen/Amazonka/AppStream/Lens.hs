{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppStream.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Lens
  ( -- * Operations

    -- ** AssociateApplicationFleet
    associateApplicationFleet_fleetName,
    associateApplicationFleet_applicationArn,
    associateApplicationFleetResponse_applicationFleetAssociation,
    associateApplicationFleetResponse_httpStatus,

    -- ** AssociateApplicationToEntitlement
    associateApplicationToEntitlement_stackName,
    associateApplicationToEntitlement_entitlementName,
    associateApplicationToEntitlement_applicationIdentifier,
    associateApplicationToEntitlementResponse_httpStatus,

    -- ** AssociateFleet
    associateFleet_fleetName,
    associateFleet_stackName,
    associateFleetResponse_httpStatus,

    -- ** BatchAssociateUserStack
    batchAssociateUserStack_userStackAssociations,
    batchAssociateUserStackResponse_errors,
    batchAssociateUserStackResponse_httpStatus,

    -- ** BatchDisassociateUserStack
    batchDisassociateUserStack_userStackAssociations,
    batchDisassociateUserStackResponse_errors,
    batchDisassociateUserStackResponse_httpStatus,

    -- ** CopyImage
    copyImage_destinationImageDescription,
    copyImage_sourceImageName,
    copyImage_destinationImageName,
    copyImage_destinationRegion,
    copyImageResponse_destinationImageName,
    copyImageResponse_httpStatus,

    -- ** CreateAppBlock
    createAppBlock_tags,
    createAppBlock_displayName,
    createAppBlock_description,
    createAppBlock_name,
    createAppBlock_sourceS3Location,
    createAppBlock_setupScriptDetails,
    createAppBlockResponse_appBlock,
    createAppBlockResponse_httpStatus,

    -- ** CreateApplication
    createApplication_tags,
    createApplication_displayName,
    createApplication_description,
    createApplication_launchParameters,
    createApplication_workingDirectory,
    createApplication_name,
    createApplication_iconS3Location,
    createApplication_launchPath,
    createApplication_platforms,
    createApplication_instanceFamilies,
    createApplication_appBlockArn,
    createApplicationResponse_application,
    createApplicationResponse_httpStatus,

    -- ** CreateDirectoryConfig
    createDirectoryConfig_serviceAccountCredentials,
    createDirectoryConfig_certificateBasedAuthProperties,
    createDirectoryConfig_directoryName,
    createDirectoryConfig_organizationalUnitDistinguishedNames,
    createDirectoryConfigResponse_directoryConfig,
    createDirectoryConfigResponse_httpStatus,

    -- ** CreateEntitlement
    createEntitlement_description,
    createEntitlement_name,
    createEntitlement_stackName,
    createEntitlement_appVisibility,
    createEntitlement_attributes,
    createEntitlementResponse_entitlement,
    createEntitlementResponse_httpStatus,

    -- ** CreateFleet
    createFleet_tags,
    createFleet_sessionScriptS3Location,
    createFleet_maxConcurrentSessions,
    createFleet_fleetType,
    createFleet_vpcConfig,
    createFleet_displayName,
    createFleet_imageArn,
    createFleet_platform,
    createFleet_description,
    createFleet_disconnectTimeoutInSeconds,
    createFleet_idleDisconnectTimeoutInSeconds,
    createFleet_iamRoleArn,
    createFleet_usbDeviceFilterStrings,
    createFleet_domainJoinInfo,
    createFleet_streamView,
    createFleet_enableDefaultInternetAccess,
    createFleet_computeCapacity,
    createFleet_imageName,
    createFleet_maxUserDurationInSeconds,
    createFleet_name,
    createFleet_instanceType,
    createFleetResponse_fleet,
    createFleetResponse_httpStatus,

    -- ** CreateImageBuilder
    createImageBuilder_tags,
    createImageBuilder_vpcConfig,
    createImageBuilder_displayName,
    createImageBuilder_accessEndpoints,
    createImageBuilder_imageArn,
    createImageBuilder_description,
    createImageBuilder_iamRoleArn,
    createImageBuilder_domainJoinInfo,
    createImageBuilder_appstreamAgentVersion,
    createImageBuilder_enableDefaultInternetAccess,
    createImageBuilder_imageName,
    createImageBuilder_name,
    createImageBuilder_instanceType,
    createImageBuilderResponse_imageBuilder,
    createImageBuilderResponse_httpStatus,

    -- ** CreateImageBuilderStreamingURL
    createImageBuilderStreamingURL_validity,
    createImageBuilderStreamingURL_name,
    createImageBuilderStreamingURLResponse_streamingURL,
    createImageBuilderStreamingURLResponse_expires,
    createImageBuilderStreamingURLResponse_httpStatus,

    -- ** CreateStack
    createStack_tags,
    createStack_storageConnectors,
    createStack_embedHostDomains,
    createStack_applicationSettings,
    createStack_displayName,
    createStack_accessEndpoints,
    createStack_description,
    createStack_redirectURL,
    createStack_streamingExperienceSettings,
    createStack_feedbackURL,
    createStack_userSettings,
    createStack_name,
    createStackResponse_stack,
    createStackResponse_httpStatus,

    -- ** CreateStreamingURL
    createStreamingURL_sessionContext,
    createStreamingURL_applicationId,
    createStreamingURL_validity,
    createStreamingURL_stackName,
    createStreamingURL_fleetName,
    createStreamingURL_userId,
    createStreamingURLResponse_streamingURL,
    createStreamingURLResponse_expires,
    createStreamingURLResponse_httpStatus,

    -- ** CreateUpdatedImage
    createUpdatedImage_newImageTags,
    createUpdatedImage_dryRun,
    createUpdatedImage_newImageDescription,
    createUpdatedImage_newImageDisplayName,
    createUpdatedImage_existingImageName,
    createUpdatedImage_newImageName,
    createUpdatedImageResponse_canUpdateImage,
    createUpdatedImageResponse_image,
    createUpdatedImageResponse_httpStatus,

    -- ** CreateUsageReportSubscription
    createUsageReportSubscriptionResponse_schedule,
    createUsageReportSubscriptionResponse_s3BucketName,
    createUsageReportSubscriptionResponse_httpStatus,

    -- ** CreateUser
    createUser_messageAction,
    createUser_firstName,
    createUser_lastName,
    createUser_userName,
    createUser_authenticationType,
    createUserResponse_httpStatus,

    -- ** DeleteAppBlock
    deleteAppBlock_name,
    deleteAppBlockResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_name,
    deleteApplicationResponse_httpStatus,

    -- ** DeleteDirectoryConfig
    deleteDirectoryConfig_directoryName,
    deleteDirectoryConfigResponse_httpStatus,

    -- ** DeleteEntitlement
    deleteEntitlement_name,
    deleteEntitlement_stackName,
    deleteEntitlementResponse_httpStatus,

    -- ** DeleteFleet
    deleteFleet_name,
    deleteFleetResponse_httpStatus,

    -- ** DeleteImage
    deleteImage_name,
    deleteImageResponse_image,
    deleteImageResponse_httpStatus,

    -- ** DeleteImageBuilder
    deleteImageBuilder_name,
    deleteImageBuilderResponse_imageBuilder,
    deleteImageBuilderResponse_httpStatus,

    -- ** DeleteImagePermissions
    deleteImagePermissions_name,
    deleteImagePermissions_sharedAccountId,
    deleteImagePermissionsResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_name,
    deleteStackResponse_httpStatus,

    -- ** DeleteUsageReportSubscription
    deleteUsageReportSubscriptionResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userName,
    deleteUser_authenticationType,
    deleteUserResponse_httpStatus,

    -- ** DescribeAppBlocks
    describeAppBlocks_nextToken,
    describeAppBlocks_arns,
    describeAppBlocks_maxResults,
    describeAppBlocksResponse_nextToken,
    describeAppBlocksResponse_appBlocks,
    describeAppBlocksResponse_httpStatus,

    -- ** DescribeApplicationFleetAssociations
    describeApplicationFleetAssociations_nextToken,
    describeApplicationFleetAssociations_applicationArn,
    describeApplicationFleetAssociations_fleetName,
    describeApplicationFleetAssociations_maxResults,
    describeApplicationFleetAssociationsResponse_nextToken,
    describeApplicationFleetAssociationsResponse_applicationFleetAssociations,
    describeApplicationFleetAssociationsResponse_httpStatus,

    -- ** DescribeApplications
    describeApplications_nextToken,
    describeApplications_arns,
    describeApplications_maxResults,
    describeApplicationsResponse_nextToken,
    describeApplicationsResponse_applications,
    describeApplicationsResponse_httpStatus,

    -- ** DescribeDirectoryConfigs
    describeDirectoryConfigs_nextToken,
    describeDirectoryConfigs_directoryNames,
    describeDirectoryConfigs_maxResults,
    describeDirectoryConfigsResponse_nextToken,
    describeDirectoryConfigsResponse_directoryConfigs,
    describeDirectoryConfigsResponse_httpStatus,

    -- ** DescribeEntitlements
    describeEntitlements_name,
    describeEntitlements_nextToken,
    describeEntitlements_maxResults,
    describeEntitlements_stackName,
    describeEntitlementsResponse_nextToken,
    describeEntitlementsResponse_entitlements,
    describeEntitlementsResponse_httpStatus,

    -- ** DescribeFleets
    describeFleets_nextToken,
    describeFleets_names,
    describeFleetsResponse_nextToken,
    describeFleetsResponse_fleets,
    describeFleetsResponse_httpStatus,

    -- ** DescribeImageBuilders
    describeImageBuilders_nextToken,
    describeImageBuilders_names,
    describeImageBuilders_maxResults,
    describeImageBuildersResponse_nextToken,
    describeImageBuildersResponse_imageBuilders,
    describeImageBuildersResponse_httpStatus,

    -- ** DescribeImagePermissions
    describeImagePermissions_nextToken,
    describeImagePermissions_maxResults,
    describeImagePermissions_sharedAwsAccountIds,
    describeImagePermissions_name,
    describeImagePermissionsResponse_name,
    describeImagePermissionsResponse_nextToken,
    describeImagePermissionsResponse_sharedImagePermissionsList,
    describeImagePermissionsResponse_httpStatus,

    -- ** DescribeImages
    describeImages_nextToken,
    describeImages_type,
    describeImages_arns,
    describeImages_names,
    describeImages_maxResults,
    describeImagesResponse_nextToken,
    describeImagesResponse_images,
    describeImagesResponse_httpStatus,

    -- ** DescribeSessions
    describeSessions_nextToken,
    describeSessions_authenticationType,
    describeSessions_limit,
    describeSessions_userId,
    describeSessions_stackName,
    describeSessions_fleetName,
    describeSessionsResponse_nextToken,
    describeSessionsResponse_sessions,
    describeSessionsResponse_httpStatus,

    -- ** DescribeStacks
    describeStacks_nextToken,
    describeStacks_names,
    describeStacksResponse_stacks,
    describeStacksResponse_nextToken,
    describeStacksResponse_httpStatus,

    -- ** DescribeUsageReportSubscriptions
    describeUsageReportSubscriptions_nextToken,
    describeUsageReportSubscriptions_maxResults,
    describeUsageReportSubscriptionsResponse_usageReportSubscriptions,
    describeUsageReportSubscriptionsResponse_nextToken,
    describeUsageReportSubscriptionsResponse_httpStatus,

    -- ** DescribeUserStackAssociations
    describeUserStackAssociations_nextToken,
    describeUserStackAssociations_authenticationType,
    describeUserStackAssociations_userName,
    describeUserStackAssociations_stackName,
    describeUserStackAssociations_maxResults,
    describeUserStackAssociationsResponse_nextToken,
    describeUserStackAssociationsResponse_userStackAssociations,
    describeUserStackAssociationsResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_nextToken,
    describeUsers_maxResults,
    describeUsers_authenticationType,
    describeUsersResponse_nextToken,
    describeUsersResponse_users,
    describeUsersResponse_httpStatus,

    -- ** DisableUser
    disableUser_userName,
    disableUser_authenticationType,
    disableUserResponse_httpStatus,

    -- ** DisassociateApplicationFleet
    disassociateApplicationFleet_fleetName,
    disassociateApplicationFleet_applicationArn,
    disassociateApplicationFleetResponse_httpStatus,

    -- ** DisassociateApplicationFromEntitlement
    disassociateApplicationFromEntitlement_stackName,
    disassociateApplicationFromEntitlement_entitlementName,
    disassociateApplicationFromEntitlement_applicationIdentifier,
    disassociateApplicationFromEntitlementResponse_httpStatus,

    -- ** DisassociateFleet
    disassociateFleet_fleetName,
    disassociateFleet_stackName,
    disassociateFleetResponse_httpStatus,

    -- ** EnableUser
    enableUser_userName,
    enableUser_authenticationType,
    enableUserResponse_httpStatus,

    -- ** ExpireSession
    expireSession_sessionId,
    expireSessionResponse_httpStatus,

    -- ** ListAssociatedFleets
    listAssociatedFleets_nextToken,
    listAssociatedFleets_stackName,
    listAssociatedFleetsResponse_nextToken,
    listAssociatedFleetsResponse_names,
    listAssociatedFleetsResponse_httpStatus,

    -- ** ListAssociatedStacks
    listAssociatedStacks_nextToken,
    listAssociatedStacks_fleetName,
    listAssociatedStacksResponse_nextToken,
    listAssociatedStacksResponse_names,
    listAssociatedStacksResponse_httpStatus,

    -- ** ListEntitledApplications
    listEntitledApplications_nextToken,
    listEntitledApplications_maxResults,
    listEntitledApplications_stackName,
    listEntitledApplications_entitlementName,
    listEntitledApplicationsResponse_nextToken,
    listEntitledApplicationsResponse_entitledApplications,
    listEntitledApplicationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartFleet
    startFleet_name,
    startFleetResponse_httpStatus,

    -- ** StartImageBuilder
    startImageBuilder_appstreamAgentVersion,
    startImageBuilder_name,
    startImageBuilderResponse_imageBuilder,
    startImageBuilderResponse_httpStatus,

    -- ** StopFleet
    stopFleet_name,
    stopFleetResponse_httpStatus,

    -- ** StopImageBuilder
    stopImageBuilder_name,
    stopImageBuilderResponse_imageBuilder,
    stopImageBuilderResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_launchPath,
    updateApplication_attributesToDelete,
    updateApplication_displayName,
    updateApplication_description,
    updateApplication_iconS3Location,
    updateApplication_appBlockArn,
    updateApplication_launchParameters,
    updateApplication_workingDirectory,
    updateApplication_name,
    updateApplicationResponse_application,
    updateApplicationResponse_httpStatus,

    -- ** UpdateDirectoryConfig
    updateDirectoryConfig_serviceAccountCredentials,
    updateDirectoryConfig_organizationalUnitDistinguishedNames,
    updateDirectoryConfig_certificateBasedAuthProperties,
    updateDirectoryConfig_directoryName,
    updateDirectoryConfigResponse_directoryConfig,
    updateDirectoryConfigResponse_httpStatus,

    -- ** UpdateEntitlement
    updateEntitlement_description,
    updateEntitlement_attributes,
    updateEntitlement_appVisibility,
    updateEntitlement_name,
    updateEntitlement_stackName,
    updateEntitlementResponse_entitlement,
    updateEntitlementResponse_httpStatus,

    -- ** UpdateFleet
    updateFleet_name,
    updateFleet_sessionScriptS3Location,
    updateFleet_maxConcurrentSessions,
    updateFleet_attributesToDelete,
    updateFleet_vpcConfig,
    updateFleet_deleteVpcConfig,
    updateFleet_displayName,
    updateFleet_imageArn,
    updateFleet_platform,
    updateFleet_description,
    updateFleet_disconnectTimeoutInSeconds,
    updateFleet_idleDisconnectTimeoutInSeconds,
    updateFleet_instanceType,
    updateFleet_iamRoleArn,
    updateFleet_usbDeviceFilterStrings,
    updateFleet_domainJoinInfo,
    updateFleet_streamView,
    updateFleet_enableDefaultInternetAccess,
    updateFleet_computeCapacity,
    updateFleet_imageName,
    updateFleet_maxUserDurationInSeconds,
    updateFleetResponse_fleet,
    updateFleetResponse_httpStatus,

    -- ** UpdateImagePermissions
    updateImagePermissions_name,
    updateImagePermissions_sharedAccountId,
    updateImagePermissions_imagePermissions,
    updateImagePermissionsResponse_httpStatus,

    -- ** UpdateStack
    updateStack_storageConnectors,
    updateStack_attributesToDelete,
    updateStack_embedHostDomains,
    updateStack_deleteStorageConnectors,
    updateStack_applicationSettings,
    updateStack_displayName,
    updateStack_accessEndpoints,
    updateStack_description,
    updateStack_redirectURL,
    updateStack_streamingExperienceSettings,
    updateStack_feedbackURL,
    updateStack_userSettings,
    updateStack_name,
    updateStackResponse_stack,
    updateStackResponse_httpStatus,

    -- * Types

    -- ** AccessEndpoint
    accessEndpoint_vpceId,
    accessEndpoint_endpointType,

    -- ** AppBlock
    appBlock_createdTime,
    appBlock_displayName,
    appBlock_description,
    appBlock_sourceS3Location,
    appBlock_name,
    appBlock_arn,
    appBlock_setupScriptDetails,

    -- ** Application
    application_name,
    application_createdTime,
    application_launchPath,
    application_metadata,
    application_arn,
    application_displayName,
    application_description,
    application_iconS3Location,
    application_enabled,
    application_iconURL,
    application_instanceFamilies,
    application_platforms,
    application_appBlockArn,
    application_launchParameters,
    application_workingDirectory,

    -- ** ApplicationFleetAssociation
    applicationFleetAssociation_fleetName,
    applicationFleetAssociation_applicationArn,

    -- ** ApplicationSettings
    applicationSettings_settingsGroup,
    applicationSettings_enabled,

    -- ** ApplicationSettingsResponse
    applicationSettingsResponse_s3BucketName,
    applicationSettingsResponse_enabled,
    applicationSettingsResponse_settingsGroup,

    -- ** CertificateBasedAuthProperties
    certificateBasedAuthProperties_certificateAuthorityArn,
    certificateBasedAuthProperties_status,

    -- ** ComputeCapacity
    computeCapacity_desiredInstances,

    -- ** ComputeCapacityStatus
    computeCapacityStatus_available,
    computeCapacityStatus_running,
    computeCapacityStatus_inUse,
    computeCapacityStatus_desired,

    -- ** DirectoryConfig
    directoryConfig_serviceAccountCredentials,
    directoryConfig_organizationalUnitDistinguishedNames,
    directoryConfig_createdTime,
    directoryConfig_certificateBasedAuthProperties,
    directoryConfig_directoryName,

    -- ** DomainJoinInfo
    domainJoinInfo_directoryName,
    domainJoinInfo_organizationalUnitDistinguishedName,

    -- ** EntitledApplication
    entitledApplication_applicationIdentifier,

    -- ** Entitlement
    entitlement_createdTime,
    entitlement_description,
    entitlement_lastModifiedTime,
    entitlement_name,
    entitlement_stackName,
    entitlement_appVisibility,
    entitlement_attributes,

    -- ** EntitlementAttribute
    entitlementAttribute_name,
    entitlementAttribute_value,

    -- ** Fleet
    fleet_createdTime,
    fleet_sessionScriptS3Location,
    fleet_maxConcurrentSessions,
    fleet_fleetType,
    fleet_vpcConfig,
    fleet_fleetErrors,
    fleet_displayName,
    fleet_imageArn,
    fleet_platform,
    fleet_description,
    fleet_disconnectTimeoutInSeconds,
    fleet_idleDisconnectTimeoutInSeconds,
    fleet_iamRoleArn,
    fleet_usbDeviceFilterStrings,
    fleet_domainJoinInfo,
    fleet_streamView,
    fleet_enableDefaultInternetAccess,
    fleet_imageName,
    fleet_maxUserDurationInSeconds,
    fleet_arn,
    fleet_name,
    fleet_instanceType,
    fleet_computeCapacityStatus,
    fleet_state,

    -- ** FleetError
    fleetError_errorMessage,
    fleetError_errorCode,

    -- ** Image
    image_createdTime,
    image_stateChangeReason,
    image_applications,
    image_imageBuilderSupported,
    image_visibility,
    image_arn,
    image_displayName,
    image_state,
    image_platform,
    image_description,
    image_publicBaseImageReleasedDate,
    image_imagePermissions,
    image_appstreamAgentVersion,
    image_imageErrors,
    image_baseImageArn,
    image_imageBuilderName,
    image_name,

    -- ** ImageBuilder
    imageBuilder_createdTime,
    imageBuilder_stateChangeReason,
    imageBuilder_vpcConfig,
    imageBuilder_networkAccessConfiguration,
    imageBuilder_arn,
    imageBuilder_displayName,
    imageBuilder_state,
    imageBuilder_accessEndpoints,
    imageBuilder_imageArn,
    imageBuilder_platform,
    imageBuilder_description,
    imageBuilder_instanceType,
    imageBuilder_iamRoleArn,
    imageBuilder_domainJoinInfo,
    imageBuilder_imageBuilderErrors,
    imageBuilder_appstreamAgentVersion,
    imageBuilder_enableDefaultInternetAccess,
    imageBuilder_name,

    -- ** ImageBuilderStateChangeReason
    imageBuilderStateChangeReason_message,
    imageBuilderStateChangeReason_code,

    -- ** ImagePermissions
    imagePermissions_allowFleet,
    imagePermissions_allowImageBuilder,

    -- ** ImageStateChangeReason
    imageStateChangeReason_message,
    imageStateChangeReason_code,

    -- ** LastReportGenerationExecutionError
    lastReportGenerationExecutionError_errorMessage,
    lastReportGenerationExecutionError_errorCode,

    -- ** NetworkAccessConfiguration
    networkAccessConfiguration_eniPrivateIpAddress,
    networkAccessConfiguration_eniId,

    -- ** ResourceError
    resourceError_errorMessage,
    resourceError_errorTimestamp,
    resourceError_errorCode,

    -- ** S3Location
    s3Location_s3Bucket,
    s3Location_s3Key,

    -- ** ScriptDetails
    scriptDetails_executableParameters,
    scriptDetails_scriptS3Location,
    scriptDetails_executablePath,
    scriptDetails_timeoutInSeconds,

    -- ** ServiceAccountCredentials
    serviceAccountCredentials_accountName,
    serviceAccountCredentials_accountPassword,

    -- ** Session
    session_authenticationType,
    session_networkAccessConfiguration,
    session_connectionState,
    session_maxExpirationTime,
    session_startTime,
    session_id,
    session_userId,
    session_stackName,
    session_fleetName,
    session_state,

    -- ** SharedImagePermissions
    sharedImagePermissions_sharedAccountId,
    sharedImagePermissions_imagePermissions,

    -- ** Stack
    stack_storageConnectors,
    stack_createdTime,
    stack_embedHostDomains,
    stack_applicationSettings,
    stack_arn,
    stack_stackErrors,
    stack_displayName,
    stack_accessEndpoints,
    stack_description,
    stack_redirectURL,
    stack_streamingExperienceSettings,
    stack_feedbackURL,
    stack_userSettings,
    stack_name,

    -- ** StackError
    stackError_errorMessage,
    stackError_errorCode,

    -- ** StorageConnector
    storageConnector_domains,
    storageConnector_resourceIdentifier,
    storageConnector_connectorType,

    -- ** StreamingExperienceSettings
    streamingExperienceSettings_preferredProtocol,

    -- ** UsageReportSubscription
    usageReportSubscription_schedule,
    usageReportSubscription_s3BucketName,
    usageReportSubscription_lastGeneratedReportDate,
    usageReportSubscription_subscriptionErrors,

    -- ** User
    user_createdTime,
    user_firstName,
    user_userName,
    user_arn,
    user_status,
    user_enabled,
    user_lastName,
    user_authenticationType,

    -- ** UserSetting
    userSetting_action,
    userSetting_permission,

    -- ** UserStackAssociation
    userStackAssociation_sendEmailNotification,
    userStackAssociation_stackName,
    userStackAssociation_userName,
    userStackAssociation_authenticationType,

    -- ** UserStackAssociationError
    userStackAssociationError_userStackAssociation,
    userStackAssociationError_errorMessage,
    userStackAssociationError_errorCode,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnetIds,
  )
where

import Amazonka.AppStream.AssociateApplicationFleet
import Amazonka.AppStream.AssociateApplicationToEntitlement
import Amazonka.AppStream.AssociateFleet
import Amazonka.AppStream.BatchAssociateUserStack
import Amazonka.AppStream.BatchDisassociateUserStack
import Amazonka.AppStream.CopyImage
import Amazonka.AppStream.CreateAppBlock
import Amazonka.AppStream.CreateApplication
import Amazonka.AppStream.CreateDirectoryConfig
import Amazonka.AppStream.CreateEntitlement
import Amazonka.AppStream.CreateFleet
import Amazonka.AppStream.CreateImageBuilder
import Amazonka.AppStream.CreateImageBuilderStreamingURL
import Amazonka.AppStream.CreateStack
import Amazonka.AppStream.CreateStreamingURL
import Amazonka.AppStream.CreateUpdatedImage
import Amazonka.AppStream.CreateUsageReportSubscription
import Amazonka.AppStream.CreateUser
import Amazonka.AppStream.DeleteAppBlock
import Amazonka.AppStream.DeleteApplication
import Amazonka.AppStream.DeleteDirectoryConfig
import Amazonka.AppStream.DeleteEntitlement
import Amazonka.AppStream.DeleteFleet
import Amazonka.AppStream.DeleteImage
import Amazonka.AppStream.DeleteImageBuilder
import Amazonka.AppStream.DeleteImagePermissions
import Amazonka.AppStream.DeleteStack
import Amazonka.AppStream.DeleteUsageReportSubscription
import Amazonka.AppStream.DeleteUser
import Amazonka.AppStream.DescribeAppBlocks
import Amazonka.AppStream.DescribeApplicationFleetAssociations
import Amazonka.AppStream.DescribeApplications
import Amazonka.AppStream.DescribeDirectoryConfigs
import Amazonka.AppStream.DescribeEntitlements
import Amazonka.AppStream.DescribeFleets
import Amazonka.AppStream.DescribeImageBuilders
import Amazonka.AppStream.DescribeImagePermissions
import Amazonka.AppStream.DescribeImages
import Amazonka.AppStream.DescribeSessions
import Amazonka.AppStream.DescribeStacks
import Amazonka.AppStream.DescribeUsageReportSubscriptions
import Amazonka.AppStream.DescribeUserStackAssociations
import Amazonka.AppStream.DescribeUsers
import Amazonka.AppStream.DisableUser
import Amazonka.AppStream.DisassociateApplicationFleet
import Amazonka.AppStream.DisassociateApplicationFromEntitlement
import Amazonka.AppStream.DisassociateFleet
import Amazonka.AppStream.EnableUser
import Amazonka.AppStream.ExpireSession
import Amazonka.AppStream.ListAssociatedFleets
import Amazonka.AppStream.ListAssociatedStacks
import Amazonka.AppStream.ListEntitledApplications
import Amazonka.AppStream.ListTagsForResource
import Amazonka.AppStream.StartFleet
import Amazonka.AppStream.StartImageBuilder
import Amazonka.AppStream.StopFleet
import Amazonka.AppStream.StopImageBuilder
import Amazonka.AppStream.TagResource
import Amazonka.AppStream.Types.AccessEndpoint
import Amazonka.AppStream.Types.AppBlock
import Amazonka.AppStream.Types.Application
import Amazonka.AppStream.Types.ApplicationFleetAssociation
import Amazonka.AppStream.Types.ApplicationSettings
import Amazonka.AppStream.Types.ApplicationSettingsResponse
import Amazonka.AppStream.Types.CertificateBasedAuthProperties
import Amazonka.AppStream.Types.ComputeCapacity
import Amazonka.AppStream.Types.ComputeCapacityStatus
import Amazonka.AppStream.Types.DirectoryConfig
import Amazonka.AppStream.Types.DomainJoinInfo
import Amazonka.AppStream.Types.EntitledApplication
import Amazonka.AppStream.Types.Entitlement
import Amazonka.AppStream.Types.EntitlementAttribute
import Amazonka.AppStream.Types.Fleet
import Amazonka.AppStream.Types.FleetError
import Amazonka.AppStream.Types.Image
import Amazonka.AppStream.Types.ImageBuilder
import Amazonka.AppStream.Types.ImageBuilderStateChangeReason
import Amazonka.AppStream.Types.ImagePermissions
import Amazonka.AppStream.Types.ImageStateChangeReason
import Amazonka.AppStream.Types.LastReportGenerationExecutionError
import Amazonka.AppStream.Types.NetworkAccessConfiguration
import Amazonka.AppStream.Types.ResourceError
import Amazonka.AppStream.Types.S3Location
import Amazonka.AppStream.Types.ScriptDetails
import Amazonka.AppStream.Types.ServiceAccountCredentials
import Amazonka.AppStream.Types.Session
import Amazonka.AppStream.Types.SharedImagePermissions
import Amazonka.AppStream.Types.Stack
import Amazonka.AppStream.Types.StackError
import Amazonka.AppStream.Types.StorageConnector
import Amazonka.AppStream.Types.StreamingExperienceSettings
import Amazonka.AppStream.Types.UsageReportSubscription
import Amazonka.AppStream.Types.User
import Amazonka.AppStream.Types.UserSetting
import Amazonka.AppStream.Types.UserStackAssociation
import Amazonka.AppStream.Types.UserStackAssociationError
import Amazonka.AppStream.Types.VpcConfig
import Amazonka.AppStream.UntagResource
import Amazonka.AppStream.UpdateApplication
import Amazonka.AppStream.UpdateDirectoryConfig
import Amazonka.AppStream.UpdateEntitlement
import Amazonka.AppStream.UpdateFleet
import Amazonka.AppStream.UpdateImagePermissions
import Amazonka.AppStream.UpdateStack
