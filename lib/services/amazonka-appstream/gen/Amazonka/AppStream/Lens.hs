{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppStream.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createAppBlock_description,
    createAppBlock_displayName,
    createAppBlock_tags,
    createAppBlock_name,
    createAppBlock_sourceS3Location,
    createAppBlock_setupScriptDetails,
    createAppBlockResponse_appBlock,
    createAppBlockResponse_httpStatus,

    -- ** CreateApplication
    createApplication_description,
    createApplication_displayName,
    createApplication_launchParameters,
    createApplication_tags,
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
    createDirectoryConfig_certificateBasedAuthProperties,
    createDirectoryConfig_serviceAccountCredentials,
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
    createFleet_computeCapacity,
    createFleet_description,
    createFleet_disconnectTimeoutInSeconds,
    createFleet_displayName,
    createFleet_domainJoinInfo,
    createFleet_enableDefaultInternetAccess,
    createFleet_fleetType,
    createFleet_iamRoleArn,
    createFleet_idleDisconnectTimeoutInSeconds,
    createFleet_imageArn,
    createFleet_imageName,
    createFleet_maxConcurrentSessions,
    createFleet_maxUserDurationInSeconds,
    createFleet_platform,
    createFleet_sessionScriptS3Location,
    createFleet_streamView,
    createFleet_tags,
    createFleet_usbDeviceFilterStrings,
    createFleet_vpcConfig,
    createFleet_name,
    createFleet_instanceType,
    createFleetResponse_fleet,
    createFleetResponse_httpStatus,

    -- ** CreateImageBuilder
    createImageBuilder_accessEndpoints,
    createImageBuilder_appstreamAgentVersion,
    createImageBuilder_description,
    createImageBuilder_displayName,
    createImageBuilder_domainJoinInfo,
    createImageBuilder_enableDefaultInternetAccess,
    createImageBuilder_iamRoleArn,
    createImageBuilder_imageArn,
    createImageBuilder_imageName,
    createImageBuilder_tags,
    createImageBuilder_vpcConfig,
    createImageBuilder_name,
    createImageBuilder_instanceType,
    createImageBuilderResponse_imageBuilder,
    createImageBuilderResponse_httpStatus,

    -- ** CreateImageBuilderStreamingURL
    createImageBuilderStreamingURL_validity,
    createImageBuilderStreamingURL_name,
    createImageBuilderStreamingURLResponse_expires,
    createImageBuilderStreamingURLResponse_streamingURL,
    createImageBuilderStreamingURLResponse_httpStatus,

    -- ** CreateStack
    createStack_accessEndpoints,
    createStack_applicationSettings,
    createStack_description,
    createStack_displayName,
    createStack_embedHostDomains,
    createStack_feedbackURL,
    createStack_redirectURL,
    createStack_storageConnectors,
    createStack_streamingExperienceSettings,
    createStack_tags,
    createStack_userSettings,
    createStack_name,
    createStackResponse_stack,
    createStackResponse_httpStatus,

    -- ** CreateStreamingURL
    createStreamingURL_applicationId,
    createStreamingURL_sessionContext,
    createStreamingURL_validity,
    createStreamingURL_stackName,
    createStreamingURL_fleetName,
    createStreamingURL_userId,
    createStreamingURLResponse_expires,
    createStreamingURLResponse_streamingURL,
    createStreamingURLResponse_httpStatus,

    -- ** CreateUpdatedImage
    createUpdatedImage_dryRun,
    createUpdatedImage_newImageDescription,
    createUpdatedImage_newImageDisplayName,
    createUpdatedImage_newImageTags,
    createUpdatedImage_existingImageName,
    createUpdatedImage_newImageName,
    createUpdatedImageResponse_canUpdateImage,
    createUpdatedImageResponse_image,
    createUpdatedImageResponse_httpStatus,

    -- ** CreateUsageReportSubscription
    createUsageReportSubscriptionResponse_s3BucketName,
    createUsageReportSubscriptionResponse_schedule,
    createUsageReportSubscriptionResponse_httpStatus,

    -- ** CreateUser
    createUser_firstName,
    createUser_lastName,
    createUser_messageAction,
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
    describeAppBlocks_arns,
    describeAppBlocks_maxResults,
    describeAppBlocks_nextToken,
    describeAppBlocksResponse_appBlocks,
    describeAppBlocksResponse_nextToken,
    describeAppBlocksResponse_httpStatus,

    -- ** DescribeApplicationFleetAssociations
    describeApplicationFleetAssociations_applicationArn,
    describeApplicationFleetAssociations_fleetName,
    describeApplicationFleetAssociations_maxResults,
    describeApplicationFleetAssociations_nextToken,
    describeApplicationFleetAssociationsResponse_applicationFleetAssociations,
    describeApplicationFleetAssociationsResponse_nextToken,
    describeApplicationFleetAssociationsResponse_httpStatus,

    -- ** DescribeApplications
    describeApplications_arns,
    describeApplications_maxResults,
    describeApplications_nextToken,
    describeApplicationsResponse_applications,
    describeApplicationsResponse_nextToken,
    describeApplicationsResponse_httpStatus,

    -- ** DescribeDirectoryConfigs
    describeDirectoryConfigs_directoryNames,
    describeDirectoryConfigs_maxResults,
    describeDirectoryConfigs_nextToken,
    describeDirectoryConfigsResponse_directoryConfigs,
    describeDirectoryConfigsResponse_nextToken,
    describeDirectoryConfigsResponse_httpStatus,

    -- ** DescribeEntitlements
    describeEntitlements_maxResults,
    describeEntitlements_name,
    describeEntitlements_nextToken,
    describeEntitlements_stackName,
    describeEntitlementsResponse_entitlements,
    describeEntitlementsResponse_nextToken,
    describeEntitlementsResponse_httpStatus,

    -- ** DescribeFleets
    describeFleets_names,
    describeFleets_nextToken,
    describeFleetsResponse_fleets,
    describeFleetsResponse_nextToken,
    describeFleetsResponse_httpStatus,

    -- ** DescribeImageBuilders
    describeImageBuilders_maxResults,
    describeImageBuilders_names,
    describeImageBuilders_nextToken,
    describeImageBuildersResponse_imageBuilders,
    describeImageBuildersResponse_nextToken,
    describeImageBuildersResponse_httpStatus,

    -- ** DescribeImagePermissions
    describeImagePermissions_maxResults,
    describeImagePermissions_nextToken,
    describeImagePermissions_sharedAwsAccountIds,
    describeImagePermissions_name,
    describeImagePermissionsResponse_name,
    describeImagePermissionsResponse_nextToken,
    describeImagePermissionsResponse_sharedImagePermissionsList,
    describeImagePermissionsResponse_httpStatus,

    -- ** DescribeImages
    describeImages_arns,
    describeImages_maxResults,
    describeImages_names,
    describeImages_nextToken,
    describeImages_type,
    describeImagesResponse_images,
    describeImagesResponse_nextToken,
    describeImagesResponse_httpStatus,

    -- ** DescribeSessions
    describeSessions_authenticationType,
    describeSessions_limit,
    describeSessions_nextToken,
    describeSessions_userId,
    describeSessions_stackName,
    describeSessions_fleetName,
    describeSessionsResponse_nextToken,
    describeSessionsResponse_sessions,
    describeSessionsResponse_httpStatus,

    -- ** DescribeStacks
    describeStacks_names,
    describeStacks_nextToken,
    describeStacksResponse_nextToken,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,

    -- ** DescribeUsageReportSubscriptions
    describeUsageReportSubscriptions_maxResults,
    describeUsageReportSubscriptions_nextToken,
    describeUsageReportSubscriptionsResponse_nextToken,
    describeUsageReportSubscriptionsResponse_usageReportSubscriptions,
    describeUsageReportSubscriptionsResponse_httpStatus,

    -- ** DescribeUserStackAssociations
    describeUserStackAssociations_authenticationType,
    describeUserStackAssociations_maxResults,
    describeUserStackAssociations_nextToken,
    describeUserStackAssociations_stackName,
    describeUserStackAssociations_userName,
    describeUserStackAssociationsResponse_nextToken,
    describeUserStackAssociationsResponse_userStackAssociations,
    describeUserStackAssociationsResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_maxResults,
    describeUsers_nextToken,
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
    listAssociatedFleetsResponse_names,
    listAssociatedFleetsResponse_nextToken,
    listAssociatedFleetsResponse_httpStatus,

    -- ** ListAssociatedStacks
    listAssociatedStacks_nextToken,
    listAssociatedStacks_fleetName,
    listAssociatedStacksResponse_names,
    listAssociatedStacksResponse_nextToken,
    listAssociatedStacksResponse_httpStatus,

    -- ** ListEntitledApplications
    listEntitledApplications_maxResults,
    listEntitledApplications_nextToken,
    listEntitledApplications_stackName,
    listEntitledApplications_entitlementName,
    listEntitledApplicationsResponse_entitledApplications,
    listEntitledApplicationsResponse_nextToken,
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
    updateApplication_appBlockArn,
    updateApplication_attributesToDelete,
    updateApplication_description,
    updateApplication_displayName,
    updateApplication_iconS3Location,
    updateApplication_launchParameters,
    updateApplication_launchPath,
    updateApplication_workingDirectory,
    updateApplication_name,
    updateApplicationResponse_application,
    updateApplicationResponse_httpStatus,

    -- ** UpdateDirectoryConfig
    updateDirectoryConfig_certificateBasedAuthProperties,
    updateDirectoryConfig_organizationalUnitDistinguishedNames,
    updateDirectoryConfig_serviceAccountCredentials,
    updateDirectoryConfig_directoryName,
    updateDirectoryConfigResponse_directoryConfig,
    updateDirectoryConfigResponse_httpStatus,

    -- ** UpdateEntitlement
    updateEntitlement_appVisibility,
    updateEntitlement_attributes,
    updateEntitlement_description,
    updateEntitlement_name,
    updateEntitlement_stackName,
    updateEntitlementResponse_entitlement,
    updateEntitlementResponse_httpStatus,

    -- ** UpdateFleet
    updateFleet_attributesToDelete,
    updateFleet_computeCapacity,
    updateFleet_deleteVpcConfig,
    updateFleet_description,
    updateFleet_disconnectTimeoutInSeconds,
    updateFleet_displayName,
    updateFleet_domainJoinInfo,
    updateFleet_enableDefaultInternetAccess,
    updateFleet_iamRoleArn,
    updateFleet_idleDisconnectTimeoutInSeconds,
    updateFleet_imageArn,
    updateFleet_imageName,
    updateFleet_instanceType,
    updateFleet_maxConcurrentSessions,
    updateFleet_maxUserDurationInSeconds,
    updateFleet_name,
    updateFleet_platform,
    updateFleet_sessionScriptS3Location,
    updateFleet_streamView,
    updateFleet_usbDeviceFilterStrings,
    updateFleet_vpcConfig,
    updateFleetResponse_fleet,
    updateFleetResponse_httpStatus,

    -- ** UpdateImagePermissions
    updateImagePermissions_name,
    updateImagePermissions_sharedAccountId,
    updateImagePermissions_imagePermissions,
    updateImagePermissionsResponse_httpStatus,

    -- ** UpdateStack
    updateStack_accessEndpoints,
    updateStack_applicationSettings,
    updateStack_attributesToDelete,
    updateStack_deleteStorageConnectors,
    updateStack_description,
    updateStack_displayName,
    updateStack_embedHostDomains,
    updateStack_feedbackURL,
    updateStack_redirectURL,
    updateStack_storageConnectors,
    updateStack_streamingExperienceSettings,
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
    appBlock_description,
    appBlock_displayName,
    appBlock_sourceS3Location,
    appBlock_name,
    appBlock_arn,
    appBlock_setupScriptDetails,

    -- ** Application
    application_appBlockArn,
    application_arn,
    application_createdTime,
    application_description,
    application_displayName,
    application_enabled,
    application_iconS3Location,
    application_iconURL,
    application_instanceFamilies,
    application_launchParameters,
    application_launchPath,
    application_metadata,
    application_name,
    application_platforms,
    application_workingDirectory,

    -- ** ApplicationFleetAssociation
    applicationFleetAssociation_fleetName,
    applicationFleetAssociation_applicationArn,

    -- ** ApplicationSettings
    applicationSettings_settingsGroup,
    applicationSettings_enabled,

    -- ** ApplicationSettingsResponse
    applicationSettingsResponse_enabled,
    applicationSettingsResponse_s3BucketName,
    applicationSettingsResponse_settingsGroup,

    -- ** CertificateBasedAuthProperties
    certificateBasedAuthProperties_certificateAuthorityArn,
    certificateBasedAuthProperties_status,

    -- ** ComputeCapacity
    computeCapacity_desiredInstances,

    -- ** ComputeCapacityStatus
    computeCapacityStatus_available,
    computeCapacityStatus_inUse,
    computeCapacityStatus_running,
    computeCapacityStatus_desired,

    -- ** DirectoryConfig
    directoryConfig_certificateBasedAuthProperties,
    directoryConfig_createdTime,
    directoryConfig_organizationalUnitDistinguishedNames,
    directoryConfig_serviceAccountCredentials,
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
    fleet_description,
    fleet_disconnectTimeoutInSeconds,
    fleet_displayName,
    fleet_domainJoinInfo,
    fleet_enableDefaultInternetAccess,
    fleet_fleetErrors,
    fleet_fleetType,
    fleet_iamRoleArn,
    fleet_idleDisconnectTimeoutInSeconds,
    fleet_imageArn,
    fleet_imageName,
    fleet_maxConcurrentSessions,
    fleet_maxUserDurationInSeconds,
    fleet_platform,
    fleet_sessionScriptS3Location,
    fleet_streamView,
    fleet_usbDeviceFilterStrings,
    fleet_vpcConfig,
    fleet_arn,
    fleet_name,
    fleet_instanceType,
    fleet_computeCapacityStatus,
    fleet_state,

    -- ** FleetError
    fleetError_errorCode,
    fleetError_errorMessage,

    -- ** Image
    image_applications,
    image_appstreamAgentVersion,
    image_arn,
    image_baseImageArn,
    image_createdTime,
    image_description,
    image_displayName,
    image_imageBuilderName,
    image_imageBuilderSupported,
    image_imageErrors,
    image_imagePermissions,
    image_platform,
    image_publicBaseImageReleasedDate,
    image_state,
    image_stateChangeReason,
    image_visibility,
    image_name,

    -- ** ImageBuilder
    imageBuilder_accessEndpoints,
    imageBuilder_appstreamAgentVersion,
    imageBuilder_arn,
    imageBuilder_createdTime,
    imageBuilder_description,
    imageBuilder_displayName,
    imageBuilder_domainJoinInfo,
    imageBuilder_enableDefaultInternetAccess,
    imageBuilder_iamRoleArn,
    imageBuilder_imageArn,
    imageBuilder_imageBuilderErrors,
    imageBuilder_instanceType,
    imageBuilder_networkAccessConfiguration,
    imageBuilder_platform,
    imageBuilder_state,
    imageBuilder_stateChangeReason,
    imageBuilder_vpcConfig,
    imageBuilder_name,

    -- ** ImageBuilderStateChangeReason
    imageBuilderStateChangeReason_code,
    imageBuilderStateChangeReason_message,

    -- ** ImagePermissions
    imagePermissions_allowFleet,
    imagePermissions_allowImageBuilder,

    -- ** ImageStateChangeReason
    imageStateChangeReason_code,
    imageStateChangeReason_message,

    -- ** LastReportGenerationExecutionError
    lastReportGenerationExecutionError_errorCode,
    lastReportGenerationExecutionError_errorMessage,

    -- ** NetworkAccessConfiguration
    networkAccessConfiguration_eniId,
    networkAccessConfiguration_eniPrivateIpAddress,

    -- ** ResourceError
    resourceError_errorCode,
    resourceError_errorMessage,
    resourceError_errorTimestamp,

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
    session_connectionState,
    session_maxExpirationTime,
    session_networkAccessConfiguration,
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
    stack_accessEndpoints,
    stack_applicationSettings,
    stack_arn,
    stack_createdTime,
    stack_description,
    stack_displayName,
    stack_embedHostDomains,
    stack_feedbackURL,
    stack_redirectURL,
    stack_stackErrors,
    stack_storageConnectors,
    stack_streamingExperienceSettings,
    stack_userSettings,
    stack_name,

    -- ** StackError
    stackError_errorCode,
    stackError_errorMessage,

    -- ** StorageConnector
    storageConnector_domains,
    storageConnector_resourceIdentifier,
    storageConnector_connectorType,

    -- ** StreamingExperienceSettings
    streamingExperienceSettings_preferredProtocol,

    -- ** UsageReportSubscription
    usageReportSubscription_lastGeneratedReportDate,
    usageReportSubscription_s3BucketName,
    usageReportSubscription_schedule,
    usageReportSubscription_subscriptionErrors,

    -- ** User
    user_arn,
    user_createdTime,
    user_enabled,
    user_firstName,
    user_lastName,
    user_status,
    user_userName,
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
    userStackAssociationError_errorCode,
    userStackAssociationError_errorMessage,
    userStackAssociationError_userStackAssociation,

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
