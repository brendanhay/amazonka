{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppStream.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Lens
  ( -- * Operations

    -- ** CreateUsageReportSubscription
    createUsageReportSubscriptionResponse_schedule,
    createUsageReportSubscriptionResponse_s3BucketName,
    createUsageReportSubscriptionResponse_httpStatus,

    -- ** DisassociateFleet
    disassociateFleet_fleetName,
    disassociateFleet_stackName,
    disassociateFleetResponse_httpStatus,

    -- ** ListAssociatedFleets
    listAssociatedFleets_nextToken,
    listAssociatedFleets_stackName,
    listAssociatedFleetsResponse_nextToken,
    listAssociatedFleetsResponse_names,
    listAssociatedFleetsResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_name,
    deleteStackResponse_httpStatus,

    -- ** UpdateStack
    updateStack_userSettings,
    updateStack_applicationSettings,
    updateStack_feedbackURL,
    updateStack_attributesToDelete,
    updateStack_deleteStorageConnectors,
    updateStack_storageConnectors,
    updateStack_accessEndpoints,
    updateStack_displayName,
    updateStack_embedHostDomains,
    updateStack_description,
    updateStack_redirectURL,
    updateStack_name,
    updateStackResponse_stack,
    updateStackResponse_httpStatus,

    -- ** CreateDirectoryConfig
    createDirectoryConfig_serviceAccountCredentials,
    createDirectoryConfig_directoryName,
    createDirectoryConfig_organizationalUnitDistinguishedNames,
    createDirectoryConfigResponse_directoryConfig,
    createDirectoryConfigResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_nextToken,
    describeUsers_maxResults,
    describeUsers_authenticationType,
    describeUsersResponse_users,
    describeUsersResponse_nextToken,
    describeUsersResponse_httpStatus,

    -- ** ListAssociatedStacks
    listAssociatedStacks_nextToken,
    listAssociatedStacks_fleetName,
    listAssociatedStacksResponse_nextToken,
    listAssociatedStacksResponse_names,
    listAssociatedStacksResponse_httpStatus,

    -- ** DeleteFleet
    deleteFleet_name,
    deleteFleetResponse_httpStatus,

    -- ** UpdateFleet
    updateFleet_domainJoinInfo,
    updateFleet_iamRoleArn,
    updateFleet_disconnectTimeoutInSeconds,
    updateFleet_maxUserDurationInSeconds,
    updateFleet_attributesToDelete,
    updateFleet_idleDisconnectTimeoutInSeconds,
    updateFleet_deleteVpcConfig,
    updateFleet_instanceType,
    updateFleet_vpcConfig,
    updateFleet_name,
    updateFleet_imageArn,
    updateFleet_displayName,
    updateFleet_enableDefaultInternetAccess,
    updateFleet_imageName,
    updateFleet_description,
    updateFleet_streamView,
    updateFleet_computeCapacity,
    updateFleetResponse_fleet,
    updateFleetResponse_httpStatus,

    -- ** DeleteImageBuilder
    deleteImageBuilder_name,
    deleteImageBuilderResponse_imageBuilder,
    deleteImageBuilderResponse_httpStatus,

    -- ** AssociateFleet
    associateFleet_fleetName,
    associateFleet_stackName,
    associateFleetResponse_httpStatus,

    -- ** CreateImageBuilder
    createImageBuilder_domainJoinInfo,
    createImageBuilder_iamRoleArn,
    createImageBuilder_accessEndpoints,
    createImageBuilder_vpcConfig,
    createImageBuilder_imageArn,
    createImageBuilder_displayName,
    createImageBuilder_enableDefaultInternetAccess,
    createImageBuilder_imageName,
    createImageBuilder_description,
    createImageBuilder_appstreamAgentVersion,
    createImageBuilder_tags,
    createImageBuilder_name,
    createImageBuilder_instanceType,
    createImageBuilderResponse_imageBuilder,
    createImageBuilderResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeDirectoryConfigs
    describeDirectoryConfigs_nextToken,
    describeDirectoryConfigs_directoryNames,
    describeDirectoryConfigs_maxResults,
    describeDirectoryConfigsResponse_nextToken,
    describeDirectoryConfigsResponse_directoryConfigs,
    describeDirectoryConfigsResponse_httpStatus,

    -- ** CreateImageBuilderStreamingURL
    createImageBuilderStreamingURL_validity,
    createImageBuilderStreamingURL_name,
    createImageBuilderStreamingURLResponse_streamingURL,
    createImageBuilderStreamingURLResponse_expires,
    createImageBuilderStreamingURLResponse_httpStatus,

    -- ** DescribeSessions
    describeSessions_userId,
    describeSessions_nextToken,
    describeSessions_limit,
    describeSessions_authenticationType,
    describeSessions_stackName,
    describeSessions_fleetName,
    describeSessionsResponse_nextToken,
    describeSessionsResponse_sessions,
    describeSessionsResponse_httpStatus,

    -- ** DescribeStacks
    describeStacks_nextToken,
    describeStacks_names,
    describeStacksResponse_nextToken,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,

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
    describeImageBuildersResponse_imageBuilders,
    describeImageBuildersResponse_nextToken,
    describeImageBuildersResponse_httpStatus,

    -- ** EnableUser
    enableUser_userName,
    enableUser_authenticationType,
    enableUserResponse_httpStatus,

    -- ** DescribeUserStackAssociations
    describeUserStackAssociations_userName,
    describeUserStackAssociations_nextToken,
    describeUserStackAssociations_authenticationType,
    describeUserStackAssociations_maxResults,
    describeUserStackAssociations_stackName,
    describeUserStackAssociationsResponse_userStackAssociations,
    describeUserStackAssociationsResponse_nextToken,
    describeUserStackAssociationsResponse_httpStatus,

    -- ** CreateUpdatedImage
    createUpdatedImage_newImageTags,
    createUpdatedImage_newImageDescription,
    createUpdatedImage_newImageDisplayName,
    createUpdatedImage_dryRun,
    createUpdatedImage_existingImageName,
    createUpdatedImage_newImageName,
    createUpdatedImageResponse_image,
    createUpdatedImageResponse_canUpdateImage,
    createUpdatedImageResponse_httpStatus,

    -- ** DescribeUsageReportSubscriptions
    describeUsageReportSubscriptions_nextToken,
    describeUsageReportSubscriptions_maxResults,
    describeUsageReportSubscriptionsResponse_usageReportSubscriptions,
    describeUsageReportSubscriptionsResponse_nextToken,
    describeUsageReportSubscriptionsResponse_httpStatus,

    -- ** UpdateImagePermissions
    updateImagePermissions_name,
    updateImagePermissions_sharedAccountId,
    updateImagePermissions_imagePermissions,
    updateImagePermissionsResponse_httpStatus,

    -- ** DeleteImagePermissions
    deleteImagePermissions_name,
    deleteImagePermissions_sharedAccountId,
    deleteImagePermissionsResponse_httpStatus,

    -- ** StopFleet
    stopFleet_name,
    stopFleetResponse_httpStatus,

    -- ** StartImageBuilder
    startImageBuilder_appstreamAgentVersion,
    startImageBuilder_name,
    startImageBuilderResponse_imageBuilder,
    startImageBuilderResponse_httpStatus,

    -- ** BatchAssociateUserStack
    batchAssociateUserStack_userStackAssociations,
    batchAssociateUserStackResponse_errors,
    batchAssociateUserStackResponse_httpStatus,

    -- ** DescribeImagePermissions
    describeImagePermissions_nextToken,
    describeImagePermissions_sharedAwsAccountIds,
    describeImagePermissions_maxResults,
    describeImagePermissions_name,
    describeImagePermissionsResponse_sharedImagePermissionsList,
    describeImagePermissionsResponse_nextToken,
    describeImagePermissionsResponse_name,
    describeImagePermissionsResponse_httpStatus,

    -- ** DeleteDirectoryConfig
    deleteDirectoryConfig_directoryName,
    deleteDirectoryConfigResponse_httpStatus,

    -- ** UpdateDirectoryConfig
    updateDirectoryConfig_serviceAccountCredentials,
    updateDirectoryConfig_organizationalUnitDistinguishedNames,
    updateDirectoryConfig_directoryName,
    updateDirectoryConfigResponse_directoryConfig,
    updateDirectoryConfigResponse_httpStatus,

    -- ** CreateFleet
    createFleet_domainJoinInfo,
    createFleet_iamRoleArn,
    createFleet_disconnectTimeoutInSeconds,
    createFleet_maxUserDurationInSeconds,
    createFleet_idleDisconnectTimeoutInSeconds,
    createFleet_fleetType,
    createFleet_vpcConfig,
    createFleet_imageArn,
    createFleet_displayName,
    createFleet_enableDefaultInternetAccess,
    createFleet_imageName,
    createFleet_description,
    createFleet_streamView,
    createFleet_tags,
    createFleet_name,
    createFleet_instanceType,
    createFleet_computeCapacity,
    createFleetResponse_fleet,
    createFleetResponse_httpStatus,

    -- ** CreateStack
    createStack_userSettings,
    createStack_applicationSettings,
    createStack_feedbackURL,
    createStack_storageConnectors,
    createStack_accessEndpoints,
    createStack_displayName,
    createStack_embedHostDomains,
    createStack_description,
    createStack_tags,
    createStack_redirectURL,
    createStack_name,
    createStackResponse_stack,
    createStackResponse_httpStatus,

    -- ** CopyImage
    copyImage_destinationImageDescription,
    copyImage_sourceImageName,
    copyImage_destinationImageName,
    copyImage_destinationRegion,
    copyImageResponse_destinationImageName,
    copyImageResponse_httpStatus,

    -- ** ExpireSession
    expireSession_sessionId,
    expireSessionResponse_httpStatus,

    -- ** CreateUser
    createUser_lastName,
    createUser_messageAction,
    createUser_firstName,
    createUser_userName,
    createUser_authenticationType,
    createUserResponse_httpStatus,

    -- ** DisableUser
    disableUser_userName,
    disableUser_authenticationType,
    disableUserResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userName,
    deleteUser_authenticationType,
    deleteUserResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

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

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** StartFleet
    startFleet_name,
    startFleetResponse_httpStatus,

    -- ** StopImageBuilder
    stopImageBuilder_name,
    stopImageBuilderResponse_imageBuilder,
    stopImageBuilderResponse_httpStatus,

    -- ** DeleteImage
    deleteImage_name,
    deleteImageResponse_image,
    deleteImageResponse_httpStatus,

    -- ** DeleteUsageReportSubscription
    deleteUsageReportSubscriptionResponse_httpStatus,

    -- ** BatchDisassociateUserStack
    batchDisassociateUserStack_userStackAssociations,
    batchDisassociateUserStackResponse_errors,
    batchDisassociateUserStackResponse_httpStatus,

    -- ** DescribeImages
    describeImages_nextToken,
    describeImages_names,
    describeImages_type,
    describeImages_arns,
    describeImages_maxResults,
    describeImagesResponse_images,
    describeImagesResponse_nextToken,
    describeImagesResponse_httpStatus,

    -- * Types

    -- ** AccessEndpoint
    accessEndpoint_vpceId,
    accessEndpoint_endpointType,

    -- ** Application
    application_enabled,
    application_launchPath,
    application_launchParameters,
    application_name,
    application_displayName,
    application_metadata,
    application_iconURL,

    -- ** ApplicationSettings
    applicationSettings_settingsGroup,
    applicationSettings_enabled,

    -- ** ApplicationSettingsResponse
    applicationSettingsResponse_enabled,
    applicationSettingsResponse_settingsGroup,
    applicationSettingsResponse_s3BucketName,

    -- ** ComputeCapacity
    computeCapacity_desiredInstances,

    -- ** ComputeCapacityStatus
    computeCapacityStatus_inUse,
    computeCapacityStatus_running,
    computeCapacityStatus_available,
    computeCapacityStatus_desired,

    -- ** DirectoryConfig
    directoryConfig_createdTime,
    directoryConfig_serviceAccountCredentials,
    directoryConfig_organizationalUnitDistinguishedNames,
    directoryConfig_directoryName,

    -- ** DomainJoinInfo
    domainJoinInfo_organizationalUnitDistinguishedName,
    domainJoinInfo_directoryName,

    -- ** Fleet
    fleet_domainJoinInfo,
    fleet_iamRoleArn,
    fleet_disconnectTimeoutInSeconds,
    fleet_maxUserDurationInSeconds,
    fleet_createdTime,
    fleet_idleDisconnectTimeoutInSeconds,
    fleet_fleetType,
    fleet_vpcConfig,
    fleet_imageArn,
    fleet_fleetErrors,
    fleet_displayName,
    fleet_enableDefaultInternetAccess,
    fleet_imageName,
    fleet_description,
    fleet_streamView,
    fleet_arn,
    fleet_name,
    fleet_instanceType,
    fleet_computeCapacityStatus,
    fleet_state,

    -- ** FleetError
    fleetError_errorCode,
    fleetError_errorMessage,

    -- ** Image
    image_state,
    image_imagePermissions,
    image_platform,
    image_publicBaseImageReleasedDate,
    image_stateChangeReason,
    image_arn,
    image_createdTime,
    image_imageBuilderSupported,
    image_visibility,
    image_imageBuilderName,
    image_imageErrors,
    image_baseImageArn,
    image_displayName,
    image_description,
    image_appstreamAgentVersion,
    image_applications,
    image_name,

    -- ** ImageBuilder
    imageBuilder_domainJoinInfo,
    imageBuilder_iamRoleArn,
    imageBuilder_state,
    imageBuilder_platform,
    imageBuilder_networkAccessConfiguration,
    imageBuilder_stateChangeReason,
    imageBuilder_arn,
    imageBuilder_createdTime,
    imageBuilder_imageBuilderErrors,
    imageBuilder_instanceType,
    imageBuilder_accessEndpoints,
    imageBuilder_vpcConfig,
    imageBuilder_imageArn,
    imageBuilder_displayName,
    imageBuilder_enableDefaultInternetAccess,
    imageBuilder_description,
    imageBuilder_appstreamAgentVersion,
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

    -- ** ServiceAccountCredentials
    serviceAccountCredentials_accountName,
    serviceAccountCredentials_accountPassword,

    -- ** Session
    session_networkAccessConfiguration,
    session_maxExpirationTime,
    session_startTime,
    session_authenticationType,
    session_connectionState,
    session_id,
    session_userId,
    session_stackName,
    session_fleetName,
    session_state,

    -- ** SharedImagePermissions
    sharedImagePermissions_sharedAccountId,
    sharedImagePermissions_imagePermissions,

    -- ** Stack
    stack_userSettings,
    stack_applicationSettings,
    stack_feedbackURL,
    stack_arn,
    stack_createdTime,
    stack_storageConnectors,
    stack_accessEndpoints,
    stack_displayName,
    stack_stackErrors,
    stack_embedHostDomains,
    stack_description,
    stack_redirectURL,
    stack_name,

    -- ** StackError
    stackError_errorCode,
    stackError_errorMessage,

    -- ** StorageConnector
    storageConnector_domains,
    storageConnector_resourceIdentifier,
    storageConnector_connectorType,

    -- ** UsageReportSubscription
    usageReportSubscription_lastGeneratedReportDate,
    usageReportSubscription_schedule,
    usageReportSubscription_subscriptionErrors,
    usageReportSubscription_s3BucketName,

    -- ** User
    user_status,
    user_enabled,
    user_lastName,
    user_arn,
    user_createdTime,
    user_userName,
    user_firstName,
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
    userStackAssociationError_errorCode,
    userStackAssociationError_errorMessage,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnetIds,
  )
where

import Amazonka.AppStream.AssociateFleet
import Amazonka.AppStream.BatchAssociateUserStack
import Amazonka.AppStream.BatchDisassociateUserStack
import Amazonka.AppStream.CopyImage
import Amazonka.AppStream.CreateDirectoryConfig
import Amazonka.AppStream.CreateFleet
import Amazonka.AppStream.CreateImageBuilder
import Amazonka.AppStream.CreateImageBuilderStreamingURL
import Amazonka.AppStream.CreateStack
import Amazonka.AppStream.CreateStreamingURL
import Amazonka.AppStream.CreateUpdatedImage
import Amazonka.AppStream.CreateUsageReportSubscription
import Amazonka.AppStream.CreateUser
import Amazonka.AppStream.DeleteDirectoryConfig
import Amazonka.AppStream.DeleteFleet
import Amazonka.AppStream.DeleteImage
import Amazonka.AppStream.DeleteImageBuilder
import Amazonka.AppStream.DeleteImagePermissions
import Amazonka.AppStream.DeleteStack
import Amazonka.AppStream.DeleteUsageReportSubscription
import Amazonka.AppStream.DeleteUser
import Amazonka.AppStream.DescribeDirectoryConfigs
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
import Amazonka.AppStream.DisassociateFleet
import Amazonka.AppStream.EnableUser
import Amazonka.AppStream.ExpireSession
import Amazonka.AppStream.ListAssociatedFleets
import Amazonka.AppStream.ListAssociatedStacks
import Amazonka.AppStream.ListTagsForResource
import Amazonka.AppStream.StartFleet
import Amazonka.AppStream.StartImageBuilder
import Amazonka.AppStream.StopFleet
import Amazonka.AppStream.StopImageBuilder
import Amazonka.AppStream.TagResource
import Amazonka.AppStream.Types.AccessEndpoint
import Amazonka.AppStream.Types.Application
import Amazonka.AppStream.Types.ApplicationSettings
import Amazonka.AppStream.Types.ApplicationSettingsResponse
import Amazonka.AppStream.Types.ComputeCapacity
import Amazonka.AppStream.Types.ComputeCapacityStatus
import Amazonka.AppStream.Types.DirectoryConfig
import Amazonka.AppStream.Types.DomainJoinInfo
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
import Amazonka.AppStream.Types.ServiceAccountCredentials
import Amazonka.AppStream.Types.Session
import Amazonka.AppStream.Types.SharedImagePermissions
import Amazonka.AppStream.Types.Stack
import Amazonka.AppStream.Types.StackError
import Amazonka.AppStream.Types.StorageConnector
import Amazonka.AppStream.Types.UsageReportSubscription
import Amazonka.AppStream.Types.User
import Amazonka.AppStream.Types.UserSetting
import Amazonka.AppStream.Types.UserStackAssociation
import Amazonka.AppStream.Types.UserStackAssociationError
import Amazonka.AppStream.Types.VpcConfig
import Amazonka.AppStream.UntagResource
import Amazonka.AppStream.UpdateDirectoryConfig
import Amazonka.AppStream.UpdateFleet
import Amazonka.AppStream.UpdateImagePermissions
import Amazonka.AppStream.UpdateStack
