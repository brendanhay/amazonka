{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Lens
  ( -- * Operations

    -- ** DeleteImageBuilder
    deleteImageBuilder_name,
    deleteImageBuilderResponse_imageBuilder,
    deleteImageBuilderResponse_httpStatus,

    -- ** ListAssociatedFleets
    listAssociatedFleets_nextToken,
    listAssociatedFleets_stackName,
    listAssociatedFleetsResponse_names,
    listAssociatedFleetsResponse_nextToken,
    listAssociatedFleetsResponse_httpStatus,

    -- ** BatchAssociateUserStack
    batchAssociateUserStack_userStackAssociations,
    batchAssociateUserStackResponse_errors,
    batchAssociateUserStackResponse_httpStatus,

    -- ** ListAssociatedStacks
    listAssociatedStacks_nextToken,
    listAssociatedStacks_fleetName,
    listAssociatedStacksResponse_names,
    listAssociatedStacksResponse_nextToken,
    listAssociatedStacksResponse_httpStatus,

    -- ** DeleteUsageReportSubscription
    deleteUsageReportSubscriptionResponse_httpStatus,

    -- ** StopImageBuilder
    stopImageBuilder_name,
    stopImageBuilderResponse_imageBuilder,
    stopImageBuilderResponse_httpStatus,

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

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** EnableUser
    enableUser_userName,
    enableUser_authenticationType,
    enableUserResponse_httpStatus,

    -- ** DescribeSessions
    describeSessions_nextToken,
    describeSessions_userId,
    describeSessions_authenticationType,
    describeSessions_limit,
    describeSessions_stackName,
    describeSessions_fleetName,
    describeSessionsResponse_nextToken,
    describeSessionsResponse_sessions,
    describeSessionsResponse_httpStatus,

    -- ** DescribeFleets
    describeFleets_names,
    describeFleets_nextToken,
    describeFleetsResponse_nextToken,
    describeFleetsResponse_fleets,
    describeFleetsResponse_httpStatus,

    -- ** DescribeStacks
    describeStacks_names,
    describeStacks_nextToken,
    describeStacksResponse_nextToken,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreateUser
    createUser_messageAction,
    createUser_firstName,
    createUser_lastName,
    createUser_userName,
    createUser_authenticationType,
    createUserResponse_httpStatus,

    -- ** UpdateDirectoryConfig
    updateDirectoryConfig_serviceAccountCredentials,
    updateDirectoryConfig_organizationalUnitDistinguishedNames,
    updateDirectoryConfig_directoryName,
    updateDirectoryConfigResponse_directoryConfig,
    updateDirectoryConfigResponse_httpStatus,

    -- ** CreateStack
    createStack_accessEndpoints,
    createStack_userSettings,
    createStack_redirectURL,
    createStack_applicationSettings,
    createStack_tags,
    createStack_storageConnectors,
    createStack_description,
    createStack_embedHostDomains,
    createStack_displayName,
    createStack_feedbackURL,
    createStack_name,
    createStackResponse_stack,
    createStackResponse_httpStatus,

    -- ** DeleteDirectoryConfig
    deleteDirectoryConfig_directoryName,
    deleteDirectoryConfigResponse_httpStatus,

    -- ** CopyImage
    copyImage_destinationImageDescription,
    copyImage_sourceImageName,
    copyImage_destinationImageName,
    copyImage_destinationRegion,
    copyImageResponse_destinationImageName,
    copyImageResponse_httpStatus,

    -- ** CreateFleet
    createFleet_maxUserDurationInSeconds,
    createFleet_disconnectTimeoutInSeconds,
    createFleet_vpcConfig,
    createFleet_iamRoleArn,
    createFleet_domainJoinInfo,
    createFleet_fleetType,
    createFleet_idleDisconnectTimeoutInSeconds,
    createFleet_imageName,
    createFleet_tags,
    createFleet_streamView,
    createFleet_description,
    createFleet_displayName,
    createFleet_enableDefaultInternetAccess,
    createFleet_imageArn,
    createFleet_name,
    createFleet_instanceType,
    createFleet_computeCapacity,
    createFleetResponse_fleet,
    createFleetResponse_httpStatus,

    -- ** CreateImageBuilder
    createImageBuilder_vpcConfig,
    createImageBuilder_iamRoleArn,
    createImageBuilder_accessEndpoints,
    createImageBuilder_domainJoinInfo,
    createImageBuilder_imageName,
    createImageBuilder_tags,
    createImageBuilder_appstreamAgentVersion,
    createImageBuilder_description,
    createImageBuilder_displayName,
    createImageBuilder_enableDefaultInternetAccess,
    createImageBuilder_imageArn,
    createImageBuilder_name,
    createImageBuilder_instanceType,
    createImageBuilderResponse_imageBuilder,
    createImageBuilderResponse_httpStatus,

    -- ** AssociateFleet
    associateFleet_fleetName,
    associateFleet_stackName,
    associateFleetResponse_httpStatus,

    -- ** CreateDirectoryConfig
    createDirectoryConfig_serviceAccountCredentials,
    createDirectoryConfig_directoryName,
    createDirectoryConfig_organizationalUnitDistinguishedNames,
    createDirectoryConfigResponse_directoryConfig,
    createDirectoryConfigResponse_httpStatus,

    -- ** UpdateFleet
    updateFleet_maxUserDurationInSeconds,
    updateFleet_disconnectTimeoutInSeconds,
    updateFleet_vpcConfig,
    updateFleet_iamRoleArn,
    updateFleet_domainJoinInfo,
    updateFleet_instanceType,
    updateFleet_computeCapacity,
    updateFleet_deleteVpcConfig,
    updateFleet_idleDisconnectTimeoutInSeconds,
    updateFleet_imageName,
    updateFleet_name,
    updateFleet_streamView,
    updateFleet_description,
    updateFleet_displayName,
    updateFleet_enableDefaultInternetAccess,
    updateFleet_attributesToDelete,
    updateFleet_imageArn,
    updateFleetResponse_fleet,
    updateFleetResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_name,
    deleteStackResponse_httpStatus,

    -- ** DeleteFleet
    deleteFleet_name,
    deleteFleetResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_nextToken,
    describeUsers_maxResults,
    describeUsers_authenticationType,
    describeUsersResponse_nextToken,
    describeUsersResponse_users,
    describeUsersResponse_httpStatus,

    -- ** UpdateStack
    updateStack_accessEndpoints,
    updateStack_userSettings,
    updateStack_redirectURL,
    updateStack_applicationSettings,
    updateStack_storageConnectors,
    updateStack_description,
    updateStack_embedHostDomains,
    updateStack_deleteStorageConnectors,
    updateStack_displayName,
    updateStack_attributesToDelete,
    updateStack_feedbackURL,
    updateStack_name,
    updateStackResponse_stack,
    updateStackResponse_httpStatus,

    -- ** CreateUsageReportSubscription
    createUsageReportSubscriptionResponse_s3BucketName,
    createUsageReportSubscriptionResponse_schedule,
    createUsageReportSubscriptionResponse_httpStatus,

    -- ** DisassociateFleet
    disassociateFleet_fleetName,
    disassociateFleet_stackName,
    disassociateFleetResponse_httpStatus,

    -- ** DescribeImages
    describeImages_names,
    describeImages_nextToken,
    describeImages_arns,
    describeImages_maxResults,
    describeImages_type,
    describeImagesResponse_nextToken,
    describeImagesResponse_images,
    describeImagesResponse_httpStatus,

    -- ** BatchDisassociateUserStack
    batchDisassociateUserStack_userStackAssociations,
    batchDisassociateUserStackResponse_errors,
    batchDisassociateUserStackResponse_httpStatus,

    -- ** DescribeUsageReportSubscriptions
    describeUsageReportSubscriptions_nextToken,
    describeUsageReportSubscriptions_maxResults,
    describeUsageReportSubscriptionsResponse_nextToken,
    describeUsageReportSubscriptionsResponse_usageReportSubscriptions,
    describeUsageReportSubscriptionsResponse_httpStatus,

    -- ** DeleteImage
    deleteImage_name,
    deleteImageResponse_image,
    deleteImageResponse_httpStatus,

    -- ** DeleteImagePermissions
    deleteImagePermissions_name,
    deleteImagePermissions_sharedAccountId,
    deleteImagePermissionsResponse_httpStatus,

    -- ** UpdateImagePermissions
    updateImagePermissions_name,
    updateImagePermissions_sharedAccountId,
    updateImagePermissions_imagePermissions,
    updateImagePermissionsResponse_httpStatus,

    -- ** CreateStreamingURL
    createStreamingURL_applicationId,
    createStreamingURL_sessionContext,
    createStreamingURL_validity,
    createStreamingURL_stackName,
    createStreamingURL_fleetName,
    createStreamingURL_userId,
    createStreamingURLResponse_streamingURL,
    createStreamingURLResponse_expires,
    createStreamingURLResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userName,
    deleteUser_authenticationType,
    deleteUserResponse_httpStatus,

    -- ** DescribeUserStackAssociations
    describeUserStackAssociations_nextToken,
    describeUserStackAssociations_stackName,
    describeUserStackAssociations_maxResults,
    describeUserStackAssociations_userName,
    describeUserStackAssociations_authenticationType,
    describeUserStackAssociationsResponse_nextToken,
    describeUserStackAssociationsResponse_userStackAssociations,
    describeUserStackAssociationsResponse_httpStatus,

    -- ** DescribeImageBuilders
    describeImageBuilders_names,
    describeImageBuilders_nextToken,
    describeImageBuilders_maxResults,
    describeImageBuildersResponse_nextToken,
    describeImageBuildersResponse_imageBuilders,
    describeImageBuildersResponse_httpStatus,

    -- ** DescribeDirectoryConfigs
    describeDirectoryConfigs_nextToken,
    describeDirectoryConfigs_maxResults,
    describeDirectoryConfigs_directoryNames,
    describeDirectoryConfigsResponse_nextToken,
    describeDirectoryConfigsResponse_directoryConfigs,
    describeDirectoryConfigsResponse_httpStatus,

    -- ** DisableUser
    disableUser_userName,
    disableUser_authenticationType,
    disableUserResponse_httpStatus,

    -- ** ExpireSession
    expireSession_sessionId,
    expireSessionResponse_httpStatus,

    -- ** CreateImageBuilderStreamingURL
    createImageBuilderStreamingURL_validity,
    createImageBuilderStreamingURL_name,
    createImageBuilderStreamingURLResponse_streamingURL,
    createImageBuilderStreamingURLResponse_expires,
    createImageBuilderStreamingURLResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeImagePermissions
    describeImagePermissions_nextToken,
    describeImagePermissions_maxResults,
    describeImagePermissions_sharedAwsAccountIds,
    describeImagePermissions_name,
    describeImagePermissionsResponse_sharedImagePermissionsList,
    describeImagePermissionsResponse_nextToken,
    describeImagePermissionsResponse_name,
    describeImagePermissionsResponse_httpStatus,

    -- * Types

    -- ** AccessEndpoint
    accessEndpoint_vpceId,
    accessEndpoint_endpointType,

    -- ** Application
    application_iconURL,
    application_launchPath,
    application_enabled,
    application_metadata,
    application_launchParameters,
    application_name,
    application_displayName,

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
    computeCapacityStatus_running,
    computeCapacityStatus_available,
    computeCapacityStatus_inUse,
    computeCapacityStatus_desired,

    -- ** DirectoryConfig
    directoryConfig_serviceAccountCredentials,
    directoryConfig_createdTime,
    directoryConfig_organizationalUnitDistinguishedNames,
    directoryConfig_directoryName,

    -- ** DomainJoinInfo
    domainJoinInfo_organizationalUnitDistinguishedName,
    domainJoinInfo_directoryName,

    -- ** Fleet
    fleet_maxUserDurationInSeconds,
    fleet_disconnectTimeoutInSeconds,
    fleet_vpcConfig,
    fleet_iamRoleArn,
    fleet_domainJoinInfo,
    fleet_fleetType,
    fleet_idleDisconnectTimeoutInSeconds,
    fleet_imageName,
    fleet_createdTime,
    fleet_streamView,
    fleet_description,
    fleet_displayName,
    fleet_enableDefaultInternetAccess,
    fleet_fleetErrors,
    fleet_imageArn,
    fleet_arn,
    fleet_name,
    fleet_instanceType,
    fleet_computeCapacityStatus,
    fleet_state,

    -- ** FleetError
    fleetError_errorMessage,
    fleetError_errorCode,

    -- ** Image
    image_imagePermissions,
    image_platform,
    image_imageBuilderName,
    image_arn,
    image_stateChangeReason,
    image_createdTime,
    image_state,
    image_baseImageArn,
    image_applications,
    image_visibility,
    image_appstreamAgentVersion,
    image_description,
    image_imageBuilderSupported,
    image_displayName,
    image_publicBaseImageReleasedDate,
    image_name,

    -- ** ImageBuilder
    imageBuilder_platform,
    imageBuilder_vpcConfig,
    imageBuilder_iamRoleArn,
    imageBuilder_accessEndpoints,
    imageBuilder_domainJoinInfo,
    imageBuilder_instanceType,
    imageBuilder_arn,
    imageBuilder_stateChangeReason,
    imageBuilder_createdTime,
    imageBuilder_networkAccessConfiguration,
    imageBuilder_state,
    imageBuilder_appstreamAgentVersion,
    imageBuilder_description,
    imageBuilder_displayName,
    imageBuilder_enableDefaultInternetAccess,
    imageBuilder_imageBuilderErrors,
    imageBuilder_imageArn,
    imageBuilder_name,

    -- ** ImageBuilderStateChangeReason
    imageBuilderStateChangeReason_message,
    imageBuilderStateChangeReason_code,

    -- ** ImagePermissions
    imagePermissions_allowImageBuilder,
    imagePermissions_allowFleet,

    -- ** ImageStateChangeReason
    imageStateChangeReason_message,
    imageStateChangeReason_code,

    -- ** LastReportGenerationExecutionError
    lastReportGenerationExecutionError_errorMessage,
    lastReportGenerationExecutionError_errorCode,

    -- ** NetworkAccessConfiguration
    networkAccessConfiguration_eniId,
    networkAccessConfiguration_eniPrivateIpAddress,

    -- ** ResourceError
    resourceError_errorTimestamp,
    resourceError_errorMessage,
    resourceError_errorCode,

    -- ** ServiceAccountCredentials
    serviceAccountCredentials_accountName,
    serviceAccountCredentials_accountPassword,

    -- ** Session
    session_connectionState,
    session_startTime,
    session_networkAccessConfiguration,
    session_authenticationType,
    session_maxExpirationTime,
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
    stack_userSettings,
    stack_redirectURL,
    stack_arn,
    stack_createdTime,
    stack_applicationSettings,
    stack_storageConnectors,
    stack_description,
    stack_embedHostDomains,
    stack_displayName,
    stack_stackErrors,
    stack_feedbackURL,
    stack_name,

    -- ** StackError
    stackError_errorMessage,
    stackError_errorCode,

    -- ** StorageConnector
    storageConnector_domains,
    storageConnector_resourceIdentifier,
    storageConnector_connectorType,

    -- ** UsageReportSubscription
    usageReportSubscription_subscriptionErrors,
    usageReportSubscription_lastGeneratedReportDate,
    usageReportSubscription_s3BucketName,
    usageReportSubscription_schedule,

    -- ** User
    user_status,
    user_arn,
    user_enabled,
    user_createdTime,
    user_userName,
    user_firstName,
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

import Network.AWS.AppStream.AssociateFleet
import Network.AWS.AppStream.BatchAssociateUserStack
import Network.AWS.AppStream.BatchDisassociateUserStack
import Network.AWS.AppStream.CopyImage
import Network.AWS.AppStream.CreateDirectoryConfig
import Network.AWS.AppStream.CreateFleet
import Network.AWS.AppStream.CreateImageBuilder
import Network.AWS.AppStream.CreateImageBuilderStreamingURL
import Network.AWS.AppStream.CreateStack
import Network.AWS.AppStream.CreateStreamingURL
import Network.AWS.AppStream.CreateUsageReportSubscription
import Network.AWS.AppStream.CreateUser
import Network.AWS.AppStream.DeleteDirectoryConfig
import Network.AWS.AppStream.DeleteFleet
import Network.AWS.AppStream.DeleteImage
import Network.AWS.AppStream.DeleteImageBuilder
import Network.AWS.AppStream.DeleteImagePermissions
import Network.AWS.AppStream.DeleteStack
import Network.AWS.AppStream.DeleteUsageReportSubscription
import Network.AWS.AppStream.DeleteUser
import Network.AWS.AppStream.DescribeDirectoryConfigs
import Network.AWS.AppStream.DescribeFleets
import Network.AWS.AppStream.DescribeImageBuilders
import Network.AWS.AppStream.DescribeImagePermissions
import Network.AWS.AppStream.DescribeImages
import Network.AWS.AppStream.DescribeSessions
import Network.AWS.AppStream.DescribeStacks
import Network.AWS.AppStream.DescribeUsageReportSubscriptions
import Network.AWS.AppStream.DescribeUserStackAssociations
import Network.AWS.AppStream.DescribeUsers
import Network.AWS.AppStream.DisableUser
import Network.AWS.AppStream.DisassociateFleet
import Network.AWS.AppStream.EnableUser
import Network.AWS.AppStream.ExpireSession
import Network.AWS.AppStream.ListAssociatedFleets
import Network.AWS.AppStream.ListAssociatedStacks
import Network.AWS.AppStream.ListTagsForResource
import Network.AWS.AppStream.StartFleet
import Network.AWS.AppStream.StartImageBuilder
import Network.AWS.AppStream.StopFleet
import Network.AWS.AppStream.StopImageBuilder
import Network.AWS.AppStream.TagResource
import Network.AWS.AppStream.Types.AccessEndpoint
import Network.AWS.AppStream.Types.Application
import Network.AWS.AppStream.Types.ApplicationSettings
import Network.AWS.AppStream.Types.ApplicationSettingsResponse
import Network.AWS.AppStream.Types.ComputeCapacity
import Network.AWS.AppStream.Types.ComputeCapacityStatus
import Network.AWS.AppStream.Types.DirectoryConfig
import Network.AWS.AppStream.Types.DomainJoinInfo
import Network.AWS.AppStream.Types.Fleet
import Network.AWS.AppStream.Types.FleetError
import Network.AWS.AppStream.Types.Image
import Network.AWS.AppStream.Types.ImageBuilder
import Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
import Network.AWS.AppStream.Types.ImagePermissions
import Network.AWS.AppStream.Types.ImageStateChangeReason
import Network.AWS.AppStream.Types.LastReportGenerationExecutionError
import Network.AWS.AppStream.Types.NetworkAccessConfiguration
import Network.AWS.AppStream.Types.ResourceError
import Network.AWS.AppStream.Types.ServiceAccountCredentials
import Network.AWS.AppStream.Types.Session
import Network.AWS.AppStream.Types.SharedImagePermissions
import Network.AWS.AppStream.Types.Stack
import Network.AWS.AppStream.Types.StackError
import Network.AWS.AppStream.Types.StorageConnector
import Network.AWS.AppStream.Types.UsageReportSubscription
import Network.AWS.AppStream.Types.User
import Network.AWS.AppStream.Types.UserSetting
import Network.AWS.AppStream.Types.UserStackAssociation
import Network.AWS.AppStream.Types.UserStackAssociationError
import Network.AWS.AppStream.Types.VpcConfig
import Network.AWS.AppStream.UntagResource
import Network.AWS.AppStream.UpdateDirectoryConfig
import Network.AWS.AppStream.UpdateFleet
import Network.AWS.AppStream.UpdateImagePermissions
import Network.AWS.AppStream.UpdateStack
