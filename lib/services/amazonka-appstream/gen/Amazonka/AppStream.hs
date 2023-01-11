{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AppStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-12-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon AppStream 2.0
--
-- This is the /Amazon AppStream 2.0 API Reference/. This documentation
-- provides descriptions and syntax for each of the actions and data types
-- in AppStream 2.0. AppStream 2.0 is a fully managed, secure application
-- streaming service that lets you stream desktop applications to users
-- without rewriting applications. AppStream 2.0 manages the AWS resources
-- that are required to host and run your applications, scales
-- automatically, and provides access to your users on demand.
--
-- You can call the AppStream 2.0 API operations by using an interface VPC
-- endpoint (interface endpoint). For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/access-api-cli-through-interface-vpc-endpoint.html Access AppStream 2.0 API Operations and CLI Commands Through an Interface VPC Endpoint>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- To learn more about AppStream 2.0, see the following resources:
--
-- -   <http://aws.amazon.com/appstream2 Amazon AppStream 2.0 product page>
--
-- -   <http://aws.amazon.com/documentation/appstream2 Amazon AppStream 2.0 documentation>
module Amazonka.AppStream
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** EntitlementAlreadyExistsException
    _EntitlementAlreadyExistsException,

    -- ** EntitlementNotFoundException
    _EntitlementNotFoundException,

    -- ** IncompatibleImageException
    _IncompatibleImageException,

    -- ** InvalidAccountStatusException
    _InvalidAccountStatusException,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** InvalidRoleException
    _InvalidRoleException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** RequestLimitExceededException
    _RequestLimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotAvailableException
    _ResourceNotAvailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** FleetStarted
    newFleetStarted,

    -- ** FleetStopped
    newFleetStopped,

    -- * Operations
    -- $operations

    -- ** AssociateApplicationFleet
    AssociateApplicationFleet (AssociateApplicationFleet'),
    newAssociateApplicationFleet,
    AssociateApplicationFleetResponse (AssociateApplicationFleetResponse'),
    newAssociateApplicationFleetResponse,

    -- ** AssociateApplicationToEntitlement
    AssociateApplicationToEntitlement (AssociateApplicationToEntitlement'),
    newAssociateApplicationToEntitlement,
    AssociateApplicationToEntitlementResponse (AssociateApplicationToEntitlementResponse'),
    newAssociateApplicationToEntitlementResponse,

    -- ** AssociateFleet
    AssociateFleet (AssociateFleet'),
    newAssociateFleet,
    AssociateFleetResponse (AssociateFleetResponse'),
    newAssociateFleetResponse,

    -- ** BatchAssociateUserStack
    BatchAssociateUserStack (BatchAssociateUserStack'),
    newBatchAssociateUserStack,
    BatchAssociateUserStackResponse (BatchAssociateUserStackResponse'),
    newBatchAssociateUserStackResponse,

    -- ** BatchDisassociateUserStack
    BatchDisassociateUserStack (BatchDisassociateUserStack'),
    newBatchDisassociateUserStack,
    BatchDisassociateUserStackResponse (BatchDisassociateUserStackResponse'),
    newBatchDisassociateUserStackResponse,

    -- ** CopyImage
    CopyImage (CopyImage'),
    newCopyImage,
    CopyImageResponse (CopyImageResponse'),
    newCopyImageResponse,

    -- ** CreateAppBlock
    CreateAppBlock (CreateAppBlock'),
    newCreateAppBlock,
    CreateAppBlockResponse (CreateAppBlockResponse'),
    newCreateAppBlockResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** CreateDirectoryConfig
    CreateDirectoryConfig (CreateDirectoryConfig'),
    newCreateDirectoryConfig,
    CreateDirectoryConfigResponse (CreateDirectoryConfigResponse'),
    newCreateDirectoryConfigResponse,

    -- ** CreateEntitlement
    CreateEntitlement (CreateEntitlement'),
    newCreateEntitlement,
    CreateEntitlementResponse (CreateEntitlementResponse'),
    newCreateEntitlementResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

    -- ** CreateImageBuilder
    CreateImageBuilder (CreateImageBuilder'),
    newCreateImageBuilder,
    CreateImageBuilderResponse (CreateImageBuilderResponse'),
    newCreateImageBuilderResponse,

    -- ** CreateImageBuilderStreamingURL
    CreateImageBuilderStreamingURL (CreateImageBuilderStreamingURL'),
    newCreateImageBuilderStreamingURL,
    CreateImageBuilderStreamingURLResponse (CreateImageBuilderStreamingURLResponse'),
    newCreateImageBuilderStreamingURLResponse,

    -- ** CreateStack
    CreateStack (CreateStack'),
    newCreateStack,
    CreateStackResponse (CreateStackResponse'),
    newCreateStackResponse,

    -- ** CreateStreamingURL
    CreateStreamingURL (CreateStreamingURL'),
    newCreateStreamingURL,
    CreateStreamingURLResponse (CreateStreamingURLResponse'),
    newCreateStreamingURLResponse,

    -- ** CreateUpdatedImage
    CreateUpdatedImage (CreateUpdatedImage'),
    newCreateUpdatedImage,
    CreateUpdatedImageResponse (CreateUpdatedImageResponse'),
    newCreateUpdatedImageResponse,

    -- ** CreateUsageReportSubscription
    CreateUsageReportSubscription (CreateUsageReportSubscription'),
    newCreateUsageReportSubscription,
    CreateUsageReportSubscriptionResponse (CreateUsageReportSubscriptionResponse'),
    newCreateUsageReportSubscriptionResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** DeleteAppBlock
    DeleteAppBlock (DeleteAppBlock'),
    newDeleteAppBlock,
    DeleteAppBlockResponse (DeleteAppBlockResponse'),
    newDeleteAppBlockResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** DeleteDirectoryConfig
    DeleteDirectoryConfig (DeleteDirectoryConfig'),
    newDeleteDirectoryConfig,
    DeleteDirectoryConfigResponse (DeleteDirectoryConfigResponse'),
    newDeleteDirectoryConfigResponse,

    -- ** DeleteEntitlement
    DeleteEntitlement (DeleteEntitlement'),
    newDeleteEntitlement,
    DeleteEntitlementResponse (DeleteEntitlementResponse'),
    newDeleteEntitlementResponse,

    -- ** DeleteFleet
    DeleteFleet (DeleteFleet'),
    newDeleteFleet,
    DeleteFleetResponse (DeleteFleetResponse'),
    newDeleteFleetResponse,

    -- ** DeleteImage
    DeleteImage (DeleteImage'),
    newDeleteImage,
    DeleteImageResponse (DeleteImageResponse'),
    newDeleteImageResponse,

    -- ** DeleteImageBuilder
    DeleteImageBuilder (DeleteImageBuilder'),
    newDeleteImageBuilder,
    DeleteImageBuilderResponse (DeleteImageBuilderResponse'),
    newDeleteImageBuilderResponse,

    -- ** DeleteImagePermissions
    DeleteImagePermissions (DeleteImagePermissions'),
    newDeleteImagePermissions,
    DeleteImagePermissionsResponse (DeleteImagePermissionsResponse'),
    newDeleteImagePermissionsResponse,

    -- ** DeleteStack
    DeleteStack (DeleteStack'),
    newDeleteStack,
    DeleteStackResponse (DeleteStackResponse'),
    newDeleteStackResponse,

    -- ** DeleteUsageReportSubscription
    DeleteUsageReportSubscription (DeleteUsageReportSubscription'),
    newDeleteUsageReportSubscription,
    DeleteUsageReportSubscriptionResponse (DeleteUsageReportSubscriptionResponse'),
    newDeleteUsageReportSubscriptionResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DescribeAppBlocks
    DescribeAppBlocks (DescribeAppBlocks'),
    newDescribeAppBlocks,
    DescribeAppBlocksResponse (DescribeAppBlocksResponse'),
    newDescribeAppBlocksResponse,

    -- ** DescribeApplicationFleetAssociations
    DescribeApplicationFleetAssociations (DescribeApplicationFleetAssociations'),
    newDescribeApplicationFleetAssociations,
    DescribeApplicationFleetAssociationsResponse (DescribeApplicationFleetAssociationsResponse'),
    newDescribeApplicationFleetAssociationsResponse,

    -- ** DescribeApplications
    DescribeApplications (DescribeApplications'),
    newDescribeApplications,
    DescribeApplicationsResponse (DescribeApplicationsResponse'),
    newDescribeApplicationsResponse,

    -- ** DescribeDirectoryConfigs (Paginated)
    DescribeDirectoryConfigs (DescribeDirectoryConfigs'),
    newDescribeDirectoryConfigs,
    DescribeDirectoryConfigsResponse (DescribeDirectoryConfigsResponse'),
    newDescribeDirectoryConfigsResponse,

    -- ** DescribeEntitlements
    DescribeEntitlements (DescribeEntitlements'),
    newDescribeEntitlements,
    DescribeEntitlementsResponse (DescribeEntitlementsResponse'),
    newDescribeEntitlementsResponse,

    -- ** DescribeFleets (Paginated)
    DescribeFleets (DescribeFleets'),
    newDescribeFleets,
    DescribeFleetsResponse (DescribeFleetsResponse'),
    newDescribeFleetsResponse,

    -- ** DescribeImageBuilders (Paginated)
    DescribeImageBuilders (DescribeImageBuilders'),
    newDescribeImageBuilders,
    DescribeImageBuildersResponse (DescribeImageBuildersResponse'),
    newDescribeImageBuildersResponse,

    -- ** DescribeImagePermissions
    DescribeImagePermissions (DescribeImagePermissions'),
    newDescribeImagePermissions,
    DescribeImagePermissionsResponse (DescribeImagePermissionsResponse'),
    newDescribeImagePermissionsResponse,

    -- ** DescribeImages (Paginated)
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- ** DescribeSessions (Paginated)
    DescribeSessions (DescribeSessions'),
    newDescribeSessions,
    DescribeSessionsResponse (DescribeSessionsResponse'),
    newDescribeSessionsResponse,

    -- ** DescribeStacks (Paginated)
    DescribeStacks (DescribeStacks'),
    newDescribeStacks,
    DescribeStacksResponse (DescribeStacksResponse'),
    newDescribeStacksResponse,

    -- ** DescribeUsageReportSubscriptions
    DescribeUsageReportSubscriptions (DescribeUsageReportSubscriptions'),
    newDescribeUsageReportSubscriptions,
    DescribeUsageReportSubscriptionsResponse (DescribeUsageReportSubscriptionsResponse'),
    newDescribeUsageReportSubscriptionsResponse,

    -- ** DescribeUserStackAssociations (Paginated)
    DescribeUserStackAssociations (DescribeUserStackAssociations'),
    newDescribeUserStackAssociations,
    DescribeUserStackAssociationsResponse (DescribeUserStackAssociationsResponse'),
    newDescribeUserStackAssociationsResponse,

    -- ** DescribeUsers (Paginated)
    DescribeUsers (DescribeUsers'),
    newDescribeUsers,
    DescribeUsersResponse (DescribeUsersResponse'),
    newDescribeUsersResponse,

    -- ** DisableUser
    DisableUser (DisableUser'),
    newDisableUser,
    DisableUserResponse (DisableUserResponse'),
    newDisableUserResponse,

    -- ** DisassociateApplicationFleet
    DisassociateApplicationFleet (DisassociateApplicationFleet'),
    newDisassociateApplicationFleet,
    DisassociateApplicationFleetResponse (DisassociateApplicationFleetResponse'),
    newDisassociateApplicationFleetResponse,

    -- ** DisassociateApplicationFromEntitlement
    DisassociateApplicationFromEntitlement (DisassociateApplicationFromEntitlement'),
    newDisassociateApplicationFromEntitlement,
    DisassociateApplicationFromEntitlementResponse (DisassociateApplicationFromEntitlementResponse'),
    newDisassociateApplicationFromEntitlementResponse,

    -- ** DisassociateFleet
    DisassociateFleet (DisassociateFleet'),
    newDisassociateFleet,
    DisassociateFleetResponse (DisassociateFleetResponse'),
    newDisassociateFleetResponse,

    -- ** EnableUser
    EnableUser (EnableUser'),
    newEnableUser,
    EnableUserResponse (EnableUserResponse'),
    newEnableUserResponse,

    -- ** ExpireSession
    ExpireSession (ExpireSession'),
    newExpireSession,
    ExpireSessionResponse (ExpireSessionResponse'),
    newExpireSessionResponse,

    -- ** ListAssociatedFleets (Paginated)
    ListAssociatedFleets (ListAssociatedFleets'),
    newListAssociatedFleets,
    ListAssociatedFleetsResponse (ListAssociatedFleetsResponse'),
    newListAssociatedFleetsResponse,

    -- ** ListAssociatedStacks (Paginated)
    ListAssociatedStacks (ListAssociatedStacks'),
    newListAssociatedStacks,
    ListAssociatedStacksResponse (ListAssociatedStacksResponse'),
    newListAssociatedStacksResponse,

    -- ** ListEntitledApplications
    ListEntitledApplications (ListEntitledApplications'),
    newListEntitledApplications,
    ListEntitledApplicationsResponse (ListEntitledApplicationsResponse'),
    newListEntitledApplicationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartFleet
    StartFleet (StartFleet'),
    newStartFleet,
    StartFleetResponse (StartFleetResponse'),
    newStartFleetResponse,

    -- ** StartImageBuilder
    StartImageBuilder (StartImageBuilder'),
    newStartImageBuilder,
    StartImageBuilderResponse (StartImageBuilderResponse'),
    newStartImageBuilderResponse,

    -- ** StopFleet
    StopFleet (StopFleet'),
    newStopFleet,
    StopFleetResponse (StopFleetResponse'),
    newStopFleetResponse,

    -- ** StopImageBuilder
    StopImageBuilder (StopImageBuilder'),
    newStopImageBuilder,
    StopImageBuilderResponse (StopImageBuilderResponse'),
    newStopImageBuilderResponse,

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

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    UpdateApplicationResponse (UpdateApplicationResponse'),
    newUpdateApplicationResponse,

    -- ** UpdateDirectoryConfig
    UpdateDirectoryConfig (UpdateDirectoryConfig'),
    newUpdateDirectoryConfig,
    UpdateDirectoryConfigResponse (UpdateDirectoryConfigResponse'),
    newUpdateDirectoryConfigResponse,

    -- ** UpdateEntitlement
    UpdateEntitlement (UpdateEntitlement'),
    newUpdateEntitlement,
    UpdateEntitlementResponse (UpdateEntitlementResponse'),
    newUpdateEntitlementResponse,

    -- ** UpdateFleet
    UpdateFleet (UpdateFleet'),
    newUpdateFleet,
    UpdateFleetResponse (UpdateFleetResponse'),
    newUpdateFleetResponse,

    -- ** UpdateImagePermissions
    UpdateImagePermissions (UpdateImagePermissions'),
    newUpdateImagePermissions,
    UpdateImagePermissionsResponse (UpdateImagePermissionsResponse'),
    newUpdateImagePermissionsResponse,

    -- ** UpdateStack
    UpdateStack (UpdateStack'),
    newUpdateStack,
    UpdateStackResponse (UpdateStackResponse'),
    newUpdateStackResponse,

    -- * Types

    -- ** AccessEndpointType
    AccessEndpointType (..),

    -- ** Action
    Action (..),

    -- ** AppVisibility
    AppVisibility (..),

    -- ** ApplicationAttribute
    ApplicationAttribute (..),

    -- ** AuthenticationType
    AuthenticationType (..),

    -- ** CertificateBasedAuthStatus
    CertificateBasedAuthStatus (..),

    -- ** FleetAttribute
    FleetAttribute (..),

    -- ** FleetErrorCode
    FleetErrorCode (..),

    -- ** FleetState
    FleetState (..),

    -- ** FleetType
    FleetType (..),

    -- ** ImageBuilderState
    ImageBuilderState (..),

    -- ** ImageBuilderStateChangeReasonCode
    ImageBuilderStateChangeReasonCode (..),

    -- ** ImageState
    ImageState (..),

    -- ** ImageStateChangeReasonCode
    ImageStateChangeReasonCode (..),

    -- ** MessageAction
    MessageAction (..),

    -- ** Permission
    Permission (..),

    -- ** PlatformType
    PlatformType (..),

    -- ** PreferredProtocol
    PreferredProtocol (..),

    -- ** SessionConnectionState
    SessionConnectionState (..),

    -- ** SessionState
    SessionState (..),

    -- ** StackAttribute
    StackAttribute (..),

    -- ** StackErrorCode
    StackErrorCode (..),

    -- ** StorageConnectorType
    StorageConnectorType (..),

    -- ** StreamView
    StreamView (..),

    -- ** UsageReportExecutionErrorCode
    UsageReportExecutionErrorCode (..),

    -- ** UsageReportSchedule
    UsageReportSchedule (..),

    -- ** UserStackAssociationErrorCode
    UserStackAssociationErrorCode (..),

    -- ** VisibilityType
    VisibilityType (..),

    -- ** AccessEndpoint
    AccessEndpoint (AccessEndpoint'),
    newAccessEndpoint,

    -- ** AppBlock
    AppBlock (AppBlock'),
    newAppBlock,

    -- ** Application
    Application (Application'),
    newApplication,

    -- ** ApplicationFleetAssociation
    ApplicationFleetAssociation (ApplicationFleetAssociation'),
    newApplicationFleetAssociation,

    -- ** ApplicationSettings
    ApplicationSettings (ApplicationSettings'),
    newApplicationSettings,

    -- ** ApplicationSettingsResponse
    ApplicationSettingsResponse (ApplicationSettingsResponse'),
    newApplicationSettingsResponse,

    -- ** CertificateBasedAuthProperties
    CertificateBasedAuthProperties (CertificateBasedAuthProperties'),
    newCertificateBasedAuthProperties,

    -- ** ComputeCapacity
    ComputeCapacity (ComputeCapacity'),
    newComputeCapacity,

    -- ** ComputeCapacityStatus
    ComputeCapacityStatus (ComputeCapacityStatus'),
    newComputeCapacityStatus,

    -- ** DirectoryConfig
    DirectoryConfig (DirectoryConfig'),
    newDirectoryConfig,

    -- ** DomainJoinInfo
    DomainJoinInfo (DomainJoinInfo'),
    newDomainJoinInfo,

    -- ** EntitledApplication
    EntitledApplication (EntitledApplication'),
    newEntitledApplication,

    -- ** Entitlement
    Entitlement (Entitlement'),
    newEntitlement,

    -- ** EntitlementAttribute
    EntitlementAttribute (EntitlementAttribute'),
    newEntitlementAttribute,

    -- ** Fleet
    Fleet (Fleet'),
    newFleet,

    -- ** FleetError
    FleetError (FleetError'),
    newFleetError,

    -- ** Image
    Image (Image'),
    newImage,

    -- ** ImageBuilder
    ImageBuilder (ImageBuilder'),
    newImageBuilder,

    -- ** ImageBuilderStateChangeReason
    ImageBuilderStateChangeReason (ImageBuilderStateChangeReason'),
    newImageBuilderStateChangeReason,

    -- ** ImagePermissions
    ImagePermissions (ImagePermissions'),
    newImagePermissions,

    -- ** ImageStateChangeReason
    ImageStateChangeReason (ImageStateChangeReason'),
    newImageStateChangeReason,

    -- ** LastReportGenerationExecutionError
    LastReportGenerationExecutionError (LastReportGenerationExecutionError'),
    newLastReportGenerationExecutionError,

    -- ** NetworkAccessConfiguration
    NetworkAccessConfiguration (NetworkAccessConfiguration'),
    newNetworkAccessConfiguration,

    -- ** ResourceError
    ResourceError (ResourceError'),
    newResourceError,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** ScriptDetails
    ScriptDetails (ScriptDetails'),
    newScriptDetails,

    -- ** ServiceAccountCredentials
    ServiceAccountCredentials (ServiceAccountCredentials'),
    newServiceAccountCredentials,

    -- ** Session
    Session (Session'),
    newSession,

    -- ** SharedImagePermissions
    SharedImagePermissions (SharedImagePermissions'),
    newSharedImagePermissions,

    -- ** Stack
    Stack (Stack'),
    newStack,

    -- ** StackError
    StackError (StackError'),
    newStackError,

    -- ** StorageConnector
    StorageConnector (StorageConnector'),
    newStorageConnector,

    -- ** StreamingExperienceSettings
    StreamingExperienceSettings (StreamingExperienceSettings'),
    newStreamingExperienceSettings,

    -- ** UsageReportSubscription
    UsageReportSubscription (UsageReportSubscription'),
    newUsageReportSubscription,

    -- ** User
    User (User'),
    newUser,

    -- ** UserSetting
    UserSetting (UserSetting'),
    newUserSetting,

    -- ** UserStackAssociation
    UserStackAssociation (UserStackAssociation'),
    newUserStackAssociation,

    -- ** UserStackAssociationError
    UserStackAssociationError (UserStackAssociationError'),
    newUserStackAssociationError,

    -- ** VpcConfig
    VpcConfig (VpcConfig'),
    newVpcConfig,
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
import Amazonka.AppStream.Lens
import Amazonka.AppStream.ListAssociatedFleets
import Amazonka.AppStream.ListAssociatedStacks
import Amazonka.AppStream.ListEntitledApplications
import Amazonka.AppStream.ListTagsForResource
import Amazonka.AppStream.StartFleet
import Amazonka.AppStream.StartImageBuilder
import Amazonka.AppStream.StopFleet
import Amazonka.AppStream.StopImageBuilder
import Amazonka.AppStream.TagResource
import Amazonka.AppStream.Types
import Amazonka.AppStream.UntagResource
import Amazonka.AppStream.UpdateApplication
import Amazonka.AppStream.UpdateDirectoryConfig
import Amazonka.AppStream.UpdateEntitlement
import Amazonka.AppStream.UpdateFleet
import Amazonka.AppStream.UpdateImagePermissions
import Amazonka.AppStream.UpdateStack
import Amazonka.AppStream.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AppStream'.

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
