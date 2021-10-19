{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.AppStream
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.AppStream
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRoleException
    _InvalidRoleException,

    -- ** RequestLimitExceededException
    _RequestLimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** IncompatibleImageException
    _IncompatibleImageException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** InvalidAccountStatusException
    _InvalidAccountStatusException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** ResourceNotAvailableException
    _ResourceNotAvailableException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- ** FleetStopped
    newFleetStopped,

    -- ** FleetStarted
    newFleetStarted,

    -- * Operations
    -- $operations

    -- ** CreateUsageReportSubscription
    CreateUsageReportSubscription (CreateUsageReportSubscription'),
    newCreateUsageReportSubscription,
    CreateUsageReportSubscriptionResponse (CreateUsageReportSubscriptionResponse'),
    newCreateUsageReportSubscriptionResponse,

    -- ** DisassociateFleet
    DisassociateFleet (DisassociateFleet'),
    newDisassociateFleet,
    DisassociateFleetResponse (DisassociateFleetResponse'),
    newDisassociateFleetResponse,

    -- ** ListAssociatedFleets (Paginated)
    ListAssociatedFleets (ListAssociatedFleets'),
    newListAssociatedFleets,
    ListAssociatedFleetsResponse (ListAssociatedFleetsResponse'),
    newListAssociatedFleetsResponse,

    -- ** DeleteStack
    DeleteStack (DeleteStack'),
    newDeleteStack,
    DeleteStackResponse (DeleteStackResponse'),
    newDeleteStackResponse,

    -- ** UpdateStack
    UpdateStack (UpdateStack'),
    newUpdateStack,
    UpdateStackResponse (UpdateStackResponse'),
    newUpdateStackResponse,

    -- ** CreateDirectoryConfig
    CreateDirectoryConfig (CreateDirectoryConfig'),
    newCreateDirectoryConfig,
    CreateDirectoryConfigResponse (CreateDirectoryConfigResponse'),
    newCreateDirectoryConfigResponse,

    -- ** DescribeUsers (Paginated)
    DescribeUsers (DescribeUsers'),
    newDescribeUsers,
    DescribeUsersResponse (DescribeUsersResponse'),
    newDescribeUsersResponse,

    -- ** ListAssociatedStacks (Paginated)
    ListAssociatedStacks (ListAssociatedStacks'),
    newListAssociatedStacks,
    ListAssociatedStacksResponse (ListAssociatedStacksResponse'),
    newListAssociatedStacksResponse,

    -- ** DeleteFleet
    DeleteFleet (DeleteFleet'),
    newDeleteFleet,
    DeleteFleetResponse (DeleteFleetResponse'),
    newDeleteFleetResponse,

    -- ** UpdateFleet
    UpdateFleet (UpdateFleet'),
    newUpdateFleet,
    UpdateFleetResponse (UpdateFleetResponse'),
    newUpdateFleetResponse,

    -- ** DeleteImageBuilder
    DeleteImageBuilder (DeleteImageBuilder'),
    newDeleteImageBuilder,
    DeleteImageBuilderResponse (DeleteImageBuilderResponse'),
    newDeleteImageBuilderResponse,

    -- ** AssociateFleet
    AssociateFleet (AssociateFleet'),
    newAssociateFleet,
    AssociateFleetResponse (AssociateFleetResponse'),
    newAssociateFleetResponse,

    -- ** CreateImageBuilder
    CreateImageBuilder (CreateImageBuilder'),
    newCreateImageBuilder,
    CreateImageBuilderResponse (CreateImageBuilderResponse'),
    newCreateImageBuilderResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeDirectoryConfigs (Paginated)
    DescribeDirectoryConfigs (DescribeDirectoryConfigs'),
    newDescribeDirectoryConfigs,
    DescribeDirectoryConfigsResponse (DescribeDirectoryConfigsResponse'),
    newDescribeDirectoryConfigsResponse,

    -- ** CreateImageBuilderStreamingURL
    CreateImageBuilderStreamingURL (CreateImageBuilderStreamingURL'),
    newCreateImageBuilderStreamingURL,
    CreateImageBuilderStreamingURLResponse (CreateImageBuilderStreamingURLResponse'),
    newCreateImageBuilderStreamingURLResponse,

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

    -- ** EnableUser
    EnableUser (EnableUser'),
    newEnableUser,
    EnableUserResponse (EnableUserResponse'),
    newEnableUserResponse,

    -- ** DescribeUserStackAssociations (Paginated)
    DescribeUserStackAssociations (DescribeUserStackAssociations'),
    newDescribeUserStackAssociations,
    DescribeUserStackAssociationsResponse (DescribeUserStackAssociationsResponse'),
    newDescribeUserStackAssociationsResponse,

    -- ** CreateUpdatedImage
    CreateUpdatedImage (CreateUpdatedImage'),
    newCreateUpdatedImage,
    CreateUpdatedImageResponse (CreateUpdatedImageResponse'),
    newCreateUpdatedImageResponse,

    -- ** DescribeUsageReportSubscriptions
    DescribeUsageReportSubscriptions (DescribeUsageReportSubscriptions'),
    newDescribeUsageReportSubscriptions,
    DescribeUsageReportSubscriptionsResponse (DescribeUsageReportSubscriptionsResponse'),
    newDescribeUsageReportSubscriptionsResponse,

    -- ** UpdateImagePermissions
    UpdateImagePermissions (UpdateImagePermissions'),
    newUpdateImagePermissions,
    UpdateImagePermissionsResponse (UpdateImagePermissionsResponse'),
    newUpdateImagePermissionsResponse,

    -- ** DeleteImagePermissions
    DeleteImagePermissions (DeleteImagePermissions'),
    newDeleteImagePermissions,
    DeleteImagePermissionsResponse (DeleteImagePermissionsResponse'),
    newDeleteImagePermissionsResponse,

    -- ** StopFleet
    StopFleet (StopFleet'),
    newStopFleet,
    StopFleetResponse (StopFleetResponse'),
    newStopFleetResponse,

    -- ** StartImageBuilder
    StartImageBuilder (StartImageBuilder'),
    newStartImageBuilder,
    StartImageBuilderResponse (StartImageBuilderResponse'),
    newStartImageBuilderResponse,

    -- ** BatchAssociateUserStack
    BatchAssociateUserStack (BatchAssociateUserStack'),
    newBatchAssociateUserStack,
    BatchAssociateUserStackResponse (BatchAssociateUserStackResponse'),
    newBatchAssociateUserStackResponse,

    -- ** DescribeImagePermissions
    DescribeImagePermissions (DescribeImagePermissions'),
    newDescribeImagePermissions,
    DescribeImagePermissionsResponse (DescribeImagePermissionsResponse'),
    newDescribeImagePermissionsResponse,

    -- ** DeleteDirectoryConfig
    DeleteDirectoryConfig (DeleteDirectoryConfig'),
    newDeleteDirectoryConfig,
    DeleteDirectoryConfigResponse (DeleteDirectoryConfigResponse'),
    newDeleteDirectoryConfigResponse,

    -- ** UpdateDirectoryConfig
    UpdateDirectoryConfig (UpdateDirectoryConfig'),
    newUpdateDirectoryConfig,
    UpdateDirectoryConfigResponse (UpdateDirectoryConfigResponse'),
    newUpdateDirectoryConfigResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

    -- ** CreateStack
    CreateStack (CreateStack'),
    newCreateStack,
    CreateStackResponse (CreateStackResponse'),
    newCreateStackResponse,

    -- ** CopyImage
    CopyImage (CopyImage'),
    newCopyImage,
    CopyImageResponse (CopyImageResponse'),
    newCopyImageResponse,

    -- ** ExpireSession
    ExpireSession (ExpireSession'),
    newExpireSession,
    ExpireSessionResponse (ExpireSessionResponse'),
    newExpireSessionResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** DisableUser
    DisableUser (DisableUser'),
    newDisableUser,
    DisableUserResponse (DisableUserResponse'),
    newDisableUserResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateStreamingURL
    CreateStreamingURL (CreateStreamingURL'),
    newCreateStreamingURL,
    CreateStreamingURLResponse (CreateStreamingURLResponse'),
    newCreateStreamingURLResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** StartFleet
    StartFleet (StartFleet'),
    newStartFleet,
    StartFleetResponse (StartFleetResponse'),
    newStartFleetResponse,

    -- ** StopImageBuilder
    StopImageBuilder (StopImageBuilder'),
    newStopImageBuilder,
    StopImageBuilderResponse (StopImageBuilderResponse'),
    newStopImageBuilderResponse,

    -- ** DeleteImage
    DeleteImage (DeleteImage'),
    newDeleteImage,
    DeleteImageResponse (DeleteImageResponse'),
    newDeleteImageResponse,

    -- ** DeleteUsageReportSubscription
    DeleteUsageReportSubscription (DeleteUsageReportSubscription'),
    newDeleteUsageReportSubscription,
    DeleteUsageReportSubscriptionResponse (DeleteUsageReportSubscriptionResponse'),
    newDeleteUsageReportSubscriptionResponse,

    -- ** BatchDisassociateUserStack
    BatchDisassociateUserStack (BatchDisassociateUserStack'),
    newBatchDisassociateUserStack,
    BatchDisassociateUserStackResponse (BatchDisassociateUserStackResponse'),
    newBatchDisassociateUserStackResponse,

    -- ** DescribeImages (Paginated)
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- * Types

    -- ** AccessEndpointType
    AccessEndpointType (..),

    -- ** Action
    Action (..),

    -- ** AuthenticationType
    AuthenticationType (..),

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

    -- ** Application
    Application (Application'),
    newApplication,

    -- ** ApplicationSettings
    ApplicationSettings (ApplicationSettings'),
    newApplicationSettings,

    -- ** ApplicationSettingsResponse
    ApplicationSettingsResponse (ApplicationSettingsResponse'),
    newApplicationSettingsResponse,

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
import Network.AWS.AppStream.CreateUpdatedImage
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
import Network.AWS.AppStream.Lens
import Network.AWS.AppStream.ListAssociatedFleets
import Network.AWS.AppStream.ListAssociatedStacks
import Network.AWS.AppStream.ListTagsForResource
import Network.AWS.AppStream.StartFleet
import Network.AWS.AppStream.StartImageBuilder
import Network.AWS.AppStream.StopFleet
import Network.AWS.AppStream.StopImageBuilder
import Network.AWS.AppStream.TagResource
import Network.AWS.AppStream.Types
import Network.AWS.AppStream.UntagResource
import Network.AWS.AppStream.UpdateDirectoryConfig
import Network.AWS.AppStream.UpdateFleet
import Network.AWS.AppStream.UpdateImagePermissions
import Network.AWS.AppStream.UpdateStack
import Network.AWS.AppStream.Waiters

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
