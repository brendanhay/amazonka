{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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

    -- ** ResourceNotAvailableException
    _ResourceNotAvailableException,

    -- ** IncompatibleImageException
    _IncompatibleImageException,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** InvalidAccountStatusException
    _InvalidAccountStatusException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidRoleException
    _InvalidRoleException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** RequestLimitExceededException
    _RequestLimitExceededException,

    -- * Waiters
    -- $waiters

    -- ** FleetStopped
    newFleetStopped,

    -- ** FleetStarted
    newFleetStarted,

    -- * Operations
    -- $operations

    -- ** DeleteImageBuilder
    DeleteImageBuilder (DeleteImageBuilder'),
    newDeleteImageBuilder,
    DeleteImageBuilderResponse (DeleteImageBuilderResponse'),
    newDeleteImageBuilderResponse,

    -- ** ListAssociatedFleets (Paginated)
    ListAssociatedFleets (ListAssociatedFleets'),
    newListAssociatedFleets,
    ListAssociatedFleetsResponse (ListAssociatedFleetsResponse'),
    newListAssociatedFleetsResponse,

    -- ** BatchAssociateUserStack
    BatchAssociateUserStack (BatchAssociateUserStack'),
    newBatchAssociateUserStack,
    BatchAssociateUserStackResponse (BatchAssociateUserStackResponse'),
    newBatchAssociateUserStackResponse,

    -- ** ListAssociatedStacks (Paginated)
    ListAssociatedStacks (ListAssociatedStacks'),
    newListAssociatedStacks,
    ListAssociatedStacksResponse (ListAssociatedStacksResponse'),
    newListAssociatedStacksResponse,

    -- ** DeleteUsageReportSubscription
    DeleteUsageReportSubscription (DeleteUsageReportSubscription'),
    newDeleteUsageReportSubscription,
    DeleteUsageReportSubscriptionResponse (DeleteUsageReportSubscriptionResponse'),
    newDeleteUsageReportSubscriptionResponse,

    -- ** StopImageBuilder
    StopImageBuilder (StopImageBuilder'),
    newStopImageBuilder,
    StopImageBuilderResponse (StopImageBuilderResponse'),
    newStopImageBuilderResponse,

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

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** EnableUser
    EnableUser (EnableUser'),
    newEnableUser,
    EnableUserResponse (EnableUserResponse'),
    newEnableUserResponse,

    -- ** DescribeSessions (Paginated)
    DescribeSessions (DescribeSessions'),
    newDescribeSessions,
    DescribeSessionsResponse (DescribeSessionsResponse'),
    newDescribeSessionsResponse,

    -- ** DescribeFleets (Paginated)
    DescribeFleets (DescribeFleets'),
    newDescribeFleets,
    DescribeFleetsResponse (DescribeFleetsResponse'),
    newDescribeFleetsResponse,

    -- ** DescribeStacks (Paginated)
    DescribeStacks (DescribeStacks'),
    newDescribeStacks,
    DescribeStacksResponse (DescribeStacksResponse'),
    newDescribeStacksResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** UpdateDirectoryConfig
    UpdateDirectoryConfig (UpdateDirectoryConfig'),
    newUpdateDirectoryConfig,
    UpdateDirectoryConfigResponse (UpdateDirectoryConfigResponse'),
    newUpdateDirectoryConfigResponse,

    -- ** CreateStack
    CreateStack (CreateStack'),
    newCreateStack,
    CreateStackResponse (CreateStackResponse'),
    newCreateStackResponse,

    -- ** DeleteDirectoryConfig
    DeleteDirectoryConfig (DeleteDirectoryConfig'),
    newDeleteDirectoryConfig,
    DeleteDirectoryConfigResponse (DeleteDirectoryConfigResponse'),
    newDeleteDirectoryConfigResponse,

    -- ** CopyImage
    CopyImage (CopyImage'),
    newCopyImage,
    CopyImageResponse (CopyImageResponse'),
    newCopyImageResponse,

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

    -- ** AssociateFleet
    AssociateFleet (AssociateFleet'),
    newAssociateFleet,
    AssociateFleetResponse (AssociateFleetResponse'),
    newAssociateFleetResponse,

    -- ** CreateDirectoryConfig
    CreateDirectoryConfig (CreateDirectoryConfig'),
    newCreateDirectoryConfig,
    CreateDirectoryConfigResponse (CreateDirectoryConfigResponse'),
    newCreateDirectoryConfigResponse,

    -- ** UpdateFleet
    UpdateFleet (UpdateFleet'),
    newUpdateFleet,
    UpdateFleetResponse (UpdateFleetResponse'),
    newUpdateFleetResponse,

    -- ** DeleteStack
    DeleteStack (DeleteStack'),
    newDeleteStack,
    DeleteStackResponse (DeleteStackResponse'),
    newDeleteStackResponse,

    -- ** DeleteFleet
    DeleteFleet (DeleteFleet'),
    newDeleteFleet,
    DeleteFleetResponse (DeleteFleetResponse'),
    newDeleteFleetResponse,

    -- ** DescribeUsers (Paginated)
    DescribeUsers (DescribeUsers'),
    newDescribeUsers,
    DescribeUsersResponse (DescribeUsersResponse'),
    newDescribeUsersResponse,

    -- ** UpdateStack
    UpdateStack (UpdateStack'),
    newUpdateStack,
    UpdateStackResponse (UpdateStackResponse'),
    newUpdateStackResponse,

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

    -- ** DescribeImages (Paginated)
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- ** BatchDisassociateUserStack
    BatchDisassociateUserStack (BatchDisassociateUserStack'),
    newBatchDisassociateUserStack,
    BatchDisassociateUserStackResponse (BatchDisassociateUserStackResponse'),
    newBatchDisassociateUserStackResponse,

    -- ** DescribeUsageReportSubscriptions
    DescribeUsageReportSubscriptions (DescribeUsageReportSubscriptions'),
    newDescribeUsageReportSubscriptions,
    DescribeUsageReportSubscriptionsResponse (DescribeUsageReportSubscriptionsResponse'),
    newDescribeUsageReportSubscriptionsResponse,

    -- ** DeleteImage
    DeleteImage (DeleteImage'),
    newDeleteImage,
    DeleteImageResponse (DeleteImageResponse'),
    newDeleteImageResponse,

    -- ** DeleteImagePermissions
    DeleteImagePermissions (DeleteImagePermissions'),
    newDeleteImagePermissions,
    DeleteImagePermissionsResponse (DeleteImagePermissionsResponse'),
    newDeleteImagePermissionsResponse,

    -- ** UpdateImagePermissions
    UpdateImagePermissions (UpdateImagePermissions'),
    newUpdateImagePermissions,
    UpdateImagePermissionsResponse (UpdateImagePermissionsResponse'),
    newUpdateImagePermissionsResponse,

    -- ** CreateStreamingURL
    CreateStreamingURL (CreateStreamingURL'),
    newCreateStreamingURL,
    CreateStreamingURLResponse (CreateStreamingURLResponse'),
    newCreateStreamingURLResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DescribeUserStackAssociations (Paginated)
    DescribeUserStackAssociations (DescribeUserStackAssociations'),
    newDescribeUserStackAssociations,
    DescribeUserStackAssociationsResponse (DescribeUserStackAssociationsResponse'),
    newDescribeUserStackAssociationsResponse,

    -- ** DescribeImageBuilders (Paginated)
    DescribeImageBuilders (DescribeImageBuilders'),
    newDescribeImageBuilders,
    DescribeImageBuildersResponse (DescribeImageBuildersResponse'),
    newDescribeImageBuildersResponse,

    -- ** DescribeDirectoryConfigs (Paginated)
    DescribeDirectoryConfigs (DescribeDirectoryConfigs'),
    newDescribeDirectoryConfigs,
    DescribeDirectoryConfigsResponse (DescribeDirectoryConfigsResponse'),
    newDescribeDirectoryConfigsResponse,

    -- ** DisableUser
    DisableUser (DisableUser'),
    newDisableUser,
    DisableUserResponse (DisableUserResponse'),
    newDisableUserResponse,

    -- ** ExpireSession
    ExpireSession (ExpireSession'),
    newExpireSession,
    ExpireSessionResponse (ExpireSessionResponse'),
    newExpireSessionResponse,

    -- ** CreateImageBuilderStreamingURL
    CreateImageBuilderStreamingURL (CreateImageBuilderStreamingURL'),
    newCreateImageBuilderStreamingURL,
    CreateImageBuilderStreamingURLResponse (CreateImageBuilderStreamingURLResponse'),
    newCreateImageBuilderStreamingURLResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeImagePermissions
    DescribeImagePermissions (DescribeImagePermissions'),
    newDescribeImagePermissions,
    DescribeImagePermissionsResponse (DescribeImagePermissionsResponse'),
    newDescribeImagePermissionsResponse,

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
