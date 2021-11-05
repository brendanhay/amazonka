{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AppStream
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
module Amazonka.AppStream
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
import Amazonka.AppStream.Lens
import Amazonka.AppStream.ListAssociatedFleets
import Amazonka.AppStream.ListAssociatedStacks
import Amazonka.AppStream.ListTagsForResource
import Amazonka.AppStream.StartFleet
import Amazonka.AppStream.StartImageBuilder
import Amazonka.AppStream.StopFleet
import Amazonka.AppStream.StopImageBuilder
import Amazonka.AppStream.TagResource
import Amazonka.AppStream.Types
import Amazonka.AppStream.UntagResource
import Amazonka.AppStream.UpdateDirectoryConfig
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
