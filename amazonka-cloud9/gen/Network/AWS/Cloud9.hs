{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Cloud9
--
-- AWS Cloud9 is a collection of tools that you can use to code, build,
-- run, test, debug, and release software in the cloud.
--
-- For more information about AWS Cloud9, see the
-- <https://docs.aws.amazon.com/cloud9/latest/user-guide AWS Cloud9 User Guide>.
--
-- AWS Cloud9 supports these operations:
--
-- -   @CreateEnvironmentEC2@: Creates an AWS Cloud9 development
--     environment, launches an Amazon EC2 instance, and then connects from
--     the instance to the environment.
--
-- -   @CreateEnvironmentMembership@: Adds an environment member to an
--     environment.
--
-- -   @DeleteEnvironment@: Deletes an environment. If an Amazon EC2
--     instance is connected to the environment, also terminates the
--     instance.
--
-- -   @DeleteEnvironmentMembership@: Deletes an environment member from an
--     environment.
--
-- -   @DescribeEnvironmentMemberships@: Gets information about environment
--     members for an environment.
--
-- -   @DescribeEnvironments@: Gets information about environments.
--
-- -   @DescribeEnvironmentStatus@: Gets status information for an
--     environment.
--
-- -   @ListEnvironments@: Gets a list of environment identifiers.
--
-- -   @ListTagsForResource@: Gets the tags for an environment.
--
-- -   @TagResource@: Adds tags to an environment.
--
-- -   @UntagResource@: Removes tags from an environment.
--
-- -   @UpdateEnvironment@: Changes the settings of an existing
--     environment.
--
-- -   @UpdateEnvironmentMembership@: Changes the settings of an existing
--     environment member for an environment.
module Network.AWS.Cloud9
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConcurrentAccessException
    _ConcurrentAccessException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListEnvironments (Paginated)
    ListEnvironments (ListEnvironments'),
    newListEnvironments,
    ListEnvironmentsResponse (ListEnvironmentsResponse'),
    newListEnvironmentsResponse,

    -- ** CreateEnvironmentMembership
    CreateEnvironmentMembership (CreateEnvironmentMembership'),
    newCreateEnvironmentMembership,
    CreateEnvironmentMembershipResponse (CreateEnvironmentMembershipResponse'),
    newCreateEnvironmentMembershipResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeEnvironmentStatus
    DescribeEnvironmentStatus (DescribeEnvironmentStatus'),
    newDescribeEnvironmentStatus,
    DescribeEnvironmentStatusResponse (DescribeEnvironmentStatusResponse'),
    newDescribeEnvironmentStatusResponse,

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    UpdateEnvironmentResponse (UpdateEnvironmentResponse'),
    newUpdateEnvironmentResponse,

    -- ** DeleteEnvironment
    DeleteEnvironment (DeleteEnvironment'),
    newDeleteEnvironment,
    DeleteEnvironmentResponse (DeleteEnvironmentResponse'),
    newDeleteEnvironmentResponse,

    -- ** DescribeEnvironmentMemberships (Paginated)
    DescribeEnvironmentMemberships (DescribeEnvironmentMemberships'),
    newDescribeEnvironmentMemberships,
    DescribeEnvironmentMembershipsResponse (DescribeEnvironmentMembershipsResponse'),
    newDescribeEnvironmentMembershipsResponse,

    -- ** UpdateEnvironmentMembership
    UpdateEnvironmentMembership (UpdateEnvironmentMembership'),
    newUpdateEnvironmentMembership,
    UpdateEnvironmentMembershipResponse (UpdateEnvironmentMembershipResponse'),
    newUpdateEnvironmentMembershipResponse,

    -- ** DeleteEnvironmentMembership
    DeleteEnvironmentMembership (DeleteEnvironmentMembership'),
    newDeleteEnvironmentMembership,
    DeleteEnvironmentMembershipResponse (DeleteEnvironmentMembershipResponse'),
    newDeleteEnvironmentMembershipResponse,

    -- ** DescribeEnvironments
    DescribeEnvironments (DescribeEnvironments'),
    newDescribeEnvironments,
    DescribeEnvironmentsResponse (DescribeEnvironmentsResponse'),
    newDescribeEnvironmentsResponse,

    -- ** CreateEnvironmentEC
    CreateEnvironmentEC (CreateEnvironmentEC'),
    newCreateEnvironmentEC,
    CreateEnvironmentECResponse (CreateEnvironmentECResponse'),
    newCreateEnvironmentECResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- * Types

    -- ** ConnectionType
    ConnectionType (..),

    -- ** EnvironmentLifecycleStatus
    EnvironmentLifecycleStatus (..),

    -- ** EnvironmentStatus
    EnvironmentStatus (..),

    -- ** EnvironmentType
    EnvironmentType (..),

    -- ** MemberPermissions
    MemberPermissions (..),

    -- ** Permissions
    Permissions (..),

    -- ** Environment
    Environment (Environment'),
    newEnvironment,

    -- ** EnvironmentLifecycle
    EnvironmentLifecycle (EnvironmentLifecycle'),
    newEnvironmentLifecycle,

    -- ** EnvironmentMember
    EnvironmentMember (EnvironmentMember'),
    newEnvironmentMember,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.Cloud9.CreateEnvironmentEC
import Network.AWS.Cloud9.CreateEnvironmentMembership
import Network.AWS.Cloud9.DeleteEnvironment
import Network.AWS.Cloud9.DeleteEnvironmentMembership
import Network.AWS.Cloud9.DescribeEnvironmentMemberships
import Network.AWS.Cloud9.DescribeEnvironmentStatus
import Network.AWS.Cloud9.DescribeEnvironments
import Network.AWS.Cloud9.Lens
import Network.AWS.Cloud9.ListEnvironments
import Network.AWS.Cloud9.ListTagsForResource
import Network.AWS.Cloud9.TagResource
import Network.AWS.Cloud9.Types
import Network.AWS.Cloud9.UntagResource
import Network.AWS.Cloud9.UpdateEnvironment
import Network.AWS.Cloud9.UpdateEnvironmentMembership
import Network.AWS.Cloud9.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Cloud9'.

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
