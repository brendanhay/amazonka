{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Cloud9
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-09-23@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Cloud9
--
-- Cloud9 is a collection of tools that you can use to code, build, run,
-- test, debug, and release software in the cloud.
--
-- For more information about Cloud9, see the
-- <https://docs.aws.amazon.com/cloud9/latest/user-guide Cloud9 User Guide>.
--
-- Cloud9 supports these operations:
--
-- -   @CreateEnvironmentEC2@: Creates an Cloud9 development environment,
--     launches an Amazon EC2 instance, and then connects from the instance
--     to the environment.
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
module Amazonka.Cloud9
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConcurrentAccessException
    _ConcurrentAccessException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateEnvironmentEC2
    CreateEnvironmentEC2 (CreateEnvironmentEC2'),
    newCreateEnvironmentEC2,
    CreateEnvironmentEC2Response (CreateEnvironmentEC2Response'),
    newCreateEnvironmentEC2Response,

    -- ** CreateEnvironmentMembership
    CreateEnvironmentMembership (CreateEnvironmentMembership'),
    newCreateEnvironmentMembership,
    CreateEnvironmentMembershipResponse (CreateEnvironmentMembershipResponse'),
    newCreateEnvironmentMembershipResponse,

    -- ** DeleteEnvironment
    DeleteEnvironment (DeleteEnvironment'),
    newDeleteEnvironment,
    DeleteEnvironmentResponse (DeleteEnvironmentResponse'),
    newDeleteEnvironmentResponse,

    -- ** DeleteEnvironmentMembership
    DeleteEnvironmentMembership (DeleteEnvironmentMembership'),
    newDeleteEnvironmentMembership,
    DeleteEnvironmentMembershipResponse (DeleteEnvironmentMembershipResponse'),
    newDeleteEnvironmentMembershipResponse,

    -- ** DescribeEnvironmentMemberships (Paginated)
    DescribeEnvironmentMemberships (DescribeEnvironmentMemberships'),
    newDescribeEnvironmentMemberships,
    DescribeEnvironmentMembershipsResponse (DescribeEnvironmentMembershipsResponse'),
    newDescribeEnvironmentMembershipsResponse,

    -- ** DescribeEnvironmentStatus
    DescribeEnvironmentStatus (DescribeEnvironmentStatus'),
    newDescribeEnvironmentStatus,
    DescribeEnvironmentStatusResponse (DescribeEnvironmentStatusResponse'),
    newDescribeEnvironmentStatusResponse,

    -- ** DescribeEnvironments
    DescribeEnvironments (DescribeEnvironments'),
    newDescribeEnvironments,
    DescribeEnvironmentsResponse (DescribeEnvironmentsResponse'),
    newDescribeEnvironmentsResponse,

    -- ** ListEnvironments (Paginated)
    ListEnvironments (ListEnvironments'),
    newListEnvironments,
    ListEnvironmentsResponse (ListEnvironmentsResponse'),
    newListEnvironmentsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    UpdateEnvironmentResponse (UpdateEnvironmentResponse'),
    newUpdateEnvironmentResponse,

    -- ** UpdateEnvironmentMembership
    UpdateEnvironmentMembership (UpdateEnvironmentMembership'),
    newUpdateEnvironmentMembership,
    UpdateEnvironmentMembershipResponse (UpdateEnvironmentMembershipResponse'),
    newUpdateEnvironmentMembershipResponse,

    -- * Types

    -- ** ConnectionType
    ConnectionType (..),

    -- ** EnvironmentLifecycleStatus
    EnvironmentLifecycleStatus (..),

    -- ** EnvironmentStatus
    EnvironmentStatus (..),

    -- ** EnvironmentType
    EnvironmentType (..),

    -- ** ManagedCredentialsAction
    ManagedCredentialsAction (..),

    -- ** ManagedCredentialsStatus
    ManagedCredentialsStatus (..),

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

import Amazonka.Cloud9.CreateEnvironmentEC2
import Amazonka.Cloud9.CreateEnvironmentMembership
import Amazonka.Cloud9.DeleteEnvironment
import Amazonka.Cloud9.DeleteEnvironmentMembership
import Amazonka.Cloud9.DescribeEnvironmentMemberships
import Amazonka.Cloud9.DescribeEnvironmentStatus
import Amazonka.Cloud9.DescribeEnvironments
import Amazonka.Cloud9.Lens
import Amazonka.Cloud9.ListEnvironments
import Amazonka.Cloud9.ListTagsForResource
import Amazonka.Cloud9.TagResource
import Amazonka.Cloud9.Types
import Amazonka.Cloud9.UntagResource
import Amazonka.Cloud9.UpdateEnvironment
import Amazonka.Cloud9.UpdateEnvironmentMembership
import Amazonka.Cloud9.Waiters

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
