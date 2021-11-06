{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ChimeSDKIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-04-20@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon Chime SDK Identity APIs in this section allow software
-- developers to create and manage unique instances of their messaging
-- applications. These APIs provide the overarching framework for creating
-- and sending messages. For more information about the identity APIs,
-- refer to
-- <https://docs.aws.amazon.com/chime/latest/APIReference/API_Operations_Amazon_Chime_SDK_Identity.html Amazon Chime SDK identity>.
module Amazonka.ChimeSDKIdentity
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ThrottledClientException
    _ThrottledClientException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** UnauthorizedClientException
    _UnauthorizedClientException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAppInstance
    CreateAppInstance (CreateAppInstance'),
    newCreateAppInstance,
    CreateAppInstanceResponse (CreateAppInstanceResponse'),
    newCreateAppInstanceResponse,

    -- ** GetAppInstanceRetentionSettings
    GetAppInstanceRetentionSettings (GetAppInstanceRetentionSettings'),
    newGetAppInstanceRetentionSettings,
    GetAppInstanceRetentionSettingsResponse (GetAppInstanceRetentionSettingsResponse'),
    newGetAppInstanceRetentionSettingsResponse,

    -- ** DescribeAppInstanceAdmin
    DescribeAppInstanceAdmin (DescribeAppInstanceAdmin'),
    newDescribeAppInstanceAdmin,
    DescribeAppInstanceAdminResponse (DescribeAppInstanceAdminResponse'),
    newDescribeAppInstanceAdminResponse,

    -- ** CreateAppInstanceUser
    CreateAppInstanceUser (CreateAppInstanceUser'),
    newCreateAppInstanceUser,
    CreateAppInstanceUserResponse (CreateAppInstanceUserResponse'),
    newCreateAppInstanceUserResponse,

    -- ** PutAppInstanceRetentionSettings
    PutAppInstanceRetentionSettings (PutAppInstanceRetentionSettings'),
    newPutAppInstanceRetentionSettings,
    PutAppInstanceRetentionSettingsResponse (PutAppInstanceRetentionSettingsResponse'),
    newPutAppInstanceRetentionSettingsResponse,

    -- ** CreateAppInstanceAdmin
    CreateAppInstanceAdmin (CreateAppInstanceAdmin'),
    newCreateAppInstanceAdmin,
    CreateAppInstanceAdminResponse (CreateAppInstanceAdminResponse'),
    newCreateAppInstanceAdminResponse,

    -- ** ListAppInstanceAdmins
    ListAppInstanceAdmins (ListAppInstanceAdmins'),
    newListAppInstanceAdmins,
    ListAppInstanceAdminsResponse (ListAppInstanceAdminsResponse'),
    newListAppInstanceAdminsResponse,

    -- ** ListAppInstances
    ListAppInstances (ListAppInstances'),
    newListAppInstances,
    ListAppInstancesResponse (ListAppInstancesResponse'),
    newListAppInstancesResponse,

    -- ** DescribeAppInstanceUser
    DescribeAppInstanceUser (DescribeAppInstanceUser'),
    newDescribeAppInstanceUser,
    DescribeAppInstanceUserResponse (DescribeAppInstanceUserResponse'),
    newDescribeAppInstanceUserResponse,

    -- ** DescribeAppInstance
    DescribeAppInstance (DescribeAppInstance'),
    newDescribeAppInstance,
    DescribeAppInstanceResponse (DescribeAppInstanceResponse'),
    newDescribeAppInstanceResponse,

    -- ** ListAppInstanceUsers
    ListAppInstanceUsers (ListAppInstanceUsers'),
    newListAppInstanceUsers,
    ListAppInstanceUsersResponse (ListAppInstanceUsersResponse'),
    newListAppInstanceUsersResponse,

    -- ** DeleteAppInstanceUser
    DeleteAppInstanceUser (DeleteAppInstanceUser'),
    newDeleteAppInstanceUser,
    DeleteAppInstanceUserResponse (DeleteAppInstanceUserResponse'),
    newDeleteAppInstanceUserResponse,

    -- ** UpdateAppInstanceUser
    UpdateAppInstanceUser (UpdateAppInstanceUser'),
    newUpdateAppInstanceUser,
    UpdateAppInstanceUserResponse (UpdateAppInstanceUserResponse'),
    newUpdateAppInstanceUserResponse,

    -- ** DeleteAppInstanceAdmin
    DeleteAppInstanceAdmin (DeleteAppInstanceAdmin'),
    newDeleteAppInstanceAdmin,
    DeleteAppInstanceAdminResponse (DeleteAppInstanceAdminResponse'),
    newDeleteAppInstanceAdminResponse,

    -- ** DeleteAppInstance
    DeleteAppInstance (DeleteAppInstance'),
    newDeleteAppInstance,
    DeleteAppInstanceResponse (DeleteAppInstanceResponse'),
    newDeleteAppInstanceResponse,

    -- ** UpdateAppInstance
    UpdateAppInstance (UpdateAppInstance'),
    newUpdateAppInstance,
    UpdateAppInstanceResponse (UpdateAppInstanceResponse'),
    newUpdateAppInstanceResponse,

    -- * Types

    -- ** AppInstance
    AppInstance (AppInstance'),
    newAppInstance,

    -- ** AppInstanceAdmin
    AppInstanceAdmin (AppInstanceAdmin'),
    newAppInstanceAdmin,

    -- ** AppInstanceAdminSummary
    AppInstanceAdminSummary (AppInstanceAdminSummary'),
    newAppInstanceAdminSummary,

    -- ** AppInstanceRetentionSettings
    AppInstanceRetentionSettings (AppInstanceRetentionSettings'),
    newAppInstanceRetentionSettings,

    -- ** AppInstanceSummary
    AppInstanceSummary (AppInstanceSummary'),
    newAppInstanceSummary,

    -- ** AppInstanceUser
    AppInstanceUser (AppInstanceUser'),
    newAppInstanceUser,

    -- ** AppInstanceUserSummary
    AppInstanceUserSummary (AppInstanceUserSummary'),
    newAppInstanceUserSummary,

    -- ** ChannelRetentionSettings
    ChannelRetentionSettings (ChannelRetentionSettings'),
    newChannelRetentionSettings,

    -- ** Identity
    Identity (Identity'),
    newIdentity,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.ChimeSDKIdentity.CreateAppInstance
import Amazonka.ChimeSDKIdentity.CreateAppInstanceAdmin
import Amazonka.ChimeSDKIdentity.CreateAppInstanceUser
import Amazonka.ChimeSDKIdentity.DeleteAppInstance
import Amazonka.ChimeSDKIdentity.DeleteAppInstanceAdmin
import Amazonka.ChimeSDKIdentity.DeleteAppInstanceUser
import Amazonka.ChimeSDKIdentity.DescribeAppInstance
import Amazonka.ChimeSDKIdentity.DescribeAppInstanceAdmin
import Amazonka.ChimeSDKIdentity.DescribeAppInstanceUser
import Amazonka.ChimeSDKIdentity.GetAppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.Lens
import Amazonka.ChimeSDKIdentity.ListAppInstanceAdmins
import Amazonka.ChimeSDKIdentity.ListAppInstanceUsers
import Amazonka.ChimeSDKIdentity.ListAppInstances
import Amazonka.ChimeSDKIdentity.PutAppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.Types
import Amazonka.ChimeSDKIdentity.UpdateAppInstance
import Amazonka.ChimeSDKIdentity.UpdateAppInstanceUser
import Amazonka.ChimeSDKIdentity.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ChimeSDKIdentity'.

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
