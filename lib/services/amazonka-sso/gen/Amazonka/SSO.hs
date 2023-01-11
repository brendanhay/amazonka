{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SSO
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-06-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IAM Identity Center (successor to AWS Single Sign-On) Portal is a
-- web service that makes it easy for you to assign user access to IAM
-- Identity Center resources such as the AWS access portal. Users can get
-- AWS account applications and roles assigned to them and get federated
-- into the application.
--
-- Although AWS Single Sign-On was renamed, the @sso@ and @identitystore@
-- API namespaces will continue to retain their original name for backward
-- compatibility purposes. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/what-is.html#renamed IAM Identity Center rename>.
--
-- This reference guide describes the IAM Identity Center Portal operations
-- that you can call programatically and includes detailed information on
-- data types and errors.
--
-- AWS provides SDKs that consist of libraries and sample code for various
-- programming languages and platforms, such as Java, Ruby, .Net, iOS, or
-- Android. The SDKs provide a convenient way to create programmatic access
-- to IAM Identity Center and other AWS services. For more information
-- about the AWS SDKs, including how to download and install them, see
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
module Amazonka.SSO
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetRoleCredentials
    GetRoleCredentials (GetRoleCredentials'),
    newGetRoleCredentials,
    GetRoleCredentialsResponse (GetRoleCredentialsResponse'),
    newGetRoleCredentialsResponse,

    -- ** ListAccountRoles (Paginated)
    ListAccountRoles (ListAccountRoles'),
    newListAccountRoles,
    ListAccountRolesResponse (ListAccountRolesResponse'),
    newListAccountRolesResponse,

    -- ** ListAccounts (Paginated)
    ListAccounts (ListAccounts'),
    newListAccounts,
    ListAccountsResponse (ListAccountsResponse'),
    newListAccountsResponse,

    -- ** Logout
    Logout (Logout'),
    newLogout,
    LogoutResponse (LogoutResponse'),
    newLogoutResponse,

    -- * Types

    -- ** AccountInfo
    AccountInfo (AccountInfo'),
    newAccountInfo,

    -- ** RoleCredentials
    RoleCredentials (RoleCredentials'),
    newRoleCredentials,

    -- ** RoleInfo
    RoleInfo (RoleInfo'),
    newRoleInfo,
  )
where

import Amazonka.SSO.GetRoleCredentials
import Amazonka.SSO.Lens
import Amazonka.SSO.ListAccountRoles
import Amazonka.SSO.ListAccounts
import Amazonka.SSO.Logout
import Amazonka.SSO.Types
import Amazonka.SSO.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SSO'.

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
