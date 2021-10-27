{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSO.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSO.Lens
  ( -- * Operations

    -- ** Logout
    logout_accessToken,

    -- ** GetRoleCredentials
    getRoleCredentials_roleName,
    getRoleCredentials_accountId,
    getRoleCredentials_accessToken,
    getRoleCredentialsResponse_roleCredentials,
    getRoleCredentialsResponse_httpStatus,

    -- ** ListAccounts
    listAccounts_nextToken,
    listAccounts_maxResults,
    listAccounts_accessToken,
    listAccountsResponse_accountList,
    listAccountsResponse_nextToken,
    listAccountsResponse_httpStatus,

    -- ** ListAccountRoles
    listAccountRoles_nextToken,
    listAccountRoles_maxResults,
    listAccountRoles_accessToken,
    listAccountRoles_accountId,
    listAccountRolesResponse_roleList,
    listAccountRolesResponse_nextToken,
    listAccountRolesResponse_httpStatus,

    -- * Types

    -- ** AccountInfo
    accountInfo_accountName,
    accountInfo_accountId,
    accountInfo_emailAddress,

    -- ** RoleCredentials
    roleCredentials_secretAccessKey,
    roleCredentials_sessionToken,
    roleCredentials_expiration,
    roleCredentials_accessKeyId,

    -- ** RoleInfo
    roleInfo_roleName,
    roleInfo_accountId,
  )
where

import Network.AWS.SSO.GetRoleCredentials
import Network.AWS.SSO.ListAccountRoles
import Network.AWS.SSO.ListAccounts
import Network.AWS.SSO.Logout
import Network.AWS.SSO.Types.AccountInfo
import Network.AWS.SSO.Types.RoleCredentials
import Network.AWS.SSO.Types.RoleInfo
