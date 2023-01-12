{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSO.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSO.Lens
  ( -- * Operations

    -- ** GetRoleCredentials
    getRoleCredentials_roleName,
    getRoleCredentials_accountId,
    getRoleCredentials_accessToken,
    getRoleCredentialsResponse_httpStatus,
    getRoleCredentialsResponse_roleCredentials,

    -- ** ListAccountRoles
    listAccountRoles_maxResults,
    listAccountRoles_nextToken,
    listAccountRoles_accessToken,
    listAccountRoles_accountId,
    listAccountRolesResponse_nextToken,
    listAccountRolesResponse_roleList,
    listAccountRolesResponse_httpStatus,

    -- ** ListAccounts
    listAccounts_maxResults,
    listAccounts_nextToken,
    listAccounts_accessToken,
    listAccountsResponse_accountList,
    listAccountsResponse_nextToken,
    listAccountsResponse_httpStatus,

    -- ** Logout
    logout_accessToken,

    -- * Types

    -- ** AccountInfo
    accountInfo_accountId,
    accountInfo_accountName,
    accountInfo_emailAddress,

    -- ** RoleCredentials
    roleCredentials_expiration,
    roleCredentials_sessionToken,
    roleCredentials_accessKeyId,
    roleCredentials_secretAccessKey,

    -- ** RoleInfo
    roleInfo_accountId,
    roleInfo_roleName,
  )
where

import Amazonka.SSO.GetRoleCredentials
import Amazonka.SSO.ListAccountRoles
import Amazonka.SSO.ListAccounts
import Amazonka.SSO.Logout
import Amazonka.SSO.Types.AccountInfo
import Amazonka.SSO.Types.RoleCredentials
import Amazonka.SSO.Types.RoleInfo
