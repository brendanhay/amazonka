{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IdentityStore.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Lens
  ( -- * Operations

    -- ** DescribeGroup
    describeGroup_identityStoreId,
    describeGroup_groupId,
    describeGroupResponse_httpStatus,
    describeGroupResponse_groupId,
    describeGroupResponse_displayName,

    -- ** ListUsers
    listUsers_filters,
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_identityStoreId,
    listUsersResponse_nextToken,
    listUsersResponse_httpStatus,
    listUsersResponse_users,

    -- ** DescribeUser
    describeUser_identityStoreId,
    describeUser_userId,
    describeUserResponse_httpStatus,
    describeUserResponse_userName,
    describeUserResponse_userId,

    -- ** ListGroups
    listGroups_filters,
    listGroups_nextToken,
    listGroups_maxResults,
    listGroups_identityStoreId,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,
    listGroupsResponse_groups,

    -- * Types

    -- ** Filter
    filter_attributePath,
    filter_attributeValue,

    -- ** Group
    group_groupId,
    group_displayName,

    -- ** User
    user_userName,
    user_userId,
  )
where

import Amazonka.IdentityStore.DescribeGroup
import Amazonka.IdentityStore.DescribeUser
import Amazonka.IdentityStore.ListGroups
import Amazonka.IdentityStore.ListUsers
import Amazonka.IdentityStore.Types.Filter
import Amazonka.IdentityStore.Types.Group
import Amazonka.IdentityStore.Types.User
