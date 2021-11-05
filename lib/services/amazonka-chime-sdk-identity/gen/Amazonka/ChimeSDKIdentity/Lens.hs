{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSDKIdentity.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Lens
  ( -- * Operations

    -- ** CreateAppInstance
    createAppInstance_metadata,
    createAppInstance_tags,
    createAppInstance_name,
    createAppInstance_clientRequestToken,
    createAppInstanceResponse_appInstanceArn,
    createAppInstanceResponse_httpStatus,

    -- ** GetAppInstanceRetentionSettings
    getAppInstanceRetentionSettings_appInstanceArn,
    getAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings,
    getAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp,
    getAppInstanceRetentionSettingsResponse_httpStatus,

    -- ** DescribeAppInstanceAdmin
    describeAppInstanceAdmin_appInstanceAdminArn,
    describeAppInstanceAdmin_appInstanceArn,
    describeAppInstanceAdminResponse_appInstanceAdmin,
    describeAppInstanceAdminResponse_httpStatus,

    -- ** CreateAppInstanceUser
    createAppInstanceUser_metadata,
    createAppInstanceUser_tags,
    createAppInstanceUser_appInstanceArn,
    createAppInstanceUser_appInstanceUserId,
    createAppInstanceUser_name,
    createAppInstanceUser_clientRequestToken,
    createAppInstanceUserResponse_appInstanceUserArn,
    createAppInstanceUserResponse_httpStatus,

    -- ** PutAppInstanceRetentionSettings
    putAppInstanceRetentionSettings_appInstanceArn,
    putAppInstanceRetentionSettings_appInstanceRetentionSettings,
    putAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings,
    putAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp,
    putAppInstanceRetentionSettingsResponse_httpStatus,

    -- ** CreateAppInstanceAdmin
    createAppInstanceAdmin_appInstanceAdminArn,
    createAppInstanceAdmin_appInstanceArn,
    createAppInstanceAdminResponse_appInstanceAdmin,
    createAppInstanceAdminResponse_appInstanceArn,
    createAppInstanceAdminResponse_httpStatus,

    -- ** ListAppInstanceAdmins
    listAppInstanceAdmins_nextToken,
    listAppInstanceAdmins_maxResults,
    listAppInstanceAdmins_appInstanceArn,
    listAppInstanceAdminsResponse_nextToken,
    listAppInstanceAdminsResponse_appInstanceAdmins,
    listAppInstanceAdminsResponse_appInstanceArn,
    listAppInstanceAdminsResponse_httpStatus,

    -- ** ListAppInstances
    listAppInstances_nextToken,
    listAppInstances_maxResults,
    listAppInstancesResponse_appInstances,
    listAppInstancesResponse_nextToken,
    listAppInstancesResponse_httpStatus,

    -- ** DescribeAppInstanceUser
    describeAppInstanceUser_appInstanceUserArn,
    describeAppInstanceUserResponse_appInstanceUser,
    describeAppInstanceUserResponse_httpStatus,

    -- ** DescribeAppInstance
    describeAppInstance_appInstanceArn,
    describeAppInstanceResponse_appInstance,
    describeAppInstanceResponse_httpStatus,

    -- ** ListAppInstanceUsers
    listAppInstanceUsers_nextToken,
    listAppInstanceUsers_maxResults,
    listAppInstanceUsers_appInstanceArn,
    listAppInstanceUsersResponse_nextToken,
    listAppInstanceUsersResponse_appInstanceUsers,
    listAppInstanceUsersResponse_appInstanceArn,
    listAppInstanceUsersResponse_httpStatus,

    -- ** DeleteAppInstanceUser
    deleteAppInstanceUser_appInstanceUserArn,

    -- ** UpdateAppInstanceUser
    updateAppInstanceUser_appInstanceUserArn,
    updateAppInstanceUser_name,
    updateAppInstanceUser_metadata,
    updateAppInstanceUserResponse_appInstanceUserArn,
    updateAppInstanceUserResponse_httpStatus,

    -- ** DeleteAppInstanceAdmin
    deleteAppInstanceAdmin_appInstanceAdminArn,
    deleteAppInstanceAdmin_appInstanceArn,

    -- ** DeleteAppInstance
    deleteAppInstance_appInstanceArn,

    -- ** UpdateAppInstance
    updateAppInstance_appInstanceArn,
    updateAppInstance_name,
    updateAppInstance_metadata,
    updateAppInstanceResponse_appInstanceArn,
    updateAppInstanceResponse_httpStatus,

    -- * Types

    -- ** AppInstance
    appInstance_name,
    appInstance_metadata,
    appInstance_appInstanceArn,
    appInstance_createdTimestamp,
    appInstance_lastUpdatedTimestamp,

    -- ** AppInstanceAdmin
    appInstanceAdmin_admin,
    appInstanceAdmin_appInstanceArn,
    appInstanceAdmin_createdTimestamp,

    -- ** AppInstanceAdminSummary
    appInstanceAdminSummary_admin,

    -- ** AppInstanceRetentionSettings
    appInstanceRetentionSettings_channelRetentionSettings,

    -- ** AppInstanceSummary
    appInstanceSummary_name,
    appInstanceSummary_metadata,
    appInstanceSummary_appInstanceArn,

    -- ** AppInstanceUser
    appInstanceUser_appInstanceUserArn,
    appInstanceUser_name,
    appInstanceUser_metadata,
    appInstanceUser_createdTimestamp,
    appInstanceUser_lastUpdatedTimestamp,

    -- ** AppInstanceUserSummary
    appInstanceUserSummary_appInstanceUserArn,
    appInstanceUserSummary_name,
    appInstanceUserSummary_metadata,

    -- ** ChannelRetentionSettings
    channelRetentionSettings_retentionDays,

    -- ** Identity
    identity_arn,
    identity_name,

    -- ** Tag
    tag_key,
    tag_value,
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
import Amazonka.ChimeSDKIdentity.ListAppInstanceAdmins
import Amazonka.ChimeSDKIdentity.ListAppInstanceUsers
import Amazonka.ChimeSDKIdentity.ListAppInstances
import Amazonka.ChimeSDKIdentity.PutAppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.Types.AppInstance
import Amazonka.ChimeSDKIdentity.Types.AppInstanceAdmin
import Amazonka.ChimeSDKIdentity.Types.AppInstanceAdminSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.Types.AppInstanceSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUser
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserSummary
import Amazonka.ChimeSDKIdentity.Types.ChannelRetentionSettings
import Amazonka.ChimeSDKIdentity.Types.Identity
import Amazonka.ChimeSDKIdentity.Types.Tag
import Amazonka.ChimeSDKIdentity.UpdateAppInstance
import Amazonka.ChimeSDKIdentity.UpdateAppInstanceUser
