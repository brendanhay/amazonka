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
    createAppInstance_tags,
    createAppInstance_metadata,
    createAppInstance_name,
    createAppInstance_clientRequestToken,
    createAppInstanceResponse_appInstanceArn,
    createAppInstanceResponse_httpStatus,

    -- ** CreateAppInstanceAdmin
    createAppInstanceAdmin_appInstanceAdminArn,
    createAppInstanceAdmin_appInstanceArn,
    createAppInstanceAdminResponse_appInstanceArn,
    createAppInstanceAdminResponse_appInstanceAdmin,
    createAppInstanceAdminResponse_httpStatus,

    -- ** CreateAppInstanceUser
    createAppInstanceUser_tags,
    createAppInstanceUser_metadata,
    createAppInstanceUser_appInstanceArn,
    createAppInstanceUser_appInstanceUserId,
    createAppInstanceUser_name,
    createAppInstanceUser_clientRequestToken,
    createAppInstanceUserResponse_appInstanceUserArn,
    createAppInstanceUserResponse_httpStatus,

    -- ** DeleteAppInstance
    deleteAppInstance_appInstanceArn,

    -- ** DeleteAppInstanceAdmin
    deleteAppInstanceAdmin_appInstanceAdminArn,
    deleteAppInstanceAdmin_appInstanceArn,

    -- ** DeleteAppInstanceUser
    deleteAppInstanceUser_appInstanceUserArn,

    -- ** DeregisterAppInstanceUserEndpoint
    deregisterAppInstanceUserEndpoint_appInstanceUserArn,
    deregisterAppInstanceUserEndpoint_endpointId,

    -- ** DescribeAppInstance
    describeAppInstance_appInstanceArn,
    describeAppInstanceResponse_appInstance,
    describeAppInstanceResponse_httpStatus,

    -- ** DescribeAppInstanceAdmin
    describeAppInstanceAdmin_appInstanceAdminArn,
    describeAppInstanceAdmin_appInstanceArn,
    describeAppInstanceAdminResponse_appInstanceAdmin,
    describeAppInstanceAdminResponse_httpStatus,

    -- ** DescribeAppInstanceUser
    describeAppInstanceUser_appInstanceUserArn,
    describeAppInstanceUserResponse_appInstanceUser,
    describeAppInstanceUserResponse_httpStatus,

    -- ** DescribeAppInstanceUserEndpoint
    describeAppInstanceUserEndpoint_appInstanceUserArn,
    describeAppInstanceUserEndpoint_endpointId,
    describeAppInstanceUserEndpointResponse_appInstanceUserEndpoint,
    describeAppInstanceUserEndpointResponse_httpStatus,

    -- ** GetAppInstanceRetentionSettings
    getAppInstanceRetentionSettings_appInstanceArn,
    getAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings,
    getAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp,
    getAppInstanceRetentionSettingsResponse_httpStatus,

    -- ** ListAppInstanceAdmins
    listAppInstanceAdmins_nextToken,
    listAppInstanceAdmins_maxResults,
    listAppInstanceAdmins_appInstanceArn,
    listAppInstanceAdminsResponse_nextToken,
    listAppInstanceAdminsResponse_appInstanceArn,
    listAppInstanceAdminsResponse_appInstanceAdmins,
    listAppInstanceAdminsResponse_httpStatus,

    -- ** ListAppInstanceUserEndpoints
    listAppInstanceUserEndpoints_nextToken,
    listAppInstanceUserEndpoints_maxResults,
    listAppInstanceUserEndpoints_appInstanceUserArn,
    listAppInstanceUserEndpointsResponse_nextToken,
    listAppInstanceUserEndpointsResponse_appInstanceUserEndpoints,
    listAppInstanceUserEndpointsResponse_httpStatus,

    -- ** ListAppInstanceUsers
    listAppInstanceUsers_nextToken,
    listAppInstanceUsers_maxResults,
    listAppInstanceUsers_appInstanceArn,
    listAppInstanceUsersResponse_nextToken,
    listAppInstanceUsersResponse_appInstanceUsers,
    listAppInstanceUsersResponse_appInstanceArn,
    listAppInstanceUsersResponse_httpStatus,

    -- ** ListAppInstances
    listAppInstances_nextToken,
    listAppInstances_maxResults,
    listAppInstancesResponse_nextToken,
    listAppInstancesResponse_appInstances,
    listAppInstancesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutAppInstanceRetentionSettings
    putAppInstanceRetentionSettings_appInstanceArn,
    putAppInstanceRetentionSettings_appInstanceRetentionSettings,
    putAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings,
    putAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp,
    putAppInstanceRetentionSettingsResponse_httpStatus,

    -- ** RegisterAppInstanceUserEndpoint
    registerAppInstanceUserEndpoint_name,
    registerAppInstanceUserEndpoint_allowMessages,
    registerAppInstanceUserEndpoint_appInstanceUserArn,
    registerAppInstanceUserEndpoint_type,
    registerAppInstanceUserEndpoint_resourceArn,
    registerAppInstanceUserEndpoint_endpointAttributes,
    registerAppInstanceUserEndpoint_clientRequestToken,
    registerAppInstanceUserEndpointResponse_endpointId,
    registerAppInstanceUserEndpointResponse_appInstanceUserArn,
    registerAppInstanceUserEndpointResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,

    -- ** UpdateAppInstance
    updateAppInstance_appInstanceArn,
    updateAppInstance_name,
    updateAppInstance_metadata,
    updateAppInstanceResponse_appInstanceArn,
    updateAppInstanceResponse_httpStatus,

    -- ** UpdateAppInstanceUser
    updateAppInstanceUser_appInstanceUserArn,
    updateAppInstanceUser_name,
    updateAppInstanceUser_metadata,
    updateAppInstanceUserResponse_appInstanceUserArn,
    updateAppInstanceUserResponse_httpStatus,

    -- ** UpdateAppInstanceUserEndpoint
    updateAppInstanceUserEndpoint_name,
    updateAppInstanceUserEndpoint_allowMessages,
    updateAppInstanceUserEndpoint_appInstanceUserArn,
    updateAppInstanceUserEndpoint_endpointId,
    updateAppInstanceUserEndpointResponse_endpointId,
    updateAppInstanceUserEndpointResponse_appInstanceUserArn,
    updateAppInstanceUserEndpointResponse_httpStatus,

    -- * Types

    -- ** AppInstance
    appInstance_lastUpdatedTimestamp,
    appInstance_name,
    appInstance_metadata,
    appInstance_createdTimestamp,
    appInstance_appInstanceArn,

    -- ** AppInstanceAdmin
    appInstanceAdmin_createdTimestamp,
    appInstanceAdmin_appInstanceArn,
    appInstanceAdmin_admin,

    -- ** AppInstanceAdminSummary
    appInstanceAdminSummary_admin,

    -- ** AppInstanceRetentionSettings
    appInstanceRetentionSettings_channelRetentionSettings,

    -- ** AppInstanceSummary
    appInstanceSummary_name,
    appInstanceSummary_metadata,
    appInstanceSummary_appInstanceArn,

    -- ** AppInstanceUser
    appInstanceUser_lastUpdatedTimestamp,
    appInstanceUser_name,
    appInstanceUser_metadata,
    appInstanceUser_appInstanceUserArn,
    appInstanceUser_createdTimestamp,

    -- ** AppInstanceUserEndpoint
    appInstanceUserEndpoint_lastUpdatedTimestamp,
    appInstanceUserEndpoint_name,
    appInstanceUserEndpoint_type,
    appInstanceUserEndpoint_endpointId,
    appInstanceUserEndpoint_appInstanceUserArn,
    appInstanceUserEndpoint_createdTimestamp,
    appInstanceUserEndpoint_endpointAttributes,
    appInstanceUserEndpoint_endpointState,
    appInstanceUserEndpoint_resourceArn,
    appInstanceUserEndpoint_allowMessages,

    -- ** AppInstanceUserEndpointSummary
    appInstanceUserEndpointSummary_name,
    appInstanceUserEndpointSummary_type,
    appInstanceUserEndpointSummary_endpointId,
    appInstanceUserEndpointSummary_appInstanceUserArn,
    appInstanceUserEndpointSummary_endpointState,
    appInstanceUserEndpointSummary_allowMessages,

    -- ** AppInstanceUserSummary
    appInstanceUserSummary_name,
    appInstanceUserSummary_metadata,
    appInstanceUserSummary_appInstanceUserArn,

    -- ** ChannelRetentionSettings
    channelRetentionSettings_retentionDays,

    -- ** EndpointAttributes
    endpointAttributes_voipDeviceToken,
    endpointAttributes_deviceToken,

    -- ** EndpointState
    endpointState_statusReason,
    endpointState_status,

    -- ** Identity
    identity_name,
    identity_arn,

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
import Amazonka.ChimeSDKIdentity.DeregisterAppInstanceUserEndpoint
import Amazonka.ChimeSDKIdentity.DescribeAppInstance
import Amazonka.ChimeSDKIdentity.DescribeAppInstanceAdmin
import Amazonka.ChimeSDKIdentity.DescribeAppInstanceUser
import Amazonka.ChimeSDKIdentity.DescribeAppInstanceUserEndpoint
import Amazonka.ChimeSDKIdentity.GetAppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.ListAppInstanceAdmins
import Amazonka.ChimeSDKIdentity.ListAppInstanceUserEndpoints
import Amazonka.ChimeSDKIdentity.ListAppInstanceUsers
import Amazonka.ChimeSDKIdentity.ListAppInstances
import Amazonka.ChimeSDKIdentity.ListTagsForResource
import Amazonka.ChimeSDKIdentity.PutAppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.RegisterAppInstanceUserEndpoint
import Amazonka.ChimeSDKIdentity.TagResource
import Amazonka.ChimeSDKIdentity.Types.AppInstance
import Amazonka.ChimeSDKIdentity.Types.AppInstanceAdmin
import Amazonka.ChimeSDKIdentity.Types.AppInstanceAdminSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.Types.AppInstanceSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUser
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpoint
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpointSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserSummary
import Amazonka.ChimeSDKIdentity.Types.ChannelRetentionSettings
import Amazonka.ChimeSDKIdentity.Types.EndpointAttributes
import Amazonka.ChimeSDKIdentity.Types.EndpointState
import Amazonka.ChimeSDKIdentity.Types.Identity
import Amazonka.ChimeSDKIdentity.Types.Tag
import Amazonka.ChimeSDKIdentity.UntagResource
import Amazonka.ChimeSDKIdentity.UpdateAppInstance
import Amazonka.ChimeSDKIdentity.UpdateAppInstanceUser
import Amazonka.ChimeSDKIdentity.UpdateAppInstanceUserEndpoint
