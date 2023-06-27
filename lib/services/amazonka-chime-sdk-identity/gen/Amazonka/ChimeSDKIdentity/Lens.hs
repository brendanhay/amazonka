{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSDKIdentity.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- ** CreateAppInstanceAdmin
    createAppInstanceAdmin_appInstanceAdminArn,
    createAppInstanceAdmin_appInstanceArn,
    createAppInstanceAdminResponse_appInstanceAdmin,
    createAppInstanceAdminResponse_appInstanceArn,
    createAppInstanceAdminResponse_httpStatus,

    -- ** CreateAppInstanceBot
    createAppInstanceBot_metadata,
    createAppInstanceBot_name,
    createAppInstanceBot_tags,
    createAppInstanceBot_appInstanceArn,
    createAppInstanceBot_clientRequestToken,
    createAppInstanceBot_configuration,
    createAppInstanceBotResponse_appInstanceBotArn,
    createAppInstanceBotResponse_httpStatus,

    -- ** CreateAppInstanceUser
    createAppInstanceUser_expirationSettings,
    createAppInstanceUser_metadata,
    createAppInstanceUser_tags,
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

    -- ** DeleteAppInstanceBot
    deleteAppInstanceBot_appInstanceBotArn,

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

    -- ** DescribeAppInstanceBot
    describeAppInstanceBot_appInstanceBotArn,
    describeAppInstanceBotResponse_appInstanceBot,
    describeAppInstanceBotResponse_httpStatus,

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
    listAppInstanceAdmins_maxResults,
    listAppInstanceAdmins_nextToken,
    listAppInstanceAdmins_appInstanceArn,
    listAppInstanceAdminsResponse_appInstanceAdmins,
    listAppInstanceAdminsResponse_appInstanceArn,
    listAppInstanceAdminsResponse_nextToken,
    listAppInstanceAdminsResponse_httpStatus,

    -- ** ListAppInstanceBots
    listAppInstanceBots_maxResults,
    listAppInstanceBots_nextToken,
    listAppInstanceBots_appInstanceArn,
    listAppInstanceBotsResponse_appInstanceArn,
    listAppInstanceBotsResponse_appInstanceBots,
    listAppInstanceBotsResponse_nextToken,
    listAppInstanceBotsResponse_httpStatus,

    -- ** ListAppInstanceUserEndpoints
    listAppInstanceUserEndpoints_maxResults,
    listAppInstanceUserEndpoints_nextToken,
    listAppInstanceUserEndpoints_appInstanceUserArn,
    listAppInstanceUserEndpointsResponse_appInstanceUserEndpoints,
    listAppInstanceUserEndpointsResponse_nextToken,
    listAppInstanceUserEndpointsResponse_httpStatus,

    -- ** ListAppInstanceUsers
    listAppInstanceUsers_maxResults,
    listAppInstanceUsers_nextToken,
    listAppInstanceUsers_appInstanceArn,
    listAppInstanceUsersResponse_appInstanceArn,
    listAppInstanceUsersResponse_appInstanceUsers,
    listAppInstanceUsersResponse_nextToken,
    listAppInstanceUsersResponse_httpStatus,

    -- ** ListAppInstances
    listAppInstances_maxResults,
    listAppInstances_nextToken,
    listAppInstancesResponse_appInstances,
    listAppInstancesResponse_nextToken,
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

    -- ** PutAppInstanceUserExpirationSettings
    putAppInstanceUserExpirationSettings_expirationSettings,
    putAppInstanceUserExpirationSettings_appInstanceUserArn,
    putAppInstanceUserExpirationSettingsResponse_appInstanceUserArn,
    putAppInstanceUserExpirationSettingsResponse_expirationSettings,
    putAppInstanceUserExpirationSettingsResponse_httpStatus,

    -- ** RegisterAppInstanceUserEndpoint
    registerAppInstanceUserEndpoint_allowMessages,
    registerAppInstanceUserEndpoint_name,
    registerAppInstanceUserEndpoint_appInstanceUserArn,
    registerAppInstanceUserEndpoint_type,
    registerAppInstanceUserEndpoint_resourceArn,
    registerAppInstanceUserEndpoint_endpointAttributes,
    registerAppInstanceUserEndpoint_clientRequestToken,
    registerAppInstanceUserEndpointResponse_appInstanceUserArn,
    registerAppInstanceUserEndpointResponse_endpointId,
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

    -- ** UpdateAppInstanceBot
    updateAppInstanceBot_configuration,
    updateAppInstanceBot_appInstanceBotArn,
    updateAppInstanceBot_name,
    updateAppInstanceBot_metadata,
    updateAppInstanceBotResponse_appInstanceBotArn,
    updateAppInstanceBotResponse_httpStatus,

    -- ** UpdateAppInstanceUser
    updateAppInstanceUser_appInstanceUserArn,
    updateAppInstanceUser_name,
    updateAppInstanceUser_metadata,
    updateAppInstanceUserResponse_appInstanceUserArn,
    updateAppInstanceUserResponse_httpStatus,

    -- ** UpdateAppInstanceUserEndpoint
    updateAppInstanceUserEndpoint_allowMessages,
    updateAppInstanceUserEndpoint_name,
    updateAppInstanceUserEndpoint_appInstanceUserArn,
    updateAppInstanceUserEndpoint_endpointId,
    updateAppInstanceUserEndpointResponse_appInstanceUserArn,
    updateAppInstanceUserEndpointResponse_endpointId,
    updateAppInstanceUserEndpointResponse_httpStatus,

    -- * Types

    -- ** AppInstance
    appInstance_appInstanceArn,
    appInstance_createdTimestamp,
    appInstance_lastUpdatedTimestamp,
    appInstance_metadata,
    appInstance_name,

    -- ** AppInstanceAdmin
    appInstanceAdmin_admin,
    appInstanceAdmin_appInstanceArn,
    appInstanceAdmin_createdTimestamp,

    -- ** AppInstanceAdminSummary
    appInstanceAdminSummary_admin,

    -- ** AppInstanceBot
    appInstanceBot_appInstanceBotArn,
    appInstanceBot_configuration,
    appInstanceBot_createdTimestamp,
    appInstanceBot_lastUpdatedTimestamp,
    appInstanceBot_metadata,
    appInstanceBot_name,

    -- ** AppInstanceBotSummary
    appInstanceBotSummary_appInstanceBotArn,
    appInstanceBotSummary_metadata,
    appInstanceBotSummary_name,

    -- ** AppInstanceRetentionSettings
    appInstanceRetentionSettings_channelRetentionSettings,

    -- ** AppInstanceSummary
    appInstanceSummary_appInstanceArn,
    appInstanceSummary_metadata,
    appInstanceSummary_name,

    -- ** AppInstanceUser
    appInstanceUser_appInstanceUserArn,
    appInstanceUser_createdTimestamp,
    appInstanceUser_expirationSettings,
    appInstanceUser_lastUpdatedTimestamp,
    appInstanceUser_metadata,
    appInstanceUser_name,

    -- ** AppInstanceUserEndpoint
    appInstanceUserEndpoint_allowMessages,
    appInstanceUserEndpoint_appInstanceUserArn,
    appInstanceUserEndpoint_createdTimestamp,
    appInstanceUserEndpoint_endpointAttributes,
    appInstanceUserEndpoint_endpointId,
    appInstanceUserEndpoint_endpointState,
    appInstanceUserEndpoint_lastUpdatedTimestamp,
    appInstanceUserEndpoint_name,
    appInstanceUserEndpoint_resourceArn,
    appInstanceUserEndpoint_type,

    -- ** AppInstanceUserEndpointSummary
    appInstanceUserEndpointSummary_allowMessages,
    appInstanceUserEndpointSummary_appInstanceUserArn,
    appInstanceUserEndpointSummary_endpointId,
    appInstanceUserEndpointSummary_endpointState,
    appInstanceUserEndpointSummary_name,
    appInstanceUserEndpointSummary_type,

    -- ** AppInstanceUserSummary
    appInstanceUserSummary_appInstanceUserArn,
    appInstanceUserSummary_metadata,
    appInstanceUserSummary_name,

    -- ** ChannelRetentionSettings
    channelRetentionSettings_retentionDays,

    -- ** Configuration
    configuration_lex,

    -- ** EndpointAttributes
    endpointAttributes_voipDeviceToken,
    endpointAttributes_deviceToken,

    -- ** EndpointState
    endpointState_statusReason,
    endpointState_status,

    -- ** ExpirationSettings
    expirationSettings_expirationDays,
    expirationSettings_expirationCriterion,

    -- ** Identity
    identity_arn,
    identity_name,

    -- ** InvokedBy
    invokedBy_standardMessages,
    invokedBy_targetedMessages,

    -- ** LexConfiguration
    lexConfiguration_invokedBy,
    lexConfiguration_respondsTo,
    lexConfiguration_welcomeIntent,
    lexConfiguration_lexBotAliasArn,
    lexConfiguration_localeId,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.ChimeSDKIdentity.CreateAppInstance
import Amazonka.ChimeSDKIdentity.CreateAppInstanceAdmin
import Amazonka.ChimeSDKIdentity.CreateAppInstanceBot
import Amazonka.ChimeSDKIdentity.CreateAppInstanceUser
import Amazonka.ChimeSDKIdentity.DeleteAppInstance
import Amazonka.ChimeSDKIdentity.DeleteAppInstanceAdmin
import Amazonka.ChimeSDKIdentity.DeleteAppInstanceBot
import Amazonka.ChimeSDKIdentity.DeleteAppInstanceUser
import Amazonka.ChimeSDKIdentity.DeregisterAppInstanceUserEndpoint
import Amazonka.ChimeSDKIdentity.DescribeAppInstance
import Amazonka.ChimeSDKIdentity.DescribeAppInstanceAdmin
import Amazonka.ChimeSDKIdentity.DescribeAppInstanceBot
import Amazonka.ChimeSDKIdentity.DescribeAppInstanceUser
import Amazonka.ChimeSDKIdentity.DescribeAppInstanceUserEndpoint
import Amazonka.ChimeSDKIdentity.GetAppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.ListAppInstanceAdmins
import Amazonka.ChimeSDKIdentity.ListAppInstanceBots
import Amazonka.ChimeSDKIdentity.ListAppInstanceUserEndpoints
import Amazonka.ChimeSDKIdentity.ListAppInstanceUsers
import Amazonka.ChimeSDKIdentity.ListAppInstances
import Amazonka.ChimeSDKIdentity.ListTagsForResource
import Amazonka.ChimeSDKIdentity.PutAppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.PutAppInstanceUserExpirationSettings
import Amazonka.ChimeSDKIdentity.RegisterAppInstanceUserEndpoint
import Amazonka.ChimeSDKIdentity.TagResource
import Amazonka.ChimeSDKIdentity.Types.AppInstance
import Amazonka.ChimeSDKIdentity.Types.AppInstanceAdmin
import Amazonka.ChimeSDKIdentity.Types.AppInstanceAdminSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceBot
import Amazonka.ChimeSDKIdentity.Types.AppInstanceBotSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.Types.AppInstanceSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUser
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpoint
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpointSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserSummary
import Amazonka.ChimeSDKIdentity.Types.ChannelRetentionSettings
import Amazonka.ChimeSDKIdentity.Types.Configuration
import Amazonka.ChimeSDKIdentity.Types.EndpointAttributes
import Amazonka.ChimeSDKIdentity.Types.EndpointState
import Amazonka.ChimeSDKIdentity.Types.ExpirationSettings
import Amazonka.ChimeSDKIdentity.Types.Identity
import Amazonka.ChimeSDKIdentity.Types.InvokedBy
import Amazonka.ChimeSDKIdentity.Types.LexConfiguration
import Amazonka.ChimeSDKIdentity.Types.Tag
import Amazonka.ChimeSDKIdentity.UntagResource
import Amazonka.ChimeSDKIdentity.UpdateAppInstance
import Amazonka.ChimeSDKIdentity.UpdateAppInstanceBot
import Amazonka.ChimeSDKIdentity.UpdateAppInstanceUser
import Amazonka.ChimeSDKIdentity.UpdateAppInstanceUserEndpoint
