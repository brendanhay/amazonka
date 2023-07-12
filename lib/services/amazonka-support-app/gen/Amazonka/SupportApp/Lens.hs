{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SupportApp.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SupportApp.Lens
  ( -- * Operations

    -- ** CreateSlackChannelConfiguration
    createSlackChannelConfiguration_channelName,
    createSlackChannelConfiguration_notifyOnAddCorrespondenceToCase,
    createSlackChannelConfiguration_notifyOnCreateOrReopenCase,
    createSlackChannelConfiguration_notifyOnResolveCase,
    createSlackChannelConfiguration_channelId,
    createSlackChannelConfiguration_channelRoleArn,
    createSlackChannelConfiguration_notifyOnCaseSeverity,
    createSlackChannelConfiguration_teamId,
    createSlackChannelConfigurationResponse_httpStatus,

    -- ** DeleteAccountAlias
    deleteAccountAliasResponse_httpStatus,

    -- ** DeleteSlackChannelConfiguration
    deleteSlackChannelConfiguration_channelId,
    deleteSlackChannelConfiguration_teamId,
    deleteSlackChannelConfigurationResponse_httpStatus,

    -- ** DeleteSlackWorkspaceConfiguration
    deleteSlackWorkspaceConfiguration_teamId,
    deleteSlackWorkspaceConfigurationResponse_httpStatus,

    -- ** GetAccountAlias
    getAccountAliasResponse_accountAlias,
    getAccountAliasResponse_httpStatus,

    -- ** ListSlackChannelConfigurations
    listSlackChannelConfigurations_nextToken,
    listSlackChannelConfigurationsResponse_nextToken,
    listSlackChannelConfigurationsResponse_httpStatus,
    listSlackChannelConfigurationsResponse_slackChannelConfigurations,

    -- ** ListSlackWorkspaceConfigurations
    listSlackWorkspaceConfigurations_nextToken,
    listSlackWorkspaceConfigurationsResponse_nextToken,
    listSlackWorkspaceConfigurationsResponse_slackWorkspaceConfigurations,
    listSlackWorkspaceConfigurationsResponse_httpStatus,

    -- ** PutAccountAlias
    putAccountAlias_accountAlias,
    putAccountAliasResponse_httpStatus,

    -- ** RegisterSlackWorkspaceForOrganization
    registerSlackWorkspaceForOrganization_teamId,
    registerSlackWorkspaceForOrganizationResponse_accountType,
    registerSlackWorkspaceForOrganizationResponse_teamId,
    registerSlackWorkspaceForOrganizationResponse_teamName,
    registerSlackWorkspaceForOrganizationResponse_httpStatus,

    -- ** UpdateSlackChannelConfiguration
    updateSlackChannelConfiguration_channelName,
    updateSlackChannelConfiguration_channelRoleArn,
    updateSlackChannelConfiguration_notifyOnAddCorrespondenceToCase,
    updateSlackChannelConfiguration_notifyOnCaseSeverity,
    updateSlackChannelConfiguration_notifyOnCreateOrReopenCase,
    updateSlackChannelConfiguration_notifyOnResolveCase,
    updateSlackChannelConfiguration_channelId,
    updateSlackChannelConfiguration_teamId,
    updateSlackChannelConfigurationResponse_channelId,
    updateSlackChannelConfigurationResponse_channelName,
    updateSlackChannelConfigurationResponse_channelRoleArn,
    updateSlackChannelConfigurationResponse_notifyOnAddCorrespondenceToCase,
    updateSlackChannelConfigurationResponse_notifyOnCaseSeverity,
    updateSlackChannelConfigurationResponse_notifyOnCreateOrReopenCase,
    updateSlackChannelConfigurationResponse_notifyOnResolveCase,
    updateSlackChannelConfigurationResponse_teamId,
    updateSlackChannelConfigurationResponse_httpStatus,

    -- * Types

    -- ** SlackChannelConfiguration
    slackChannelConfiguration_channelName,
    slackChannelConfiguration_channelRoleArn,
    slackChannelConfiguration_notifyOnAddCorrespondenceToCase,
    slackChannelConfiguration_notifyOnCaseSeverity,
    slackChannelConfiguration_notifyOnCreateOrReopenCase,
    slackChannelConfiguration_notifyOnResolveCase,
    slackChannelConfiguration_channelId,
    slackChannelConfiguration_teamId,

    -- ** SlackWorkspaceConfiguration
    slackWorkspaceConfiguration_allowOrganizationMemberAccount,
    slackWorkspaceConfiguration_teamName,
    slackWorkspaceConfiguration_teamId,
  )
where

import Amazonka.SupportApp.CreateSlackChannelConfiguration
import Amazonka.SupportApp.DeleteAccountAlias
import Amazonka.SupportApp.DeleteSlackChannelConfiguration
import Amazonka.SupportApp.DeleteSlackWorkspaceConfiguration
import Amazonka.SupportApp.GetAccountAlias
import Amazonka.SupportApp.ListSlackChannelConfigurations
import Amazonka.SupportApp.ListSlackWorkspaceConfigurations
import Amazonka.SupportApp.PutAccountAlias
import Amazonka.SupportApp.RegisterSlackWorkspaceForOrganization
import Amazonka.SupportApp.Types.SlackChannelConfiguration
import Amazonka.SupportApp.Types.SlackWorkspaceConfiguration
import Amazonka.SupportApp.UpdateSlackChannelConfiguration
