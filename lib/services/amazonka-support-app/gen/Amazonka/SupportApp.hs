{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SupportApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-08-20@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Support App in Slack
--
-- You can use the Amazon Web Services Support App in Slack API to manage
-- your support cases in Slack for your Amazon Web Services account. After
-- you configure your Slack workspace and channel with the Amazon Web
-- Services Support App, you can perform the following tasks directly in
-- your Slack channel:
--
-- -   Create, search, update, and resolve your support cases
--
-- -   Request service quota increases for your account
--
-- -   Invite Amazon Web Services Support agents to your channel so that
--     you can chat directly about your support cases
--
-- For more information about how to perform these actions in Slack, see
-- the following documentation in the /Amazon Web Services Support User
-- Guide/:
--
-- -   <https://docs.aws.amazon.com/awssupport/latest/user/aws-support-app-for-slack.html Amazon Web Services Support App in Slack>
--
-- -   <https://docs.aws.amazon.com/awssupport/latest/user/joining-a-live-chat-session.html Joining a live chat session with Amazon Web Services Support>
--
-- -   <https://docs.aws.amazon.com/awssupport/latest/user/service-quota-increase.html Requesting service quota increases>
--
-- -   <https://docs.aws.amazon.com/awssupport/latest/user/support-app-commands.html Amazon Web Services Support App commands in Slack>
--
-- You can also use the Amazon Web Services Management Console instead of
-- the Amazon Web Services Support App API to manage your Slack
-- configurations. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/authorize-slack-workspace.html Authorize a Slack workspace to enable the Amazon Web Services Support App>.
--
-- -   You must have a Business or Enterprise Support plan to use the
--     Amazon Web Services Support App API.
--
-- -   For more information about the Amazon Web Services Support App
--     endpoints, see the
--     <https://docs.aws.amazon.com/general/latest/gr/awssupport.html#awssupport_app_region Amazon Web Services Support App in Slack endpoints>
--     in the /Amazon Web Services General Reference/.
module Amazonka.SupportApp
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateSlackChannelConfiguration
    CreateSlackChannelConfiguration (CreateSlackChannelConfiguration'),
    newCreateSlackChannelConfiguration,
    CreateSlackChannelConfigurationResponse (CreateSlackChannelConfigurationResponse'),
    newCreateSlackChannelConfigurationResponse,

    -- ** DeleteAccountAlias
    DeleteAccountAlias (DeleteAccountAlias'),
    newDeleteAccountAlias,
    DeleteAccountAliasResponse (DeleteAccountAliasResponse'),
    newDeleteAccountAliasResponse,

    -- ** DeleteSlackChannelConfiguration
    DeleteSlackChannelConfiguration (DeleteSlackChannelConfiguration'),
    newDeleteSlackChannelConfiguration,
    DeleteSlackChannelConfigurationResponse (DeleteSlackChannelConfigurationResponse'),
    newDeleteSlackChannelConfigurationResponse,

    -- ** DeleteSlackWorkspaceConfiguration
    DeleteSlackWorkspaceConfiguration (DeleteSlackWorkspaceConfiguration'),
    newDeleteSlackWorkspaceConfiguration,
    DeleteSlackWorkspaceConfigurationResponse (DeleteSlackWorkspaceConfigurationResponse'),
    newDeleteSlackWorkspaceConfigurationResponse,

    -- ** GetAccountAlias
    GetAccountAlias (GetAccountAlias'),
    newGetAccountAlias,
    GetAccountAliasResponse (GetAccountAliasResponse'),
    newGetAccountAliasResponse,

    -- ** ListSlackChannelConfigurations
    ListSlackChannelConfigurations (ListSlackChannelConfigurations'),
    newListSlackChannelConfigurations,
    ListSlackChannelConfigurationsResponse (ListSlackChannelConfigurationsResponse'),
    newListSlackChannelConfigurationsResponse,

    -- ** ListSlackWorkspaceConfigurations
    ListSlackWorkspaceConfigurations (ListSlackWorkspaceConfigurations'),
    newListSlackWorkspaceConfigurations,
    ListSlackWorkspaceConfigurationsResponse (ListSlackWorkspaceConfigurationsResponse'),
    newListSlackWorkspaceConfigurationsResponse,

    -- ** PutAccountAlias
    PutAccountAlias (PutAccountAlias'),
    newPutAccountAlias,
    PutAccountAliasResponse (PutAccountAliasResponse'),
    newPutAccountAliasResponse,

    -- ** RegisterSlackWorkspaceForOrganization
    RegisterSlackWorkspaceForOrganization (RegisterSlackWorkspaceForOrganization'),
    newRegisterSlackWorkspaceForOrganization,
    RegisterSlackWorkspaceForOrganizationResponse (RegisterSlackWorkspaceForOrganizationResponse'),
    newRegisterSlackWorkspaceForOrganizationResponse,

    -- ** UpdateSlackChannelConfiguration
    UpdateSlackChannelConfiguration (UpdateSlackChannelConfiguration'),
    newUpdateSlackChannelConfiguration,
    UpdateSlackChannelConfigurationResponse (UpdateSlackChannelConfigurationResponse'),
    newUpdateSlackChannelConfigurationResponse,

    -- * Types

    -- ** AccountType
    AccountType (..),

    -- ** NotificationSeverityLevel
    NotificationSeverityLevel (..),

    -- ** SlackChannelConfiguration
    SlackChannelConfiguration (SlackChannelConfiguration'),
    newSlackChannelConfiguration,

    -- ** SlackWorkspaceConfiguration
    SlackWorkspaceConfiguration (SlackWorkspaceConfiguration'),
    newSlackWorkspaceConfiguration,
  )
where

import Amazonka.SupportApp.CreateSlackChannelConfiguration
import Amazonka.SupportApp.DeleteAccountAlias
import Amazonka.SupportApp.DeleteSlackChannelConfiguration
import Amazonka.SupportApp.DeleteSlackWorkspaceConfiguration
import Amazonka.SupportApp.GetAccountAlias
import Amazonka.SupportApp.Lens
import Amazonka.SupportApp.ListSlackChannelConfigurations
import Amazonka.SupportApp.ListSlackWorkspaceConfigurations
import Amazonka.SupportApp.PutAccountAlias
import Amazonka.SupportApp.RegisterSlackWorkspaceForOrganization
import Amazonka.SupportApp.Types
import Amazonka.SupportApp.UpdateSlackChannelConfiguration
import Amazonka.SupportApp.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SupportApp'.

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
