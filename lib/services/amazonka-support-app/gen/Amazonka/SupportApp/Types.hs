{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SupportApp.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SupportApp.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ValidationException,

    -- * AccountType
    AccountType (..),

    -- * NotificationSeverityLevel
    NotificationSeverityLevel (..),

    -- * SlackChannelConfiguration
    SlackChannelConfiguration (..),
    newSlackChannelConfiguration,
    slackChannelConfiguration_channelName,
    slackChannelConfiguration_channelRoleArn,
    slackChannelConfiguration_notifyOnAddCorrespondenceToCase,
    slackChannelConfiguration_notifyOnCaseSeverity,
    slackChannelConfiguration_notifyOnCreateOrReopenCase,
    slackChannelConfiguration_notifyOnResolveCase,
    slackChannelConfiguration_channelId,
    slackChannelConfiguration_teamId,

    -- * SlackWorkspaceConfiguration
    SlackWorkspaceConfiguration (..),
    newSlackWorkspaceConfiguration,
    slackWorkspaceConfiguration_allowOrganizationMemberAccount,
    slackWorkspaceConfiguration_teamName,
    slackWorkspaceConfiguration_teamId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.SupportApp.Types.AccountType
import Amazonka.SupportApp.Types.NotificationSeverityLevel
import Amazonka.SupportApp.Types.SlackChannelConfiguration
import Amazonka.SupportApp.Types.SlackWorkspaceConfiguration

-- | API version @2021-08-20@ of the Amazon Support App SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SupportApp",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "supportapp",
      Core.signingName = "supportapp",
      Core.version = "2021-08-20",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SupportApp",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You don\'t have sufficient permission to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Your request has a conflict. For example, you might receive this error
-- if you try the following:
--
-- -   Add, update, or delete a Slack channel configuration before you add
--     a Slack workspace to your Amazon Web Services account.
--
-- -   Add a Slack channel configuration that already exists in your Amazon
--     Web Services account.
--
-- -   Delete a Slack channel configuration for a live chat channel.
--
-- -   Delete a Slack workspace from your Amazon Web Services account that
--     has an active live chat channel.
--
-- -   Call the @RegisterSlackWorkspaceForOrganization@ API from an Amazon
--     Web Services account that doesn\'t belong to an organization.
--
-- -   Call the @RegisterSlackWorkspaceForOrganization@ API from a member
--     account, but the management account hasn\'t registered that
--     workspace yet for the organization.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | We can’t process your request right now because of a server issue. Try
-- again later.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource is missing or doesn\'t exist, such as an account
-- alias, Slack channel configuration, or Slack workspace configuration.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Your Service Quotas request exceeds the quota for the service. For
-- example, your Service Quotas request to Amazon Web Services Support App
-- might exceed the maximum number of workspaces or channels per account,
-- or the maximum number of accounts per Slack channel.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Your request input doesn\'t meet the constraints that the Amazon Web
-- Services Support App specifies.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
