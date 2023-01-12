{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeStar.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentModificationException,
    _InvalidNextTokenException,
    _InvalidServiceRoleException,
    _LimitExceededException,
    _ProjectAlreadyExistsException,
    _ProjectConfigurationException,
    _ProjectCreationFailedException,
    _ProjectNotFoundException,
    _TeamMemberAlreadyAssociatedException,
    _TeamMemberNotFoundException,
    _UserProfileAlreadyExistsException,
    _UserProfileNotFoundException,
    _ValidationException,

    -- * Code
    Code (..),
    newCode,
    code_source,
    code_destination,

    -- * CodeCommitCodeDestination
    CodeCommitCodeDestination (..),
    newCodeCommitCodeDestination,
    codeCommitCodeDestination_name,

    -- * CodeDestination
    CodeDestination (..),
    newCodeDestination,
    codeDestination_codeCommit,
    codeDestination_gitHub,

    -- * CodeSource
    CodeSource (..),
    newCodeSource,
    codeSource_s3,

    -- * GitHubCodeDestination
    GitHubCodeDestination (..),
    newGitHubCodeDestination,
    gitHubCodeDestination_description,
    gitHubCodeDestination_name,
    gitHubCodeDestination_type,
    gitHubCodeDestination_owner,
    gitHubCodeDestination_privateRepository,
    gitHubCodeDestination_issuesEnabled,
    gitHubCodeDestination_token,

    -- * ProjectStatus
    ProjectStatus (..),
    newProjectStatus,
    projectStatus_reason,
    projectStatus_state,

    -- * ProjectSummary
    ProjectSummary (..),
    newProjectSummary,
    projectSummary_projectArn,
    projectSummary_projectId,

    -- * Resource
    Resource (..),
    newResource,
    resource_id,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucketKey,
    s3Location_bucketName,

    -- * TeamMember
    TeamMember (..),
    newTeamMember,
    teamMember_remoteAccessAllowed,
    teamMember_userArn,
    teamMember_projectRole,

    -- * Toolchain
    Toolchain (..),
    newToolchain,
    toolchain_roleArn,
    toolchain_stackParameters,
    toolchain_source,

    -- * ToolchainSource
    ToolchainSource (..),
    newToolchainSource,
    toolchainSource_s3,

    -- * UserProfileSummary
    UserProfileSummary (..),
    newUserProfileSummary,
    userProfileSummary_displayName,
    userProfileSummary_emailAddress,
    userProfileSummary_sshPublicKey,
    userProfileSummary_userArn,
  )
where

import Amazonka.CodeStar.Types.Code
import Amazonka.CodeStar.Types.CodeCommitCodeDestination
import Amazonka.CodeStar.Types.CodeDestination
import Amazonka.CodeStar.Types.CodeSource
import Amazonka.CodeStar.Types.GitHubCodeDestination
import Amazonka.CodeStar.Types.ProjectStatus
import Amazonka.CodeStar.Types.ProjectSummary
import Amazonka.CodeStar.Types.Resource
import Amazonka.CodeStar.Types.S3Location
import Amazonka.CodeStar.Types.TeamMember
import Amazonka.CodeStar.Types.Toolchain
import Amazonka.CodeStar.Types.ToolchainSource
import Amazonka.CodeStar.Types.UserProfileSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon CodeStar SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CodeStar",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "codestar",
      Core.signingName = "codestar",
      Core.version = "2017-04-19",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CodeStar",
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

-- | Another modification is being made. That modification must complete
-- before you can make your change.
_ConcurrentModificationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The next token is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The service role is not valid.
_InvalidServiceRoleException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidServiceRoleException =
  Core._MatchServiceError
    defaultService
    "InvalidServiceRoleException"

-- | A resource limit has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | An AWS CodeStar project with the same ID already exists in this region
-- for the AWS account. AWS CodeStar project IDs must be unique within a
-- region for the AWS account.
_ProjectAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ProjectAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ProjectAlreadyExistsException"

-- | Project configuration information is required but not specified.
_ProjectConfigurationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ProjectConfigurationException =
  Core._MatchServiceError
    defaultService
    "ProjectConfigurationException"

-- | The project creation request was valid, but a nonspecific exception or
-- error occurred during project creation. The project could not be created
-- in AWS CodeStar.
_ProjectCreationFailedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ProjectCreationFailedException =
  Core._MatchServiceError
    defaultService
    "ProjectCreationFailedException"

-- | The specified AWS CodeStar project was not found.
_ProjectNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ProjectNotFoundException =
  Core._MatchServiceError
    defaultService
    "ProjectNotFoundException"

-- | The team member is already associated with a role in this project.
_TeamMemberAlreadyAssociatedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TeamMemberAlreadyAssociatedException =
  Core._MatchServiceError
    defaultService
    "TeamMemberAlreadyAssociatedException"

-- | The specified team member was not found.
_TeamMemberNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TeamMemberNotFoundException =
  Core._MatchServiceError
    defaultService
    "TeamMemberNotFoundException"

-- | A user profile with that name already exists in this region for the AWS
-- account. AWS CodeStar user profile names must be unique within a region
-- for the AWS account.
_UserProfileAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UserProfileAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "UserProfileAlreadyExistsException"

-- | The user profile was not found.
_UserProfileNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UserProfileNotFoundException =
  Core._MatchServiceError
    defaultService
    "UserProfileNotFoundException"

-- | The specified input is either not valid, or it could not be validated.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
