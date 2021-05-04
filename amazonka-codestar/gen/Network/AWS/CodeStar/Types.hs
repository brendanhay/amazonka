{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ProjectAlreadyExistsException,
    _TeamMemberAlreadyAssociatedException,
    _ProjectNotFoundException,
    _UserProfileNotFoundException,
    _ProjectCreationFailedException,
    _ConcurrentModificationException,
    _InvalidNextTokenException,
    _ProjectConfigurationException,
    _InvalidServiceRoleException,
    _ValidationException,
    _LimitExceededException,
    _TeamMemberNotFoundException,
    _UserProfileAlreadyExistsException,

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
    projectSummary_projectId,
    projectSummary_projectArn,

    -- * Resource
    Resource (..),
    newResource,
    resource_id,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucketName,
    s3Location_bucketKey,

    -- * TeamMember
    TeamMember (..),
    newTeamMember,
    teamMember_remoteAccessAllowed,
    teamMember_userArn,
    teamMember_projectRole,

    -- * Toolchain
    Toolchain (..),
    newToolchain,
    toolchain_stackParameters,
    toolchain_roleArn,
    toolchain_source,

    -- * ToolchainSource
    ToolchainSource (..),
    newToolchainSource,
    toolchainSource_s3,

    -- * UserProfileSummary
    UserProfileSummary (..),
    newUserProfileSummary,
    userProfileSummary_userArn,
    userProfileSummary_sshPublicKey,
    userProfileSummary_displayName,
    userProfileSummary_emailAddress,
  )
where

import Network.AWS.CodeStar.Types.Code
import Network.AWS.CodeStar.Types.CodeCommitCodeDestination
import Network.AWS.CodeStar.Types.CodeDestination
import Network.AWS.CodeStar.Types.CodeSource
import Network.AWS.CodeStar.Types.GitHubCodeDestination
import Network.AWS.CodeStar.Types.ProjectStatus
import Network.AWS.CodeStar.Types.ProjectSummary
import Network.AWS.CodeStar.Types.Resource
import Network.AWS.CodeStar.Types.S3Location
import Network.AWS.CodeStar.Types.TeamMember
import Network.AWS.CodeStar.Types.Toolchain
import Network.AWS.CodeStar.Types.ToolchainSource
import Network.AWS.CodeStar.Types.UserProfileSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon CodeStar SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "CodeStar",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "codestar",
      Prelude._svcSigningName = "codestar",
      Prelude._svcVersion = "2017-04-19",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CodeStar",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | An AWS CodeStar project with the same ID already exists in this region
-- for the AWS account. AWS CodeStar project IDs must be unique within a
-- region for the AWS account.
_ProjectAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ProjectAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ProjectAlreadyExistsException"

-- | The team member is already associated with a role in this project.
_TeamMemberAlreadyAssociatedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TeamMemberAlreadyAssociatedException =
  Prelude._MatchServiceError
    defaultService
    "TeamMemberAlreadyAssociatedException"

-- | The specified AWS CodeStar project was not found.
_ProjectNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ProjectNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ProjectNotFoundException"

-- | The user profile was not found.
_UserProfileNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UserProfileNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "UserProfileNotFoundException"

-- | The project creation request was valid, but a nonspecific exception or
-- error occurred during project creation. The project could not be created
-- in AWS CodeStar.
_ProjectCreationFailedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ProjectCreationFailedException =
  Prelude._MatchServiceError
    defaultService
    "ProjectCreationFailedException"

-- | Another modification is being made. That modification must complete
-- before you can make your change.
_ConcurrentModificationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentModificationException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The next token is not valid.
_InvalidNextTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | Project configuration information is required but not specified.
_ProjectConfigurationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ProjectConfigurationException =
  Prelude._MatchServiceError
    defaultService
    "ProjectConfigurationException"

-- | The service role is not valid.
_InvalidServiceRoleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidServiceRoleException =
  Prelude._MatchServiceError
    defaultService
    "InvalidServiceRoleException"

-- | The specified input is either not valid, or it could not be validated.
_ValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ValidationException =
  Prelude._MatchServiceError
    defaultService
    "ValidationException"

-- | A resource limit has been exceeded.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified team member was not found.
_TeamMemberNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TeamMemberNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "TeamMemberNotFoundException"

-- | A user profile with that name already exists in this region for the AWS
-- account. AWS CodeStar user profile names must be unique within a region
-- for the AWS account.
_UserProfileAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UserProfileAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "UserProfileAlreadyExistsException"
