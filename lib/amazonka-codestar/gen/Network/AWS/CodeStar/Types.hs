-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _TeamMemberAlreadyAssociatedException,
    _ValidationException,
    _InvalidServiceRoleException,
    _ProjectCreationFailedException,
    _UserProfileAlreadyExistsException,
    _ProjectNotFoundException,
    _TeamMemberNotFoundException,
    _ProjectAlreadyExistsException,
    _ProjectConfigurationException,
    _ConcurrentModificationException,
    _InvalidNextTokenException,
    _UserProfileNotFoundException,
    _LimitExceededException,

    -- * GitHubCodeDestination
    GitHubCodeDestination (..),
    mkGitHubCodeDestination,
    ghcdName,
    ghcdType,
    ghcdOwner,
    ghcdPrivateRepository,
    ghcdIssuesEnabled,
    ghcdToken,
    ghcdDescription,

    -- * Email
    Email (..),

    -- * PaginationToken
    PaginationToken (..),

    -- * State
    State (..),

    -- * UserProfileDisplayName
    UserProfileDisplayName (..),

    -- * ResourceId
    ResourceId (..),

    -- * TeamMember
    TeamMember (..),
    mkTeamMember,
    tmUserArn,
    tmProjectRole,
    tmRemoteAccessAllowed,

    -- * CodeCommitCodeDestination
    CodeCommitCodeDestination (..),
    mkCodeCommitCodeDestination,
    cccdName,

    -- * ToolchainSource
    ToolchainSource (..),
    mkToolchainSource,
    tsS3,

    -- * CodeSource
    CodeSource (..),
    mkCodeSource,
    csS3,

    -- * ProjectTemplateId
    ProjectTemplateId (..),

    -- * UserProfileSummary
    UserProfileSummary (..),
    mkUserProfileSummary,
    upsDisplayName,
    upsEmailAddress,
    upsSshPublicKey,
    upsUserArn,

    -- * SshPublicKey
    SshPublicKey (..),

    -- * BucketKey
    BucketKey (..),

    -- * ProjectStatus
    ProjectStatus (..),
    mkProjectStatus,
    psState,
    psReason,

    -- * TagValue
    TagValue (..),

    -- * CodeDestination
    CodeDestination (..),
    mkCodeDestination,
    cdCodeCommit,
    cdGitHub,

    -- * BucketName
    BucketName (..),

    -- * Reason
    Reason (..),

    -- * UserArn
    UserArn (..),

    -- * Role
    Role (..),

    -- * TemplateParameterValue
    TemplateParameterValue (..),

    -- * Resource
    Resource (..),
    mkResource,
    rId,

    -- * ProjectName
    ProjectName (..),

    -- * Toolchain
    Toolchain (..),
    mkToolchain,
    tSource,
    tRoleArn,
    tStackParameters,

    -- * Code
    Code (..),
    mkCode,
    cSource,
    cDestination,

    -- * TagKey
    TagKey (..),

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBucketKey,
    slBucketName,

    -- * ProjectId
    ProjectId (..),

    -- * ProjectArn
    ProjectArn (..),

    -- * StackId
    StackId (..),

    -- * ClientRequestToken
    ClientRequestToken (..),

    -- * TemplateParameterKey
    TemplateParameterKey (..),

    -- * ProjectSummary
    ProjectSummary (..),
    mkProjectSummary,
    psProjectArn,
    psProjectId,

    -- * RoleArn
    RoleArn (..),

    -- * Name
    Name (..),

    -- * Type
    Type (..),

    -- * Owner
    Owner (..),

    -- * Token
    Token (..),

    -- * Description
    Description (..),

    -- * DisplayName
    DisplayName (..),

    -- * Id
    Id (..),

    -- * ProjectRole
    ProjectRole (..),

    -- * Arn
    Arn (..),
  )
where

import Network.AWS.CodeStar.Types.Arn
import Network.AWS.CodeStar.Types.BucketKey
import Network.AWS.CodeStar.Types.BucketName
import Network.AWS.CodeStar.Types.ClientRequestToken
import Network.AWS.CodeStar.Types.Code
import Network.AWS.CodeStar.Types.CodeCommitCodeDestination
import Network.AWS.CodeStar.Types.CodeDestination
import Network.AWS.CodeStar.Types.CodeSource
import Network.AWS.CodeStar.Types.Description
import Network.AWS.CodeStar.Types.DisplayName
import Network.AWS.CodeStar.Types.Email
import Network.AWS.CodeStar.Types.GitHubCodeDestination
import Network.AWS.CodeStar.Types.Id
import Network.AWS.CodeStar.Types.Name
import Network.AWS.CodeStar.Types.Owner
import Network.AWS.CodeStar.Types.PaginationToken
import Network.AWS.CodeStar.Types.ProjectArn
import Network.AWS.CodeStar.Types.ProjectId
import Network.AWS.CodeStar.Types.ProjectName
import Network.AWS.CodeStar.Types.ProjectRole
import Network.AWS.CodeStar.Types.ProjectStatus
import Network.AWS.CodeStar.Types.ProjectSummary
import Network.AWS.CodeStar.Types.ProjectTemplateId
import Network.AWS.CodeStar.Types.Reason
import Network.AWS.CodeStar.Types.Resource
import Network.AWS.CodeStar.Types.ResourceId
import Network.AWS.CodeStar.Types.Role
import Network.AWS.CodeStar.Types.RoleArn
import Network.AWS.CodeStar.Types.S3Location
import Network.AWS.CodeStar.Types.SshPublicKey
import Network.AWS.CodeStar.Types.StackId
import Network.AWS.CodeStar.Types.State
import Network.AWS.CodeStar.Types.TagKey
import Network.AWS.CodeStar.Types.TagValue
import Network.AWS.CodeStar.Types.TeamMember
import Network.AWS.CodeStar.Types.TemplateParameterKey
import Network.AWS.CodeStar.Types.TemplateParameterValue
import Network.AWS.CodeStar.Types.Token
import Network.AWS.CodeStar.Types.Toolchain
import Network.AWS.CodeStar.Types.ToolchainSource
import Network.AWS.CodeStar.Types.Type
import Network.AWS.CodeStar.Types.UserArn
import Network.AWS.CodeStar.Types.UserProfileDisplayName
import Network.AWS.CodeStar.Types.UserProfileSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon CodeStar SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CodeStar",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "codestar",
      Core._svcVersion = "2017-04-19",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CodeStar",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The team member is already associated with a role in this project.
_TeamMemberAlreadyAssociatedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TeamMemberAlreadyAssociatedException =
  Core._MatchServiceError
    mkServiceConfig
    "TeamMemberAlreadyAssociatedException"
{-# DEPRECATED _TeamMemberAlreadyAssociatedException "Use generic-lens or generic-optics instead." #-}

-- | The specified input is either not valid, or it could not be validated.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError mkServiceConfig "ValidationException"
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead." #-}

-- | The service role is not valid.
_InvalidServiceRoleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidServiceRoleException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidServiceRoleException"
{-# DEPRECATED _InvalidServiceRoleException "Use generic-lens or generic-optics instead." #-}

-- | The project creation request was valid, but a nonspecific exception or error occurred during project creation. The project could not be created in AWS CodeStar.
_ProjectCreationFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProjectCreationFailedException =
  Core._MatchServiceError
    mkServiceConfig
    "ProjectCreationFailedException"
{-# DEPRECATED _ProjectCreationFailedException "Use generic-lens or generic-optics instead." #-}

-- | A user profile with that name already exists in this region for the AWS account. AWS CodeStar user profile names must be unique within a region for the AWS account.
_UserProfileAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserProfileAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "UserProfileAlreadyExistsException"
{-# DEPRECATED _UserProfileAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The specified AWS CodeStar project was not found.
_ProjectNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProjectNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ProjectNotFoundException"
{-# DEPRECATED _ProjectNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified team member was not found.
_TeamMemberNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TeamMemberNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "TeamMemberNotFoundException"
{-# DEPRECATED _TeamMemberNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | An AWS CodeStar project with the same ID already exists in this region for the AWS account. AWS CodeStar project IDs must be unique within a region for the AWS account.
_ProjectAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProjectAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ProjectAlreadyExistsException"
{-# DEPRECATED _ProjectAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | Project configuration information is required but not specified.
_ProjectConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProjectConfigurationException =
  Core._MatchServiceError
    mkServiceConfig
    "ProjectConfigurationException"
{-# DEPRECATED _ProjectConfigurationException "Use generic-lens or generic-optics instead." #-}

-- | Another modification is being made. That modification must complete before you can make your change.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentModificationException"
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | The next token is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidNextTokenException"
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | The user profile was not found.
_UserProfileNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserProfileNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "UserProfileNotFoundException"
{-# DEPRECATED _UserProfileNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | A resource limit has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
