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
    codeStarService,

    -- * Errors

    -- * Code
    Code (..),
    mkCode,
    cDestination,
    cSource,

    -- * CodeCommitCodeDestination
    CodeCommitCodeDestination (..),
    mkCodeCommitCodeDestination,
    cccdName,

    -- * CodeDestination
    CodeDestination (..),
    mkCodeDestination,
    cdCodeCommit,
    cdGitHub,

    -- * CodeSource
    CodeSource (..),
    mkCodeSource,
    csS3,

    -- * GitHubCodeDestination
    GitHubCodeDestination (..),
    mkGitHubCodeDestination,
    ghcdPrivateRepository,
    ghcdToken,
    ghcdOwner,
    ghcdName,
    ghcdIssuesEnabled,
    ghcdType,
    ghcdDescription,

    -- * ProjectStatus
    ProjectStatus (..),
    mkProjectStatus,
    psState,
    psReason,

    -- * ProjectSummary
    ProjectSummary (..),
    mkProjectSummary,
    psProjectARN,
    psProjectId,

    -- * Resource
    Resource (..),
    mkResource,
    rId,

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBucketKey,
    slBucketName,

    -- * TeamMember
    TeamMember (..),
    mkTeamMember,
    tmUserARN,
    tmRemoteAccessAllowed,
    tmProjectRole,

    -- * Toolchain
    Toolchain (..),
    mkToolchain,
    tStackParameters,
    tSource,
    tRoleARN,

    -- * ToolchainSource
    ToolchainSource (..),
    mkToolchainSource,
    tsS3,

    -- * UserProfileSummary
    UserProfileSummary (..),
    mkUserProfileSummary,
    upsSshPublicKey,
    upsUserARN,
    upsEmailAddress,
    upsDisplayName,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon CodeStar SDK configuration.
codeStarService :: Lude.Service
codeStarService =
  Lude.Service
    { Lude._svcAbbrev = "CodeStar",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "codestar",
      Lude._svcVersion = "2017-04-19",
      Lude._svcEndpoint = Lude.defaultEndpoint codeStarService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CodeStar",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
