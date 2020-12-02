{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types
  ( -- * Service Configuration
    codeStar,

    -- * Errors

    -- * Code
    Code,
    code,
    cSource,
    cDestination,

    -- * CodeCommitCodeDestination
    CodeCommitCodeDestination,
    codeCommitCodeDestination,
    cccdName,

    -- * CodeDestination
    CodeDestination,
    codeDestination,
    cdCodeCommit,
    cdGitHub,

    -- * CodeSource
    CodeSource,
    codeSource,
    csS3,

    -- * GitHubCodeDestination
    GitHubCodeDestination,
    gitHubCodeDestination,
    ghcdDescription,
    ghcdName,
    ghcdType,
    ghcdOwner,
    ghcdPrivateRepository,
    ghcdIssuesEnabled,
    ghcdToken,

    -- * ProjectStatus
    ProjectStatus,
    projectStatus,
    psReason,
    psState,

    -- * ProjectSummary
    ProjectSummary,
    projectSummary,
    psProjectARN,
    psProjectId,

    -- * Resource
    Resource,
    resource,
    rId,

    -- * S3Location
    S3Location,
    s3Location,
    slBucketKey,
    slBucketName,

    -- * TeamMember
    TeamMember,
    teamMember,
    tmRemoteAccessAllowed,
    tmUserARN,
    tmProjectRole,

    -- * Toolchain
    Toolchain,
    toolchain,
    tStackParameters,
    tRoleARN,
    tSource,

    -- * ToolchainSource
    ToolchainSource,
    toolchainSource,
    tsS3,

    -- * UserProfileSummary
    UserProfileSummary,
    userProfileSummary,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-04-19@ of the Amazon CodeStar SDK configuration.
codeStar :: Service
codeStar =
  Service
    { _svcAbbrev = "CodeStar",
      _svcSigner = v4,
      _svcPrefix = "codestar",
      _svcVersion = "2017-04-19",
      _svcEndpoint = defaultEndpoint codeStar,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CodeStar",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
