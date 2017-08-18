{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types
    (
    -- * Service Configuration
      codeBuild

    -- * Errors
    , _ResourceAlreadyExistsException
    , _AccountLimitExceededException
    , _InvalidInputException
    , _ResourceNotFoundException

    -- * ArtifactNamespace
    , ArtifactNamespace (..)

    -- * ArtifactPackaging
    , ArtifactPackaging (..)

    -- * ArtifactsType
    , ArtifactsType (..)

    -- * BuildPhaseType
    , BuildPhaseType (..)

    -- * ComputeType
    , ComputeType (..)

    -- * EnvironmentType
    , EnvironmentType (..)

    -- * LanguageType
    , LanguageType (..)

    -- * PlatformType
    , PlatformType (..)

    -- * ProjectSortByType
    , ProjectSortByType (..)

    -- * SortOrderType
    , SortOrderType (..)

    -- * SourceAuthType
    , SourceAuthType (..)

    -- * SourceType
    , SourceType (..)

    -- * StatusType
    , StatusType (..)

    -- * Build
    , Build
    , build
    , bPhases
    , bBuildComplete
    , bArn
    , bStartTime
    , bArtifacts
    , bEnvironment
    , bInitiator
    , bCurrentPhase
    , bSourceVersion
    , bLogs
    , bEndTime
    , bProjectName
    , bBuildStatus
    , bSource
    , bId
    , bTimeoutInMinutes

    -- * BuildArtifacts
    , BuildArtifacts
    , buildArtifacts
    , baLocation
    , baMd5sum
    , baSha256sum

    -- * BuildPhase
    , BuildPhase
    , buildPhase
    , bpContexts
    , bpStartTime
    , bpPhaseStatus
    , bpPhaseType
    , bpEndTime
    , bpDurationInSeconds

    -- * EnvironmentImage
    , EnvironmentImage
    , environmentImage
    , eiName
    , eiDescription

    -- * EnvironmentLanguage
    , EnvironmentLanguage
    , environmentLanguage
    , elImages
    , elLanguage

    -- * EnvironmentPlatform
    , EnvironmentPlatform
    , environmentPlatform
    , epPlatform
    , epLanguages

    -- * EnvironmentVariable
    , EnvironmentVariable
    , environmentVariable
    , evName
    , evValue

    -- * LogsLocation
    , LogsLocation
    , logsLocation
    , llDeepLink
    , llGroupName
    , llStreamName

    -- * PhaseContext
    , PhaseContext
    , phaseContext
    , pcMessage
    , pcStatusCode

    -- * Project
    , Project
    , project
    , pArn
    , pArtifacts
    , pEnvironment
    , pCreated
    , pName
    , pSource
    , pEncryptionKey
    , pLastModified
    , pDescription
    , pServiceRole
    , pTags
    , pTimeoutInMinutes

    -- * ProjectArtifacts
    , ProjectArtifacts
    , projectArtifacts
    , paPackaging
    , paPath
    , paLocation
    , paName
    , paNamespaceType
    , paType

    -- * ProjectEnvironment
    , ProjectEnvironment
    , projectEnvironment
    , pePrivilegedMode
    , peEnvironmentVariables
    , peType
    , peImage
    , peComputeType

    -- * ProjectSource
    , ProjectSource
    , projectSource
    , psLocation
    , psAuth
    , psBuildspec
    , psType

    -- * SourceAuth
    , SourceAuth
    , sourceAuth
    , saResource
    , saType

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import           Network.AWS.CodeBuild.Types.Product
import           Network.AWS.CodeBuild.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2016-10-06@ of the Amazon CodeBuild SDK configuration.
codeBuild :: Service
codeBuild =
    Service
    { _svcAbbrev = "CodeBuild"
    , _svcSigner = v4
    , _svcPrefix = "codebuild"
    , _svcVersion = "2016-10-06"
    , _svcEndpoint = defaultEndpoint codeBuild
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CodeBuild"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
          Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The specified AWS resource cannot be created, because an AWS resource with the same settings already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
    _MatchServiceError codeBuild "ResourceAlreadyExistsException"

-- | An AWS service limit was exceeded for the calling AWS account.
--
--
_AccountLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountLimitExceededException =
    _MatchServiceError codeBuild "AccountLimitExceededException"

-- | The input value that was provided is not valid.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _MatchServiceError codeBuild "InvalidInputException"

-- | The specified AWS resource cannot be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _MatchServiceError codeBuild "ResourceNotFoundException"
