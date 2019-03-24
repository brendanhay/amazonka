{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types
    (
    -- * Service Configuration
      codeBuild

    -- * Errors
    , _ResourceAlreadyExistsException
    , _OAuthProviderException
    , _AccountLimitExceededException
    , _InvalidInputException
    , _ResourceNotFoundException

    -- * ArtifactNamespace
    , ArtifactNamespace (..)

    -- * ArtifactPackaging
    , ArtifactPackaging (..)

    -- * ArtifactsType
    , ArtifactsType (..)

    -- * AuthType
    , AuthType (..)

    -- * BuildPhaseType
    , BuildPhaseType (..)

    -- * CacheMode
    , CacheMode (..)

    -- * CacheType
    , CacheType (..)

    -- * ComputeType
    , ComputeType (..)

    -- * CredentialProviderType
    , CredentialProviderType (..)

    -- * EnvironmentType
    , EnvironmentType (..)

    -- * EnvironmentVariableType
    , EnvironmentVariableType (..)

    -- * ImagePullCredentialsType
    , ImagePullCredentialsType (..)

    -- * LanguageType
    , LanguageType (..)

    -- * LogsConfigStatusType
    , LogsConfigStatusType (..)

    -- * PlatformType
    , PlatformType (..)

    -- * ProjectSortByType
    , ProjectSortByType (..)

    -- * ServerType
    , ServerType (..)

    -- * SortOrderType
    , SortOrderType (..)

    -- * SourceAuthType
    , SourceAuthType (..)

    -- * SourceType
    , SourceType (..)

    -- * StatusType
    , StatusType (..)

    -- * WebhookFilterType
    , WebhookFilterType (..)

    -- * Build
    , Build
    , build
    , bPhases
    , bBuildComplete
    , bSecondaryArtifacts
    , bArn
    , bStartTime
    , bArtifacts
    , bEnvironment
    , bInitiator
    , bNetworkInterface
    , bSecondarySourceVersions
    , bCurrentPhase
    , bQueuedTimeoutInMinutes
    , bCache
    , bSecondarySources
    , bSourceVersion
    , bLogs
    , bResolvedSourceVersion
    , bVpcConfig
    , bEndTime
    , bProjectName
    , bBuildStatus
    , bSource
    , bId
    , bEncryptionKey
    , bServiceRole
    , bTimeoutInMinutes

    -- * BuildArtifacts
    , BuildArtifacts
    , buildArtifacts
    , baLocation
    , baMd5sum
    , baEncryptionDisabled
    , baOverrideArtifactName
    , baArtifactIdentifier
    , baSha256sum

    -- * BuildNotDeleted
    , BuildNotDeleted
    , buildNotDeleted
    , bndId
    , bndStatusCode

    -- * BuildPhase
    , BuildPhase
    , buildPhase
    , bpContexts
    , bpStartTime
    , bpPhaseStatus
    , bpPhaseType
    , bpEndTime
    , bpDurationInSeconds

    -- * CloudWatchLogsConfig
    , CloudWatchLogsConfig
    , cloudWatchLogsConfig
    , cwlcGroupName
    , cwlcStreamName
    , cwlcStatus

    -- * EnvironmentImage
    , EnvironmentImage
    , environmentImage
    , eiVersions
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
    , evType
    , evName
    , evValue

    -- * GitSubmodulesConfig
    , GitSubmodulesConfig
    , gitSubmodulesConfig
    , gscFetchSubmodules

    -- * LogsConfig
    , LogsConfig
    , logsConfig
    , lcS3Logs
    , lcCloudWatchLogs

    -- * LogsLocation
    , LogsLocation
    , logsLocation
    , llDeepLink
    , llS3Logs
    , llCloudWatchLogs
    , llS3DeepLink
    , llGroupName
    , llStreamName

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niSubnetId
    , niNetworkInterfaceId

    -- * PhaseContext
    , PhaseContext
    , phaseContext
    , pcMessage
    , pcStatusCode

    -- * Project
    , Project
    , project
    , pSecondaryArtifacts
    , pArn
    , pArtifacts
    , pEnvironment
    , pCreated
    , pQueuedTimeoutInMinutes
    , pCache
    , pSecondarySources
    , pName
    , pVpcConfig
    , pSource
    , pBadge
    , pLogsConfig
    , pEncryptionKey
    , pLastModified
    , pWebhook
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
    , paEncryptionDisabled
    , paOverrideArtifactName
    , paArtifactIdentifier
    , paNamespaceType
    , paType

    -- * ProjectBadge
    , ProjectBadge
    , projectBadge
    , pbBadgeEnabled
    , pbBadgeRequestURL

    -- * ProjectCache
    , ProjectCache
    , projectCache
    , pcLocation
    , pcModes
    , pcType

    -- * ProjectEnvironment
    , ProjectEnvironment
    , projectEnvironment
    , peImagePullCredentialsType
    , pePrivilegedMode
    , peRegistryCredential
    , peCertificate
    , peEnvironmentVariables
    , peType
    , peImage
    , peComputeType

    -- * ProjectSource
    , ProjectSource
    , projectSource
    , psReportBuildStatus
    , psInsecureSSL
    , psLocation
    , psAuth
    , psBuildspec
    , psSourceIdentifier
    , psGitCloneDepth
    , psGitSubmodulesConfig
    , psType

    -- * ProjectSourceVersion
    , ProjectSourceVersion
    , projectSourceVersion
    , psvSourceIdentifier
    , psvSourceVersion

    -- * RegistryCredential
    , RegistryCredential
    , registryCredential
    , rcCredential
    , rcCredentialProvider

    -- * S3LogsConfig
    , S3LogsConfig
    , s3LogsConfig
    , slcLocation
    , slcEncryptionDisabled
    , slcStatus

    -- * SourceAuth
    , SourceAuth
    , sourceAuth
    , saResource
    , saType

    -- * SourceCredentialsInfo
    , SourceCredentialsInfo
    , sourceCredentialsInfo
    , sciArn
    , sciServerType
    , sciAuthType

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSecurityGroupIds
    , vcVpcId
    , vcSubnets

    -- * Webhook
    , Webhook
    , webhook
    , wBranchFilter
    , wLastModifiedSecret
    , wUrl
    , wSecret
    , wFilterGroups
    , wPayloadURL

    -- * WebhookFilter
    , WebhookFilter
    , webhookFilter
    , wfExcludeMatchedPattern
    , wfType
    , wfPattern
    ) where

import Network.AWS.CodeBuild.Types.Product
import Network.AWS.CodeBuild.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

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
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
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


-- | There was a problem with the underlying OAuth provider.
--
--
_OAuthProviderException :: AsError a => Getting (First ServiceError) a ServiceError
_OAuthProviderException = _MatchServiceError codeBuild "OAuthProviderException"


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

