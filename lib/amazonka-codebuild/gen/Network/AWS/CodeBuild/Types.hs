-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ResourceAlreadyExistsException
    , _OAuthProviderException
    , _AccountLimitExceededException
    , _InvalidInputException
    , _ResourceNotFoundException

    -- * ProjectSortByType
    , ProjectSortByType (..)

    -- * CacheType
    , CacheType (..)

    -- * TestReportSummary
    , TestReportSummary (..)
    , mkTestReportSummary
    , trsTotal
    , trsStatusCounts
    , trsDurationInNanoSeconds

    -- * SortOrderType
    , SortOrderType (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * TestCase
    , TestCase (..)
    , mkTestCase
    , tcDurationInNanoSeconds
    , tcExpired
    , tcMessage
    , tcName
    , tcPrefix
    , tcReportArn
    , tcStatus
    , tcTestRawDataPath

    -- * LanguageType
    , LanguageType (..)

    -- * ReportGroupTrendFieldType
    , ReportGroupTrendFieldType (..)

    -- * ImagePullCredentialsType
    , ImagePullCredentialsType (..)

    -- * EnvironmentPlatform
    , EnvironmentPlatform (..)
    , mkEnvironmentPlatform
    , epLanguages
    , epPlatform

    -- * RetryBuildBatchType
    , RetryBuildBatchType (..)

    -- * CredentialProviderType
    , CredentialProviderType (..)

    -- * BuildBatchFilter
    , BuildBatchFilter (..)
    , mkBuildBatchFilter
    , bbfStatus

    -- * ReportStatusType
    , ReportStatusType (..)

    -- * ProjectSourceVersion
    , ProjectSourceVersion (..)
    , mkProjectSourceVersion
    , psvSourceIdentifier
    , psvSourceVersion

    -- * S3LogsConfig
    , S3LogsConfig (..)
    , mkS3LogsConfig
    , slcStatus
    , slcEncryptionDisabled
    , slcLocation

    -- * BuildBatchPhaseType
    , BuildBatchPhaseType (..)

    -- * SourceAuthType
    , SourceAuthType (..)

    -- * CloudWatchLogsConfig
    , CloudWatchLogsConfig (..)
    , mkCloudWatchLogsConfig
    , cwlcStatus
    , cwlcGroupName
    , cwlcStreamName

    -- * ReportGroupStatusType
    , ReportGroupStatusType (..)

    -- * SourceType
    , SourceType (..)

    -- * WebhookBuildType
    , WebhookBuildType (..)

    -- * Project
    , Project (..)
    , mkProject
    , pArn
    , pArtifacts
    , pBadge
    , pBuildBatchConfig
    , pCache
    , pCreated
    , pDescription
    , pEncryptionKey
    , pEnvironment
    , pFileSystemLocations
    , pLastModified
    , pLogsConfig
    , pName
    , pQueuedTimeoutInMinutes
    , pSecondaryArtifacts
    , pSecondarySourceVersions
    , pSecondarySources
    , pServiceRole
    , pSource
    , pSourceVersion
    , pTags
    , pTimeoutInMinutes
    , pVpcConfig
    , pWebhook

    -- * ServerType
    , ServerType (..)

    -- * S3ReportExportConfig
    , S3ReportExportConfig (..)
    , mkS3ReportExportConfig
    , srecBucket
    , srecEncryptionDisabled
    , srecEncryptionKey
    , srecPackaging
    , srecPath

    -- * ProjectBadge
    , ProjectBadge (..)
    , mkProjectBadge
    , pbBadgeEnabled
    , pbBadgeRequestUrl

    -- * ReportExportConfigType
    , ReportExportConfigType (..)

    -- * NetworkInterface
    , NetworkInterface (..)
    , mkNetworkInterface
    , niNetworkInterfaceId
    , niSubnetId

    -- * WebhookFilter
    , WebhookFilter (..)
    , mkWebhookFilter
    , wfType
    , wfPattern
    , wfExcludeMatchedPattern

    -- * EnvironmentVariableType
    , EnvironmentVariableType (..)

    -- * SourceCredentialsInfo
    , SourceCredentialsInfo (..)
    , mkSourceCredentialsInfo
    , sciArn
    , sciAuthType
    , sciServerType

    -- * ProjectFileSystemLocation
    , ProjectFileSystemLocation (..)
    , mkProjectFileSystemLocation
    , pfslIdentifier
    , pfslLocation
    , pfslMountOptions
    , pfslMountPoint
    , pfslType

    -- * CodeCoverage
    , CodeCoverage (..)
    , mkCodeCoverage
    , ccBranchCoveragePercentage
    , ccBranchesCovered
    , ccBranchesMissed
    , ccExpired
    , ccFilePath
    , ccId
    , ccLineCoveragePercentage
    , ccLinesCovered
    , ccLinesMissed
    , ccReportARN

    -- * ExportedEnvironmentVariable
    , ExportedEnvironmentVariable (..)
    , mkExportedEnvironmentVariable
    , eevName
    , eevValue

    -- * Build
    , Build (..)
    , mkBuild
    , bArn
    , bArtifacts
    , bBuildBatchArn
    , bBuildComplete
    , bBuildNumber
    , bBuildStatus
    , bCache
    , bCurrentPhase
    , bDebugSession
    , bEncryptionKey
    , bEndTime
    , bEnvironment
    , bExportedEnvironmentVariables
    , bFileSystemLocations
    , bId
    , bInitiator
    , bLogs
    , bNetworkInterface
    , bPhases
    , bProjectName
    , bQueuedTimeoutInMinutes
    , bReportArns
    , bResolvedSourceVersion
    , bSecondaryArtifacts
    , bSecondarySourceVersions
    , bSecondarySources
    , bServiceRole
    , bSource
    , bSourceVersion
    , bStartTime
    , bTimeoutInMinutes
    , bVpcConfig

    -- * Report
    , Report (..)
    , mkReport
    , rArn
    , rCodeCoverageSummary
    , rCreated
    , rExecutionId
    , rExpired
    , rExportConfig
    , rName
    , rReportGroupArn
    , rStatus
    , rTestSummary
    , rTruncated
    , rType

    -- * RegistryCredential
    , RegistryCredential (..)
    , mkRegistryCredential
    , rcCredential
    , rcCredentialProvider

    -- * ResolvedArtifact
    , ResolvedArtifact (..)
    , mkResolvedArtifact
    , raIdentifier
    , raLocation
    , raType

    -- * BuildNotDeleted
    , BuildNotDeleted (..)
    , mkBuildNotDeleted
    , bndId
    , bndStatusCode

    -- * LogsConfigStatusType
    , LogsConfigStatusType (..)

    -- * CodeCoverageReportSummary
    , CodeCoverageReportSummary (..)
    , mkCodeCoverageReportSummary
    , ccrsBranchCoveragePercentage
    , ccrsBranchesCovered
    , ccrsBranchesMissed
    , ccrsLineCoveragePercentage
    , ccrsLinesCovered
    , ccrsLinesMissed

    -- * BuildPhaseType
    , BuildPhaseType (..)

    -- * ReportPackagingType
    , ReportPackagingType (..)

    -- * ProjectCache
    , ProjectCache (..)
    , mkProjectCache
    , pcType
    , pcLocation
    , pcModes

    -- * ReportCodeCoverageSortByType
    , ReportCodeCoverageSortByType (..)

    -- * StatusType
    , StatusType (..)

    -- * NonEmptyString
    , NonEmptyString (..)

    -- * VpcConfig
    , VpcConfig (..)
    , mkVpcConfig
    , vcSecurityGroupIds
    , vcSubnets
    , vcVpcId

    -- * DebugSession
    , DebugSession (..)
    , mkDebugSession
    , dsSessionEnabled
    , dsSessionTarget

    -- * CacheMode
    , CacheMode (..)

    -- * PlatformType
    , PlatformType (..)

    -- * EnvironmentLanguage
    , EnvironmentLanguage (..)
    , mkEnvironmentLanguage
    , elImages
    , elLanguage

    -- * EnvironmentImage
    , EnvironmentImage (..)
    , mkEnvironmentImage
    , eiDescription
    , eiName
    , eiVersions

    -- * LogsLocation
    , LogsLocation (..)
    , mkLogsLocation
    , llCloudWatchLogs
    , llCloudWatchLogsArn
    , llDeepLink
    , llGroupName
    , llS3DeepLink
    , llS3Logs
    , llS3LogsArn
    , llStreamName

    -- * BuildSummary
    , BuildSummary (..)
    , mkBuildSummary
    , bsArn
    , bsBuildStatus
    , bsPrimaryArtifact
    , bsRequestedOn
    , bsSecondaryArtifacts

    -- * BuildBatch
    , BuildBatch (..)
    , mkBuildBatch
    , bbArn
    , bbArtifacts
    , bbBuildBatchConfig
    , bbBuildBatchNumber
    , bbBuildBatchStatus
    , bbBuildGroups
    , bbBuildTimeoutInMinutes
    , bbCache
    , bbComplete
    , bbCurrentPhase
    , bbEncryptionKey
    , bbEndTime
    , bbEnvironment
    , bbFileSystemLocations
    , bbId
    , bbInitiator
    , bbLogConfig
    , bbPhases
    , bbProjectName
    , bbQueuedTimeoutInMinutes
    , bbResolvedSourceVersion
    , bbSecondaryArtifacts
    , bbSecondarySourceVersions
    , bbSecondarySources
    , bbServiceRole
    , bbSource
    , bbSourceVersion
    , bbStartTime
    , bbVpcConfig

    -- * TestCaseFilter
    , TestCaseFilter (..)
    , mkTestCaseFilter
    , tcfKeyword
    , tcfStatus

    -- * SharedResourceSortByType
    , SharedResourceSortByType (..)

    -- * ProjectName
    , ProjectName (..)

    -- * ArtifactNamespace
    , ArtifactNamespace (..)

    -- * EnvironmentType
    , EnvironmentType (..)

    -- * ArtifactsType
    , ArtifactsType (..)

    -- * GitSubmodulesConfig
    , GitSubmodulesConfig (..)
    , mkGitSubmodulesConfig
    , gscFetchSubmodules

    -- * LogsConfig
    , LogsConfig (..)
    , mkLogsConfig
    , lcCloudWatchLogs
    , lcS3Logs

    -- * ComputeType
    , ComputeType (..)

    -- * ProjectBuildBatchConfig
    , ProjectBuildBatchConfig (..)
    , mkProjectBuildBatchConfig
    , pbbcCombineArtifacts
    , pbbcRestrictions
    , pbbcServiceRole
    , pbbcTimeoutInMins

    -- * SourceAuth
    , SourceAuth (..)
    , mkSourceAuth
    , saType
    , saResource

    -- * BuildBatchPhase
    , BuildBatchPhase (..)
    , mkBuildBatchPhase
    , bbpContexts
    , bbpDurationInSeconds
    , bbpEndTime
    , bbpPhaseStatus
    , bbpPhaseType
    , bbpStartTime

    -- * ProjectEnvironment
    , ProjectEnvironment (..)
    , mkProjectEnvironment
    , peType
    , peImage
    , peComputeType
    , peCertificate
    , peEnvironmentVariables
    , peImagePullCredentialsType
    , pePrivilegedMode
    , peRegistryCredential

    -- * ReportGroupSortByType
    , ReportGroupSortByType (..)

    -- * ReportWithRawData
    , ReportWithRawData (..)
    , mkReportWithRawData
    , rwrdData
    , rwrdReportArn

    -- * BuildStatusConfig
    , BuildStatusConfig (..)
    , mkBuildStatusConfig
    , bscContext
    , bscTargetUrl

    -- * ProjectSource
    , ProjectSource (..)
    , mkProjectSource
    , psType
    , psAuth
    , psBuildStatusConfig
    , psBuildspec
    , psGitCloneDepth
    , psGitSubmodulesConfig
    , psInsecureSsl
    , psLocation
    , psReportBuildStatus
    , psSourceIdentifier

    -- * ReportExportConfig
    , ReportExportConfig (..)
    , mkReportExportConfig
    , recExportConfigType
    , recS3Destination

    -- * PhaseContext
    , PhaseContext (..)
    , mkPhaseContext
    , pcMessage
    , pcStatusCode

    -- * AuthType
    , AuthType (..)

    -- * ProjectArtifacts
    , ProjectArtifacts (..)
    , mkProjectArtifacts
    , paType
    , paArtifactIdentifier
    , paEncryptionDisabled
    , paLocation
    , paName
    , paNamespaceType
    , paOverrideArtifactName
    , paPackaging
    , paPath

    -- * Webhook
    , Webhook (..)
    , mkWebhook
    , wBranchFilter
    , wBuildType
    , wFilterGroups
    , wLastModifiedSecret
    , wPayloadUrl
    , wSecret
    , wUrl

    -- * BatchRestrictions
    , BatchRestrictions (..)
    , mkBatchRestrictions
    , brComputeTypesAllowed
    , brMaximumBuildsAllowed

    -- * WebhookFilterType
    , WebhookFilterType (..)

    -- * EnvironmentVariable
    , EnvironmentVariable (..)
    , mkEnvironmentVariable
    , evName
    , evValue
    , evType

    -- * BuildArtifacts
    , BuildArtifacts (..)
    , mkBuildArtifacts
    , baArtifactIdentifier
    , baEncryptionDisabled
    , baLocation
    , baMd5sum
    , baOverrideArtifactName
    , baSha256sum

    -- * FileSystemType
    , FileSystemType (..)

    -- * ReportType
    , ReportType (..)

    -- * ReportGroupName
    , ReportGroupName (..)

    -- * ReportFilter
    , ReportFilter (..)
    , mkReportFilter
    , rfStatus

    -- * ArtifactPackaging
    , ArtifactPackaging (..)

    -- * BuildGroup
    , BuildGroup (..)
    , mkBuildGroup
    , bgCurrentBuildSummary
    , bgDependsOn
    , bgIdentifier
    , bgIgnoreFailure
    , bgPriorBuildSummaryList

    -- * ReportGroupTrendStats
    , ReportGroupTrendStats (..)
    , mkReportGroupTrendStats
    , rgtsAverage
    , rgtsMax
    , rgtsMin

    -- * BuildPhase
    , BuildPhase (..)
    , mkBuildPhase
    , bpContexts
    , bpDurationInSeconds
    , bpEndTime
    , bpPhaseStatus
    , bpPhaseType
    , bpStartTime

    -- * ReportGroup
    , ReportGroup (..)
    , mkReportGroup
    , rgArn
    , rgCreated
    , rgExportConfig
    , rgLastModified
    , rgName
    , rgStatus
    , rgTags
    , rgType

    -- * ResourceArn
    , ResourceArn (..)

    -- * EncryptionKeyOverride
    , EncryptionKeyOverride (..)

    -- * ImageOverride
    , ImageOverride (..)

    -- * ServiceRoleOverride
    , ServiceRoleOverride (..)

    -- * Id
    , Id (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * ReportArn
    , ReportArn (..)

    -- * Description
    , Description (..)

    -- * EncryptionKey
    , EncryptionKey (..)

    -- * Name
    , Name (..)

    -- * ServiceRole
    , ServiceRole (..)

    -- * Bucket
    , Bucket (..)

    -- * NetworkInterfaceId
    , NetworkInterfaceId (..)

    -- * SubnetId
    , SubnetId (..)

    -- * NextToken
    , NextToken (..)

    -- * Arn
    , Arn (..)

    -- * Policy
    , Policy (..)

    -- * FilePath
    , FilePath (..)

    -- * ReportARN
    , ReportARN (..)

    -- * Token
    , Token (..)

    -- * Username
    , Username (..)

    -- * ResolvedSourceVersion
    , ResolvedSourceVersion (..)

    -- * SourceVersion
    , SourceVersion (..)

    -- * ReportGroupArn
    , ReportGroupArn (..)

    -- * Credential
    , Credential (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CodeBuild.Types.ProjectSortByType
  
import Network.AWS.CodeBuild.Types.CacheType
  
import Network.AWS.CodeBuild.Types.TestReportSummary
  
import Network.AWS.CodeBuild.Types.SortOrderType
  
import Network.AWS.CodeBuild.Types.Tag
  
import Network.AWS.CodeBuild.Types.TestCase
  
import Network.AWS.CodeBuild.Types.LanguageType
  
import Network.AWS.CodeBuild.Types.ReportGroupTrendFieldType
  
import Network.AWS.CodeBuild.Types.ImagePullCredentialsType
  
import Network.AWS.CodeBuild.Types.EnvironmentPlatform
  
import Network.AWS.CodeBuild.Types.RetryBuildBatchType
  
import Network.AWS.CodeBuild.Types.CredentialProviderType
  
import Network.AWS.CodeBuild.Types.BuildBatchFilter
  
import Network.AWS.CodeBuild.Types.ReportStatusType
  
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
  
import Network.AWS.CodeBuild.Types.S3LogsConfig
  
import Network.AWS.CodeBuild.Types.BuildBatchPhaseType
  
import Network.AWS.CodeBuild.Types.SourceAuthType
  
import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
  
import Network.AWS.CodeBuild.Types.ReportGroupStatusType
  
import Network.AWS.CodeBuild.Types.SourceType
  
import Network.AWS.CodeBuild.Types.WebhookBuildType
  
import Network.AWS.CodeBuild.Types.Project
  
import Network.AWS.CodeBuild.Types.ServerType
  
import Network.AWS.CodeBuild.Types.S3ReportExportConfig
  
import Network.AWS.CodeBuild.Types.ProjectBadge
  
import Network.AWS.CodeBuild.Types.ReportExportConfigType
  
import Network.AWS.CodeBuild.Types.NetworkInterface
  
  
import Network.AWS.CodeBuild.Types.WebhookFilter
  
import Network.AWS.CodeBuild.Types.EnvironmentVariableType
  
import Network.AWS.CodeBuild.Types.SourceCredentialsInfo
  
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
  
import Network.AWS.CodeBuild.Types.CodeCoverage
  
import Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
  
import Network.AWS.CodeBuild.Types.Build
  
import Network.AWS.CodeBuild.Types.Report
  
import Network.AWS.CodeBuild.Types.RegistryCredential
  
import Network.AWS.CodeBuild.Types.ResolvedArtifact
  
import Network.AWS.CodeBuild.Types.BuildNotDeleted
  
import Network.AWS.CodeBuild.Types.LogsConfigStatusType
  
import Network.AWS.CodeBuild.Types.CodeCoverageReportSummary
  
import Network.AWS.CodeBuild.Types.BuildPhaseType
  
  
import Network.AWS.CodeBuild.Types.ReportPackagingType
  
import Network.AWS.CodeBuild.Types.ProjectCache
  
import Network.AWS.CodeBuild.Types.ReportCodeCoverageSortByType
  
import Network.AWS.CodeBuild.Types.StatusType
  
import Network.AWS.CodeBuild.Types.NonEmptyString
  
import Network.AWS.CodeBuild.Types.VpcConfig
  
import Network.AWS.CodeBuild.Types.DebugSession
  
import Network.AWS.CodeBuild.Types.CacheMode
  
import Network.AWS.CodeBuild.Types.PlatformType
  
import Network.AWS.CodeBuild.Types.EnvironmentLanguage
  
import Network.AWS.CodeBuild.Types.EnvironmentImage
  
import Network.AWS.CodeBuild.Types.LogsLocation
  
import Network.AWS.CodeBuild.Types.BuildSummary
  
import Network.AWS.CodeBuild.Types.BuildBatch
  
import Network.AWS.CodeBuild.Types.TestCaseFilter
  
import Network.AWS.CodeBuild.Types.SharedResourceSortByType
  
import Network.AWS.CodeBuild.Types.ProjectName
  
import Network.AWS.CodeBuild.Types.ArtifactNamespace
  
import Network.AWS.CodeBuild.Types.EnvironmentType
  
import Network.AWS.CodeBuild.Types.ArtifactsType
  
import Network.AWS.CodeBuild.Types.GitSubmodulesConfig
  
import Network.AWS.CodeBuild.Types.LogsConfig
  
import Network.AWS.CodeBuild.Types.ComputeType
  
import Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
  
import Network.AWS.CodeBuild.Types.SourceAuth
  
import Network.AWS.CodeBuild.Types.BuildBatchPhase
  
import Network.AWS.CodeBuild.Types.ProjectEnvironment
  
import Network.AWS.CodeBuild.Types.ReportGroupSortByType
  
  
import Network.AWS.CodeBuild.Types.ReportWithRawData
  
import Network.AWS.CodeBuild.Types.BuildStatusConfig
  
  
import Network.AWS.CodeBuild.Types.ProjectSource
  
import Network.AWS.CodeBuild.Types.ReportExportConfig
  
import Network.AWS.CodeBuild.Types.PhaseContext
  
import Network.AWS.CodeBuild.Types.AuthType
  
import Network.AWS.CodeBuild.Types.ProjectArtifacts
  
import Network.AWS.CodeBuild.Types.Webhook
  
import Network.AWS.CodeBuild.Types.BatchRestrictions
  
import Network.AWS.CodeBuild.Types.WebhookFilterType
  
import Network.AWS.CodeBuild.Types.EnvironmentVariable
  
import Network.AWS.CodeBuild.Types.BuildArtifacts
  
import Network.AWS.CodeBuild.Types.FileSystemType
  
import Network.AWS.CodeBuild.Types.ReportType
  
import Network.AWS.CodeBuild.Types.ReportGroupName
  
import Network.AWS.CodeBuild.Types.ReportFilter
  
  
import Network.AWS.CodeBuild.Types.ArtifactPackaging
  
import Network.AWS.CodeBuild.Types.BuildGroup
  
import Network.AWS.CodeBuild.Types.ReportGroupTrendStats
  
import Network.AWS.CodeBuild.Types.BuildPhase
  
import Network.AWS.CodeBuild.Types.ReportGroup
  
import Network.AWS.CodeBuild.Types.ResourceArn
  
import Network.AWS.CodeBuild.Types.EncryptionKeyOverride
  
import Network.AWS.CodeBuild.Types.ImageOverride
  
import Network.AWS.CodeBuild.Types.ServiceRoleOverride
  
import Network.AWS.CodeBuild.Types.Id
  
import Network.AWS.CodeBuild.Types.Key
  
import Network.AWS.CodeBuild.Types.Value
  
import Network.AWS.CodeBuild.Types.ReportArn
  
import Network.AWS.CodeBuild.Types.Description
  
import Network.AWS.CodeBuild.Types.EncryptionKey
  
import Network.AWS.CodeBuild.Types.Name
  
import Network.AWS.CodeBuild.Types.ServiceRole
  
import Network.AWS.CodeBuild.Types.Bucket
  
import Network.AWS.CodeBuild.Types.NetworkInterfaceId
  
import Network.AWS.CodeBuild.Types.SubnetId
  
import Network.AWS.CodeBuild.Types.NextToken
  
import Network.AWS.CodeBuild.Types.Arn
  
import Network.AWS.CodeBuild.Types.Policy
  
import Network.AWS.CodeBuild.Types.FilePath
  
import Network.AWS.CodeBuild.Types.ReportARN
  
import Network.AWS.CodeBuild.Types.Token
  
import Network.AWS.CodeBuild.Types.Username
  
import Network.AWS.CodeBuild.Types.ResolvedSourceVersion
  
import Network.AWS.CodeBuild.Types.SourceVersion
  
import Network.AWS.CodeBuild.Types.ReportGroupArn
  
import Network.AWS.CodeBuild.Types.Credential
  

-- | API version @2016-10-06@ of the Amazon CodeBuild SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CodeBuild",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "codebuild",
                 Core._svcVersion = "2016-10-06", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "CodeBuild",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The specified AWS resource cannot be created, because an AWS resource with the same settings already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "ResourceAlreadyExistsException"
{-# INLINEABLE _ResourceAlreadyExistsException #-}
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | There was a problem with the underlying OAuth provider.
_OAuthProviderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OAuthProviderException
  = Core._MatchServiceError mkServiceConfig "OAuthProviderException"
{-# INLINEABLE _OAuthProviderException #-}
{-# DEPRECATED _OAuthProviderException "Use generic-lens or generic-optics instead"  #-}

-- | An AWS service limit was exceeded for the calling AWS account.
_AccountLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "AccountLimitExceededException"
{-# INLINEABLE _AccountLimitExceededException #-}
{-# DEPRECATED _AccountLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The input value that was provided is not valid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException
  = Core._MatchServiceError mkServiceConfig "InvalidInputException"
{-# INLINEABLE _InvalidInputException #-}
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead"  #-}

-- | The specified AWS resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}
