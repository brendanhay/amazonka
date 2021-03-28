{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodeBuild__ 
--
-- AWS CodeBuild is a fully managed build service in the cloud. AWS CodeBuild compiles your source code, runs unit tests, and produces artifacts that are ready to deploy. AWS CodeBuild eliminates the need to provision, manage, and scale your own build servers. It provides prepackaged build environments for the most popular programming languages and build tools, such as Apache Maven, Gradle, and more. You can also fully customize build environments in AWS CodeBuild to use your own build tools. AWS CodeBuild scales automatically to meet peak build requests. You pay only for the build time you consume. For more information about AWS CodeBuild, see the /<https:\/\/docs.aws.amazon.com\/codebuild\/latest\/userguide\/welcome.html AWS CodeBuild User Guide> ./ 
-- AWS CodeBuild supports these operations:
--
--     * @BatchDeleteBuilds@ : Deletes one or more builds.
--
--
--     * @BatchGetBuilds@ : Gets information about one or more builds.
--
--
--     * @BatchGetProjects@ : Gets information about one or more build projects. A /build project/ defines how AWS CodeBuild runs a build. This includes information such as where to get the source code to build, the build environment to use, the build commands to run, and where to store the build output. A /build environment/ is a representation of operating system, programming language runtime, and tools that AWS CodeBuild uses to run a build. You can add tags to build projects to help manage your resources and costs.
--
--
--     * @BatchGetReportGroups@ : Returns an array of report groups. 
--
--
--     * @BatchGetReports@ : Returns an array of reports. 
--
--
--     * @CreateProject@ : Creates a build project.
--
--
--     * @CreateReportGroup@ : Creates a report group. A report group contains a collection of reports. 
--
--
--     * @CreateWebhook@ : For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, enables AWS CodeBuild to start rebuilding the source code every time a code change is pushed to the repository.
--
--
--     * @DeleteProject@ : Deletes a build project.
--
--
--     * @DeleteReport@ : Deletes a report. 
--
--
--     * @DeleteReportGroup@ : Deletes a report group. 
--
--
--     * @DeleteResourcePolicy@ : Deletes a resource policy that is identified by its resource ARN. 
--
--
--     * @DeleteSourceCredentials@ : Deletes a set of GitHub, GitHub Enterprise, or Bitbucket source credentials.
--
--
--     * @DeleteWebhook@ : For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, stops AWS CodeBuild from rebuilding the source code every time a code change is pushed to the repository.
--
--
--     * @DescribeTestCases@ : Returns a list of details about test cases for a report. 
--
--
--     * @GetResourcePolicy@ : Gets a resource policy that is identified by its resource ARN. 
--
--
--     * @ImportSourceCredentials@ : Imports the source repository credentials for an AWS CodeBuild project that has its source code stored in a GitHub, GitHub Enterprise, or Bitbucket repository.
--
--
--     * @InvalidateProjectCache@ : Resets the cache for a project.
--
--
--     * @ListBuilds@ : Gets a list of build IDs, with each build ID representing a single build.
--
--
--     * @ListBuildsForProject@ : Gets a list of build IDs for the specified build project, with each build ID representing a single build.
--
--
--     * @ListCuratedEnvironmentImages@ : Gets information about Docker images that are managed by AWS CodeBuild.
--
--
--     * @ListProjects@ : Gets a list of build project names, with each build project name representing a single build project.
--
--
--     * @ListReportGroups@ : Gets a list ARNs for the report groups in the current AWS account. 
--
--
--     * @ListReports@ : Gets a list ARNs for the reports in the current AWS account. 
--
--
--     * @ListReportsForReportGroup@ : Returns a list of ARNs for the reports that belong to a @ReportGroup@ . 
--
--
--     * @ListSharedProjects@ : Gets a list of ARNs associated with projects shared with the current AWS account or user.
--
--
--     * @ListSharedReportGroups@ : Gets a list of ARNs associated with report groups shared with the current AWS account or user
--
--
--     * @ListSourceCredentials@ : Returns a list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@ object includes the authentication type, token ARN, and type of source provider for one set of credentials.
--
--
--     * @PutResourcePolicy@ : Stores a resource policy for the ARN of a @Project@ or @ReportGroup@ object. 
--
--
--     * @StartBuild@ : Starts running a build.
--
--
--     * @StopBuild@ : Attempts to stop running a build.
--
--
--     * @UpdateProject@ : Changes the settings of an existing build project.
--
--
--     * @UpdateReportGroup@ : Changes a report group.
--
--
--     * @UpdateWebhook@ : Changes the settings of an existing webhook.
--
--
module Network.AWS.CodeBuild
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** OAuthProviderException
    , _OAuthProviderException

    -- ** AccountLimitExceededException
    , _AccountLimitExceededException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects (Paginated)
    , module Network.AWS.CodeBuild.ListProjects

    -- ** DeleteProject 
    , module Network.AWS.CodeBuild.DeleteProject

    -- ** UpdateProject 
    , module Network.AWS.CodeBuild.UpdateProject

    -- ** DeleteSourceCredentials 
    , module Network.AWS.CodeBuild.DeleteSourceCredentials

    -- ** ListBuilds (Paginated)
    , module Network.AWS.CodeBuild.ListBuilds

    -- ** ListSourceCredentials 
    , module Network.AWS.CodeBuild.ListSourceCredentials

    -- ** ListReports (Paginated)
    , module Network.AWS.CodeBuild.ListReports

    -- ** DeleteReport 
    , module Network.AWS.CodeBuild.DeleteReport

    -- ** CreateWebhook 
    , module Network.AWS.CodeBuild.CreateWebhook

    -- ** StopBuildBatch 
    , module Network.AWS.CodeBuild.StopBuildBatch

    -- ** ListSharedProjects (Paginated)
    , module Network.AWS.CodeBuild.ListSharedProjects

    -- ** CreateReportGroup 
    , module Network.AWS.CodeBuild.CreateReportGroup

    -- ** DescribeCodeCoverages (Paginated)
    , module Network.AWS.CodeBuild.DescribeCodeCoverages

    -- ** ImportSourceCredentials 
    , module Network.AWS.CodeBuild.ImportSourceCredentials

    -- ** ListBuildBatchesForProject (Paginated)
    , module Network.AWS.CodeBuild.ListBuildBatchesForProject

    -- ** BatchGetReportGroups 
    , module Network.AWS.CodeBuild.BatchGetReportGroups

    -- ** DeleteBuildBatch 
    , module Network.AWS.CodeBuild.DeleteBuildBatch

    -- ** StartBuild 
    , module Network.AWS.CodeBuild.StartBuild

    -- ** BatchGetBuildBatches 
    , module Network.AWS.CodeBuild.BatchGetBuildBatches

    -- ** RetryBuild 
    , module Network.AWS.CodeBuild.RetryBuild

    -- ** ListBuildsForProject (Paginated)
    , module Network.AWS.CodeBuild.ListBuildsForProject

    -- ** DescribeTestCases (Paginated)
    , module Network.AWS.CodeBuild.DescribeTestCases

    -- ** GetResourcePolicy 
    , module Network.AWS.CodeBuild.GetResourcePolicy

    -- ** BatchGetProjects 
    , module Network.AWS.CodeBuild.BatchGetProjects

    -- ** BatchGetBuilds 
    , module Network.AWS.CodeBuild.BatchGetBuilds

    -- ** BatchGetReports 
    , module Network.AWS.CodeBuild.BatchGetReports

    -- ** UpdateWebhook 
    , module Network.AWS.CodeBuild.UpdateWebhook

    -- ** DeleteWebhook 
    , module Network.AWS.CodeBuild.DeleteWebhook

    -- ** StartBuildBatch 
    , module Network.AWS.CodeBuild.StartBuildBatch

    -- ** RetryBuildBatch 
    , module Network.AWS.CodeBuild.RetryBuildBatch

    -- ** ListReportsForReportGroup (Paginated)
    , module Network.AWS.CodeBuild.ListReportsForReportGroup

    -- ** InvalidateProjectCache 
    , module Network.AWS.CodeBuild.InvalidateProjectCache

    -- ** UpdateReportGroup 
    , module Network.AWS.CodeBuild.UpdateReportGroup

    -- ** DeleteReportGroup 
    , module Network.AWS.CodeBuild.DeleteReportGroup

    -- ** BatchDeleteBuilds 
    , module Network.AWS.CodeBuild.BatchDeleteBuilds

    -- ** ListReportGroups (Paginated)
    , module Network.AWS.CodeBuild.ListReportGroups

    -- ** PutResourcePolicy 
    , module Network.AWS.CodeBuild.PutResourcePolicy

    -- ** DeleteResourcePolicy 
    , module Network.AWS.CodeBuild.DeleteResourcePolicy

    -- ** ListCuratedEnvironmentImages 
    , module Network.AWS.CodeBuild.ListCuratedEnvironmentImages

    -- ** GetReportGroupTrend 
    , module Network.AWS.CodeBuild.GetReportGroupTrend

    -- ** StopBuild 
    , module Network.AWS.CodeBuild.StopBuild

    -- ** ListBuildBatches (Paginated)
    , module Network.AWS.CodeBuild.ListBuildBatches

    -- ** CreateProject 
    , module Network.AWS.CodeBuild.CreateProject

    -- ** ListSharedReportGroups (Paginated)
    , module Network.AWS.CodeBuild.ListSharedReportGroups

    -- * Types

    -- ** ProjectSortByType
    , ProjectSortByType (..)

    -- ** CacheType
    , CacheType (..)

    -- ** TestReportSummary
    , TestReportSummary (..)
    , mkTestReportSummary
    , trsTotal
    , trsStatusCounts
    , trsDurationInNanoSeconds

    -- ** SortOrderType
    , SortOrderType (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** TestCase
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

    -- ** LanguageType
    , LanguageType (..)

    -- ** ReportGroupTrendFieldType
    , ReportGroupTrendFieldType (..)

    -- ** ImagePullCredentialsType
    , ImagePullCredentialsType (..)

    -- ** EnvironmentPlatform
    , EnvironmentPlatform (..)
    , mkEnvironmentPlatform
    , epLanguages
    , epPlatform

    -- ** RetryBuildBatchType
    , RetryBuildBatchType (..)

    -- ** CredentialProviderType
    , CredentialProviderType (..)

    -- ** BuildBatchFilter
    , BuildBatchFilter (..)
    , mkBuildBatchFilter
    , bbfStatus

    -- ** ReportStatusType
    , ReportStatusType (..)

    -- ** ProjectSourceVersion
    , ProjectSourceVersion (..)
    , mkProjectSourceVersion
    , psvSourceIdentifier
    , psvSourceVersion

    -- ** S3LogsConfig
    , S3LogsConfig (..)
    , mkS3LogsConfig
    , slcStatus
    , slcEncryptionDisabled
    , slcLocation

    -- ** BuildBatchPhaseType
    , BuildBatchPhaseType (..)

    -- ** SourceAuthType
    , SourceAuthType (..)

    -- ** CloudWatchLogsConfig
    , CloudWatchLogsConfig (..)
    , mkCloudWatchLogsConfig
    , cwlcStatus
    , cwlcGroupName
    , cwlcStreamName

    -- ** ReportGroupStatusType
    , ReportGroupStatusType (..)

    -- ** SourceType
    , SourceType (..)

    -- ** WebhookBuildType
    , WebhookBuildType (..)

    -- ** Project
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

    -- ** ServerType
    , ServerType (..)

    -- ** S3ReportExportConfig
    , S3ReportExportConfig (..)
    , mkS3ReportExportConfig
    , srecBucket
    , srecEncryptionDisabled
    , srecEncryptionKey
    , srecPackaging
    , srecPath

    -- ** ProjectBadge
    , ProjectBadge (..)
    , mkProjectBadge
    , pbBadgeEnabled
    , pbBadgeRequestUrl

    -- ** ReportExportConfigType
    , ReportExportConfigType (..)

    -- ** NetworkInterface
    , NetworkInterface (..)
    , mkNetworkInterface
    , niNetworkInterfaceId
    , niSubnetId

    -- ** WebhookFilter
    , WebhookFilter (..)
    , mkWebhookFilter
    , wfType
    , wfPattern
    , wfExcludeMatchedPattern

    -- ** EnvironmentVariableType
    , EnvironmentVariableType (..)

    -- ** SourceCredentialsInfo
    , SourceCredentialsInfo (..)
    , mkSourceCredentialsInfo
    , sciArn
    , sciAuthType
    , sciServerType

    -- ** ProjectFileSystemLocation
    , ProjectFileSystemLocation (..)
    , mkProjectFileSystemLocation
    , pfslIdentifier
    , pfslLocation
    , pfslMountOptions
    , pfslMountPoint
    , pfslType

    -- ** CodeCoverage
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

    -- ** ExportedEnvironmentVariable
    , ExportedEnvironmentVariable (..)
    , mkExportedEnvironmentVariable
    , eevName
    , eevValue

    -- ** Build
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

    -- ** Report
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

    -- ** RegistryCredential
    , RegistryCredential (..)
    , mkRegistryCredential
    , rcCredential
    , rcCredentialProvider

    -- ** ResolvedArtifact
    , ResolvedArtifact (..)
    , mkResolvedArtifact
    , raIdentifier
    , raLocation
    , raType

    -- ** BuildNotDeleted
    , BuildNotDeleted (..)
    , mkBuildNotDeleted
    , bndId
    , bndStatusCode

    -- ** LogsConfigStatusType
    , LogsConfigStatusType (..)

    -- ** CodeCoverageReportSummary
    , CodeCoverageReportSummary (..)
    , mkCodeCoverageReportSummary
    , ccrsBranchCoveragePercentage
    , ccrsBranchesCovered
    , ccrsBranchesMissed
    , ccrsLineCoveragePercentage
    , ccrsLinesCovered
    , ccrsLinesMissed

    -- ** BuildPhaseType
    , BuildPhaseType (..)

    -- ** ReportPackagingType
    , ReportPackagingType (..)

    -- ** ProjectCache
    , ProjectCache (..)
    , mkProjectCache
    , pcType
    , pcLocation
    , pcModes

    -- ** ReportCodeCoverageSortByType
    , ReportCodeCoverageSortByType (..)

    -- ** StatusType
    , StatusType (..)

    -- ** NonEmptyString
    , NonEmptyString (..)

    -- ** VpcConfig
    , VpcConfig (..)
    , mkVpcConfig
    , vcSecurityGroupIds
    , vcSubnets
    , vcVpcId

    -- ** DebugSession
    , DebugSession (..)
    , mkDebugSession
    , dsSessionEnabled
    , dsSessionTarget

    -- ** CacheMode
    , CacheMode (..)

    -- ** PlatformType
    , PlatformType (..)

    -- ** EnvironmentLanguage
    , EnvironmentLanguage (..)
    , mkEnvironmentLanguage
    , elImages
    , elLanguage

    -- ** EnvironmentImage
    , EnvironmentImage (..)
    , mkEnvironmentImage
    , eiDescription
    , eiName
    , eiVersions

    -- ** LogsLocation
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

    -- ** BuildSummary
    , BuildSummary (..)
    , mkBuildSummary
    , bsArn
    , bsBuildStatus
    , bsPrimaryArtifact
    , bsRequestedOn
    , bsSecondaryArtifacts

    -- ** BuildBatch
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

    -- ** TestCaseFilter
    , TestCaseFilter (..)
    , mkTestCaseFilter
    , tcfKeyword
    , tcfStatus

    -- ** SharedResourceSortByType
    , SharedResourceSortByType (..)

    -- ** ProjectName
    , ProjectName (..)

    -- ** ArtifactNamespace
    , ArtifactNamespace (..)

    -- ** EnvironmentType
    , EnvironmentType (..)

    -- ** ArtifactsType
    , ArtifactsType (..)

    -- ** GitSubmodulesConfig
    , GitSubmodulesConfig (..)
    , mkGitSubmodulesConfig
    , gscFetchSubmodules

    -- ** LogsConfig
    , LogsConfig (..)
    , mkLogsConfig
    , lcCloudWatchLogs
    , lcS3Logs

    -- ** ComputeType
    , ComputeType (..)

    -- ** ProjectBuildBatchConfig
    , ProjectBuildBatchConfig (..)
    , mkProjectBuildBatchConfig
    , pbbcCombineArtifacts
    , pbbcRestrictions
    , pbbcServiceRole
    , pbbcTimeoutInMins

    -- ** SourceAuth
    , SourceAuth (..)
    , mkSourceAuth
    , saType
    , saResource

    -- ** BuildBatchPhase
    , BuildBatchPhase (..)
    , mkBuildBatchPhase
    , bbpContexts
    , bbpDurationInSeconds
    , bbpEndTime
    , bbpPhaseStatus
    , bbpPhaseType
    , bbpStartTime

    -- ** ProjectEnvironment
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

    -- ** ReportGroupSortByType
    , ReportGroupSortByType (..)

    -- ** ReportWithRawData
    , ReportWithRawData (..)
    , mkReportWithRawData
    , rwrdData
    , rwrdReportArn

    -- ** BuildStatusConfig
    , BuildStatusConfig (..)
    , mkBuildStatusConfig
    , bscContext
    , bscTargetUrl

    -- ** ProjectSource
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

    -- ** ReportExportConfig
    , ReportExportConfig (..)
    , mkReportExportConfig
    , recExportConfigType
    , recS3Destination

    -- ** PhaseContext
    , PhaseContext (..)
    , mkPhaseContext
    , pcMessage
    , pcStatusCode

    -- ** AuthType
    , AuthType (..)

    -- ** ProjectArtifacts
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

    -- ** Webhook
    , Webhook (..)
    , mkWebhook
    , wBranchFilter
    , wBuildType
    , wFilterGroups
    , wLastModifiedSecret
    , wPayloadUrl
    , wSecret
    , wUrl

    -- ** BatchRestrictions
    , BatchRestrictions (..)
    , mkBatchRestrictions
    , brComputeTypesAllowed
    , brMaximumBuildsAllowed

    -- ** WebhookFilterType
    , WebhookFilterType (..)

    -- ** EnvironmentVariable
    , EnvironmentVariable (..)
    , mkEnvironmentVariable
    , evName
    , evValue
    , evType

    -- ** BuildArtifacts
    , BuildArtifacts (..)
    , mkBuildArtifacts
    , baArtifactIdentifier
    , baEncryptionDisabled
    , baLocation
    , baMd5sum
    , baOverrideArtifactName
    , baSha256sum

    -- ** FileSystemType
    , FileSystemType (..)

    -- ** ReportType
    , ReportType (..)

    -- ** ReportGroupName
    , ReportGroupName (..)

    -- ** ReportFilter
    , ReportFilter (..)
    , mkReportFilter
    , rfStatus

    -- ** ArtifactPackaging
    , ArtifactPackaging (..)

    -- ** BuildGroup
    , BuildGroup (..)
    , mkBuildGroup
    , bgCurrentBuildSummary
    , bgDependsOn
    , bgIdentifier
    , bgIgnoreFailure
    , bgPriorBuildSummaryList

    -- ** ReportGroupTrendStats
    , ReportGroupTrendStats (..)
    , mkReportGroupTrendStats
    , rgtsAverage
    , rgtsMax
    , rgtsMin

    -- ** BuildPhase
    , BuildPhase (..)
    , mkBuildPhase
    , bpContexts
    , bpDurationInSeconds
    , bpEndTime
    , bpPhaseStatus
    , bpPhaseType
    , bpStartTime

    -- ** ReportGroup
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

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** EncryptionKeyOverride
    , EncryptionKeyOverride (..)

    -- ** ImageOverride
    , ImageOverride (..)

    -- ** ServiceRoleOverride
    , ServiceRoleOverride (..)

    -- ** Id
    , Id (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** ReportArn
    , ReportArn (..)

    -- ** Description
    , Description (..)

    -- ** EncryptionKey
    , EncryptionKey (..)

    -- ** Name
    , Name (..)

    -- ** ServiceRole
    , ServiceRole (..)

    -- ** Bucket
    , Bucket (..)

    -- ** NetworkInterfaceId
    , NetworkInterfaceId (..)

    -- ** SubnetId
    , SubnetId (..)

    -- ** NextToken
    , NextToken (..)

    -- ** Arn
    , Arn (..)

    -- ** Policy
    , Policy (..)

    -- ** FilePath
    , FilePath (..)

    -- ** ReportARN
    , ReportARN (..)

    -- ** Token
    , Token (..)

    -- ** Username
    , Username (..)

    -- ** ResolvedSourceVersion
    , ResolvedSourceVersion (..)

    -- ** SourceVersion
    , SourceVersion (..)

    -- ** ReportGroupArn
    , ReportGroupArn (..)

    -- ** Credential
    , Credential (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Waiters
import Network.AWS.CodeBuild.ListProjects
import Network.AWS.CodeBuild.DeleteProject
import Network.AWS.CodeBuild.UpdateProject
import Network.AWS.CodeBuild.DeleteSourceCredentials
import Network.AWS.CodeBuild.ListBuilds
import Network.AWS.CodeBuild.ListSourceCredentials
import Network.AWS.CodeBuild.ListReports
import Network.AWS.CodeBuild.DeleteReport
import Network.AWS.CodeBuild.CreateWebhook
import Network.AWS.CodeBuild.StopBuildBatch
import Network.AWS.CodeBuild.ListSharedProjects
import Network.AWS.CodeBuild.CreateReportGroup
import Network.AWS.CodeBuild.DescribeCodeCoverages
import Network.AWS.CodeBuild.ImportSourceCredentials
import Network.AWS.CodeBuild.ListBuildBatchesForProject
import Network.AWS.CodeBuild.BatchGetReportGroups
import Network.AWS.CodeBuild.DeleteBuildBatch
import Network.AWS.CodeBuild.StartBuild
import Network.AWS.CodeBuild.BatchGetBuildBatches
import Network.AWS.CodeBuild.RetryBuild
import Network.AWS.CodeBuild.ListBuildsForProject
import Network.AWS.CodeBuild.DescribeTestCases
import Network.AWS.CodeBuild.GetResourcePolicy
import Network.AWS.CodeBuild.BatchGetProjects
import Network.AWS.CodeBuild.BatchGetBuilds
import Network.AWS.CodeBuild.BatchGetReports
import Network.AWS.CodeBuild.UpdateWebhook
import Network.AWS.CodeBuild.DeleteWebhook
import Network.AWS.CodeBuild.StartBuildBatch
import Network.AWS.CodeBuild.RetryBuildBatch
import Network.AWS.CodeBuild.ListReportsForReportGroup
import Network.AWS.CodeBuild.InvalidateProjectCache
import Network.AWS.CodeBuild.UpdateReportGroup
import Network.AWS.CodeBuild.DeleteReportGroup
import Network.AWS.CodeBuild.BatchDeleteBuilds
import Network.AWS.CodeBuild.ListReportGroups
import Network.AWS.CodeBuild.PutResourcePolicy
import Network.AWS.CodeBuild.DeleteResourcePolicy
import Network.AWS.CodeBuild.ListCuratedEnvironmentImages
import Network.AWS.CodeBuild.GetReportGroupTrend
import Network.AWS.CodeBuild.StopBuild
import Network.AWS.CodeBuild.ListBuildBatches
import Network.AWS.CodeBuild.CreateProject
import Network.AWS.CodeBuild.ListSharedReportGroups
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CodeBuild'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
