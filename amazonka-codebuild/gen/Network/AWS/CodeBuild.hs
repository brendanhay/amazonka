{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodeBuild__
--
-- AWS CodeBuild is a fully managed build service in the cloud. AWS CodeBuild compiles your source code, runs unit tests, and produces artifacts that are ready to deploy. AWS CodeBuild eliminates the need to provision, manage, and scale your own build servers. It provides prepackaged build environments for the most popular programming languages and build tools, such as Apache Maven, Gradle, and more. You can also fully customize build environments in AWS CodeBuild to use your own build tools. AWS CodeBuild scales automatically to meet peak build requests. You pay only for the build time you consume. For more information about AWS CodeBuild, see the /AWS CodeBuild User Guide/ .
--
-- AWS CodeBuild supports these operations:
--
--     * @BatchDeleteBuilds@ : Deletes one or more builds.
--
--     * @BatchGetProjects@ : Gets information about one or more build projects. A /build project/ defines how AWS CodeBuild runs a build. This includes information such as where to get the source code to build, the build environment to use, the build commands to run, and where to store the build output. A /build environment/ is a representation of operating system, programming language runtime, and tools that AWS CodeBuild uses to run a build. You can add tags to build projects to help manage your resources and costs.
--
--     * @CreateProject@ : Creates a build project.
--
--     * @CreateWebhook@ : For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, enables AWS CodeBuild to start rebuilding the source code every time a code change is pushed to the repository.
--
--     * @UpdateWebhook@ : Changes the settings of an existing webhook.
--
--     * @DeleteProject@ : Deletes a build project.
--
--     * @DeleteWebhook@ : For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, stops AWS CodeBuild from rebuilding the source code every time a code change is pushed to the repository.
--
--     * @ListProjects@ : Gets a list of build project names, with each build project name representing a single build project.
--
--     * @UpdateProject@ : Changes the settings of an existing build project.
--
--     * @BatchGetBuilds@ : Gets information about one or more builds.
--
--     * @ListBuilds@ : Gets a list of build IDs, with each build ID representing a single build.
--
--     * @ListBuildsForProject@ : Gets a list of build IDs for the specified build project, with each build ID representing a single build.
--
--     * @StartBuild@ : Starts running a build.
--
--     * @StopBuild@ : Attempts to stop running a build.
--
--     * @ListCuratedEnvironmentImages@ : Gets information about Docker images that are managed by AWS CodeBuild.
--
--     * @DeleteSourceCredentials@ : Deletes a set of GitHub, GitHub Enterprise, or Bitbucket source credentials.
--
--     * @ImportSourceCredentials@ : Imports the source repository credentials for an AWS CodeBuild project that has its source code stored in a GitHub, GitHub Enterprise, or Bitbucket repository.
--
--     * @ListSourceCredentials@ : Returns a list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@ object includes the authentication type, token ARN, and type of source provider for one set of credentials.
--
--
--
module Network.AWS.CodeBuild
    (
    -- * Service Configuration
      codeBuild

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

    -- ** CreateWebhook
    , module Network.AWS.CodeBuild.CreateWebhook

    -- ** ImportSourceCredentials
    , module Network.AWS.CodeBuild.ImportSourceCredentials

    -- ** StartBuild
    , module Network.AWS.CodeBuild.StartBuild

    -- ** ListBuildsForProject (Paginated)
    , module Network.AWS.CodeBuild.ListBuildsForProject

    -- ** BatchGetProjects
    , module Network.AWS.CodeBuild.BatchGetProjects

    -- ** BatchGetBuilds
    , module Network.AWS.CodeBuild.BatchGetBuilds

    -- ** UpdateWebhook
    , module Network.AWS.CodeBuild.UpdateWebhook

    -- ** DeleteWebhook
    , module Network.AWS.CodeBuild.DeleteWebhook

    -- ** InvalidateProjectCache
    , module Network.AWS.CodeBuild.InvalidateProjectCache

    -- ** BatchDeleteBuilds
    , module Network.AWS.CodeBuild.BatchDeleteBuilds

    -- ** ListCuratedEnvironmentImages
    , module Network.AWS.CodeBuild.ListCuratedEnvironmentImages

    -- ** StopBuild
    , module Network.AWS.CodeBuild.StopBuild

    -- ** CreateProject
    , module Network.AWS.CodeBuild.CreateProject

    -- * Types

    -- ** ArtifactNamespace
    , ArtifactNamespace (..)

    -- ** ArtifactPackaging
    , ArtifactPackaging (..)

    -- ** ArtifactsType
    , ArtifactsType (..)

    -- ** AuthType
    , AuthType (..)

    -- ** BuildPhaseType
    , BuildPhaseType (..)

    -- ** CacheMode
    , CacheMode (..)

    -- ** CacheType
    , CacheType (..)

    -- ** ComputeType
    , ComputeType (..)

    -- ** CredentialProviderType
    , CredentialProviderType (..)

    -- ** EnvironmentType
    , EnvironmentType (..)

    -- ** EnvironmentVariableType
    , EnvironmentVariableType (..)

    -- ** ImagePullCredentialsType
    , ImagePullCredentialsType (..)

    -- ** LanguageType
    , LanguageType (..)

    -- ** LogsConfigStatusType
    , LogsConfigStatusType (..)

    -- ** PlatformType
    , PlatformType (..)

    -- ** ProjectSortByType
    , ProjectSortByType (..)

    -- ** ServerType
    , ServerType (..)

    -- ** SortOrderType
    , SortOrderType (..)

    -- ** SourceAuthType
    , SourceAuthType (..)

    -- ** SourceType
    , SourceType (..)

    -- ** StatusType
    , StatusType (..)

    -- ** WebhookFilterType
    , WebhookFilterType (..)

    -- ** Build
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

    -- ** BuildArtifacts
    , BuildArtifacts
    , buildArtifacts
    , baLocation
    , baMd5sum
    , baEncryptionDisabled
    , baOverrideArtifactName
    , baArtifactIdentifier
    , baSha256sum

    -- ** BuildNotDeleted
    , BuildNotDeleted
    , buildNotDeleted
    , bndId
    , bndStatusCode

    -- ** BuildPhase
    , BuildPhase
    , buildPhase
    , bpContexts
    , bpStartTime
    , bpPhaseStatus
    , bpPhaseType
    , bpEndTime
    , bpDurationInSeconds

    -- ** CloudWatchLogsConfig
    , CloudWatchLogsConfig
    , cloudWatchLogsConfig
    , cwlcGroupName
    , cwlcStreamName
    , cwlcStatus

    -- ** EnvironmentImage
    , EnvironmentImage
    , environmentImage
    , eiVersions
    , eiName
    , eiDescription

    -- ** EnvironmentLanguage
    , EnvironmentLanguage
    , environmentLanguage
    , elImages
    , elLanguage

    -- ** EnvironmentPlatform
    , EnvironmentPlatform
    , environmentPlatform
    , epPlatform
    , epLanguages

    -- ** EnvironmentVariable
    , EnvironmentVariable
    , environmentVariable
    , evType
    , evName
    , evValue

    -- ** GitSubmodulesConfig
    , GitSubmodulesConfig
    , gitSubmodulesConfig
    , gscFetchSubmodules

    -- ** LogsConfig
    , LogsConfig
    , logsConfig
    , lcS3Logs
    , lcCloudWatchLogs

    -- ** LogsLocation
    , LogsLocation
    , logsLocation
    , llDeepLink
    , llS3Logs
    , llCloudWatchLogs
    , llS3DeepLink
    , llGroupName
    , llStreamName

    -- ** NetworkInterface
    , NetworkInterface
    , networkInterface
    , niSubnetId
    , niNetworkInterfaceId

    -- ** PhaseContext
    , PhaseContext
    , phaseContext
    , pcMessage
    , pcStatusCode

    -- ** Project
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

    -- ** ProjectArtifacts
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

    -- ** ProjectBadge
    , ProjectBadge
    , projectBadge
    , pbBadgeEnabled
    , pbBadgeRequestURL

    -- ** ProjectCache
    , ProjectCache
    , projectCache
    , pcLocation
    , pcModes
    , pcType

    -- ** ProjectEnvironment
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

    -- ** ProjectSource
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

    -- ** ProjectSourceVersion
    , ProjectSourceVersion
    , projectSourceVersion
    , psvSourceIdentifier
    , psvSourceVersion

    -- ** RegistryCredential
    , RegistryCredential
    , registryCredential
    , rcCredential
    , rcCredentialProvider

    -- ** S3LogsConfig
    , S3LogsConfig
    , s3LogsConfig
    , slcLocation
    , slcEncryptionDisabled
    , slcStatus

    -- ** SourceAuth
    , SourceAuth
    , sourceAuth
    , saResource
    , saType

    -- ** SourceCredentialsInfo
    , SourceCredentialsInfo
    , sourceCredentialsInfo
    , sciArn
    , sciServerType
    , sciAuthType

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSecurityGroupIds
    , vcVpcId
    , vcSubnets

    -- ** Webhook
    , Webhook
    , webhook
    , wBranchFilter
    , wLastModifiedSecret
    , wUrl
    , wSecret
    , wFilterGroups
    , wPayloadURL

    -- ** WebhookFilter
    , WebhookFilter
    , webhookFilter
    , wfExcludeMatchedPattern
    , wfType
    , wfPattern
    ) where

import Network.AWS.CodeBuild.BatchDeleteBuilds
import Network.AWS.CodeBuild.BatchGetBuilds
import Network.AWS.CodeBuild.BatchGetProjects
import Network.AWS.CodeBuild.CreateProject
import Network.AWS.CodeBuild.CreateWebhook
import Network.AWS.CodeBuild.DeleteProject
import Network.AWS.CodeBuild.DeleteSourceCredentials
import Network.AWS.CodeBuild.DeleteWebhook
import Network.AWS.CodeBuild.ImportSourceCredentials
import Network.AWS.CodeBuild.InvalidateProjectCache
import Network.AWS.CodeBuild.ListBuilds
import Network.AWS.CodeBuild.ListBuildsForProject
import Network.AWS.CodeBuild.ListCuratedEnvironmentImages
import Network.AWS.CodeBuild.ListProjects
import Network.AWS.CodeBuild.ListSourceCredentials
import Network.AWS.CodeBuild.StartBuild
import Network.AWS.CodeBuild.StopBuild
import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.UpdateProject
import Network.AWS.CodeBuild.UpdateWebhook
import Network.AWS.CodeBuild.Waiters

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
