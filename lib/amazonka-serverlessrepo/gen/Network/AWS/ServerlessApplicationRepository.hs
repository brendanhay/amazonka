{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The AWS Serverless Application Repository makes it easy for developers and enterprises to quickly find
--
--  and deploy serverless applications in the AWS Cloud. For more information about serverless applications,
--  see Serverless Computing and Applications on the AWS website.
-- The AWS Serverless Application Repository is deeply integrated with the AWS Lambda console, so that developers of
--  all levels can get started with serverless computing without needing to learn anything new. You can use category
--  keywords to browse for applications such as web and mobile backends, data processing applications, or chatbots.
--  You can also search for applications by name, publisher, or event source. To use an application, you simply choose it,
--  configure any required fields, and deploy it with a few clicks.
-- You can also easily publish applications, sharing them publicly with the community at large, or privately
--  within your team or across your organization. To publish a serverless application (or app), you can use the
--  AWS Management Console, AWS Command Line Interface (AWS CLI), or AWS SDKs to upload the code. Along with the
--  code, you upload a simple manifest file, also known as the AWS Serverless Application Model (AWS SAM) template.
--  For more information about AWS SAM, see AWS Serverless Application Model (AWS SAM) on the AWS Labs
--  GitHub repository.
-- The AWS Serverless Application Repository Developer Guide contains more information about the two developer
--  experiences available:
--
--     * Consuming Applications – Browse for applications and view information about them, including
--  source code and readme files. Also install, configure, and deploy applications of your choosing.
-- Publishing Applications – Configure and upload applications to make them available to other
--  developers, and publish new versions of applications.
module Network.AWS.ServerlessApplicationRepository
  ( -- * Service configuration
    serverlessApplicationRepositoryService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetApplicationPolicy
    module Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy,

    -- ** CreateApplicationVersion
    module Network.AWS.ServerlessApplicationRepository.CreateApplicationVersion,

    -- ** UnshareApplication
    module Network.AWS.ServerlessApplicationRepository.UnshareApplication,

    -- ** DeleteApplication
    module Network.AWS.ServerlessApplicationRepository.DeleteApplication,

    -- ** UpdateApplication
    module Network.AWS.ServerlessApplicationRepository.UpdateApplication,

    -- ** CreateCloudFormationTemplate
    module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate,

    -- ** CreateApplication
    module Network.AWS.ServerlessApplicationRepository.CreateApplication,

    -- ** ListApplicationDependencies (Paginated)
    module Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies,

    -- ** ListApplicationVersions (Paginated)
    module Network.AWS.ServerlessApplicationRepository.ListApplicationVersions,

    -- ** GetApplication
    module Network.AWS.ServerlessApplicationRepository.GetApplication,

    -- ** GetCloudFormationTemplate
    module Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate,

    -- ** CreateCloudFormationChangeSet
    module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationChangeSet,

    -- ** PutApplicationPolicy
    module Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy,

    -- ** ListApplications (Paginated)
    module Network.AWS.ServerlessApplicationRepository.ListApplications,

    -- * Types

    -- ** Capability
    Capability (..),

    -- ** Status
    Status (..),

    -- ** ApplicationDependencySummary
    ApplicationDependencySummary (..),
    mkApplicationDependencySummary,
    adsApplicationId,
    adsSemanticVersion,

    -- ** ApplicationPolicyStatement
    ApplicationPolicyStatement (..),
    mkApplicationPolicyStatement,
    apsStatementId,
    apsPrincipalOrgIds,
    apsPrincipals,
    apsActions,

    -- ** ApplicationSummary
    ApplicationSummary (..),
    mkApplicationSummary,
    asCreationTime,
    asHomePageURL,
    asLabels,
    asSpdxLicenseId,
    asDescription,
    asAuthor,
    asApplicationId,
    asName,

    -- ** ParameterDefinition
    ParameterDefinition (..),
    mkParameterDefinition,
    pdMaxValue,
    pdMaxLength,
    pdConstraintDescription,
    pdMinLength,
    pdDefaultValue,
    pdAllowedPattern,
    pdNoEcho,
    pdType,
    pdAllowedValues,
    pdDescription,
    pdMinValue,
    pdReferencedByResources,
    pdName,

    -- ** ParameterValue
    ParameterValue (..),
    mkParameterValue,
    pvValue,
    pvName,

    -- ** RollbackConfiguration
    RollbackConfiguration (..),
    mkRollbackConfiguration,
    rcRollbackTriggers,
    rcMonitoringTimeInMinutes,

    -- ** RollbackTrigger
    RollbackTrigger (..),
    mkRollbackTrigger,
    rtType,
    rtARN,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** Version
    Version (..),
    mkVersion,
    vSourceCodeURL,
    vSourceCodeArchiveURL,
    vTemplateURL,
    vParameterDefinitions,
    vResourcesSupported,
    vCreationTime,
    vRequiredCapabilities,
    vApplicationId,
    vSemanticVersion,

    -- ** VersionSummary
    VersionSummary (..),
    mkVersionSummary,
    vsSourceCodeURL,
    vsCreationTime,
    vsApplicationId,
    vsSemanticVersion,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServerlessApplicationRepository.CreateApplication
import Network.AWS.ServerlessApplicationRepository.CreateApplicationVersion
import Network.AWS.ServerlessApplicationRepository.CreateCloudFormationChangeSet
import Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate
import Network.AWS.ServerlessApplicationRepository.DeleteApplication
import Network.AWS.ServerlessApplicationRepository.GetApplication
import Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
import Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate
import Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies
import Network.AWS.ServerlessApplicationRepository.ListApplicationVersions
import Network.AWS.ServerlessApplicationRepository.ListApplications
import Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.UnshareApplication
import Network.AWS.ServerlessApplicationRepository.UpdateApplication
import Network.AWS.ServerlessApplicationRepository.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ServerlessApplicationRepository'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
