{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Lens
  ( -- * Operations

    -- ** GetApplicationPolicy
    getApplicationPolicy_applicationId,
    getApplicationPolicyResponse_statements,
    getApplicationPolicyResponse_httpStatus,

    -- ** CreateApplicationVersion
    createApplicationVersion_templateUrl,
    createApplicationVersion_sourceCodeArchiveUrl,
    createApplicationVersion_sourceCodeUrl,
    createApplicationVersion_templateBody,
    createApplicationVersion_applicationId,
    createApplicationVersion_semanticVersion,
    createApplicationVersionResponse_applicationId,
    createApplicationVersionResponse_requiredCapabilities,
    createApplicationVersionResponse_resourcesSupported,
    createApplicationVersionResponse_parameterDefinitions,
    createApplicationVersionResponse_creationTime,
    createApplicationVersionResponse_templateUrl,
    createApplicationVersionResponse_sourceCodeArchiveUrl,
    createApplicationVersionResponse_sourceCodeUrl,
    createApplicationVersionResponse_semanticVersion,
    createApplicationVersionResponse_httpStatus,

    -- ** GetCloudFormationTemplate
    getCloudFormationTemplate_applicationId,
    getCloudFormationTemplate_templateId,
    getCloudFormationTemplateResponse_applicationId,
    getCloudFormationTemplateResponse_creationTime,
    getCloudFormationTemplateResponse_status,
    getCloudFormationTemplateResponse_expirationTime,
    getCloudFormationTemplateResponse_templateUrl,
    getCloudFormationTemplateResponse_semanticVersion,
    getCloudFormationTemplateResponse_templateId,
    getCloudFormationTemplateResponse_httpStatus,

    -- ** UnshareApplication
    unshareApplication_applicationId,
    unshareApplication_organizationId,

    -- ** CreateApplication
    createApplication_spdxLicenseId,
    createApplication_licenseBody,
    createApplication_templateUrl,
    createApplication_licenseUrl,
    createApplication_labels,
    createApplication_readmeBody,
    createApplication_homePageUrl,
    createApplication_sourceCodeArchiveUrl,
    createApplication_readmeUrl,
    createApplication_sourceCodeUrl,
    createApplication_templateBody,
    createApplication_semanticVersion,
    createApplication_description,
    createApplication_name,
    createApplication_author,
    createApplicationResponse_applicationId,
    createApplicationResponse_creationTime,
    createApplicationResponse_spdxLicenseId,
    createApplicationResponse_licenseUrl,
    createApplicationResponse_verifiedAuthorUrl,
    createApplicationResponse_labels,
    createApplicationResponse_author,
    createApplicationResponse_version,
    createApplicationResponse_name,
    createApplicationResponse_homePageUrl,
    createApplicationResponse_readmeUrl,
    createApplicationResponse_isVerifiedAuthor,
    createApplicationResponse_description,
    createApplicationResponse_httpStatus,

    -- ** CreateCloudFormationChangeSet
    createCloudFormationChangeSet_resourceTypes,
    createCloudFormationChangeSet_capabilities,
    createCloudFormationChangeSet_parameterOverrides,
    createCloudFormationChangeSet_notificationArns,
    createCloudFormationChangeSet_tags,
    createCloudFormationChangeSet_rollbackConfiguration,
    createCloudFormationChangeSet_description,
    createCloudFormationChangeSet_changeSetName,
    createCloudFormationChangeSet_semanticVersion,
    createCloudFormationChangeSet_templateId,
    createCloudFormationChangeSet_clientToken,
    createCloudFormationChangeSet_applicationId,
    createCloudFormationChangeSet_stackName,
    createCloudFormationChangeSetResponse_applicationId,
    createCloudFormationChangeSetResponse_stackId,
    createCloudFormationChangeSetResponse_changeSetId,
    createCloudFormationChangeSetResponse_semanticVersion,
    createCloudFormationChangeSetResponse_httpStatus,

    -- ** GetApplication
    getApplication_semanticVersion,
    getApplication_applicationId,
    getApplicationResponse_applicationId,
    getApplicationResponse_creationTime,
    getApplicationResponse_spdxLicenseId,
    getApplicationResponse_licenseUrl,
    getApplicationResponse_verifiedAuthorUrl,
    getApplicationResponse_labels,
    getApplicationResponse_author,
    getApplicationResponse_version,
    getApplicationResponse_name,
    getApplicationResponse_homePageUrl,
    getApplicationResponse_readmeUrl,
    getApplicationResponse_isVerifiedAuthor,
    getApplicationResponse_description,
    getApplicationResponse_httpStatus,

    -- ** ListApplicationDependencies
    listApplicationDependencies_nextToken,
    listApplicationDependencies_semanticVersion,
    listApplicationDependencies_maxItems,
    listApplicationDependencies_applicationId,
    listApplicationDependenciesResponse_nextToken,
    listApplicationDependenciesResponse_dependencies,
    listApplicationDependenciesResponse_httpStatus,

    -- ** ListApplicationVersions
    listApplicationVersions_nextToken,
    listApplicationVersions_maxItems,
    listApplicationVersions_applicationId,
    listApplicationVersionsResponse_nextToken,
    listApplicationVersionsResponse_versions,
    listApplicationVersionsResponse_httpStatus,

    -- ** CreateCloudFormationTemplate
    createCloudFormationTemplate_semanticVersion,
    createCloudFormationTemplate_applicationId,
    createCloudFormationTemplateResponse_applicationId,
    createCloudFormationTemplateResponse_creationTime,
    createCloudFormationTemplateResponse_status,
    createCloudFormationTemplateResponse_expirationTime,
    createCloudFormationTemplateResponse_templateUrl,
    createCloudFormationTemplateResponse_semanticVersion,
    createCloudFormationTemplateResponse_templateId,
    createCloudFormationTemplateResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxItems,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_applicationId,

    -- ** UpdateApplication
    updateApplication_labels,
    updateApplication_author,
    updateApplication_readmeBody,
    updateApplication_homePageUrl,
    updateApplication_readmeUrl,
    updateApplication_description,
    updateApplication_applicationId,
    updateApplicationResponse_applicationId,
    updateApplicationResponse_creationTime,
    updateApplicationResponse_spdxLicenseId,
    updateApplicationResponse_licenseUrl,
    updateApplicationResponse_verifiedAuthorUrl,
    updateApplicationResponse_labels,
    updateApplicationResponse_author,
    updateApplicationResponse_version,
    updateApplicationResponse_name,
    updateApplicationResponse_homePageUrl,
    updateApplicationResponse_readmeUrl,
    updateApplicationResponse_isVerifiedAuthor,
    updateApplicationResponse_description,
    updateApplicationResponse_httpStatus,

    -- ** PutApplicationPolicy
    putApplicationPolicy_applicationId,
    putApplicationPolicy_statements,
    putApplicationPolicyResponse_statements,
    putApplicationPolicyResponse_httpStatus,

    -- * Types

    -- ** ApplicationDependencySummary
    applicationDependencySummary_applicationId,
    applicationDependencySummary_semanticVersion,

    -- ** ApplicationPolicyStatement
    applicationPolicyStatement_statementId,
    applicationPolicyStatement_principalOrgIDs,
    applicationPolicyStatement_principals,
    applicationPolicyStatement_actions,

    -- ** ApplicationSummary
    applicationSummary_creationTime,
    applicationSummary_spdxLicenseId,
    applicationSummary_labels,
    applicationSummary_homePageUrl,
    applicationSummary_description,
    applicationSummary_author,
    applicationSummary_applicationId,
    applicationSummary_name,

    -- ** ParameterDefinition
    parameterDefinition_maxValue,
    parameterDefinition_minLength,
    parameterDefinition_allowedValues,
    parameterDefinition_minValue,
    parameterDefinition_description,
    parameterDefinition_constraintDescription,
    parameterDefinition_maxLength,
    parameterDefinition_defaultValue,
    parameterDefinition_allowedPattern,
    parameterDefinition_type,
    parameterDefinition_noEcho,
    parameterDefinition_referencedByResources,
    parameterDefinition_name,

    -- ** ParameterValue
    parameterValue_value,
    parameterValue_name,

    -- ** RollbackConfiguration
    rollbackConfiguration_monitoringTimeInMinutes,
    rollbackConfiguration_rollbackTriggers,

    -- ** RollbackTrigger
    rollbackTrigger_type,
    rollbackTrigger_arn,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** Version
    version_sourceCodeArchiveUrl,
    version_sourceCodeUrl,
    version_templateUrl,
    version_parameterDefinitions,
    version_resourcesSupported,
    version_creationTime,
    version_requiredCapabilities,
    version_applicationId,
    version_semanticVersion,

    -- ** VersionSummary
    versionSummary_sourceCodeUrl,
    versionSummary_creationTime,
    versionSummary_applicationId,
    versionSummary_semanticVersion,
  )
where

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
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary
import Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
import Network.AWS.ServerlessApplicationRepository.Types.ParameterValue
import Network.AWS.ServerlessApplicationRepository.Types.RollbackConfiguration
import Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger
import Network.AWS.ServerlessApplicationRepository.Types.Tag
import Network.AWS.ServerlessApplicationRepository.Types.Version
import Network.AWS.ServerlessApplicationRepository.Types.VersionSummary
import Network.AWS.ServerlessApplicationRepository.UnshareApplication
import Network.AWS.ServerlessApplicationRepository.UpdateApplication
