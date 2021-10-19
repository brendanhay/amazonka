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
    createApplicationVersion_sourceCodeUrl,
    createApplicationVersion_templateBody,
    createApplicationVersion_templateUrl,
    createApplicationVersion_sourceCodeArchiveUrl,
    createApplicationVersion_applicationId,
    createApplicationVersion_semanticVersion,
    createApplicationVersionResponse_creationTime,
    createApplicationVersionResponse_resourcesSupported,
    createApplicationVersionResponse_requiredCapabilities,
    createApplicationVersionResponse_parameterDefinitions,
    createApplicationVersionResponse_semanticVersion,
    createApplicationVersionResponse_sourceCodeUrl,
    createApplicationVersionResponse_applicationId,
    createApplicationVersionResponse_templateUrl,
    createApplicationVersionResponse_sourceCodeArchiveUrl,
    createApplicationVersionResponse_httpStatus,

    -- ** UnshareApplication
    unshareApplication_applicationId,
    unshareApplication_organizationId,

    -- ** DeleteApplication
    deleteApplication_applicationId,

    -- ** UpdateApplication
    updateApplication_homePageUrl,
    updateApplication_readmeBody,
    updateApplication_readmeUrl,
    updateApplication_author,
    updateApplication_labels,
    updateApplication_description,
    updateApplication_applicationId,
    updateApplicationResponse_creationTime,
    updateApplicationResponse_homePageUrl,
    updateApplicationResponse_licenseUrl,
    updateApplicationResponse_readmeUrl,
    updateApplicationResponse_applicationId,
    updateApplicationResponse_name,
    updateApplicationResponse_version,
    updateApplicationResponse_author,
    updateApplicationResponse_labels,
    updateApplicationResponse_verifiedAuthorUrl,
    updateApplicationResponse_description,
    updateApplicationResponse_spdxLicenseId,
    updateApplicationResponse_isVerifiedAuthor,
    updateApplicationResponse_httpStatus,

    -- ** CreateCloudFormationTemplate
    createCloudFormationTemplate_semanticVersion,
    createCloudFormationTemplate_applicationId,
    createCloudFormationTemplateResponse_creationTime,
    createCloudFormationTemplateResponse_status,
    createCloudFormationTemplateResponse_templateId,
    createCloudFormationTemplateResponse_semanticVersion,
    createCloudFormationTemplateResponse_applicationId,
    createCloudFormationTemplateResponse_templateUrl,
    createCloudFormationTemplateResponse_expirationTime,
    createCloudFormationTemplateResponse_httpStatus,

    -- ** CreateApplication
    createApplication_homePageUrl,
    createApplication_readmeBody,
    createApplication_licenseUrl,
    createApplication_semanticVersion,
    createApplication_sourceCodeUrl,
    createApplication_readmeUrl,
    createApplication_labels,
    createApplication_templateBody,
    createApplication_templateUrl,
    createApplication_licenseBody,
    createApplication_spdxLicenseId,
    createApplication_sourceCodeArchiveUrl,
    createApplication_description,
    createApplication_name,
    createApplication_author,
    createApplicationResponse_creationTime,
    createApplicationResponse_homePageUrl,
    createApplicationResponse_licenseUrl,
    createApplicationResponse_readmeUrl,
    createApplicationResponse_applicationId,
    createApplicationResponse_name,
    createApplicationResponse_version,
    createApplicationResponse_author,
    createApplicationResponse_labels,
    createApplicationResponse_verifiedAuthorUrl,
    createApplicationResponse_description,
    createApplicationResponse_spdxLicenseId,
    createApplicationResponse_isVerifiedAuthor,
    createApplicationResponse_httpStatus,

    -- ** ListApplicationDependencies
    listApplicationDependencies_semanticVersion,
    listApplicationDependencies_nextToken,
    listApplicationDependencies_maxItems,
    listApplicationDependencies_applicationId,
    listApplicationDependenciesResponse_dependencies,
    listApplicationDependenciesResponse_nextToken,
    listApplicationDependenciesResponse_httpStatus,

    -- ** ListApplicationVersions
    listApplicationVersions_nextToken,
    listApplicationVersions_maxItems,
    listApplicationVersions_applicationId,
    listApplicationVersionsResponse_versions,
    listApplicationVersionsResponse_nextToken,
    listApplicationVersionsResponse_httpStatus,

    -- ** GetApplication
    getApplication_semanticVersion,
    getApplication_applicationId,
    getApplicationResponse_creationTime,
    getApplicationResponse_homePageUrl,
    getApplicationResponse_licenseUrl,
    getApplicationResponse_readmeUrl,
    getApplicationResponse_applicationId,
    getApplicationResponse_name,
    getApplicationResponse_version,
    getApplicationResponse_author,
    getApplicationResponse_labels,
    getApplicationResponse_verifiedAuthorUrl,
    getApplicationResponse_description,
    getApplicationResponse_spdxLicenseId,
    getApplicationResponse_isVerifiedAuthor,
    getApplicationResponse_httpStatus,

    -- ** GetCloudFormationTemplate
    getCloudFormationTemplate_applicationId,
    getCloudFormationTemplate_templateId,
    getCloudFormationTemplateResponse_creationTime,
    getCloudFormationTemplateResponse_status,
    getCloudFormationTemplateResponse_templateId,
    getCloudFormationTemplateResponse_semanticVersion,
    getCloudFormationTemplateResponse_applicationId,
    getCloudFormationTemplateResponse_templateUrl,
    getCloudFormationTemplateResponse_expirationTime,
    getCloudFormationTemplateResponse_httpStatus,

    -- ** CreateCloudFormationChangeSet
    createCloudFormationChangeSet_clientToken,
    createCloudFormationChangeSet_templateId,
    createCloudFormationChangeSet_semanticVersion,
    createCloudFormationChangeSet_notificationArns,
    createCloudFormationChangeSet_changeSetName,
    createCloudFormationChangeSet_description,
    createCloudFormationChangeSet_capabilities,
    createCloudFormationChangeSet_parameterOverrides,
    createCloudFormationChangeSet_rollbackConfiguration,
    createCloudFormationChangeSet_resourceTypes,
    createCloudFormationChangeSet_tags,
    createCloudFormationChangeSet_applicationId,
    createCloudFormationChangeSet_stackName,
    createCloudFormationChangeSetResponse_semanticVersion,
    createCloudFormationChangeSetResponse_changeSetId,
    createCloudFormationChangeSetResponse_applicationId,
    createCloudFormationChangeSetResponse_stackId,
    createCloudFormationChangeSetResponse_httpStatus,

    -- ** PutApplicationPolicy
    putApplicationPolicy_applicationId,
    putApplicationPolicy_statements,
    putApplicationPolicyResponse_statements,
    putApplicationPolicyResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxItems,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,

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
    applicationSummary_homePageUrl,
    applicationSummary_labels,
    applicationSummary_spdxLicenseId,
    applicationSummary_description,
    applicationSummary_author,
    applicationSummary_applicationId,
    applicationSummary_name,

    -- ** ParameterDefinition
    parameterDefinition_maxValue,
    parameterDefinition_maxLength,
    parameterDefinition_constraintDescription,
    parameterDefinition_minLength,
    parameterDefinition_defaultValue,
    parameterDefinition_allowedPattern,
    parameterDefinition_noEcho,
    parameterDefinition_type,
    parameterDefinition_allowedValues,
    parameterDefinition_description,
    parameterDefinition_minValue,
    parameterDefinition_referencedByResources,
    parameterDefinition_name,

    -- ** ParameterValue
    parameterValue_value,
    parameterValue_name,

    -- ** RollbackConfiguration
    rollbackConfiguration_rollbackTriggers,
    rollbackConfiguration_monitoringTimeInMinutes,

    -- ** RollbackTrigger
    rollbackTrigger_type,
    rollbackTrigger_arn,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** Version
    version_sourceCodeUrl,
    version_sourceCodeArchiveUrl,
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
