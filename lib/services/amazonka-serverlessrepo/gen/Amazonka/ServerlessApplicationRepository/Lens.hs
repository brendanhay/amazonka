{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServerlessApplicationRepository.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServerlessApplicationRepository.Lens
  ( -- * Operations

    -- ** CreateApplication
    createApplication_homePageUrl,
    createApplication_labels,
    createApplication_licenseBody,
    createApplication_licenseUrl,
    createApplication_readmeBody,
    createApplication_readmeUrl,
    createApplication_semanticVersion,
    createApplication_sourceCodeArchiveUrl,
    createApplication_sourceCodeUrl,
    createApplication_spdxLicenseId,
    createApplication_templateBody,
    createApplication_templateUrl,
    createApplication_description,
    createApplication_name,
    createApplication_author,
    createApplicationResponse_applicationId,
    createApplicationResponse_author,
    createApplicationResponse_creationTime,
    createApplicationResponse_description,
    createApplicationResponse_homePageUrl,
    createApplicationResponse_isVerifiedAuthor,
    createApplicationResponse_labels,
    createApplicationResponse_licenseUrl,
    createApplicationResponse_name,
    createApplicationResponse_readmeUrl,
    createApplicationResponse_spdxLicenseId,
    createApplicationResponse_verifiedAuthorUrl,
    createApplicationResponse_version,
    createApplicationResponse_httpStatus,

    -- ** CreateApplicationVersion
    createApplicationVersion_sourceCodeArchiveUrl,
    createApplicationVersion_sourceCodeUrl,
    createApplicationVersion_templateBody,
    createApplicationVersion_templateUrl,
    createApplicationVersion_applicationId,
    createApplicationVersion_semanticVersion,
    createApplicationVersionResponse_applicationId,
    createApplicationVersionResponse_creationTime,
    createApplicationVersionResponse_parameterDefinitions,
    createApplicationVersionResponse_requiredCapabilities,
    createApplicationVersionResponse_resourcesSupported,
    createApplicationVersionResponse_semanticVersion,
    createApplicationVersionResponse_sourceCodeArchiveUrl,
    createApplicationVersionResponse_sourceCodeUrl,
    createApplicationVersionResponse_templateUrl,
    createApplicationVersionResponse_httpStatus,

    -- ** CreateCloudFormationChangeSet
    createCloudFormationChangeSet_capabilities,
    createCloudFormationChangeSet_changeSetName,
    createCloudFormationChangeSet_clientToken,
    createCloudFormationChangeSet_description,
    createCloudFormationChangeSet_notificationArns,
    createCloudFormationChangeSet_parameterOverrides,
    createCloudFormationChangeSet_resourceTypes,
    createCloudFormationChangeSet_rollbackConfiguration,
    createCloudFormationChangeSet_semanticVersion,
    createCloudFormationChangeSet_tags,
    createCloudFormationChangeSet_templateId,
    createCloudFormationChangeSet_applicationId,
    createCloudFormationChangeSet_stackName,
    createCloudFormationChangeSetResponse_applicationId,
    createCloudFormationChangeSetResponse_changeSetId,
    createCloudFormationChangeSetResponse_semanticVersion,
    createCloudFormationChangeSetResponse_stackId,
    createCloudFormationChangeSetResponse_httpStatus,

    -- ** CreateCloudFormationTemplate
    createCloudFormationTemplate_semanticVersion,
    createCloudFormationTemplate_applicationId,
    createCloudFormationTemplateResponse_applicationId,
    createCloudFormationTemplateResponse_creationTime,
    createCloudFormationTemplateResponse_expirationTime,
    createCloudFormationTemplateResponse_semanticVersion,
    createCloudFormationTemplateResponse_status,
    createCloudFormationTemplateResponse_templateId,
    createCloudFormationTemplateResponse_templateUrl,
    createCloudFormationTemplateResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_applicationId,

    -- ** GetApplication
    getApplication_semanticVersion,
    getApplication_applicationId,
    getApplicationResponse_applicationId,
    getApplicationResponse_author,
    getApplicationResponse_creationTime,
    getApplicationResponse_description,
    getApplicationResponse_homePageUrl,
    getApplicationResponse_isVerifiedAuthor,
    getApplicationResponse_labels,
    getApplicationResponse_licenseUrl,
    getApplicationResponse_name,
    getApplicationResponse_readmeUrl,
    getApplicationResponse_spdxLicenseId,
    getApplicationResponse_verifiedAuthorUrl,
    getApplicationResponse_version,
    getApplicationResponse_httpStatus,

    -- ** GetApplicationPolicy
    getApplicationPolicy_applicationId,
    getApplicationPolicyResponse_statements,
    getApplicationPolicyResponse_httpStatus,

    -- ** GetCloudFormationTemplate
    getCloudFormationTemplate_applicationId,
    getCloudFormationTemplate_templateId,
    getCloudFormationTemplateResponse_applicationId,
    getCloudFormationTemplateResponse_creationTime,
    getCloudFormationTemplateResponse_expirationTime,
    getCloudFormationTemplateResponse_semanticVersion,
    getCloudFormationTemplateResponse_status,
    getCloudFormationTemplateResponse_templateId,
    getCloudFormationTemplateResponse_templateUrl,
    getCloudFormationTemplateResponse_httpStatus,

    -- ** ListApplicationDependencies
    listApplicationDependencies_maxItems,
    listApplicationDependencies_nextToken,
    listApplicationDependencies_semanticVersion,
    listApplicationDependencies_applicationId,
    listApplicationDependenciesResponse_dependencies,
    listApplicationDependenciesResponse_nextToken,
    listApplicationDependenciesResponse_httpStatus,

    -- ** ListApplicationVersions
    listApplicationVersions_maxItems,
    listApplicationVersions_nextToken,
    listApplicationVersions_applicationId,
    listApplicationVersionsResponse_nextToken,
    listApplicationVersionsResponse_versions,
    listApplicationVersionsResponse_httpStatus,

    -- ** ListApplications
    listApplications_maxItems,
    listApplications_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** PutApplicationPolicy
    putApplicationPolicy_applicationId,
    putApplicationPolicy_statements,
    putApplicationPolicyResponse_statements,
    putApplicationPolicyResponse_httpStatus,

    -- ** UnshareApplication
    unshareApplication_applicationId,
    unshareApplication_organizationId,

    -- ** UpdateApplication
    updateApplication_author,
    updateApplication_description,
    updateApplication_homePageUrl,
    updateApplication_labels,
    updateApplication_readmeBody,
    updateApplication_readmeUrl,
    updateApplication_applicationId,
    updateApplicationResponse_applicationId,
    updateApplicationResponse_author,
    updateApplicationResponse_creationTime,
    updateApplicationResponse_description,
    updateApplicationResponse_homePageUrl,
    updateApplicationResponse_isVerifiedAuthor,
    updateApplicationResponse_labels,
    updateApplicationResponse_licenseUrl,
    updateApplicationResponse_name,
    updateApplicationResponse_readmeUrl,
    updateApplicationResponse_spdxLicenseId,
    updateApplicationResponse_verifiedAuthorUrl,
    updateApplicationResponse_version,
    updateApplicationResponse_httpStatus,

    -- * Types

    -- ** ApplicationDependencySummary
    applicationDependencySummary_applicationId,
    applicationDependencySummary_semanticVersion,

    -- ** ApplicationPolicyStatement
    applicationPolicyStatement_principalOrgIDs,
    applicationPolicyStatement_statementId,
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
    parameterDefinition_allowedPattern,
    parameterDefinition_allowedValues,
    parameterDefinition_constraintDescription,
    parameterDefinition_defaultValue,
    parameterDefinition_description,
    parameterDefinition_maxLength,
    parameterDefinition_maxValue,
    parameterDefinition_minLength,
    parameterDefinition_minValue,
    parameterDefinition_noEcho,
    parameterDefinition_type,
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

import Amazonka.ServerlessApplicationRepository.CreateApplication
import Amazonka.ServerlessApplicationRepository.CreateApplicationVersion
import Amazonka.ServerlessApplicationRepository.CreateCloudFormationChangeSet
import Amazonka.ServerlessApplicationRepository.CreateCloudFormationTemplate
import Amazonka.ServerlessApplicationRepository.DeleteApplication
import Amazonka.ServerlessApplicationRepository.GetApplication
import Amazonka.ServerlessApplicationRepository.GetApplicationPolicy
import Amazonka.ServerlessApplicationRepository.GetCloudFormationTemplate
import Amazonka.ServerlessApplicationRepository.ListApplicationDependencies
import Amazonka.ServerlessApplicationRepository.ListApplicationVersions
import Amazonka.ServerlessApplicationRepository.ListApplications
import Amazonka.ServerlessApplicationRepository.PutApplicationPolicy
import Amazonka.ServerlessApplicationRepository.Types.ApplicationDependencySummary
import Amazonka.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
import Amazonka.ServerlessApplicationRepository.Types.ApplicationSummary
import Amazonka.ServerlessApplicationRepository.Types.ParameterDefinition
import Amazonka.ServerlessApplicationRepository.Types.ParameterValue
import Amazonka.ServerlessApplicationRepository.Types.RollbackConfiguration
import Amazonka.ServerlessApplicationRepository.Types.RollbackTrigger
import Amazonka.ServerlessApplicationRepository.Types.Tag
import Amazonka.ServerlessApplicationRepository.Types.Version
import Amazonka.ServerlessApplicationRepository.Types.VersionSummary
import Amazonka.ServerlessApplicationRepository.UnshareApplication
import Amazonka.ServerlessApplicationRepository.UpdateApplication
