{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MQ.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Lens
  ( -- * Operations

    -- ** CreateBroker
    createBroker_tags,
    createBroker_ldapServerMetadata,
    createBroker_configuration,
    createBroker_storageType,
    createBroker_creatorRequestId,
    createBroker_logs,
    createBroker_securityGroups,
    createBroker_authenticationStrategy,
    createBroker_maintenanceWindowStartTime,
    createBroker_subnetIds,
    createBroker_encryptionOptions,
    createBroker_engineVersion,
    createBroker_hostInstanceType,
    createBroker_autoMinorVersionUpgrade,
    createBroker_users,
    createBroker_brokerName,
    createBroker_deploymentMode,
    createBroker_engineType,
    createBroker_publiclyAccessible,
    createBrokerResponse_brokerId,
    createBrokerResponse_brokerArn,
    createBrokerResponse_httpStatus,

    -- ** CreateConfiguration
    createConfiguration_tags,
    createConfiguration_authenticationStrategy,
    createConfiguration_engineVersion,
    createConfiguration_engineType,
    createConfiguration_name,
    createConfigurationResponse_latestRevision,
    createConfigurationResponse_name,
    createConfigurationResponse_created,
    createConfigurationResponse_arn,
    createConfigurationResponse_id,
    createConfigurationResponse_authenticationStrategy,
    createConfigurationResponse_httpStatus,

    -- ** CreateTags
    createTags_tags,
    createTags_resourceArn,

    -- ** CreateUser
    createUser_consoleAccess,
    createUser_groups,
    createUser_username,
    createUser_brokerId,
    createUser_password,
    createUserResponse_httpStatus,

    -- ** DeleteBroker
    deleteBroker_brokerId,
    deleteBrokerResponse_brokerId,
    deleteBrokerResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceArn,

    -- ** DeleteUser
    deleteUser_username,
    deleteUser_brokerId,
    deleteUserResponse_httpStatus,

    -- ** DescribeBroker
    describeBroker_brokerId,
    describeBrokerResponse_tags,
    describeBrokerResponse_brokerName,
    describeBrokerResponse_engineType,
    describeBrokerResponse_autoMinorVersionUpgrade,
    describeBrokerResponse_created,
    describeBrokerResponse_pendingSecurityGroups,
    describeBrokerResponse_ldapServerMetadata,
    describeBrokerResponse_deploymentMode,
    describeBrokerResponse_brokerInstances,
    describeBrokerResponse_users,
    describeBrokerResponse_pendingEngineVersion,
    describeBrokerResponse_publiclyAccessible,
    describeBrokerResponse_storageType,
    describeBrokerResponse_configurations,
    describeBrokerResponse_brokerState,
    describeBrokerResponse_brokerId,
    describeBrokerResponse_logs,
    describeBrokerResponse_pendingHostInstanceType,
    describeBrokerResponse_securityGroups,
    describeBrokerResponse_actionsRequired,
    describeBrokerResponse_hostInstanceType,
    describeBrokerResponse_authenticationStrategy,
    describeBrokerResponse_maintenanceWindowStartTime,
    describeBrokerResponse_brokerArn,
    describeBrokerResponse_pendingLdapServerMetadata,
    describeBrokerResponse_subnetIds,
    describeBrokerResponse_engineVersion,
    describeBrokerResponse_pendingAuthenticationStrategy,
    describeBrokerResponse_encryptionOptions,
    describeBrokerResponse_httpStatus,

    -- ** DescribeBrokerEngineTypes
    describeBrokerEngineTypes_nextToken,
    describeBrokerEngineTypes_engineType,
    describeBrokerEngineTypes_maxResults,
    describeBrokerEngineTypesResponse_nextToken,
    describeBrokerEngineTypesResponse_brokerEngineTypes,
    describeBrokerEngineTypesResponse_maxResults,
    describeBrokerEngineTypesResponse_httpStatus,

    -- ** DescribeBrokerInstanceOptions
    describeBrokerInstanceOptions_nextToken,
    describeBrokerInstanceOptions_engineType,
    describeBrokerInstanceOptions_storageType,
    describeBrokerInstanceOptions_maxResults,
    describeBrokerInstanceOptions_hostInstanceType,
    describeBrokerInstanceOptionsResponse_nextToken,
    describeBrokerInstanceOptionsResponse_maxResults,
    describeBrokerInstanceOptionsResponse_brokerInstanceOptions,
    describeBrokerInstanceOptionsResponse_httpStatus,

    -- ** DescribeConfiguration
    describeConfiguration_configurationId,
    describeConfigurationResponse_tags,
    describeConfigurationResponse_latestRevision,
    describeConfigurationResponse_name,
    describeConfigurationResponse_engineType,
    describeConfigurationResponse_created,
    describeConfigurationResponse_arn,
    describeConfigurationResponse_id,
    describeConfigurationResponse_description,
    describeConfigurationResponse_authenticationStrategy,
    describeConfigurationResponse_engineVersion,
    describeConfigurationResponse_httpStatus,

    -- ** DescribeConfigurationRevision
    describeConfigurationRevision_configurationRevision,
    describeConfigurationRevision_configurationId,
    describeConfigurationRevisionResponse_created,
    describeConfigurationRevisionResponse_description,
    describeConfigurationRevisionResponse_configurationId,
    describeConfigurationRevisionResponse_data,
    describeConfigurationRevisionResponse_httpStatus,

    -- ** DescribeUser
    describeUser_username,
    describeUser_brokerId,
    describeUserResponse_username,
    describeUserResponse_brokerId,
    describeUserResponse_consoleAccess,
    describeUserResponse_groups,
    describeUserResponse_pending,
    describeUserResponse_httpStatus,

    -- ** ListBrokers
    listBrokers_nextToken,
    listBrokers_maxResults,
    listBrokersResponse_nextToken,
    listBrokersResponse_brokerSummaries,
    listBrokersResponse_httpStatus,

    -- ** ListConfigurationRevisions
    listConfigurationRevisions_nextToken,
    listConfigurationRevisions_maxResults,
    listConfigurationRevisions_configurationId,
    listConfigurationRevisionsResponse_nextToken,
    listConfigurationRevisionsResponse_revisions,
    listConfigurationRevisionsResponse_maxResults,
    listConfigurationRevisionsResponse_configurationId,
    listConfigurationRevisionsResponse_httpStatus,

    -- ** ListConfigurations
    listConfigurations_nextToken,
    listConfigurations_maxResults,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_maxResults,
    listConfigurationsResponse_httpStatus,

    -- ** ListTags
    listTags_resourceArn,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_brokerId,
    listUsersResponse_nextToken,
    listUsersResponse_users,
    listUsersResponse_brokerId,
    listUsersResponse_maxResults,
    listUsersResponse_httpStatus,

    -- ** RebootBroker
    rebootBroker_brokerId,
    rebootBrokerResponse_httpStatus,

    -- ** UpdateBroker
    updateBroker_autoMinorVersionUpgrade,
    updateBroker_ldapServerMetadata,
    updateBroker_configuration,
    updateBroker_logs,
    updateBroker_securityGroups,
    updateBroker_hostInstanceType,
    updateBroker_authenticationStrategy,
    updateBroker_maintenanceWindowStartTime,
    updateBroker_engineVersion,
    updateBroker_brokerId,
    updateBrokerResponse_autoMinorVersionUpgrade,
    updateBrokerResponse_ldapServerMetadata,
    updateBrokerResponse_configuration,
    updateBrokerResponse_brokerId,
    updateBrokerResponse_logs,
    updateBrokerResponse_securityGroups,
    updateBrokerResponse_hostInstanceType,
    updateBrokerResponse_authenticationStrategy,
    updateBrokerResponse_maintenanceWindowStartTime,
    updateBrokerResponse_engineVersion,
    updateBrokerResponse_httpStatus,

    -- ** UpdateConfiguration
    updateConfiguration_description,
    updateConfiguration_configurationId,
    updateConfiguration_data,
    updateConfigurationResponse_latestRevision,
    updateConfigurationResponse_name,
    updateConfigurationResponse_created,
    updateConfigurationResponse_arn,
    updateConfigurationResponse_id,
    updateConfigurationResponse_warnings,
    updateConfigurationResponse_httpStatus,

    -- ** UpdateUser
    updateUser_password,
    updateUser_consoleAccess,
    updateUser_groups,
    updateUser_username,
    updateUser_brokerId,
    updateUserResponse_httpStatus,

    -- * Types

    -- ** ActionRequired
    actionRequired_actionRequiredInfo,
    actionRequired_actionRequiredCode,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** BrokerEngineType
    brokerEngineType_engineType,
    brokerEngineType_engineVersions,

    -- ** BrokerInstance
    brokerInstance_consoleURL,
    brokerInstance_endpoints,
    brokerInstance_ipAddress,

    -- ** BrokerInstanceOption
    brokerInstanceOption_engineType,
    brokerInstanceOption_supportedDeploymentModes,
    brokerInstanceOption_availabilityZones,
    brokerInstanceOption_storageType,
    brokerInstanceOption_hostInstanceType,
    brokerInstanceOption_supportedEngineVersions,

    -- ** BrokerSummary
    brokerSummary_brokerName,
    brokerSummary_created,
    brokerSummary_brokerState,
    brokerSummary_brokerId,
    brokerSummary_hostInstanceType,
    brokerSummary_brokerArn,
    brokerSummary_deploymentMode,
    brokerSummary_engineType,

    -- ** Configuration
    configuration_tags,
    configuration_description,
    configuration_engineVersion,
    configuration_latestRevision,
    configuration_authenticationStrategy,
    configuration_engineType,
    configuration_id,
    configuration_arn,
    configuration_name,
    configuration_created,

    -- ** ConfigurationId
    configurationId_revision,
    configurationId_id,

    -- ** ConfigurationRevision
    configurationRevision_description,
    configurationRevision_revision,
    configurationRevision_created,

    -- ** Configurations
    configurations_history,
    configurations_current,
    configurations_pending,

    -- ** EncryptionOptions
    encryptionOptions_kmsKeyId,
    encryptionOptions_useAwsOwnedKey,

    -- ** EngineVersion
    engineVersion_name,

    -- ** LdapServerMetadataInput
    ldapServerMetadataInput_roleName,
    ldapServerMetadataInput_userSearchSubtree,
    ldapServerMetadataInput_userRoleName,
    ldapServerMetadataInput_roleSearchSubtree,
    ldapServerMetadataInput_hosts,
    ldapServerMetadataInput_userSearchMatching,
    ldapServerMetadataInput_userBase,
    ldapServerMetadataInput_roleSearchMatching,
    ldapServerMetadataInput_serviceAccountUsername,
    ldapServerMetadataInput_roleBase,
    ldapServerMetadataInput_serviceAccountPassword,

    -- ** LdapServerMetadataOutput
    ldapServerMetadataOutput_roleName,
    ldapServerMetadataOutput_userSearchSubtree,
    ldapServerMetadataOutput_userRoleName,
    ldapServerMetadataOutput_roleSearchSubtree,
    ldapServerMetadataOutput_hosts,
    ldapServerMetadataOutput_userSearchMatching,
    ldapServerMetadataOutput_userBase,
    ldapServerMetadataOutput_roleSearchMatching,
    ldapServerMetadataOutput_serviceAccountUsername,
    ldapServerMetadataOutput_roleBase,

    -- ** Logs
    logs_general,
    logs_audit,

    -- ** LogsSummary
    logsSummary_auditLogGroup,
    logsSummary_audit,
    logsSummary_pending,
    logsSummary_generalLogGroup,
    logsSummary_general,

    -- ** PendingLogs
    pendingLogs_general,
    pendingLogs_audit,

    -- ** SanitizationWarning
    sanitizationWarning_elementName,
    sanitizationWarning_attributeName,
    sanitizationWarning_reason,

    -- ** User
    user_consoleAccess,
    user_groups,
    user_username,
    user_password,

    -- ** UserPendingChanges
    userPendingChanges_consoleAccess,
    userPendingChanges_groups,
    userPendingChanges_pendingChange,

    -- ** UserSummary
    userSummary_pendingChange,
    userSummary_username,

    -- ** WeeklyStartTime
    weeklyStartTime_timeZone,
    weeklyStartTime_timeOfDay,
    weeklyStartTime_dayOfWeek,
  )
where

import Amazonka.MQ.CreateBroker
import Amazonka.MQ.CreateConfiguration
import Amazonka.MQ.CreateTags
import Amazonka.MQ.CreateUser
import Amazonka.MQ.DeleteBroker
import Amazonka.MQ.DeleteTags
import Amazonka.MQ.DeleteUser
import Amazonka.MQ.DescribeBroker
import Amazonka.MQ.DescribeBrokerEngineTypes
import Amazonka.MQ.DescribeBrokerInstanceOptions
import Amazonka.MQ.DescribeConfiguration
import Amazonka.MQ.DescribeConfigurationRevision
import Amazonka.MQ.DescribeUser
import Amazonka.MQ.ListBrokers
import Amazonka.MQ.ListConfigurationRevisions
import Amazonka.MQ.ListConfigurations
import Amazonka.MQ.ListTags
import Amazonka.MQ.ListUsers
import Amazonka.MQ.RebootBroker
import Amazonka.MQ.Types.ActionRequired
import Amazonka.MQ.Types.AvailabilityZone
import Amazonka.MQ.Types.BrokerEngineType
import Amazonka.MQ.Types.BrokerInstance
import Amazonka.MQ.Types.BrokerInstanceOption
import Amazonka.MQ.Types.BrokerSummary
import Amazonka.MQ.Types.Configuration
import Amazonka.MQ.Types.ConfigurationId
import Amazonka.MQ.Types.ConfigurationRevision
import Amazonka.MQ.Types.Configurations
import Amazonka.MQ.Types.EncryptionOptions
import Amazonka.MQ.Types.EngineVersion
import Amazonka.MQ.Types.LdapServerMetadataInput
import Amazonka.MQ.Types.LdapServerMetadataOutput
import Amazonka.MQ.Types.Logs
import Amazonka.MQ.Types.LogsSummary
import Amazonka.MQ.Types.PendingLogs
import Amazonka.MQ.Types.SanitizationWarning
import Amazonka.MQ.Types.User
import Amazonka.MQ.Types.UserPendingChanges
import Amazonka.MQ.Types.UserSummary
import Amazonka.MQ.Types.WeeklyStartTime
import Amazonka.MQ.UpdateBroker
import Amazonka.MQ.UpdateConfiguration
import Amazonka.MQ.UpdateUser
