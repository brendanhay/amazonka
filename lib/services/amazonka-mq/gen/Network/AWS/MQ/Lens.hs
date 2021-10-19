{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Lens
  ( -- * Operations

    -- ** CreateConfiguration
    createConfiguration_authenticationStrategy,
    createConfiguration_tags,
    createConfiguration_engineVersion,
    createConfiguration_engineType,
    createConfiguration_name,
    createConfigurationResponse_arn,
    createConfigurationResponse_latestRevision,
    createConfigurationResponse_created,
    createConfigurationResponse_authenticationStrategy,
    createConfigurationResponse_name,
    createConfigurationResponse_id,
    createConfigurationResponse_httpStatus,

    -- ** CreateBroker
    createBroker_securityGroups,
    createBroker_subnetIds,
    createBroker_creatorRequestId,
    createBroker_authenticationStrategy,
    createBroker_ldapServerMetadata,
    createBroker_maintenanceWindowStartTime,
    createBroker_logs,
    createBroker_encryptionOptions,
    createBroker_configuration,
    createBroker_tags,
    createBroker_storageType,
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

    -- ** DeleteBroker
    deleteBroker_brokerId,
    deleteBrokerResponse_brokerId,
    deleteBrokerResponse_httpStatus,

    -- ** UpdateBroker
    updateBroker_engineVersion,
    updateBroker_autoMinorVersionUpgrade,
    updateBroker_securityGroups,
    updateBroker_authenticationStrategy,
    updateBroker_ldapServerMetadata,
    updateBroker_maintenanceWindowStartTime,
    updateBroker_logs,
    updateBroker_configuration,
    updateBroker_hostInstanceType,
    updateBroker_brokerId,
    updateBrokerResponse_engineVersion,
    updateBrokerResponse_autoMinorVersionUpgrade,
    updateBrokerResponse_securityGroups,
    updateBrokerResponse_authenticationStrategy,
    updateBrokerResponse_ldapServerMetadata,
    updateBrokerResponse_maintenanceWindowStartTime,
    updateBrokerResponse_logs,
    updateBrokerResponse_configuration,
    updateBrokerResponse_brokerId,
    updateBrokerResponse_hostInstanceType,
    updateBrokerResponse_httpStatus,

    -- ** RebootBroker
    rebootBroker_brokerId,
    rebootBrokerResponse_httpStatus,

    -- ** ListConfigurationRevisions
    listConfigurationRevisions_nextToken,
    listConfigurationRevisions_maxResults,
    listConfigurationRevisions_configurationId,
    listConfigurationRevisionsResponse_configurationId,
    listConfigurationRevisionsResponse_nextToken,
    listConfigurationRevisionsResponse_revisions,
    listConfigurationRevisionsResponse_maxResults,
    listConfigurationRevisionsResponse_httpStatus,

    -- ** CreateTags
    createTags_tags,
    createTags_resourceArn,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_brokerId,
    listUsersResponse_users,
    listUsersResponse_nextToken,
    listUsersResponse_brokerId,
    listUsersResponse_maxResults,
    listUsersResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceArn,

    -- ** ListConfigurations
    listConfigurations_nextToken,
    listConfigurations_maxResults,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_maxResults,
    listConfigurationsResponse_httpStatus,

    -- ** DescribeUser
    describeUser_username,
    describeUser_brokerId,
    describeUserResponse_groups,
    describeUserResponse_pending,
    describeUserResponse_consoleAccess,
    describeUserResponse_username,
    describeUserResponse_brokerId,
    describeUserResponse_httpStatus,

    -- ** DescribeBrokerInstanceOptions
    describeBrokerInstanceOptions_nextToken,
    describeBrokerInstanceOptions_engineType,
    describeBrokerInstanceOptions_maxResults,
    describeBrokerInstanceOptions_hostInstanceType,
    describeBrokerInstanceOptions_storageType,
    describeBrokerInstanceOptionsResponse_nextToken,
    describeBrokerInstanceOptionsResponse_brokerInstanceOptions,
    describeBrokerInstanceOptionsResponse_maxResults,
    describeBrokerInstanceOptionsResponse_httpStatus,

    -- ** ListBrokers
    listBrokers_nextToken,
    listBrokers_maxResults,
    listBrokersResponse_nextToken,
    listBrokersResponse_brokerSummaries,
    listBrokersResponse_httpStatus,

    -- ** CreateUser
    createUser_groups,
    createUser_consoleAccess,
    createUser_username,
    createUser_brokerId,
    createUser_password,
    createUserResponse_httpStatus,

    -- ** DescribeConfiguration
    describeConfiguration_configurationId,
    describeConfigurationResponse_engineVersion,
    describeConfigurationResponse_arn,
    describeConfigurationResponse_latestRevision,
    describeConfigurationResponse_created,
    describeConfigurationResponse_authenticationStrategy,
    describeConfigurationResponse_name,
    describeConfigurationResponse_id,
    describeConfigurationResponse_description,
    describeConfigurationResponse_engineType,
    describeConfigurationResponse_tags,
    describeConfigurationResponse_httpStatus,

    -- ** UpdateUser
    updateUser_groups,
    updateUser_consoleAccess,
    updateUser_password,
    updateUser_username,
    updateUser_brokerId,
    updateUserResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_username,
    deleteUser_brokerId,
    deleteUserResponse_httpStatus,

    -- ** ListTags
    listTags_resourceArn,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DescribeBrokerEngineTypes
    describeBrokerEngineTypes_nextToken,
    describeBrokerEngineTypes_engineType,
    describeBrokerEngineTypes_maxResults,
    describeBrokerEngineTypesResponse_brokerEngineTypes,
    describeBrokerEngineTypesResponse_nextToken,
    describeBrokerEngineTypesResponse_maxResults,
    describeBrokerEngineTypesResponse_httpStatus,

    -- ** DescribeConfigurationRevision
    describeConfigurationRevision_configurationRevision,
    describeConfigurationRevision_configurationId,
    describeConfigurationRevisionResponse_configurationId,
    describeConfigurationRevisionResponse_data,
    describeConfigurationRevisionResponse_created,
    describeConfigurationRevisionResponse_description,
    describeConfigurationRevisionResponse_httpStatus,

    -- ** DescribeBroker
    describeBroker_brokerId,
    describeBrokerResponse_brokerName,
    describeBrokerResponse_engineVersion,
    describeBrokerResponse_pendingAuthenticationStrategy,
    describeBrokerResponse_brokerState,
    describeBrokerResponse_publiclyAccessible,
    describeBrokerResponse_autoMinorVersionUpgrade,
    describeBrokerResponse_securityGroups,
    describeBrokerResponse_users,
    describeBrokerResponse_pendingSecurityGroups,
    describeBrokerResponse_subnetIds,
    describeBrokerResponse_created,
    describeBrokerResponse_configurations,
    describeBrokerResponse_authenticationStrategy,
    describeBrokerResponse_pendingHostInstanceType,
    describeBrokerResponse_ldapServerMetadata,
    describeBrokerResponse_maintenanceWindowStartTime,
    describeBrokerResponse_logs,
    describeBrokerResponse_encryptionOptions,
    describeBrokerResponse_deploymentMode,
    describeBrokerResponse_pendingEngineVersion,
    describeBrokerResponse_brokerId,
    describeBrokerResponse_pendingLdapServerMetadata,
    describeBrokerResponse_engineType,
    describeBrokerResponse_brokerArn,
    describeBrokerResponse_tags,
    describeBrokerResponse_brokerInstances,
    describeBrokerResponse_hostInstanceType,
    describeBrokerResponse_storageType,
    describeBrokerResponse_httpStatus,

    -- ** UpdateConfiguration
    updateConfiguration_description,
    updateConfiguration_configurationId,
    updateConfiguration_data,
    updateConfigurationResponse_arn,
    updateConfigurationResponse_latestRevision,
    updateConfigurationResponse_created,
    updateConfigurationResponse_warnings,
    updateConfigurationResponse_name,
    updateConfigurationResponse_id,
    updateConfigurationResponse_httpStatus,

    -- * Types

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** BrokerEngineType
    brokerEngineType_engineVersions,
    brokerEngineType_engineType,

    -- ** BrokerInstance
    brokerInstance_ipAddress,
    brokerInstance_consoleURL,
    brokerInstance_endpoints,

    -- ** BrokerInstanceOption
    brokerInstanceOption_supportedEngineVersions,
    brokerInstanceOption_availabilityZones,
    brokerInstanceOption_supportedDeploymentModes,
    brokerInstanceOption_engineType,
    brokerInstanceOption_hostInstanceType,
    brokerInstanceOption_storageType,

    -- ** BrokerSummary
    brokerSummary_brokerName,
    brokerSummary_brokerState,
    brokerSummary_created,
    brokerSummary_brokerId,
    brokerSummary_brokerArn,
    brokerSummary_hostInstanceType,
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
    configurations_pending,
    configurations_history,
    configurations_current,

    -- ** EncryptionOptions
    encryptionOptions_kmsKeyId,
    encryptionOptions_useAwsOwnedKey,

    -- ** EngineVersion
    engineVersion_name,

    -- ** LdapServerMetadataInput
    ldapServerMetadataInput_userRoleName,
    ldapServerMetadataInput_userSearchSubtree,
    ldapServerMetadataInput_roleSearchSubtree,
    ldapServerMetadataInput_roleName,
    ldapServerMetadataInput_hosts,
    ldapServerMetadataInput_userSearchMatching,
    ldapServerMetadataInput_userBase,
    ldapServerMetadataInput_roleSearchMatching,
    ldapServerMetadataInput_serviceAccountUsername,
    ldapServerMetadataInput_roleBase,
    ldapServerMetadataInput_serviceAccountPassword,

    -- ** LdapServerMetadataOutput
    ldapServerMetadataOutput_userRoleName,
    ldapServerMetadataOutput_userSearchSubtree,
    ldapServerMetadataOutput_roleSearchSubtree,
    ldapServerMetadataOutput_roleName,
    ldapServerMetadataOutput_hosts,
    ldapServerMetadataOutput_userSearchMatching,
    ldapServerMetadataOutput_userBase,
    ldapServerMetadataOutput_roleSearchMatching,
    ldapServerMetadataOutput_serviceAccountUsername,
    ldapServerMetadataOutput_roleBase,

    -- ** Logs
    logs_audit,
    logs_general,

    -- ** LogsSummary
    logsSummary_pending,
    logsSummary_audit,
    logsSummary_auditLogGroup,
    logsSummary_generalLogGroup,
    logsSummary_general,

    -- ** PendingLogs
    pendingLogs_audit,
    pendingLogs_general,

    -- ** SanitizationWarning
    sanitizationWarning_attributeName,
    sanitizationWarning_elementName,
    sanitizationWarning_reason,

    -- ** User
    user_groups,
    user_consoleAccess,
    user_username,
    user_password,

    -- ** UserPendingChanges
    userPendingChanges_groups,
    userPendingChanges_consoleAccess,
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

import Network.AWS.MQ.CreateBroker
import Network.AWS.MQ.CreateConfiguration
import Network.AWS.MQ.CreateTags
import Network.AWS.MQ.CreateUser
import Network.AWS.MQ.DeleteBroker
import Network.AWS.MQ.DeleteTags
import Network.AWS.MQ.DeleteUser
import Network.AWS.MQ.DescribeBroker
import Network.AWS.MQ.DescribeBrokerEngineTypes
import Network.AWS.MQ.DescribeBrokerInstanceOptions
import Network.AWS.MQ.DescribeConfiguration
import Network.AWS.MQ.DescribeConfigurationRevision
import Network.AWS.MQ.DescribeUser
import Network.AWS.MQ.ListBrokers
import Network.AWS.MQ.ListConfigurationRevisions
import Network.AWS.MQ.ListConfigurations
import Network.AWS.MQ.ListTags
import Network.AWS.MQ.ListUsers
import Network.AWS.MQ.RebootBroker
import Network.AWS.MQ.Types.AvailabilityZone
import Network.AWS.MQ.Types.BrokerEngineType
import Network.AWS.MQ.Types.BrokerInstance
import Network.AWS.MQ.Types.BrokerInstanceOption
import Network.AWS.MQ.Types.BrokerSummary
import Network.AWS.MQ.Types.Configuration
import Network.AWS.MQ.Types.ConfigurationId
import Network.AWS.MQ.Types.ConfigurationRevision
import Network.AWS.MQ.Types.Configurations
import Network.AWS.MQ.Types.EncryptionOptions
import Network.AWS.MQ.Types.EngineVersion
import Network.AWS.MQ.Types.LdapServerMetadataInput
import Network.AWS.MQ.Types.LdapServerMetadataOutput
import Network.AWS.MQ.Types.Logs
import Network.AWS.MQ.Types.LogsSummary
import Network.AWS.MQ.Types.PendingLogs
import Network.AWS.MQ.Types.SanitizationWarning
import Network.AWS.MQ.Types.User
import Network.AWS.MQ.Types.UserPendingChanges
import Network.AWS.MQ.Types.UserSummary
import Network.AWS.MQ.Types.WeeklyStartTime
import Network.AWS.MQ.UpdateBroker
import Network.AWS.MQ.UpdateConfiguration
import Network.AWS.MQ.UpdateUser
