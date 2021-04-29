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

    -- ** CreateBroker
    createBroker_encryptionOptions,
    createBroker_brokerName,
    createBroker_storageType,
    createBroker_ldapServerMetadata,
    createBroker_engineType,
    createBroker_authenticationStrategy,
    createBroker_configuration,
    createBroker_creatorRequestId,
    createBroker_subnetIds,
    createBroker_publiclyAccessible,
    createBroker_securityGroups,
    createBroker_logs,
    createBroker_maintenanceWindowStartTime,
    createBroker_engineVersion,
    createBroker_tags,
    createBroker_hostInstanceType,
    createBroker_autoMinorVersionUpgrade,
    createBroker_users,
    createBroker_deploymentMode,
    createBrokerResponse_brokerId,
    createBrokerResponse_brokerArn,
    createBrokerResponse_httpStatus,

    -- ** DescribeBrokerInstanceOptions
    describeBrokerInstanceOptions_nextToken,
    describeBrokerInstanceOptions_storageType,
    describeBrokerInstanceOptions_engineType,
    describeBrokerInstanceOptions_maxResults,
    describeBrokerInstanceOptions_hostInstanceType,
    describeBrokerInstanceOptionsResponse_nextToken,
    describeBrokerInstanceOptionsResponse_maxResults,
    describeBrokerInstanceOptionsResponse_brokerInstanceOptions,
    describeBrokerInstanceOptionsResponse_httpStatus,

    -- ** UpdateConfiguration
    updateConfiguration_data,
    updateConfiguration_description,
    updateConfiguration_configurationId,
    updateConfigurationResponse_warnings,
    updateConfigurationResponse_latestRevision,
    updateConfigurationResponse_arn,
    updateConfigurationResponse_id,
    updateConfigurationResponse_name,
    updateConfigurationResponse_created,
    updateConfigurationResponse_httpStatus,

    -- ** ListConfigurations
    listConfigurations_nextToken,
    listConfigurations_maxResults,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_maxResults,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_httpStatus,

    -- ** DescribeBroker
    describeBroker_brokerId,
    describeBrokerResponse_encryptionOptions,
    describeBrokerResponse_brokerName,
    describeBrokerResponse_brokerInstances,
    describeBrokerResponse_storageType,
    describeBrokerResponse_ldapServerMetadata,
    describeBrokerResponse_brokerId,
    describeBrokerResponse_pendingHostInstanceType,
    describeBrokerResponse_engineType,
    describeBrokerResponse_configurations,
    describeBrokerResponse_authenticationStrategy,
    describeBrokerResponse_subnetIds,
    describeBrokerResponse_pendingSecurityGroups,
    describeBrokerResponse_publiclyAccessible,
    describeBrokerResponse_securityGroups,
    describeBrokerResponse_logs,
    describeBrokerResponse_pendingAuthenticationStrategy,
    describeBrokerResponse_maintenanceWindowStartTime,
    describeBrokerResponse_engineVersion,
    describeBrokerResponse_brokerState,
    describeBrokerResponse_tags,
    describeBrokerResponse_hostInstanceType,
    describeBrokerResponse_pendingLdapServerMetadata,
    describeBrokerResponse_brokerArn,
    describeBrokerResponse_pendingEngineVersion,
    describeBrokerResponse_created,
    describeBrokerResponse_autoMinorVersionUpgrade,
    describeBrokerResponse_users,
    describeBrokerResponse_deploymentMode,
    describeBrokerResponse_httpStatus,

    -- ** DescribeBrokerEngineTypes
    describeBrokerEngineTypes_nextToken,
    describeBrokerEngineTypes_engineType,
    describeBrokerEngineTypes_maxResults,
    describeBrokerEngineTypesResponse_nextToken,
    describeBrokerEngineTypesResponse_brokerEngineTypes,
    describeBrokerEngineTypesResponse_maxResults,
    describeBrokerEngineTypesResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceArn,

    -- ** CreateUser
    createUser_groups,
    createUser_password,
    createUser_consoleAccess,
    createUser_username,
    createUser_brokerId,
    createUserResponse_httpStatus,

    -- ** ListBrokers
    listBrokers_nextToken,
    listBrokers_maxResults,
    listBrokersResponse_nextToken,
    listBrokersResponse_brokerSummaries,
    listBrokersResponse_httpStatus,

    -- ** UpdateBroker
    updateBroker_ldapServerMetadata,
    updateBroker_authenticationStrategy,
    updateBroker_configuration,
    updateBroker_securityGroups,
    updateBroker_logs,
    updateBroker_engineVersion,
    updateBroker_hostInstanceType,
    updateBroker_autoMinorVersionUpgrade,
    updateBroker_brokerId,
    updateBrokerResponse_ldapServerMetadata,
    updateBrokerResponse_brokerId,
    updateBrokerResponse_authenticationStrategy,
    updateBrokerResponse_configuration,
    updateBrokerResponse_securityGroups,
    updateBrokerResponse_logs,
    updateBrokerResponse_engineVersion,
    updateBrokerResponse_hostInstanceType,
    updateBrokerResponse_autoMinorVersionUpgrade,
    updateBrokerResponse_httpStatus,

    -- ** DeleteBroker
    deleteBroker_brokerId,
    deleteBrokerResponse_brokerId,
    deleteBrokerResponse_httpStatus,

    -- ** RebootBroker
    rebootBroker_brokerId,
    rebootBrokerResponse_httpStatus,

    -- ** ListConfigurationRevisions
    listConfigurationRevisions_nextToken,
    listConfigurationRevisions_maxResults,
    listConfigurationRevisions_configurationId,
    listConfigurationRevisionsResponse_nextToken,
    listConfigurationRevisionsResponse_maxResults,
    listConfigurationRevisionsResponse_revisions,
    listConfigurationRevisionsResponse_configurationId,
    listConfigurationRevisionsResponse_httpStatus,

    -- ** CreateConfiguration
    createConfiguration_engineType,
    createConfiguration_authenticationStrategy,
    createConfiguration_name,
    createConfiguration_engineVersion,
    createConfiguration_tags,
    createConfigurationResponse_authenticationStrategy,
    createConfigurationResponse_latestRevision,
    createConfigurationResponse_arn,
    createConfigurationResponse_id,
    createConfigurationResponse_name,
    createConfigurationResponse_created,
    createConfigurationResponse_httpStatus,

    -- ** DescribeUser
    describeUser_username,
    describeUser_brokerId,
    describeUserResponse_groups,
    describeUserResponse_brokerId,
    describeUserResponse_pending,
    describeUserResponse_username,
    describeUserResponse_consoleAccess,
    describeUserResponse_httpStatus,

    -- ** DescribeConfigurationRevision
    describeConfigurationRevision_configurationRevision,
    describeConfigurationRevision_configurationId,
    describeConfigurationRevisionResponse_data,
    describeConfigurationRevisionResponse_configurationId,
    describeConfigurationRevisionResponse_description,
    describeConfigurationRevisionResponse_created,
    describeConfigurationRevisionResponse_httpStatus,

    -- ** ListTags
    listTags_resourceArn,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_username,
    deleteUser_brokerId,
    deleteUserResponse_httpStatus,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_brokerId,
    listUsersResponse_nextToken,
    listUsersResponse_brokerId,
    listUsersResponse_maxResults,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** UpdateUser
    updateUser_groups,
    updateUser_password,
    updateUser_consoleAccess,
    updateUser_username,
    updateUser_brokerId,
    updateUserResponse_httpStatus,

    -- ** DescribeConfiguration
    describeConfiguration_configurationId,
    describeConfigurationResponse_engineType,
    describeConfigurationResponse_authenticationStrategy,
    describeConfigurationResponse_latestRevision,
    describeConfigurationResponse_arn,
    describeConfigurationResponse_id,
    describeConfigurationResponse_name,
    describeConfigurationResponse_engineVersion,
    describeConfigurationResponse_tags,
    describeConfigurationResponse_description,
    describeConfigurationResponse_created,
    describeConfigurationResponse_httpStatus,

    -- ** CreateTags
    createTags_tags,
    createTags_resourceArn,

    -- * Types

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** BrokerEngineType
    brokerEngineType_engineType,
    brokerEngineType_engineVersions,

    -- ** BrokerInstance
    brokerInstance_endpoints,
    brokerInstance_ipAddress,
    brokerInstance_consoleURL,

    -- ** BrokerInstanceOption
    brokerInstanceOption_availabilityZones,
    brokerInstanceOption_storageType,
    brokerInstanceOption_engineType,
    brokerInstanceOption_supportedDeploymentModes,
    brokerInstanceOption_supportedEngineVersions,
    brokerInstanceOption_hostInstanceType,

    -- ** BrokerSummary
    brokerSummary_brokerName,
    brokerSummary_brokerId,
    brokerSummary_engineType,
    brokerSummary_brokerState,
    brokerSummary_hostInstanceType,
    brokerSummary_brokerArn,
    brokerSummary_created,
    brokerSummary_deploymentMode,

    -- ** Configuration
    configuration_engineType,
    configuration_authenticationStrategy,
    configuration_latestRevision,
    configuration_arn,
    configuration_id,
    configuration_name,
    configuration_engineVersion,
    configuration_tags,
    configuration_description,
    configuration_created,

    -- ** ConfigurationId
    configurationId_id,
    configurationId_revision,

    -- ** ConfigurationRevision
    configurationRevision_description,
    configurationRevision_revision,
    configurationRevision_created,

    -- ** Configurations
    configurations_pending,
    configurations_current,
    configurations_history,

    -- ** EncryptionOptions
    encryptionOptions_kmsKeyId,
    encryptionOptions_useAwsOwnedKey,

    -- ** EngineVersion
    engineVersion_name,

    -- ** LdapServerMetadataInput
    ldapServerMetadataInput_userBase,
    ldapServerMetadataInput_userSearchMatching,
    ldapServerMetadataInput_roleName,
    ldapServerMetadataInput_serviceAccountPassword,
    ldapServerMetadataInput_userSearchSubtree,
    ldapServerMetadataInput_serviceAccountUsername,
    ldapServerMetadataInput_userRoleName,
    ldapServerMetadataInput_roleBase,
    ldapServerMetadataInput_roleSearchMatching,
    ldapServerMetadataInput_hosts,
    ldapServerMetadataInput_roleSearchSubtree,

    -- ** LdapServerMetadataOutput
    ldapServerMetadataOutput_userBase,
    ldapServerMetadataOutput_userSearchMatching,
    ldapServerMetadataOutput_roleName,
    ldapServerMetadataOutput_userSearchSubtree,
    ldapServerMetadataOutput_serviceAccountUsername,
    ldapServerMetadataOutput_userRoleName,
    ldapServerMetadataOutput_roleBase,
    ldapServerMetadataOutput_roleSearchMatching,
    ldapServerMetadataOutput_hosts,
    ldapServerMetadataOutput_roleSearchSubtree,

    -- ** Logs
    logs_general,
    logs_audit,

    -- ** LogsSummary
    logsSummary_general,
    logsSummary_audit,
    logsSummary_pending,
    logsSummary_auditLogGroup,
    logsSummary_generalLogGroup,

    -- ** PendingLogs
    pendingLogs_general,
    pendingLogs_audit,

    -- ** SanitizationWarning
    sanitizationWarning_elementName,
    sanitizationWarning_attributeName,
    sanitizationWarning_reason,

    -- ** User
    user_groups,
    user_password,
    user_username,
    user_consoleAccess,

    -- ** UserPendingChanges
    userPendingChanges_groups,
    userPendingChanges_pendingChange,
    userPendingChanges_consoleAccess,

    -- ** UserSummary
    userSummary_pendingChange,
    userSummary_username,

    -- ** WeeklyStartTime
    weeklyStartTime_dayOfWeek,
    weeklyStartTime_timeOfDay,
    weeklyStartTime_timeZone,
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
