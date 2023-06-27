{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MQ.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Lens
  ( -- * Operations

    -- ** CreateBroker
    createBroker_authenticationStrategy,
    createBroker_configuration,
    createBroker_creatorRequestId,
    createBroker_dataReplicationMode,
    createBroker_dataReplicationPrimaryBrokerArn,
    createBroker_encryptionOptions,
    createBroker_ldapServerMetadata,
    createBroker_logs,
    createBroker_maintenanceWindowStartTime,
    createBroker_securityGroups,
    createBroker_storageType,
    createBroker_subnetIds,
    createBroker_tags,
    createBroker_engineVersion,
    createBroker_hostInstanceType,
    createBroker_autoMinorVersionUpgrade,
    createBroker_users,
    createBroker_brokerName,
    createBroker_deploymentMode,
    createBroker_engineType,
    createBroker_publiclyAccessible,
    createBrokerResponse_brokerArn,
    createBrokerResponse_brokerId,
    createBrokerResponse_httpStatus,

    -- ** CreateConfiguration
    createConfiguration_authenticationStrategy,
    createConfiguration_tags,
    createConfiguration_engineVersion,
    createConfiguration_engineType,
    createConfiguration_name,
    createConfigurationResponse_arn,
    createConfigurationResponse_authenticationStrategy,
    createConfigurationResponse_created,
    createConfigurationResponse_id,
    createConfigurationResponse_latestRevision,
    createConfigurationResponse_name,
    createConfigurationResponse_httpStatus,

    -- ** CreateTags
    createTags_tags,
    createTags_resourceArn,

    -- ** CreateUser
    createUser_consoleAccess,
    createUser_groups,
    createUser_replicationUser,
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
    describeBrokerResponse_actionsRequired,
    describeBrokerResponse_authenticationStrategy,
    describeBrokerResponse_autoMinorVersionUpgrade,
    describeBrokerResponse_brokerArn,
    describeBrokerResponse_brokerId,
    describeBrokerResponse_brokerInstances,
    describeBrokerResponse_brokerName,
    describeBrokerResponse_brokerState,
    describeBrokerResponse_configurations,
    describeBrokerResponse_created,
    describeBrokerResponse_dataReplicationMetadata,
    describeBrokerResponse_dataReplicationMode,
    describeBrokerResponse_deploymentMode,
    describeBrokerResponse_encryptionOptions,
    describeBrokerResponse_engineType,
    describeBrokerResponse_engineVersion,
    describeBrokerResponse_hostInstanceType,
    describeBrokerResponse_ldapServerMetadata,
    describeBrokerResponse_logs,
    describeBrokerResponse_maintenanceWindowStartTime,
    describeBrokerResponse_pendingAuthenticationStrategy,
    describeBrokerResponse_pendingDataReplicationMetadata,
    describeBrokerResponse_pendingDataReplicationMode,
    describeBrokerResponse_pendingEngineVersion,
    describeBrokerResponse_pendingHostInstanceType,
    describeBrokerResponse_pendingLdapServerMetadata,
    describeBrokerResponse_pendingSecurityGroups,
    describeBrokerResponse_publiclyAccessible,
    describeBrokerResponse_securityGroups,
    describeBrokerResponse_storageType,
    describeBrokerResponse_subnetIds,
    describeBrokerResponse_tags,
    describeBrokerResponse_users,
    describeBrokerResponse_httpStatus,

    -- ** DescribeBrokerEngineTypes
    describeBrokerEngineTypes_engineType,
    describeBrokerEngineTypes_maxResults,
    describeBrokerEngineTypes_nextToken,
    describeBrokerEngineTypesResponse_brokerEngineTypes,
    describeBrokerEngineTypesResponse_maxResults,
    describeBrokerEngineTypesResponse_nextToken,
    describeBrokerEngineTypesResponse_httpStatus,

    -- ** DescribeBrokerInstanceOptions
    describeBrokerInstanceOptions_engineType,
    describeBrokerInstanceOptions_hostInstanceType,
    describeBrokerInstanceOptions_maxResults,
    describeBrokerInstanceOptions_nextToken,
    describeBrokerInstanceOptions_storageType,
    describeBrokerInstanceOptionsResponse_brokerInstanceOptions,
    describeBrokerInstanceOptionsResponse_maxResults,
    describeBrokerInstanceOptionsResponse_nextToken,
    describeBrokerInstanceOptionsResponse_httpStatus,

    -- ** DescribeConfiguration
    describeConfiguration_configurationId,
    describeConfigurationResponse_arn,
    describeConfigurationResponse_authenticationStrategy,
    describeConfigurationResponse_created,
    describeConfigurationResponse_description,
    describeConfigurationResponse_engineType,
    describeConfigurationResponse_engineVersion,
    describeConfigurationResponse_id,
    describeConfigurationResponse_latestRevision,
    describeConfigurationResponse_name,
    describeConfigurationResponse_tags,
    describeConfigurationResponse_httpStatus,

    -- ** DescribeConfigurationRevision
    describeConfigurationRevision_configurationRevision,
    describeConfigurationRevision_configurationId,
    describeConfigurationRevisionResponse_configurationId,
    describeConfigurationRevisionResponse_created,
    describeConfigurationRevisionResponse_data,
    describeConfigurationRevisionResponse_description,
    describeConfigurationRevisionResponse_httpStatus,

    -- ** DescribeUser
    describeUser_username,
    describeUser_brokerId,
    describeUserResponse_brokerId,
    describeUserResponse_consoleAccess,
    describeUserResponse_groups,
    describeUserResponse_pending,
    describeUserResponse_replicationUser,
    describeUserResponse_username,
    describeUserResponse_httpStatus,

    -- ** ListBrokers
    listBrokers_maxResults,
    listBrokers_nextToken,
    listBrokersResponse_brokerSummaries,
    listBrokersResponse_nextToken,
    listBrokersResponse_httpStatus,

    -- ** ListConfigurationRevisions
    listConfigurationRevisions_maxResults,
    listConfigurationRevisions_nextToken,
    listConfigurationRevisions_configurationId,
    listConfigurationRevisionsResponse_configurationId,
    listConfigurationRevisionsResponse_maxResults,
    listConfigurationRevisionsResponse_nextToken,
    listConfigurationRevisionsResponse_revisions,
    listConfigurationRevisionsResponse_httpStatus,

    -- ** ListConfigurations
    listConfigurations_maxResults,
    listConfigurations_nextToken,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_maxResults,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_httpStatus,

    -- ** ListTags
    listTags_resourceArn,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** ListUsers
    listUsers_maxResults,
    listUsers_nextToken,
    listUsers_brokerId,
    listUsersResponse_brokerId,
    listUsersResponse_maxResults,
    listUsersResponse_nextToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** Promote
    promote_brokerId,
    promote_mode,
    promoteResponse_brokerId,
    promoteResponse_httpStatus,

    -- ** RebootBroker
    rebootBroker_brokerId,
    rebootBrokerResponse_httpStatus,

    -- ** UpdateBroker
    updateBroker_authenticationStrategy,
    updateBroker_autoMinorVersionUpgrade,
    updateBroker_configuration,
    updateBroker_dataReplicationMode,
    updateBroker_engineVersion,
    updateBroker_hostInstanceType,
    updateBroker_ldapServerMetadata,
    updateBroker_logs,
    updateBroker_maintenanceWindowStartTime,
    updateBroker_securityGroups,
    updateBroker_brokerId,
    updateBrokerResponse_authenticationStrategy,
    updateBrokerResponse_autoMinorVersionUpgrade,
    updateBrokerResponse_brokerId,
    updateBrokerResponse_configuration,
    updateBrokerResponse_dataReplicationMetadata,
    updateBrokerResponse_dataReplicationMode,
    updateBrokerResponse_engineVersion,
    updateBrokerResponse_hostInstanceType,
    updateBrokerResponse_ldapServerMetadata,
    updateBrokerResponse_logs,
    updateBrokerResponse_maintenanceWindowStartTime,
    updateBrokerResponse_pendingDataReplicationMetadata,
    updateBrokerResponse_pendingDataReplicationMode,
    updateBrokerResponse_securityGroups,
    updateBrokerResponse_httpStatus,

    -- ** UpdateConfiguration
    updateConfiguration_description,
    updateConfiguration_configurationId,
    updateConfiguration_data,
    updateConfigurationResponse_arn,
    updateConfigurationResponse_created,
    updateConfigurationResponse_id,
    updateConfigurationResponse_latestRevision,
    updateConfigurationResponse_name,
    updateConfigurationResponse_warnings,
    updateConfigurationResponse_httpStatus,

    -- ** UpdateUser
    updateUser_consoleAccess,
    updateUser_groups,
    updateUser_password,
    updateUser_replicationUser,
    updateUser_username,
    updateUser_brokerId,
    updateUserResponse_httpStatus,

    -- * Types

    -- ** ActionRequired
    actionRequired_actionRequiredCode,
    actionRequired_actionRequiredInfo,

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
    brokerInstanceOption_availabilityZones,
    brokerInstanceOption_engineType,
    brokerInstanceOption_hostInstanceType,
    brokerInstanceOption_storageType,
    brokerInstanceOption_supportedDeploymentModes,
    brokerInstanceOption_supportedEngineVersions,

    -- ** BrokerSummary
    brokerSummary_brokerArn,
    brokerSummary_brokerId,
    brokerSummary_brokerName,
    brokerSummary_brokerState,
    brokerSummary_created,
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
    configurations_current,
    configurations_history,
    configurations_pending,

    -- ** DataReplicationCounterpart
    dataReplicationCounterpart_brokerId,
    dataReplicationCounterpart_region,

    -- ** DataReplicationMetadataOutput
    dataReplicationMetadataOutput_dataReplicationCounterpart,
    dataReplicationMetadataOutput_dataReplicationRole,

    -- ** EncryptionOptions
    encryptionOptions_kmsKeyId,
    encryptionOptions_useAwsOwnedKey,

    -- ** EngineVersion
    engineVersion_name,

    -- ** LdapServerMetadataInput
    ldapServerMetadataInput_roleName,
    ldapServerMetadataInput_roleSearchSubtree,
    ldapServerMetadataInput_userRoleName,
    ldapServerMetadataInput_userSearchSubtree,
    ldapServerMetadataInput_hosts,
    ldapServerMetadataInput_userSearchMatching,
    ldapServerMetadataInput_userBase,
    ldapServerMetadataInput_roleSearchMatching,
    ldapServerMetadataInput_serviceAccountUsername,
    ldapServerMetadataInput_roleBase,
    ldapServerMetadataInput_serviceAccountPassword,

    -- ** LdapServerMetadataOutput
    ldapServerMetadataOutput_roleName,
    ldapServerMetadataOutput_roleSearchSubtree,
    ldapServerMetadataOutput_userRoleName,
    ldapServerMetadataOutput_userSearchSubtree,
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
    logsSummary_audit,
    logsSummary_auditLogGroup,
    logsSummary_pending,
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
    user_consoleAccess,
    user_groups,
    user_replicationUser,
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
import Amazonka.MQ.Promote
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
import Amazonka.MQ.Types.DataReplicationCounterpart
import Amazonka.MQ.Types.DataReplicationMetadataOutput
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
