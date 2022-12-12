{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SMS.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Lens
  ( -- * Operations

    -- ** CreateApp
    createApp_clientToken,
    createApp_description,
    createApp_name,
    createApp_roleName,
    createApp_serverGroups,
    createApp_tags,
    createAppResponse_appSummary,
    createAppResponse_serverGroups,
    createAppResponse_tags,
    createAppResponse_httpStatus,

    -- ** CreateReplicationJob
    createReplicationJob_description,
    createReplicationJob_encrypted,
    createReplicationJob_frequency,
    createReplicationJob_kmsKeyId,
    createReplicationJob_licenseType,
    createReplicationJob_numberOfRecentAmisToKeep,
    createReplicationJob_roleName,
    createReplicationJob_runOnce,
    createReplicationJob_serverId,
    createReplicationJob_seedReplicationTime,
    createReplicationJobResponse_replicationJobId,
    createReplicationJobResponse_httpStatus,

    -- ** DeleteApp
    deleteApp_appId,
    deleteApp_forceStopAppReplication,
    deleteApp_forceTerminateApp,
    deleteAppResponse_httpStatus,

    -- ** DeleteAppLaunchConfiguration
    deleteAppLaunchConfiguration_appId,
    deleteAppLaunchConfigurationResponse_httpStatus,

    -- ** DeleteAppReplicationConfiguration
    deleteAppReplicationConfiguration_appId,
    deleteAppReplicationConfigurationResponse_httpStatus,

    -- ** DeleteAppValidationConfiguration
    deleteAppValidationConfiguration_appId,
    deleteAppValidationConfigurationResponse_httpStatus,

    -- ** DeleteReplicationJob
    deleteReplicationJob_replicationJobId,
    deleteReplicationJobResponse_httpStatus,

    -- ** DeleteServerCatalog
    deleteServerCatalogResponse_httpStatus,

    -- ** DisassociateConnector
    disassociateConnector_connectorId,
    disassociateConnectorResponse_httpStatus,

    -- ** GenerateChangeSet
    generateChangeSet_appId,
    generateChangeSet_changesetFormat,
    generateChangeSetResponse_s3Location,
    generateChangeSetResponse_httpStatus,

    -- ** GenerateTemplate
    generateTemplate_appId,
    generateTemplate_templateFormat,
    generateTemplateResponse_s3Location,
    generateTemplateResponse_httpStatus,

    -- ** GetApp
    getApp_appId,
    getAppResponse_appSummary,
    getAppResponse_serverGroups,
    getAppResponse_tags,
    getAppResponse_httpStatus,

    -- ** GetAppLaunchConfiguration
    getAppLaunchConfiguration_appId,
    getAppLaunchConfigurationResponse_appId,
    getAppLaunchConfigurationResponse_autoLaunch,
    getAppLaunchConfigurationResponse_roleName,
    getAppLaunchConfigurationResponse_serverGroupLaunchConfigurations,
    getAppLaunchConfigurationResponse_httpStatus,

    -- ** GetAppReplicationConfiguration
    getAppReplicationConfiguration_appId,
    getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations,
    getAppReplicationConfigurationResponse_httpStatus,

    -- ** GetAppValidationConfiguration
    getAppValidationConfiguration_appId,
    getAppValidationConfigurationResponse_appValidationConfigurations,
    getAppValidationConfigurationResponse_serverGroupValidationConfigurations,
    getAppValidationConfigurationResponse_httpStatus,

    -- ** GetAppValidationOutput
    getAppValidationOutput_appId,
    getAppValidationOutputResponse_validationOutputList,
    getAppValidationOutputResponse_httpStatus,

    -- ** GetConnectors
    getConnectors_maxResults,
    getConnectors_nextToken,
    getConnectorsResponse_connectorList,
    getConnectorsResponse_nextToken,
    getConnectorsResponse_httpStatus,

    -- ** GetReplicationJobs
    getReplicationJobs_maxResults,
    getReplicationJobs_nextToken,
    getReplicationJobs_replicationJobId,
    getReplicationJobsResponse_nextToken,
    getReplicationJobsResponse_replicationJobList,
    getReplicationJobsResponse_httpStatus,

    -- ** GetReplicationRuns
    getReplicationRuns_maxResults,
    getReplicationRuns_nextToken,
    getReplicationRuns_replicationJobId,
    getReplicationRunsResponse_nextToken,
    getReplicationRunsResponse_replicationJob,
    getReplicationRunsResponse_replicationRunList,
    getReplicationRunsResponse_httpStatus,

    -- ** GetServers
    getServers_maxResults,
    getServers_nextToken,
    getServers_vmServerAddressList,
    getServersResponse_lastModifiedOn,
    getServersResponse_nextToken,
    getServersResponse_serverCatalogStatus,
    getServersResponse_serverList,
    getServersResponse_httpStatus,

    -- ** ImportAppCatalog
    importAppCatalog_roleName,
    importAppCatalogResponse_httpStatus,

    -- ** ImportServerCatalog
    importServerCatalogResponse_httpStatus,

    -- ** LaunchApp
    launchApp_appId,
    launchAppResponse_httpStatus,

    -- ** ListApps
    listApps_appIds,
    listApps_maxResults,
    listApps_nextToken,
    listAppsResponse_apps,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,

    -- ** NotifyAppValidationOutput
    notifyAppValidationOutput_notificationContext,
    notifyAppValidationOutput_appId,
    notifyAppValidationOutputResponse_httpStatus,

    -- ** PutAppLaunchConfiguration
    putAppLaunchConfiguration_appId,
    putAppLaunchConfiguration_autoLaunch,
    putAppLaunchConfiguration_roleName,
    putAppLaunchConfiguration_serverGroupLaunchConfigurations,
    putAppLaunchConfigurationResponse_httpStatus,

    -- ** PutAppReplicationConfiguration
    putAppReplicationConfiguration_appId,
    putAppReplicationConfiguration_serverGroupReplicationConfigurations,
    putAppReplicationConfigurationResponse_httpStatus,

    -- ** PutAppValidationConfiguration
    putAppValidationConfiguration_appValidationConfigurations,
    putAppValidationConfiguration_serverGroupValidationConfigurations,
    putAppValidationConfiguration_appId,
    putAppValidationConfigurationResponse_httpStatus,

    -- ** StartAppReplication
    startAppReplication_appId,
    startAppReplicationResponse_httpStatus,

    -- ** StartOnDemandAppReplication
    startOnDemandAppReplication_description,
    startOnDemandAppReplication_appId,
    startOnDemandAppReplicationResponse_httpStatus,

    -- ** StartOnDemandReplicationRun
    startOnDemandReplicationRun_description,
    startOnDemandReplicationRun_replicationJobId,
    startOnDemandReplicationRunResponse_replicationRunId,
    startOnDemandReplicationRunResponse_httpStatus,

    -- ** StopAppReplication
    stopAppReplication_appId,
    stopAppReplicationResponse_httpStatus,

    -- ** TerminateApp
    terminateApp_appId,
    terminateAppResponse_httpStatus,

    -- ** UpdateApp
    updateApp_appId,
    updateApp_description,
    updateApp_name,
    updateApp_roleName,
    updateApp_serverGroups,
    updateApp_tags,
    updateAppResponse_appSummary,
    updateAppResponse_serverGroups,
    updateAppResponse_tags,
    updateAppResponse_httpStatus,

    -- ** UpdateReplicationJob
    updateReplicationJob_description,
    updateReplicationJob_encrypted,
    updateReplicationJob_frequency,
    updateReplicationJob_kmsKeyId,
    updateReplicationJob_licenseType,
    updateReplicationJob_nextReplicationRunStartTime,
    updateReplicationJob_numberOfRecentAmisToKeep,
    updateReplicationJob_roleName,
    updateReplicationJob_replicationJobId,
    updateReplicationJobResponse_httpStatus,

    -- * Types

    -- ** AppSummary
    appSummary_appId,
    appSummary_creationTime,
    appSummary_description,
    appSummary_importedAppId,
    appSummary_lastModified,
    appSummary_latestReplicationTime,
    appSummary_launchConfigurationStatus,
    appSummary_launchDetails,
    appSummary_launchStatus,
    appSummary_launchStatusMessage,
    appSummary_name,
    appSummary_replicationConfigurationStatus,
    appSummary_replicationStatus,
    appSummary_replicationStatusMessage,
    appSummary_roleName,
    appSummary_status,
    appSummary_statusMessage,
    appSummary_totalServerGroups,
    appSummary_totalServers,

    -- ** AppValidationConfiguration
    appValidationConfiguration_appValidationStrategy,
    appValidationConfiguration_name,
    appValidationConfiguration_ssmValidationParameters,
    appValidationConfiguration_validationId,

    -- ** AppValidationOutput
    appValidationOutput_ssmOutput,

    -- ** Connector
    connector_associatedOn,
    connector_capabilityList,
    connector_connectorId,
    connector_ipAddress,
    connector_macAddress,
    connector_status,
    connector_version,
    connector_vmManagerId,
    connector_vmManagerName,
    connector_vmManagerType,

    -- ** LaunchDetails
    launchDetails_latestLaunchTime,
    launchDetails_stackId,
    launchDetails_stackName,

    -- ** NotificationContext
    notificationContext_status,
    notificationContext_statusMessage,
    notificationContext_validationId,

    -- ** ReplicationJob
    replicationJob_description,
    replicationJob_encrypted,
    replicationJob_frequency,
    replicationJob_kmsKeyId,
    replicationJob_latestAmiId,
    replicationJob_licenseType,
    replicationJob_nextReplicationRunStartTime,
    replicationJob_numberOfRecentAmisToKeep,
    replicationJob_replicationJobId,
    replicationJob_replicationRunList,
    replicationJob_roleName,
    replicationJob_runOnce,
    replicationJob_seedReplicationTime,
    replicationJob_serverId,
    replicationJob_serverType,
    replicationJob_state,
    replicationJob_statusMessage,
    replicationJob_vmServer,

    -- ** ReplicationRun
    replicationRun_amiId,
    replicationRun_completedTime,
    replicationRun_description,
    replicationRun_encrypted,
    replicationRun_kmsKeyId,
    replicationRun_replicationRunId,
    replicationRun_scheduledStartTime,
    replicationRun_stageDetails,
    replicationRun_state,
    replicationRun_statusMessage,
    replicationRun_type,

    -- ** ReplicationRunStageDetails
    replicationRunStageDetails_stage,
    replicationRunStageDetails_stageProgress,

    -- ** S3Location
    s3Location_bucket,
    s3Location_key,

    -- ** SSMOutput
    sSMOutput_s3Location,

    -- ** SSMValidationParameters
    sSMValidationParameters_command,
    sSMValidationParameters_executionTimeoutSeconds,
    sSMValidationParameters_instanceId,
    sSMValidationParameters_outputS3BucketName,
    sSMValidationParameters_scriptType,
    sSMValidationParameters_source,

    -- ** Server
    server_replicationJobId,
    server_replicationJobTerminated,
    server_serverId,
    server_serverType,
    server_vmServer,

    -- ** ServerGroup
    serverGroup_name,
    serverGroup_serverGroupId,
    serverGroup_serverList,

    -- ** ServerGroupLaunchConfiguration
    serverGroupLaunchConfiguration_launchOrder,
    serverGroupLaunchConfiguration_serverGroupId,
    serverGroupLaunchConfiguration_serverLaunchConfigurations,

    -- ** ServerGroupReplicationConfiguration
    serverGroupReplicationConfiguration_serverGroupId,
    serverGroupReplicationConfiguration_serverReplicationConfigurations,

    -- ** ServerGroupValidationConfiguration
    serverGroupValidationConfiguration_serverGroupId,
    serverGroupValidationConfiguration_serverValidationConfigurations,

    -- ** ServerLaunchConfiguration
    serverLaunchConfiguration_associatePublicIpAddress,
    serverLaunchConfiguration_configureScript,
    serverLaunchConfiguration_configureScriptType,
    serverLaunchConfiguration_ec2KeyName,
    serverLaunchConfiguration_iamInstanceProfileName,
    serverLaunchConfiguration_instanceType,
    serverLaunchConfiguration_logicalId,
    serverLaunchConfiguration_securityGroup,
    serverLaunchConfiguration_server,
    serverLaunchConfiguration_subnet,
    serverLaunchConfiguration_userData,
    serverLaunchConfiguration_vpc,

    -- ** ServerReplicationConfiguration
    serverReplicationConfiguration_server,
    serverReplicationConfiguration_serverReplicationParameters,

    -- ** ServerReplicationParameters
    serverReplicationParameters_encrypted,
    serverReplicationParameters_frequency,
    serverReplicationParameters_kmsKeyId,
    serverReplicationParameters_licenseType,
    serverReplicationParameters_numberOfRecentAmisToKeep,
    serverReplicationParameters_runOnce,
    serverReplicationParameters_seedTime,

    -- ** ServerValidationConfiguration
    serverValidationConfiguration_name,
    serverValidationConfiguration_server,
    serverValidationConfiguration_serverValidationStrategy,
    serverValidationConfiguration_userDataValidationParameters,
    serverValidationConfiguration_validationId,

    -- ** ServerValidationOutput
    serverValidationOutput_server,

    -- ** Source
    source_s3Location,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UserData
    userData_s3Location,

    -- ** UserDataValidationParameters
    userDataValidationParameters_scriptType,
    userDataValidationParameters_source,

    -- ** ValidationOutput
    validationOutput_appValidationOutput,
    validationOutput_latestValidationTime,
    validationOutput_name,
    validationOutput_serverValidationOutput,
    validationOutput_status,
    validationOutput_statusMessage,
    validationOutput_validationId,

    -- ** VmServer
    vmServer_vmManagerName,
    vmServer_vmManagerType,
    vmServer_vmName,
    vmServer_vmPath,
    vmServer_vmServerAddress,

    -- ** VmServerAddress
    vmServerAddress_vmId,
    vmServerAddress_vmManagerId,
  )
where

import Amazonka.SMS.CreateApp
import Amazonka.SMS.CreateReplicationJob
import Amazonka.SMS.DeleteApp
import Amazonka.SMS.DeleteAppLaunchConfiguration
import Amazonka.SMS.DeleteAppReplicationConfiguration
import Amazonka.SMS.DeleteAppValidationConfiguration
import Amazonka.SMS.DeleteReplicationJob
import Amazonka.SMS.DeleteServerCatalog
import Amazonka.SMS.DisassociateConnector
import Amazonka.SMS.GenerateChangeSet
import Amazonka.SMS.GenerateTemplate
import Amazonka.SMS.GetApp
import Amazonka.SMS.GetAppLaunchConfiguration
import Amazonka.SMS.GetAppReplicationConfiguration
import Amazonka.SMS.GetAppValidationConfiguration
import Amazonka.SMS.GetAppValidationOutput
import Amazonka.SMS.GetConnectors
import Amazonka.SMS.GetReplicationJobs
import Amazonka.SMS.GetReplicationRuns
import Amazonka.SMS.GetServers
import Amazonka.SMS.ImportAppCatalog
import Amazonka.SMS.ImportServerCatalog
import Amazonka.SMS.LaunchApp
import Amazonka.SMS.ListApps
import Amazonka.SMS.NotifyAppValidationOutput
import Amazonka.SMS.PutAppLaunchConfiguration
import Amazonka.SMS.PutAppReplicationConfiguration
import Amazonka.SMS.PutAppValidationConfiguration
import Amazonka.SMS.StartAppReplication
import Amazonka.SMS.StartOnDemandAppReplication
import Amazonka.SMS.StartOnDemandReplicationRun
import Amazonka.SMS.StopAppReplication
import Amazonka.SMS.TerminateApp
import Amazonka.SMS.Types.AppSummary
import Amazonka.SMS.Types.AppValidationConfiguration
import Amazonka.SMS.Types.AppValidationOutput
import Amazonka.SMS.Types.Connector
import Amazonka.SMS.Types.LaunchDetails
import Amazonka.SMS.Types.NotificationContext
import Amazonka.SMS.Types.ReplicationJob
import Amazonka.SMS.Types.ReplicationRun
import Amazonka.SMS.Types.ReplicationRunStageDetails
import Amazonka.SMS.Types.S3Location
import Amazonka.SMS.Types.SSMOutput
import Amazonka.SMS.Types.SSMValidationParameters
import Amazonka.SMS.Types.Server
import Amazonka.SMS.Types.ServerGroup
import Amazonka.SMS.Types.ServerGroupLaunchConfiguration
import Amazonka.SMS.Types.ServerGroupReplicationConfiguration
import Amazonka.SMS.Types.ServerGroupValidationConfiguration
import Amazonka.SMS.Types.ServerLaunchConfiguration
import Amazonka.SMS.Types.ServerReplicationConfiguration
import Amazonka.SMS.Types.ServerReplicationParameters
import Amazonka.SMS.Types.ServerValidationConfiguration
import Amazonka.SMS.Types.ServerValidationOutput
import Amazonka.SMS.Types.Source
import Amazonka.SMS.Types.Tag
import Amazonka.SMS.Types.UserData
import Amazonka.SMS.Types.UserDataValidationParameters
import Amazonka.SMS.Types.ValidationOutput
import Amazonka.SMS.Types.VmServer
import Amazonka.SMS.Types.VmServerAddress
import Amazonka.SMS.UpdateApp
import Amazonka.SMS.UpdateReplicationJob
