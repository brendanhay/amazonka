{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SMS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Lens
  ( -- * Operations

    -- ** CreateApp
    createApp_tags,
    createApp_name,
    createApp_clientToken,
    createApp_roleName,
    createApp_description,
    createApp_serverGroups,
    createAppResponse_tags,
    createAppResponse_appSummary,
    createAppResponse_serverGroups,
    createAppResponse_httpStatus,

    -- ** CreateReplicationJob
    createReplicationJob_roleName,
    createReplicationJob_licenseType,
    createReplicationJob_frequency,
    createReplicationJob_runOnce,
    createReplicationJob_description,
    createReplicationJob_encrypted,
    createReplicationJob_kmsKeyId,
    createReplicationJob_numberOfRecentAmisToKeep,
    createReplicationJob_serverId,
    createReplicationJob_seedReplicationTime,
    createReplicationJobResponse_replicationJobId,
    createReplicationJobResponse_httpStatus,

    -- ** DeleteApp
    deleteApp_forceStopAppReplication,
    deleteApp_forceTerminateApp,
    deleteApp_appId,
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
    generateChangeSet_changesetFormat,
    generateChangeSet_appId,
    generateChangeSetResponse_s3Location,
    generateChangeSetResponse_httpStatus,

    -- ** GenerateTemplate
    generateTemplate_templateFormat,
    generateTemplate_appId,
    generateTemplateResponse_s3Location,
    generateTemplateResponse_httpStatus,

    -- ** GetApp
    getApp_appId,
    getAppResponse_tags,
    getAppResponse_appSummary,
    getAppResponse_serverGroups,
    getAppResponse_httpStatus,

    -- ** GetAppLaunchConfiguration
    getAppLaunchConfiguration_appId,
    getAppLaunchConfigurationResponse_roleName,
    getAppLaunchConfigurationResponse_serverGroupLaunchConfigurations,
    getAppLaunchConfigurationResponse_autoLaunch,
    getAppLaunchConfigurationResponse_appId,
    getAppLaunchConfigurationResponse_httpStatus,

    -- ** GetAppReplicationConfiguration
    getAppReplicationConfiguration_appId,
    getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations,
    getAppReplicationConfigurationResponse_httpStatus,

    -- ** GetAppValidationConfiguration
    getAppValidationConfiguration_appId,
    getAppValidationConfigurationResponse_serverGroupValidationConfigurations,
    getAppValidationConfigurationResponse_appValidationConfigurations,
    getAppValidationConfigurationResponse_httpStatus,

    -- ** GetAppValidationOutput
    getAppValidationOutput_appId,
    getAppValidationOutputResponse_validationOutputList,
    getAppValidationOutputResponse_httpStatus,

    -- ** GetConnectors
    getConnectors_nextToken,
    getConnectors_maxResults,
    getConnectorsResponse_nextToken,
    getConnectorsResponse_connectorList,
    getConnectorsResponse_httpStatus,

    -- ** GetReplicationJobs
    getReplicationJobs_nextToken,
    getReplicationJobs_replicationJobId,
    getReplicationJobs_maxResults,
    getReplicationJobsResponse_nextToken,
    getReplicationJobsResponse_replicationJobList,
    getReplicationJobsResponse_httpStatus,

    -- ** GetReplicationRuns
    getReplicationRuns_nextToken,
    getReplicationRuns_maxResults,
    getReplicationRuns_replicationJobId,
    getReplicationRunsResponse_replicationRunList,
    getReplicationRunsResponse_nextToken,
    getReplicationRunsResponse_replicationJob,
    getReplicationRunsResponse_httpStatus,

    -- ** GetServers
    getServers_nextToken,
    getServers_vmServerAddressList,
    getServers_maxResults,
    getServersResponse_nextToken,
    getServersResponse_lastModifiedOn,
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
    listApps_nextToken,
    listApps_appIds,
    listApps_maxResults,
    listAppsResponse_nextToken,
    listAppsResponse_apps,
    listAppsResponse_httpStatus,

    -- ** NotifyAppValidationOutput
    notifyAppValidationOutput_notificationContext,
    notifyAppValidationOutput_appId,
    notifyAppValidationOutputResponse_httpStatus,

    -- ** PutAppLaunchConfiguration
    putAppLaunchConfiguration_roleName,
    putAppLaunchConfiguration_serverGroupLaunchConfigurations,
    putAppLaunchConfiguration_autoLaunch,
    putAppLaunchConfiguration_appId,
    putAppLaunchConfigurationResponse_httpStatus,

    -- ** PutAppReplicationConfiguration
    putAppReplicationConfiguration_serverGroupReplicationConfigurations,
    putAppReplicationConfiguration_appId,
    putAppReplicationConfigurationResponse_httpStatus,

    -- ** PutAppValidationConfiguration
    putAppValidationConfiguration_serverGroupValidationConfigurations,
    putAppValidationConfiguration_appValidationConfigurations,
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
    updateApp_tags,
    updateApp_name,
    updateApp_roleName,
    updateApp_description,
    updateApp_serverGroups,
    updateApp_appId,
    updateAppResponse_tags,
    updateAppResponse_appSummary,
    updateAppResponse_serverGroups,
    updateAppResponse_httpStatus,

    -- ** UpdateReplicationJob
    updateReplicationJob_roleName,
    updateReplicationJob_licenseType,
    updateReplicationJob_frequency,
    updateReplicationJob_description,
    updateReplicationJob_encrypted,
    updateReplicationJob_kmsKeyId,
    updateReplicationJob_nextReplicationRunStartTime,
    updateReplicationJob_numberOfRecentAmisToKeep,
    updateReplicationJob_replicationJobId,
    updateReplicationJobResponse_httpStatus,

    -- * Types

    -- ** AppSummary
    appSummary_launchStatus,
    appSummary_replicationConfigurationStatus,
    appSummary_name,
    appSummary_roleName,
    appSummary_replicationStatusMessage,
    appSummary_totalServerGroups,
    appSummary_launchDetails,
    appSummary_replicationStatus,
    appSummary_totalServers,
    appSummary_status,
    appSummary_description,
    appSummary_latestReplicationTime,
    appSummary_lastModified,
    appSummary_creationTime,
    appSummary_launchConfigurationStatus,
    appSummary_launchStatusMessage,
    appSummary_importedAppId,
    appSummary_statusMessage,
    appSummary_appId,

    -- ** AppValidationConfiguration
    appValidationConfiguration_name,
    appValidationConfiguration_ssmValidationParameters,
    appValidationConfiguration_appValidationStrategy,
    appValidationConfiguration_validationId,

    -- ** AppValidationOutput
    appValidationOutput_ssmOutput,

    -- ** Connector
    connector_associatedOn,
    connector_vmManagerName,
    connector_vmManagerId,
    connector_connectorId,
    connector_capabilityList,
    connector_status,
    connector_macAddress,
    connector_vmManagerType,
    connector_version,
    connector_ipAddress,

    -- ** LaunchDetails
    launchDetails_stackId,
    launchDetails_latestLaunchTime,
    launchDetails_stackName,

    -- ** NotificationContext
    notificationContext_validationId,
    notificationContext_status,
    notificationContext_statusMessage,

    -- ** ReplicationJob
    replicationJob_replicationRunList,
    replicationJob_roleName,
    replicationJob_licenseType,
    replicationJob_frequency,
    replicationJob_runOnce,
    replicationJob_state,
    replicationJob_latestAmiId,
    replicationJob_description,
    replicationJob_serverType,
    replicationJob_replicationJobId,
    replicationJob_encrypted,
    replicationJob_seedReplicationTime,
    replicationJob_kmsKeyId,
    replicationJob_serverId,
    replicationJob_vmServer,
    replicationJob_nextReplicationRunStartTime,
    replicationJob_statusMessage,
    replicationJob_numberOfRecentAmisToKeep,

    -- ** ReplicationRun
    replicationRun_scheduledStartTime,
    replicationRun_amiId,
    replicationRun_type,
    replicationRun_completedTime,
    replicationRun_state,
    replicationRun_stageDetails,
    replicationRun_description,
    replicationRun_replicationRunId,
    replicationRun_encrypted,
    replicationRun_kmsKeyId,
    replicationRun_statusMessage,

    -- ** ReplicationRunStageDetails
    replicationRunStageDetails_stageProgress,
    replicationRunStageDetails_stage,

    -- ** S3Location
    s3Location_key,
    s3Location_bucket,

    -- ** SSMOutput
    sSMOutput_s3Location,

    -- ** SSMValidationParameters
    sSMValidationParameters_command,
    sSMValidationParameters_instanceId,
    sSMValidationParameters_source,
    sSMValidationParameters_executionTimeoutSeconds,
    sSMValidationParameters_scriptType,
    sSMValidationParameters_outputS3BucketName,

    -- ** Server
    server_serverType,
    server_replicationJobId,
    server_serverId,
    server_vmServer,
    server_replicationJobTerminated,

    -- ** ServerGroup
    serverGroup_name,
    serverGroup_serverGroupId,
    serverGroup_serverList,

    -- ** ServerGroupLaunchConfiguration
    serverGroupLaunchConfiguration_serverGroupId,
    serverGroupLaunchConfiguration_serverLaunchConfigurations,
    serverGroupLaunchConfiguration_launchOrder,

    -- ** ServerGroupReplicationConfiguration
    serverGroupReplicationConfiguration_serverReplicationConfigurations,
    serverGroupReplicationConfiguration_serverGroupId,

    -- ** ServerGroupValidationConfiguration
    serverGroupValidationConfiguration_serverGroupId,
    serverGroupValidationConfiguration_serverValidationConfigurations,

    -- ** ServerLaunchConfiguration
    serverLaunchConfiguration_ec2KeyName,
    serverLaunchConfiguration_vpc,
    serverLaunchConfiguration_userData,
    serverLaunchConfiguration_associatePublicIpAddress,
    serverLaunchConfiguration_securityGroup,
    serverLaunchConfiguration_configureScriptType,
    serverLaunchConfiguration_instanceType,
    serverLaunchConfiguration_server,
    serverLaunchConfiguration_logicalId,
    serverLaunchConfiguration_subnet,
    serverLaunchConfiguration_iamInstanceProfileName,
    serverLaunchConfiguration_configureScript,

    -- ** ServerReplicationConfiguration
    serverReplicationConfiguration_serverReplicationParameters,
    serverReplicationConfiguration_server,

    -- ** ServerReplicationParameters
    serverReplicationParameters_seedTime,
    serverReplicationParameters_licenseType,
    serverReplicationParameters_frequency,
    serverReplicationParameters_runOnce,
    serverReplicationParameters_encrypted,
    serverReplicationParameters_kmsKeyId,
    serverReplicationParameters_numberOfRecentAmisToKeep,

    -- ** ServerValidationConfiguration
    serverValidationConfiguration_name,
    serverValidationConfiguration_userDataValidationParameters,
    serverValidationConfiguration_validationId,
    serverValidationConfiguration_serverValidationStrategy,
    serverValidationConfiguration_server,

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
    userDataValidationParameters_source,
    userDataValidationParameters_scriptType,

    -- ** ValidationOutput
    validationOutput_name,
    validationOutput_validationId,
    validationOutput_status,
    validationOutput_serverValidationOutput,
    validationOutput_appValidationOutput,
    validationOutput_statusMessage,
    validationOutput_latestValidationTime,

    -- ** VmServer
    vmServer_vmServerAddress,
    vmServer_vmManagerName,
    vmServer_vmName,
    vmServer_vmManagerType,
    vmServer_vmPath,

    -- ** VmServerAddress
    vmServerAddress_vmManagerId,
    vmServerAddress_vmId,
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
