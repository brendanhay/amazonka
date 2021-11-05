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

    -- ** DeleteAppReplicationConfiguration
    deleteAppReplicationConfiguration_appId,
    deleteAppReplicationConfigurationResponse_httpStatus,

    -- ** PutAppReplicationConfiguration
    putAppReplicationConfiguration_appId,
    putAppReplicationConfiguration_serverGroupReplicationConfigurations,
    putAppReplicationConfigurationResponse_httpStatus,

    -- ** DeleteServerCatalog
    deleteServerCatalogResponse_httpStatus,

    -- ** ImportAppCatalog
    importAppCatalog_roleName,
    importAppCatalogResponse_httpStatus,

    -- ** GetAppLaunchConfiguration
    getAppLaunchConfiguration_appId,
    getAppLaunchConfigurationResponse_serverGroupLaunchConfigurations,
    getAppLaunchConfigurationResponse_autoLaunch,
    getAppLaunchConfigurationResponse_roleName,
    getAppLaunchConfigurationResponse_appId,
    getAppLaunchConfigurationResponse_httpStatus,

    -- ** DeleteAppLaunchConfiguration
    deleteAppLaunchConfiguration_appId,
    deleteAppLaunchConfigurationResponse_httpStatus,

    -- ** StartAppReplication
    startAppReplication_appId,
    startAppReplicationResponse_httpStatus,

    -- ** PutAppLaunchConfiguration
    putAppLaunchConfiguration_serverGroupLaunchConfigurations,
    putAppLaunchConfiguration_autoLaunch,
    putAppLaunchConfiguration_roleName,
    putAppLaunchConfiguration_appId,
    putAppLaunchConfigurationResponse_httpStatus,

    -- ** GetReplicationRuns
    getReplicationRuns_nextToken,
    getReplicationRuns_maxResults,
    getReplicationRuns_replicationJobId,
    getReplicationRunsResponse_replicationJob,
    getReplicationRunsResponse_nextToken,
    getReplicationRunsResponse_replicationRunList,
    getReplicationRunsResponse_httpStatus,

    -- ** TerminateApp
    terminateApp_appId,
    terminateAppResponse_httpStatus,

    -- ** ListApps
    listApps_appIds,
    listApps_nextToken,
    listApps_maxResults,
    listAppsResponse_apps,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,

    -- ** GetServers
    getServers_vmServerAddressList,
    getServers_nextToken,
    getServers_maxResults,
    getServersResponse_serverCatalogStatus,
    getServersResponse_lastModifiedOn,
    getServersResponse_nextToken,
    getServersResponse_serverList,
    getServersResponse_httpStatus,

    -- ** DeleteApp
    deleteApp_forceTerminateApp,
    deleteApp_appId,
    deleteApp_forceStopAppReplication,
    deleteAppResponse_httpStatus,

    -- ** UpdateApp
    updateApp_roleName,
    updateApp_appId,
    updateApp_name,
    updateApp_description,
    updateApp_serverGroups,
    updateApp_tags,
    updateAppResponse_appSummary,
    updateAppResponse_serverGroups,
    updateAppResponse_tags,
    updateAppResponse_httpStatus,

    -- ** StartOnDemandAppReplication
    startOnDemandAppReplication_description,
    startOnDemandAppReplication_appId,
    startOnDemandAppReplicationResponse_httpStatus,

    -- ** ImportServerCatalog
    importServerCatalogResponse_httpStatus,

    -- ** GenerateTemplate
    generateTemplate_appId,
    generateTemplate_templateFormat,
    generateTemplateResponse_s3Location,
    generateTemplateResponse_httpStatus,

    -- ** GetConnectors
    getConnectors_nextToken,
    getConnectors_maxResults,
    getConnectorsResponse_connectorList,
    getConnectorsResponse_nextToken,
    getConnectorsResponse_httpStatus,

    -- ** GetReplicationJobs
    getReplicationJobs_replicationJobId,
    getReplicationJobs_nextToken,
    getReplicationJobs_maxResults,
    getReplicationJobsResponse_replicationJobList,
    getReplicationJobsResponse_nextToken,
    getReplicationJobsResponse_httpStatus,

    -- ** DisassociateConnector
    disassociateConnector_connectorId,
    disassociateConnectorResponse_httpStatus,

    -- ** LaunchApp
    launchApp_appId,
    launchAppResponse_httpStatus,

    -- ** GetAppValidationConfiguration
    getAppValidationConfiguration_appId,
    getAppValidationConfigurationResponse_serverGroupValidationConfigurations,
    getAppValidationConfigurationResponse_appValidationConfigurations,
    getAppValidationConfigurationResponse_httpStatus,

    -- ** CreateReplicationJob
    createReplicationJob_frequency,
    createReplicationJob_numberOfRecentAmisToKeep,
    createReplicationJob_licenseType,
    createReplicationJob_roleName,
    createReplicationJob_encrypted,
    createReplicationJob_kmsKeyId,
    createReplicationJob_runOnce,
    createReplicationJob_description,
    createReplicationJob_serverId,
    createReplicationJob_seedReplicationTime,
    createReplicationJobResponse_replicationJobId,
    createReplicationJobResponse_httpStatus,

    -- ** GenerateChangeSet
    generateChangeSet_appId,
    generateChangeSet_changesetFormat,
    generateChangeSetResponse_s3Location,
    generateChangeSetResponse_httpStatus,

    -- ** GetApp
    getApp_appId,
    getAppResponse_appSummary,
    getAppResponse_serverGroups,
    getAppResponse_tags,
    getAppResponse_httpStatus,

    -- ** UpdateReplicationJob
    updateReplicationJob_frequency,
    updateReplicationJob_numberOfRecentAmisToKeep,
    updateReplicationJob_licenseType,
    updateReplicationJob_roleName,
    updateReplicationJob_encrypted,
    updateReplicationJob_nextReplicationRunStartTime,
    updateReplicationJob_kmsKeyId,
    updateReplicationJob_description,
    updateReplicationJob_replicationJobId,
    updateReplicationJobResponse_httpStatus,

    -- ** DeleteReplicationJob
    deleteReplicationJob_replicationJobId,
    deleteReplicationJobResponse_httpStatus,

    -- ** CreateApp
    createApp_clientToken,
    createApp_roleName,
    createApp_name,
    createApp_description,
    createApp_serverGroups,
    createApp_tags,
    createAppResponse_appSummary,
    createAppResponse_serverGroups,
    createAppResponse_tags,
    createAppResponse_httpStatus,

    -- ** StopAppReplication
    stopAppReplication_appId,
    stopAppReplicationResponse_httpStatus,

    -- ** DeleteAppValidationConfiguration
    deleteAppValidationConfiguration_appId,
    deleteAppValidationConfigurationResponse_httpStatus,

    -- ** PutAppValidationConfiguration
    putAppValidationConfiguration_serverGroupValidationConfigurations,
    putAppValidationConfiguration_appValidationConfigurations,
    putAppValidationConfiguration_appId,
    putAppValidationConfigurationResponse_httpStatus,

    -- ** GetAppValidationOutput
    getAppValidationOutput_appId,
    getAppValidationOutputResponse_validationOutputList,
    getAppValidationOutputResponse_httpStatus,

    -- ** GetAppReplicationConfiguration
    getAppReplicationConfiguration_appId,
    getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations,
    getAppReplicationConfigurationResponse_httpStatus,

    -- ** StartOnDemandReplicationRun
    startOnDemandReplicationRun_description,
    startOnDemandReplicationRun_replicationJobId,
    startOnDemandReplicationRunResponse_replicationRunId,
    startOnDemandReplicationRunResponse_httpStatus,

    -- ** NotifyAppValidationOutput
    notifyAppValidationOutput_notificationContext,
    notifyAppValidationOutput_appId,
    notifyAppValidationOutputResponse_httpStatus,

    -- * Types

    -- ** AppSummary
    appSummary_creationTime,
    appSummary_totalServers,
    appSummary_status,
    appSummary_launchDetails,
    appSummary_launchStatusMessage,
    appSummary_replicationConfigurationStatus,
    appSummary_replicationStatusMessage,
    appSummary_totalServerGroups,
    appSummary_roleName,
    appSummary_launchConfigurationStatus,
    appSummary_launchStatus,
    appSummary_appId,
    appSummary_name,
    appSummary_statusMessage,
    appSummary_latestReplicationTime,
    appSummary_importedAppId,
    appSummary_replicationStatus,
    appSummary_lastModified,
    appSummary_description,

    -- ** AppValidationConfiguration
    appValidationConfiguration_ssmValidationParameters,
    appValidationConfiguration_name,
    appValidationConfiguration_validationId,
    appValidationConfiguration_appValidationStrategy,

    -- ** AppValidationOutput
    appValidationOutput_ssmOutput,

    -- ** Connector
    connector_status,
    connector_vmManagerName,
    connector_ipAddress,
    connector_vmManagerId,
    connector_vmManagerType,
    connector_connectorId,
    connector_associatedOn,
    connector_macAddress,
    connector_version,
    connector_capabilityList,

    -- ** LaunchDetails
    launchDetails_stackId,
    launchDetails_latestLaunchTime,
    launchDetails_stackName,

    -- ** NotificationContext
    notificationContext_status,
    notificationContext_statusMessage,
    notificationContext_validationId,

    -- ** ReplicationJob
    replicationJob_frequency,
    replicationJob_numberOfRecentAmisToKeep,
    replicationJob_state,
    replicationJob_serverType,
    replicationJob_serverId,
    replicationJob_licenseType,
    replicationJob_roleName,
    replicationJob_vmServer,
    replicationJob_encrypted,
    replicationJob_replicationJobId,
    replicationJob_replicationRunList,
    replicationJob_nextReplicationRunStartTime,
    replicationJob_statusMessage,
    replicationJob_kmsKeyId,
    replicationJob_latestAmiId,
    replicationJob_seedReplicationTime,
    replicationJob_runOnce,
    replicationJob_description,

    -- ** ReplicationRun
    replicationRun_state,
    replicationRun_replicationRunId,
    replicationRun_encrypted,
    replicationRun_stageDetails,
    replicationRun_scheduledStartTime,
    replicationRun_statusMessage,
    replicationRun_kmsKeyId,
    replicationRun_completedTime,
    replicationRun_amiId,
    replicationRun_type,
    replicationRun_description,

    -- ** ReplicationRunStageDetails
    replicationRunStageDetails_stage,
    replicationRunStageDetails_stageProgress,

    -- ** S3Location
    s3Location_bucket,
    s3Location_key,

    -- ** SSMOutput
    sSMOutput_s3Location,

    -- ** SSMValidationParameters
    sSMValidationParameters_instanceId,
    sSMValidationParameters_command,
    sSMValidationParameters_executionTimeoutSeconds,
    sSMValidationParameters_scriptType,
    sSMValidationParameters_source,
    sSMValidationParameters_outputS3BucketName,

    -- ** Server
    server_serverType,
    server_serverId,
    server_replicationJobTerminated,
    server_vmServer,
    server_replicationJobId,

    -- ** ServerGroup
    serverGroup_serverList,
    serverGroup_name,
    serverGroup_serverGroupId,

    -- ** ServerGroupLaunchConfiguration
    serverGroupLaunchConfiguration_serverGroupId,
    serverGroupLaunchConfiguration_launchOrder,
    serverGroupLaunchConfiguration_serverLaunchConfigurations,

    -- ** ServerGroupReplicationConfiguration
    serverGroupReplicationConfiguration_serverGroupId,
    serverGroupReplicationConfiguration_serverReplicationConfigurations,

    -- ** ServerGroupValidationConfiguration
    serverGroupValidationConfiguration_serverValidationConfigurations,
    serverGroupValidationConfiguration_serverGroupId,

    -- ** ServerLaunchConfiguration
    serverLaunchConfiguration_ec2KeyName,
    serverLaunchConfiguration_configureScriptType,
    serverLaunchConfiguration_associatePublicIpAddress,
    serverLaunchConfiguration_iamInstanceProfileName,
    serverLaunchConfiguration_subnet,
    serverLaunchConfiguration_logicalId,
    serverLaunchConfiguration_securityGroup,
    serverLaunchConfiguration_userData,
    serverLaunchConfiguration_instanceType,
    serverLaunchConfiguration_configureScript,
    serverLaunchConfiguration_server,
    serverLaunchConfiguration_vpc,

    -- ** ServerReplicationConfiguration
    serverReplicationConfiguration_serverReplicationParameters,
    serverReplicationConfiguration_server,

    -- ** ServerReplicationParameters
    serverReplicationParameters_frequency,
    serverReplicationParameters_numberOfRecentAmisToKeep,
    serverReplicationParameters_seedTime,
    serverReplicationParameters_licenseType,
    serverReplicationParameters_encrypted,
    serverReplicationParameters_kmsKeyId,
    serverReplicationParameters_runOnce,

    -- ** ServerValidationConfiguration
    serverValidationConfiguration_serverValidationStrategy,
    serverValidationConfiguration_userDataValidationParameters,
    serverValidationConfiguration_name,
    serverValidationConfiguration_server,
    serverValidationConfiguration_validationId,

    -- ** ServerValidationOutput
    serverValidationOutput_server,

    -- ** Source
    source_s3Location,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** UserData
    userData_s3Location,

    -- ** UserDataValidationParameters
    userDataValidationParameters_scriptType,
    userDataValidationParameters_source,

    -- ** ValidationOutput
    validationOutput_status,
    validationOutput_appValidationOutput,
    validationOutput_latestValidationTime,
    validationOutput_name,
    validationOutput_statusMessage,
    validationOutput_validationId,
    validationOutput_serverValidationOutput,

    -- ** VmServer
    vmServer_vmManagerName,
    vmServer_vmManagerType,
    vmServer_vmServerAddress,
    vmServer_vmName,
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
