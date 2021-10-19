{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Lens
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

import Network.AWS.SMS.CreateApp
import Network.AWS.SMS.CreateReplicationJob
import Network.AWS.SMS.DeleteApp
import Network.AWS.SMS.DeleteAppLaunchConfiguration
import Network.AWS.SMS.DeleteAppReplicationConfiguration
import Network.AWS.SMS.DeleteAppValidationConfiguration
import Network.AWS.SMS.DeleteReplicationJob
import Network.AWS.SMS.DeleteServerCatalog
import Network.AWS.SMS.DisassociateConnector
import Network.AWS.SMS.GenerateChangeSet
import Network.AWS.SMS.GenerateTemplate
import Network.AWS.SMS.GetApp
import Network.AWS.SMS.GetAppLaunchConfiguration
import Network.AWS.SMS.GetAppReplicationConfiguration
import Network.AWS.SMS.GetAppValidationConfiguration
import Network.AWS.SMS.GetAppValidationOutput
import Network.AWS.SMS.GetConnectors
import Network.AWS.SMS.GetReplicationJobs
import Network.AWS.SMS.GetReplicationRuns
import Network.AWS.SMS.GetServers
import Network.AWS.SMS.ImportAppCatalog
import Network.AWS.SMS.ImportServerCatalog
import Network.AWS.SMS.LaunchApp
import Network.AWS.SMS.ListApps
import Network.AWS.SMS.NotifyAppValidationOutput
import Network.AWS.SMS.PutAppLaunchConfiguration
import Network.AWS.SMS.PutAppReplicationConfiguration
import Network.AWS.SMS.PutAppValidationConfiguration
import Network.AWS.SMS.StartAppReplication
import Network.AWS.SMS.StartOnDemandAppReplication
import Network.AWS.SMS.StartOnDemandReplicationRun
import Network.AWS.SMS.StopAppReplication
import Network.AWS.SMS.TerminateApp
import Network.AWS.SMS.Types.AppSummary
import Network.AWS.SMS.Types.AppValidationConfiguration
import Network.AWS.SMS.Types.AppValidationOutput
import Network.AWS.SMS.Types.Connector
import Network.AWS.SMS.Types.LaunchDetails
import Network.AWS.SMS.Types.NotificationContext
import Network.AWS.SMS.Types.ReplicationJob
import Network.AWS.SMS.Types.ReplicationRun
import Network.AWS.SMS.Types.ReplicationRunStageDetails
import Network.AWS.SMS.Types.S3Location
import Network.AWS.SMS.Types.SSMOutput
import Network.AWS.SMS.Types.SSMValidationParameters
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerGroup
import Network.AWS.SMS.Types.ServerGroupLaunchConfiguration
import Network.AWS.SMS.Types.ServerGroupReplicationConfiguration
import Network.AWS.SMS.Types.ServerGroupValidationConfiguration
import Network.AWS.SMS.Types.ServerLaunchConfiguration
import Network.AWS.SMS.Types.ServerReplicationConfiguration
import Network.AWS.SMS.Types.ServerReplicationParameters
import Network.AWS.SMS.Types.ServerValidationConfiguration
import Network.AWS.SMS.Types.ServerValidationOutput
import Network.AWS.SMS.Types.Source
import Network.AWS.SMS.Types.Tag
import Network.AWS.SMS.Types.UserData
import Network.AWS.SMS.Types.UserDataValidationParameters
import Network.AWS.SMS.Types.ValidationOutput
import Network.AWS.SMS.Types.VmServer
import Network.AWS.SMS.Types.VmServerAddress
import Network.AWS.SMS.UpdateApp
import Network.AWS.SMS.UpdateReplicationJob
