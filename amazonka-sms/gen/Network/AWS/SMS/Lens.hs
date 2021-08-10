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

    -- ** GenerateChangeSet
    generateChangeSet_changesetFormat,
    generateChangeSet_appId,
    generateChangeSetResponse_s3Location,
    generateChangeSetResponse_httpStatus,

    -- ** ImportAppCatalog
    importAppCatalog_roleName,
    importAppCatalogResponse_httpStatus,

    -- ** LaunchApp
    launchApp_appId,
    launchAppResponse_httpStatus,

    -- ** GetAppValidationConfiguration
    getAppValidationConfiguration_appId,
    getAppValidationConfigurationResponse_appValidationConfigurations,
    getAppValidationConfigurationResponse_serverGroupValidationConfigurations,
    getAppValidationConfigurationResponse_httpStatus,

    -- ** PutAppReplicationConfiguration
    putAppReplicationConfiguration_appId,
    putAppReplicationConfiguration_serverGroupReplicationConfigurations,
    putAppReplicationConfigurationResponse_httpStatus,

    -- ** GetConnectors
    getConnectors_nextToken,
    getConnectors_maxResults,
    getConnectorsResponse_nextToken,
    getConnectorsResponse_connectorList,
    getConnectorsResponse_httpStatus,

    -- ** GenerateTemplate
    generateTemplate_appId,
    generateTemplate_templateFormat,
    generateTemplateResponse_s3Location,
    generateTemplateResponse_httpStatus,

    -- ** PutAppValidationConfiguration
    putAppValidationConfiguration_appValidationConfigurations,
    putAppValidationConfiguration_serverGroupValidationConfigurations,
    putAppValidationConfiguration_appId,
    putAppValidationConfigurationResponse_httpStatus,

    -- ** StartOnDemandReplicationRun
    startOnDemandReplicationRun_description,
    startOnDemandReplicationRun_replicationJobId,
    startOnDemandReplicationRunResponse_replicationRunId,
    startOnDemandReplicationRunResponse_httpStatus,

    -- ** TerminateApp
    terminateApp_appId,
    terminateAppResponse_httpStatus,

    -- ** ListApps
    listApps_appIds,
    listApps_nextToken,
    listApps_maxResults,
    listAppsResponse_nextToken,
    listAppsResponse_apps,
    listAppsResponse_httpStatus,

    -- ** GetReplicationRuns
    getReplicationRuns_nextToken,
    getReplicationRuns_maxResults,
    getReplicationRuns_replicationJobId,
    getReplicationRunsResponse_nextToken,
    getReplicationRunsResponse_replicationJob,
    getReplicationRunsResponse_replicationRunList,
    getReplicationRunsResponse_httpStatus,

    -- ** GetServers
    getServers_nextToken,
    getServers_maxResults,
    getServers_vmServerAddressList,
    getServersResponse_nextToken,
    getServersResponse_lastModifiedOn,
    getServersResponse_serverList,
    getServersResponse_serverCatalogStatus,
    getServersResponse_httpStatus,

    -- ** StartAppReplication
    startAppReplication_appId,
    startAppReplicationResponse_httpStatus,

    -- ** PutAppLaunchConfiguration
    putAppLaunchConfiguration_appId,
    putAppLaunchConfiguration_roleName,
    putAppLaunchConfiguration_autoLaunch,
    putAppLaunchConfiguration_serverGroupLaunchConfigurations,
    putAppLaunchConfigurationResponse_httpStatus,

    -- ** StopAppReplication
    stopAppReplication_appId,
    stopAppReplicationResponse_httpStatus,

    -- ** CreateReplicationJob
    createReplicationJob_numberOfRecentAmisToKeep,
    createReplicationJob_encrypted,
    createReplicationJob_roleName,
    createReplicationJob_kmsKeyId,
    createReplicationJob_frequency,
    createReplicationJob_runOnce,
    createReplicationJob_description,
    createReplicationJob_licenseType,
    createReplicationJob_serverId,
    createReplicationJob_seedReplicationTime,
    createReplicationJobResponse_replicationJobId,
    createReplicationJobResponse_httpStatus,

    -- ** DeleteServerCatalog
    deleteServerCatalogResponse_httpStatus,

    -- ** GetApp
    getApp_appId,
    getAppResponse_appSummary,
    getAppResponse_serverGroups,
    getAppResponse_tags,
    getAppResponse_httpStatus,

    -- ** DeleteAppReplicationConfiguration
    deleteAppReplicationConfiguration_appId,
    deleteAppReplicationConfigurationResponse_httpStatus,

    -- ** DisassociateConnector
    disassociateConnector_connectorId,
    disassociateConnectorResponse_httpStatus,

    -- ** NotifyAppValidationOutput
    notifyAppValidationOutput_notificationContext,
    notifyAppValidationOutput_appId,
    notifyAppValidationOutputResponse_httpStatus,

    -- ** GetReplicationJobs
    getReplicationJobs_nextToken,
    getReplicationJobs_maxResults,
    getReplicationJobs_replicationJobId,
    getReplicationJobsResponse_nextToken,
    getReplicationJobsResponse_replicationJobList,
    getReplicationJobsResponse_httpStatus,

    -- ** StartOnDemandAppReplication
    startOnDemandAppReplication_description,
    startOnDemandAppReplication_appId,
    startOnDemandAppReplicationResponse_httpStatus,

    -- ** GetAppValidationOutput
    getAppValidationOutput_appId,
    getAppValidationOutputResponse_validationOutputList,
    getAppValidationOutputResponse_httpStatus,

    -- ** GetAppReplicationConfiguration
    getAppReplicationConfiguration_appId,
    getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations,
    getAppReplicationConfigurationResponse_httpStatus,

    -- ** DeleteAppValidationConfiguration
    deleteAppValidationConfiguration_appId,
    deleteAppValidationConfigurationResponse_httpStatus,

    -- ** ImportServerCatalog
    importServerCatalogResponse_httpStatus,

    -- ** DeleteApp
    deleteApp_appId,
    deleteApp_forceStopAppReplication,
    deleteApp_forceTerminateApp,
    deleteAppResponse_httpStatus,

    -- ** UpdateApp
    updateApp_appId,
    updateApp_roleName,
    updateApp_name,
    updateApp_serverGroups,
    updateApp_tags,
    updateApp_description,
    updateAppResponse_appSummary,
    updateAppResponse_serverGroups,
    updateAppResponse_tags,
    updateAppResponse_httpStatus,

    -- ** DeleteAppLaunchConfiguration
    deleteAppLaunchConfiguration_appId,
    deleteAppLaunchConfigurationResponse_httpStatus,

    -- ** CreateApp
    createApp_roleName,
    createApp_name,
    createApp_serverGroups,
    createApp_tags,
    createApp_description,
    createApp_clientToken,
    createAppResponse_appSummary,
    createAppResponse_serverGroups,
    createAppResponse_tags,
    createAppResponse_httpStatus,

    -- ** GetAppLaunchConfiguration
    getAppLaunchConfiguration_appId,
    getAppLaunchConfigurationResponse_appId,
    getAppLaunchConfigurationResponse_roleName,
    getAppLaunchConfigurationResponse_autoLaunch,
    getAppLaunchConfigurationResponse_serverGroupLaunchConfigurations,
    getAppLaunchConfigurationResponse_httpStatus,

    -- ** UpdateReplicationJob
    updateReplicationJob_nextReplicationRunStartTime,
    updateReplicationJob_numberOfRecentAmisToKeep,
    updateReplicationJob_encrypted,
    updateReplicationJob_roleName,
    updateReplicationJob_kmsKeyId,
    updateReplicationJob_frequency,
    updateReplicationJob_description,
    updateReplicationJob_licenseType,
    updateReplicationJob_replicationJobId,
    updateReplicationJobResponse_httpStatus,

    -- ** DeleteReplicationJob
    deleteReplicationJob_replicationJobId,
    deleteReplicationJobResponse_httpStatus,

    -- * Types

    -- ** AppSummary
    appSummary_statusMessage,
    appSummary_appId,
    appSummary_status,
    appSummary_creationTime,
    appSummary_totalServers,
    appSummary_launchStatus,
    appSummary_replicationStatusMessage,
    appSummary_roleName,
    appSummary_replicationStatus,
    appSummary_importedAppId,
    appSummary_replicationConfigurationStatus,
    appSummary_latestReplicationTime,
    appSummary_launchDetails,
    appSummary_name,
    appSummary_launchConfigurationStatus,
    appSummary_description,
    appSummary_lastModified,
    appSummary_totalServerGroups,
    appSummary_launchStatusMessage,

    -- ** AppValidationConfiguration
    appValidationConfiguration_ssmValidationParameters,
    appValidationConfiguration_appValidationStrategy,
    appValidationConfiguration_validationId,
    appValidationConfiguration_name,

    -- ** AppValidationOutput
    appValidationOutput_ssmOutput,

    -- ** Connector
    connector_status,
    connector_macAddress,
    connector_associatedOn,
    connector_connectorId,
    connector_vmManagerId,
    connector_version,
    connector_vmManagerName,
    connector_ipAddress,
    connector_vmManagerType,
    connector_capabilityList,

    -- ** LaunchDetails
    launchDetails_stackName,
    launchDetails_stackId,
    launchDetails_latestLaunchTime,

    -- ** NotificationContext
    notificationContext_statusMessage,
    notificationContext_status,
    notificationContext_validationId,

    -- ** ReplicationJob
    replicationJob_nextReplicationRunStartTime,
    replicationJob_statusMessage,
    replicationJob_numberOfRecentAmisToKeep,
    replicationJob_encrypted,
    replicationJob_latestAmiId,
    replicationJob_roleName,
    replicationJob_serverId,
    replicationJob_state,
    replicationJob_replicationRunList,
    replicationJob_kmsKeyId,
    replicationJob_frequency,
    replicationJob_runOnce,
    replicationJob_description,
    replicationJob_replicationJobId,
    replicationJob_seedReplicationTime,
    replicationJob_vmServer,
    replicationJob_licenseType,
    replicationJob_serverType,

    -- ** ReplicationRun
    replicationRun_statusMessage,
    replicationRun_encrypted,
    replicationRun_replicationRunId,
    replicationRun_amiId,
    replicationRun_completedTime,
    replicationRun_state,
    replicationRun_kmsKeyId,
    replicationRun_scheduledStartTime,
    replicationRun_stageDetails,
    replicationRun_description,
    replicationRun_type,

    -- ** ReplicationRunStageDetails
    replicationRunStageDetails_stage,
    replicationRunStageDetails_stageProgress,

    -- ** S3Location
    s3Location_key,
    s3Location_bucket,

    -- ** SSMOutput
    sSMOutput_s3Location,

    -- ** SSMValidationParameters
    sSMValidationParameters_instanceId,
    sSMValidationParameters_outputS3BucketName,
    sSMValidationParameters_source,
    sSMValidationParameters_scriptType,
    sSMValidationParameters_command,
    sSMValidationParameters_executionTimeoutSeconds,

    -- ** Server
    server_serverId,
    server_replicationJobId,
    server_replicationJobTerminated,
    server_vmServer,
    server_serverType,

    -- ** ServerGroup
    serverGroup_serverGroupId,
    serverGroup_name,
    serverGroup_serverList,

    -- ** ServerGroupLaunchConfiguration
    serverGroupLaunchConfiguration_serverGroupId,
    serverGroupLaunchConfiguration_launchOrder,
    serverGroupLaunchConfiguration_serverLaunchConfigurations,

    -- ** ServerGroupReplicationConfiguration
    serverGroupReplicationConfiguration_serverGroupId,
    serverGroupReplicationConfiguration_serverReplicationConfigurations,

    -- ** ServerGroupValidationConfiguration
    serverGroupValidationConfiguration_serverGroupId,
    serverGroupValidationConfiguration_serverValidationConfigurations,

    -- ** ServerLaunchConfiguration
    serverLaunchConfiguration_configureScript,
    serverLaunchConfiguration_ec2KeyName,
    serverLaunchConfiguration_instanceType,
    serverLaunchConfiguration_userData,
    serverLaunchConfiguration_logicalId,
    serverLaunchConfiguration_subnet,
    serverLaunchConfiguration_iamInstanceProfileName,
    serverLaunchConfiguration_server,
    serverLaunchConfiguration_associatePublicIpAddress,
    serverLaunchConfiguration_configureScriptType,
    serverLaunchConfiguration_securityGroup,
    serverLaunchConfiguration_vpc,

    -- ** ServerReplicationConfiguration
    serverReplicationConfiguration_server,
    serverReplicationConfiguration_serverReplicationParameters,

    -- ** ServerReplicationParameters
    serverReplicationParameters_numberOfRecentAmisToKeep,
    serverReplicationParameters_encrypted,
    serverReplicationParameters_seedTime,
    serverReplicationParameters_kmsKeyId,
    serverReplicationParameters_frequency,
    serverReplicationParameters_runOnce,
    serverReplicationParameters_licenseType,

    -- ** ServerValidationConfiguration
    serverValidationConfiguration_validationId,
    serverValidationConfiguration_userDataValidationParameters,
    serverValidationConfiguration_server,
    serverValidationConfiguration_name,
    serverValidationConfiguration_serverValidationStrategy,

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
    validationOutput_statusMessage,
    validationOutput_status,
    validationOutput_validationId,
    validationOutput_appValidationOutput,
    validationOutput_name,
    validationOutput_serverValidationOutput,
    validationOutput_latestValidationTime,

    -- ** VmServer
    vmServer_vmPath,
    vmServer_vmManagerName,
    vmServer_vmName,
    vmServer_vmServerAddress,
    vmServer_vmManagerType,

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
