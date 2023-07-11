{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubStrategy.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Lens
  ( -- * Operations

    -- ** GetApplicationComponentDetails
    getApplicationComponentDetails_applicationComponentId,
    getApplicationComponentDetailsResponse_applicationComponentDetail,
    getApplicationComponentDetailsResponse_associatedApplications,
    getApplicationComponentDetailsResponse_associatedServerIds,
    getApplicationComponentDetailsResponse_moreApplicationResource,
    getApplicationComponentDetailsResponse_httpStatus,

    -- ** GetApplicationComponentStrategies
    getApplicationComponentStrategies_applicationComponentId,
    getApplicationComponentStrategiesResponse_applicationComponentStrategies,
    getApplicationComponentStrategiesResponse_httpStatus,

    -- ** GetAssessment
    getAssessment_id,
    getAssessmentResponse_assessmentTargets,
    getAssessmentResponse_dataCollectionDetails,
    getAssessmentResponse_id,
    getAssessmentResponse_httpStatus,

    -- ** GetImportFileTask
    getImportFileTask_id,
    getImportFileTaskResponse_completionTime,
    getImportFileTaskResponse_id,
    getImportFileTaskResponse_importName,
    getImportFileTaskResponse_inputS3Bucket,
    getImportFileTaskResponse_inputS3Key,
    getImportFileTaskResponse_numberOfRecordsFailed,
    getImportFileTaskResponse_numberOfRecordsSuccess,
    getImportFileTaskResponse_startTime,
    getImportFileTaskResponse_status,
    getImportFileTaskResponse_statusReportS3Bucket,
    getImportFileTaskResponse_statusReportS3Key,
    getImportFileTaskResponse_httpStatus,

    -- ** GetLatestAssessmentId
    getLatestAssessmentIdResponse_id,
    getLatestAssessmentIdResponse_httpStatus,

    -- ** GetPortfolioPreferences
    getPortfolioPreferencesResponse_applicationMode,
    getPortfolioPreferencesResponse_applicationPreferences,
    getPortfolioPreferencesResponse_databasePreferences,
    getPortfolioPreferencesResponse_prioritizeBusinessGoals,
    getPortfolioPreferencesResponse_httpStatus,

    -- ** GetPortfolioSummary
    getPortfolioSummaryResponse_assessmentSummary,
    getPortfolioSummaryResponse_httpStatus,

    -- ** GetRecommendationReportDetails
    getRecommendationReportDetails_id,
    getRecommendationReportDetailsResponse_id,
    getRecommendationReportDetailsResponse_recommendationReportDetails,
    getRecommendationReportDetailsResponse_httpStatus,

    -- ** GetServerDetails
    getServerDetails_maxResults,
    getServerDetails_nextToken,
    getServerDetails_serverId,
    getServerDetailsResponse_associatedApplications,
    getServerDetailsResponse_nextToken,
    getServerDetailsResponse_serverDetail,
    getServerDetailsResponse_httpStatus,

    -- ** GetServerStrategies
    getServerStrategies_serverId,
    getServerStrategiesResponse_serverStrategies,
    getServerStrategiesResponse_httpStatus,

    -- ** ListApplicationComponents
    listApplicationComponents_applicationComponentCriteria,
    listApplicationComponents_filterValue,
    listApplicationComponents_groupIdFilter,
    listApplicationComponents_maxResults,
    listApplicationComponents_nextToken,
    listApplicationComponents_sort,
    listApplicationComponentsResponse_applicationComponentInfos,
    listApplicationComponentsResponse_nextToken,
    listApplicationComponentsResponse_httpStatus,

    -- ** ListCollectors
    listCollectors_maxResults,
    listCollectors_nextToken,
    listCollectorsResponse_collectors,
    listCollectorsResponse_nextToken,
    listCollectorsResponse_httpStatus,

    -- ** ListImportFileTask
    listImportFileTask_maxResults,
    listImportFileTask_nextToken,
    listImportFileTaskResponse_nextToken,
    listImportFileTaskResponse_taskInfos,
    listImportFileTaskResponse_httpStatus,

    -- ** ListServers
    listServers_filterValue,
    listServers_groupIdFilter,
    listServers_maxResults,
    listServers_nextToken,
    listServers_serverCriteria,
    listServers_sort,
    listServersResponse_nextToken,
    listServersResponse_serverInfos,
    listServersResponse_httpStatus,

    -- ** PutPortfolioPreferences
    putPortfolioPreferences_applicationMode,
    putPortfolioPreferences_applicationPreferences,
    putPortfolioPreferences_databasePreferences,
    putPortfolioPreferences_prioritizeBusinessGoals,
    putPortfolioPreferencesResponse_httpStatus,

    -- ** StartAssessment
    startAssessment_assessmentTargets,
    startAssessment_s3bucketForAnalysisData,
    startAssessment_s3bucketForReportData,
    startAssessmentResponse_assessmentId,
    startAssessmentResponse_httpStatus,

    -- ** StartImportFileTask
    startImportFileTask_dataSourceType,
    startImportFileTask_groupId,
    startImportFileTask_s3bucketForReportData,
    startImportFileTask_s3Bucket,
    startImportFileTask_name,
    startImportFileTask_s3key,
    startImportFileTaskResponse_id,
    startImportFileTaskResponse_httpStatus,

    -- ** StartRecommendationReportGeneration
    startRecommendationReportGeneration_groupIdFilter,
    startRecommendationReportGeneration_outputFormat,
    startRecommendationReportGenerationResponse_id,
    startRecommendationReportGenerationResponse_httpStatus,

    -- ** StopAssessment
    stopAssessment_assessmentId,
    stopAssessmentResponse_httpStatus,

    -- ** UpdateApplicationComponentConfig
    updateApplicationComponentConfig_appType,
    updateApplicationComponentConfig_configureOnly,
    updateApplicationComponentConfig_inclusionStatus,
    updateApplicationComponentConfig_secretsManagerKey,
    updateApplicationComponentConfig_sourceCodeList,
    updateApplicationComponentConfig_strategyOption,
    updateApplicationComponentConfig_applicationComponentId,
    updateApplicationComponentConfigResponse_httpStatus,

    -- ** UpdateServerConfig
    updateServerConfig_strategyOption,
    updateServerConfig_serverId,
    updateServerConfigResponse_httpStatus,

    -- * Types

    -- ** AntipatternSeveritySummary
    antipatternSeveritySummary_count,
    antipatternSeveritySummary_severity,

    -- ** AppUnitError
    appUnitError_appUnitErrorCategory,

    -- ** ApplicationComponentDetail
    applicationComponentDetail_analysisStatus,
    applicationComponentDetail_antipatternReportS3Object,
    applicationComponentDetail_antipatternReportStatus,
    applicationComponentDetail_antipatternReportStatusMessage,
    applicationComponentDetail_appType,
    applicationComponentDetail_appUnitError,
    applicationComponentDetail_associatedServerId,
    applicationComponentDetail_databaseConfigDetail,
    applicationComponentDetail_id,
    applicationComponentDetail_inclusionStatus,
    applicationComponentDetail_lastAnalyzedTimestamp,
    applicationComponentDetail_listAntipatternSeveritySummary,
    applicationComponentDetail_moreServerAssociationExists,
    applicationComponentDetail_name,
    applicationComponentDetail_osDriver,
    applicationComponentDetail_osVersion,
    applicationComponentDetail_recommendationSet,
    applicationComponentDetail_resourceSubType,
    applicationComponentDetail_runtimeStatus,
    applicationComponentDetail_runtimeStatusMessage,
    applicationComponentDetail_sourceCodeRepositories,
    applicationComponentDetail_statusMessage,

    -- ** ApplicationComponentStatusSummary
    applicationComponentStatusSummary_count,
    applicationComponentStatusSummary_srcCodeOrDbAnalysisStatus,

    -- ** ApplicationComponentStrategy
    applicationComponentStrategy_isPreferred,
    applicationComponentStrategy_recommendation,
    applicationComponentStrategy_status,

    -- ** ApplicationComponentSummary
    applicationComponentSummary_appType,
    applicationComponentSummary_count,

    -- ** ApplicationPreferences
    applicationPreferences_managementPreference,

    -- ** AssessmentSummary
    assessmentSummary_antipatternReportS3Object,
    assessmentSummary_antipatternReportStatus,
    assessmentSummary_antipatternReportStatusMessage,
    assessmentSummary_lastAnalyzedTimestamp,
    assessmentSummary_listAntipatternSeveritySummary,
    assessmentSummary_listApplicationComponentStatusSummary,
    assessmentSummary_listApplicationComponentStrategySummary,
    assessmentSummary_listApplicationComponentSummary,
    assessmentSummary_listServerStatusSummary,
    assessmentSummary_listServerStrategySummary,
    assessmentSummary_listServerSummary,

    -- ** AssessmentTarget
    assessmentTarget_condition,
    assessmentTarget_name,
    assessmentTarget_values,

    -- ** AssociatedApplication
    associatedApplication_id,
    associatedApplication_name,

    -- ** AwsManagedResources
    awsManagedResources_targetDestination,

    -- ** BusinessGoals
    businessGoals_licenseCostReduction,
    businessGoals_modernizeInfrastructureWithCloudNativeTechnologies,
    businessGoals_reduceOperationalOverheadWithManagedServices,
    businessGoals_speedOfMigration,

    -- ** Collector
    collector_collectorHealth,
    collector_collectorId,
    collector_collectorVersion,
    collector_configurationSummary,
    collector_hostName,
    collector_ipAddress,
    collector_lastActivityTimeStamp,
    collector_registeredTimeStamp,

    -- ** ConfigurationSummary
    configurationSummary_ipAddressBasedRemoteInfoList,
    configurationSummary_pipelineInfoList,
    configurationSummary_remoteSourceCodeAnalysisServerInfo,
    configurationSummary_vcenterBasedRemoteInfoList,
    configurationSummary_versionControlInfoList,

    -- ** DataCollectionDetails
    dataCollectionDetails_completionTime,
    dataCollectionDetails_failed,
    dataCollectionDetails_inProgress,
    dataCollectionDetails_servers,
    dataCollectionDetails_startTime,
    dataCollectionDetails_status,
    dataCollectionDetails_statusMessage,
    dataCollectionDetails_success,

    -- ** DatabaseConfigDetail
    databaseConfigDetail_secretName,

    -- ** DatabaseMigrationPreference
    databaseMigrationPreference_heterogeneous,
    databaseMigrationPreference_homogeneous,
    databaseMigrationPreference_noPreference,

    -- ** DatabasePreferences
    databasePreferences_databaseManagementPreference,
    databasePreferences_databaseMigrationPreference,

    -- ** Group
    group_name,
    group_value,

    -- ** Heterogeneous
    heterogeneous_targetDatabaseEngine,

    -- ** Homogeneous
    homogeneous_targetDatabaseEngine,

    -- ** IPAddressBasedRemoteInfo
    iPAddressBasedRemoteInfo_authType,
    iPAddressBasedRemoteInfo_ipAddressConfigurationTimeStamp,
    iPAddressBasedRemoteInfo_osType,

    -- ** ImportFileTaskInformation
    importFileTaskInformation_completionTime,
    importFileTaskInformation_id,
    importFileTaskInformation_importName,
    importFileTaskInformation_inputS3Bucket,
    importFileTaskInformation_inputS3Key,
    importFileTaskInformation_numberOfRecordsFailed,
    importFileTaskInformation_numberOfRecordsSuccess,
    importFileTaskInformation_startTime,
    importFileTaskInformation_status,
    importFileTaskInformation_statusReportS3Bucket,
    importFileTaskInformation_statusReportS3Key,

    -- ** ManagementPreference
    managementPreference_awsManagedResources,
    managementPreference_noPreference,
    managementPreference_selfManageResources,

    -- ** NetworkInfo
    networkInfo_interfaceName,
    networkInfo_ipAddress,
    networkInfo_macAddress,
    networkInfo_netMask,

    -- ** NoDatabaseMigrationPreference
    noDatabaseMigrationPreference_targetDatabaseEngine,

    -- ** NoManagementPreference
    noManagementPreference_targetDestination,

    -- ** OSInfo
    oSInfo_type,
    oSInfo_version,

    -- ** PipelineInfo
    pipelineInfo_pipelineConfigurationTimeStamp,
    pipelineInfo_pipelineType,

    -- ** PrioritizeBusinessGoals
    prioritizeBusinessGoals_businessGoals,

    -- ** RecommendationReportDetails
    recommendationReportDetails_completionTime,
    recommendationReportDetails_s3Bucket,
    recommendationReportDetails_s3Keys,
    recommendationReportDetails_startTime,
    recommendationReportDetails_status,
    recommendationReportDetails_statusMessage,

    -- ** RecommendationSet
    recommendationSet_strategy,
    recommendationSet_targetDestination,
    recommendationSet_transformationTool,

    -- ** RemoteSourceCodeAnalysisServerInfo
    remoteSourceCodeAnalysisServerInfo_remoteSourceCodeAnalysisServerConfigurationTimestamp,

    -- ** S3Object
    s3Object_s3Bucket,
    s3Object_s3key,

    -- ** SelfManageResources
    selfManageResources_targetDestination,

    -- ** ServerDetail
    serverDetail_antipatternReportS3Object,
    serverDetail_antipatternReportStatus,
    serverDetail_antipatternReportStatusMessage,
    serverDetail_applicationComponentStrategySummary,
    serverDetail_dataCollectionStatus,
    serverDetail_id,
    serverDetail_lastAnalyzedTimestamp,
    serverDetail_listAntipatternSeveritySummary,
    serverDetail_name,
    serverDetail_recommendationSet,
    serverDetail_serverError,
    serverDetail_serverType,
    serverDetail_statusMessage,
    serverDetail_systemInfo,

    -- ** ServerError
    serverError_serverErrorCategory,

    -- ** ServerStatusSummary
    serverStatusSummary_count,
    serverStatusSummary_runTimeAssessmentStatus,

    -- ** ServerStrategy
    serverStrategy_isPreferred,
    serverStrategy_numberOfApplicationComponents,
    serverStrategy_recommendation,
    serverStrategy_status,

    -- ** ServerSummary
    serverSummary_serverOsType,
    serverSummary_count,

    -- ** SourceCode
    sourceCode_location,
    sourceCode_projectName,
    sourceCode_sourceVersion,
    sourceCode_versionControl,

    -- ** SourceCodeRepository
    sourceCodeRepository_branch,
    sourceCodeRepository_projectName,
    sourceCodeRepository_repository,
    sourceCodeRepository_versionControlType,

    -- ** StrategyOption
    strategyOption_isPreferred,
    strategyOption_strategy,
    strategyOption_targetDestination,
    strategyOption_toolName,

    -- ** StrategySummary
    strategySummary_count,
    strategySummary_strategy,

    -- ** SystemInfo
    systemInfo_cpuArchitecture,
    systemInfo_fileSystemType,
    systemInfo_networkInfoList,
    systemInfo_osInfo,

    -- ** TransformationTool
    transformationTool_description,
    transformationTool_name,
    transformationTool_tranformationToolInstallationLink,

    -- ** VcenterBasedRemoteInfo
    vcenterBasedRemoteInfo_osType,
    vcenterBasedRemoteInfo_vcenterConfigurationTimeStamp,

    -- ** VersionControlInfo
    versionControlInfo_versionControlConfigurationTimeStamp,
    versionControlInfo_versionControlType,
  )
where

import Amazonka.MigrationHubStrategy.GetApplicationComponentDetails
import Amazonka.MigrationHubStrategy.GetApplicationComponentStrategies
import Amazonka.MigrationHubStrategy.GetAssessment
import Amazonka.MigrationHubStrategy.GetImportFileTask
import Amazonka.MigrationHubStrategy.GetLatestAssessmentId
import Amazonka.MigrationHubStrategy.GetPortfolioPreferences
import Amazonka.MigrationHubStrategy.GetPortfolioSummary
import Amazonka.MigrationHubStrategy.GetRecommendationReportDetails
import Amazonka.MigrationHubStrategy.GetServerDetails
import Amazonka.MigrationHubStrategy.GetServerStrategies
import Amazonka.MigrationHubStrategy.ListApplicationComponents
import Amazonka.MigrationHubStrategy.ListCollectors
import Amazonka.MigrationHubStrategy.ListImportFileTask
import Amazonka.MigrationHubStrategy.ListServers
import Amazonka.MigrationHubStrategy.PutPortfolioPreferences
import Amazonka.MigrationHubStrategy.StartAssessment
import Amazonka.MigrationHubStrategy.StartImportFileTask
import Amazonka.MigrationHubStrategy.StartRecommendationReportGeneration
import Amazonka.MigrationHubStrategy.StopAssessment
import Amazonka.MigrationHubStrategy.Types.AntipatternSeveritySummary
import Amazonka.MigrationHubStrategy.Types.AppUnitError
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentDetail
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentStatusSummary
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentStrategy
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentSummary
import Amazonka.MigrationHubStrategy.Types.ApplicationPreferences
import Amazonka.MigrationHubStrategy.Types.AssessmentSummary
import Amazonka.MigrationHubStrategy.Types.AssessmentTarget
import Amazonka.MigrationHubStrategy.Types.AssociatedApplication
import Amazonka.MigrationHubStrategy.Types.AwsManagedResources
import Amazonka.MigrationHubStrategy.Types.BusinessGoals
import Amazonka.MigrationHubStrategy.Types.Collector
import Amazonka.MigrationHubStrategy.Types.ConfigurationSummary
import Amazonka.MigrationHubStrategy.Types.DataCollectionDetails
import Amazonka.MigrationHubStrategy.Types.DatabaseConfigDetail
import Amazonka.MigrationHubStrategy.Types.DatabaseMigrationPreference
import Amazonka.MigrationHubStrategy.Types.DatabasePreferences
import Amazonka.MigrationHubStrategy.Types.Group
import Amazonka.MigrationHubStrategy.Types.Heterogeneous
import Amazonka.MigrationHubStrategy.Types.Homogeneous
import Amazonka.MigrationHubStrategy.Types.IPAddressBasedRemoteInfo
import Amazonka.MigrationHubStrategy.Types.ImportFileTaskInformation
import Amazonka.MigrationHubStrategy.Types.ManagementPreference
import Amazonka.MigrationHubStrategy.Types.NetworkInfo
import Amazonka.MigrationHubStrategy.Types.NoDatabaseMigrationPreference
import Amazonka.MigrationHubStrategy.Types.NoManagementPreference
import Amazonka.MigrationHubStrategy.Types.OSInfo
import Amazonka.MigrationHubStrategy.Types.PipelineInfo
import Amazonka.MigrationHubStrategy.Types.PrioritizeBusinessGoals
import Amazonka.MigrationHubStrategy.Types.RecommendationReportDetails
import Amazonka.MigrationHubStrategy.Types.RecommendationSet
import Amazonka.MigrationHubStrategy.Types.RemoteSourceCodeAnalysisServerInfo
import Amazonka.MigrationHubStrategy.Types.S3Object
import Amazonka.MigrationHubStrategy.Types.SelfManageResources
import Amazonka.MigrationHubStrategy.Types.ServerDetail
import Amazonka.MigrationHubStrategy.Types.ServerError
import Amazonka.MigrationHubStrategy.Types.ServerStatusSummary
import Amazonka.MigrationHubStrategy.Types.ServerStrategy
import Amazonka.MigrationHubStrategy.Types.ServerSummary
import Amazonka.MigrationHubStrategy.Types.SourceCode
import Amazonka.MigrationHubStrategy.Types.SourceCodeRepository
import Amazonka.MigrationHubStrategy.Types.StrategyOption
import Amazonka.MigrationHubStrategy.Types.StrategySummary
import Amazonka.MigrationHubStrategy.Types.SystemInfo
import Amazonka.MigrationHubStrategy.Types.TransformationTool
import Amazonka.MigrationHubStrategy.Types.VcenterBasedRemoteInfo
import Amazonka.MigrationHubStrategy.Types.VersionControlInfo
import Amazonka.MigrationHubStrategy.UpdateApplicationComponentConfig
import Amazonka.MigrationHubStrategy.UpdateServerConfig
