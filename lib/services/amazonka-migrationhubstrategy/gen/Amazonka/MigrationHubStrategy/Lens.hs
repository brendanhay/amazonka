{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubStrategy.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Lens
  ( -- * Operations

    -- ** GetApplicationComponentDetails
    getApplicationComponentDetails_applicationComponentId,
    getApplicationComponentDetailsResponse_associatedApplications,
    getApplicationComponentDetailsResponse_applicationComponentDetail,
    getApplicationComponentDetailsResponse_associatedServerIds,
    getApplicationComponentDetailsResponse_moreApplicationResource,
    getApplicationComponentDetailsResponse_httpStatus,

    -- ** GetApplicationComponentStrategies
    getApplicationComponentStrategies_applicationComponentId,
    getApplicationComponentStrategiesResponse_applicationComponentStrategies,
    getApplicationComponentStrategiesResponse_httpStatus,

    -- ** GetAssessment
    getAssessment_id,
    getAssessmentResponse_id,
    getAssessmentResponse_dataCollectionDetails,
    getAssessmentResponse_httpStatus,

    -- ** GetImportFileTask
    getImportFileTask_id,
    getImportFileTaskResponse_statusReportS3Bucket,
    getImportFileTaskResponse_status,
    getImportFileTaskResponse_completionTime,
    getImportFileTaskResponse_id,
    getImportFileTaskResponse_numberOfRecordsFailed,
    getImportFileTaskResponse_importName,
    getImportFileTaskResponse_inputS3Bucket,
    getImportFileTaskResponse_inputS3Key,
    getImportFileTaskResponse_numberOfRecordsSuccess,
    getImportFileTaskResponse_startTime,
    getImportFileTaskResponse_statusReportS3Key,
    getImportFileTaskResponse_httpStatus,

    -- ** GetPortfolioPreferences
    getPortfolioPreferencesResponse_databasePreferences,
    getPortfolioPreferencesResponse_prioritizeBusinessGoals,
    getPortfolioPreferencesResponse_applicationPreferences,
    getPortfolioPreferencesResponse_httpStatus,

    -- ** GetPortfolioSummary
    getPortfolioSummaryResponse_assessmentSummary,
    getPortfolioSummaryResponse_httpStatus,

    -- ** GetRecommendationReportDetails
    getRecommendationReportDetails_id,
    getRecommendationReportDetailsResponse_recommendationReportDetails,
    getRecommendationReportDetailsResponse_id,
    getRecommendationReportDetailsResponse_httpStatus,

    -- ** GetServerDetails
    getServerDetails_nextToken,
    getServerDetails_maxResults,
    getServerDetails_serverId,
    getServerDetailsResponse_nextToken,
    getServerDetailsResponse_associatedApplications,
    getServerDetailsResponse_serverDetail,
    getServerDetailsResponse_httpStatus,

    -- ** GetServerStrategies
    getServerStrategies_serverId,
    getServerStrategiesResponse_serverStrategies,
    getServerStrategiesResponse_httpStatus,

    -- ** ListApplicationComponents
    listApplicationComponents_nextToken,
    listApplicationComponents_groupIdFilter,
    listApplicationComponents_filterValue,
    listApplicationComponents_sort,
    listApplicationComponents_maxResults,
    listApplicationComponents_applicationComponentCriteria,
    listApplicationComponentsResponse_nextToken,
    listApplicationComponentsResponse_applicationComponentInfos,
    listApplicationComponentsResponse_httpStatus,

    -- ** ListCollectors
    listCollectors_nextToken,
    listCollectors_maxResults,
    listCollectorsResponse_nextToken,
    listCollectorsResponse_collectors,
    listCollectorsResponse_httpStatus,

    -- ** ListImportFileTask
    listImportFileTask_nextToken,
    listImportFileTask_maxResults,
    listImportFileTaskResponse_nextToken,
    listImportFileTaskResponse_taskInfos,
    listImportFileTaskResponse_httpStatus,

    -- ** ListServers
    listServers_nextToken,
    listServers_groupIdFilter,
    listServers_filterValue,
    listServers_sort,
    listServers_maxResults,
    listServers_serverCriteria,
    listServersResponse_nextToken,
    listServersResponse_serverInfos,
    listServersResponse_httpStatus,

    -- ** PutPortfolioPreferences
    putPortfolioPreferences_databasePreferences,
    putPortfolioPreferences_prioritizeBusinessGoals,
    putPortfolioPreferences_applicationPreferences,
    putPortfolioPreferencesResponse_httpStatus,

    -- ** StartAssessment
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
    updateApplicationComponentConfig_sourceCodeList,
    updateApplicationComponentConfig_inclusionStatus,
    updateApplicationComponentConfig_secretsManagerKey,
    updateApplicationComponentConfig_strategyOption,
    updateApplicationComponentConfig_applicationComponentId,
    updateApplicationComponentConfigResponse_httpStatus,

    -- ** UpdateServerConfig
    updateServerConfig_strategyOption,
    updateServerConfig_serverId,
    updateServerConfigResponse_httpStatus,

    -- * Types

    -- ** AntipatternSeveritySummary
    antipatternSeveritySummary_severity,
    antipatternSeveritySummary_count,

    -- ** ApplicationComponentDetail
    applicationComponentDetail_antipatternReportStatus,
    applicationComponentDetail_antipatternReportS3Object,
    applicationComponentDetail_name,
    applicationComponentDetail_recommendationSet,
    applicationComponentDetail_appType,
    applicationComponentDetail_moreServerAssociationExists,
    applicationComponentDetail_listAntipatternSeveritySummary,
    applicationComponentDetail_osVersion,
    applicationComponentDetail_id,
    applicationComponentDetail_inclusionStatus,
    applicationComponentDetail_analysisStatus,
    applicationComponentDetail_antipatternReportStatusMessage,
    applicationComponentDetail_resourceSubType,
    applicationComponentDetail_sourceCodeRepositories,
    applicationComponentDetail_databaseConfigDetail,
    applicationComponentDetail_associatedServerId,
    applicationComponentDetail_osDriver,
    applicationComponentDetail_statusMessage,
    applicationComponentDetail_lastAnalyzedTimestamp,

    -- ** ApplicationComponentStrategy
    applicationComponentStrategy_status,
    applicationComponentStrategy_recommendation,
    applicationComponentStrategy_isPreferred,

    -- ** ApplicationComponentSummary
    applicationComponentSummary_appType,
    applicationComponentSummary_count,

    -- ** ApplicationPreferences
    applicationPreferences_managementPreference,

    -- ** AssessmentSummary
    assessmentSummary_antipatternReportStatus,
    assessmentSummary_antipatternReportS3Object,
    assessmentSummary_listApplicationComponentSummary,
    assessmentSummary_listAntipatternSeveritySummary,
    assessmentSummary_listServerStrategySummary,
    assessmentSummary_antipatternReportStatusMessage,
    assessmentSummary_listServerSummary,
    assessmentSummary_listApplicationComponentStrategySummary,
    assessmentSummary_lastAnalyzedTimestamp,

    -- ** AssociatedApplication
    associatedApplication_name,
    associatedApplication_id,

    -- ** AwsManagedResources
    awsManagedResources_targetDestination,

    -- ** BusinessGoals
    businessGoals_reduceOperationalOverheadWithManagedServices,
    businessGoals_licenseCostReduction,
    businessGoals_speedOfMigration,
    businessGoals_modernizeInfrastructureWithCloudNativeTechnologies,

    -- ** Collector
    collector_lastActivityTimeStamp,
    collector_registeredTimeStamp,
    collector_hostName,
    collector_collectorId,
    collector_collectorVersion,
    collector_collectorHealth,
    collector_ipAddress,

    -- ** DataCollectionDetails
    dataCollectionDetails_failed,
    dataCollectionDetails_servers,
    dataCollectionDetails_status,
    dataCollectionDetails_completionTime,
    dataCollectionDetails_success,
    dataCollectionDetails_startTime,
    dataCollectionDetails_inProgress,

    -- ** DatabaseConfigDetail
    databaseConfigDetail_secretName,

    -- ** DatabaseMigrationPreference
    databaseMigrationPreference_homogeneous,
    databaseMigrationPreference_heterogeneous,
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

    -- ** ImportFileTaskInformation
    importFileTaskInformation_statusReportS3Bucket,
    importFileTaskInformation_status,
    importFileTaskInformation_completionTime,
    importFileTaskInformation_id,
    importFileTaskInformation_numberOfRecordsFailed,
    importFileTaskInformation_importName,
    importFileTaskInformation_inputS3Bucket,
    importFileTaskInformation_inputS3Key,
    importFileTaskInformation_numberOfRecordsSuccess,
    importFileTaskInformation_startTime,
    importFileTaskInformation_statusReportS3Key,

    -- ** ManagementPreference
    managementPreference_awsManagedResources,
    managementPreference_selfManageResources,
    managementPreference_noPreference,

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

    -- ** PrioritizeBusinessGoals
    prioritizeBusinessGoals_businessGoals,

    -- ** RecommendationReportDetails
    recommendationReportDetails_s3Bucket,
    recommendationReportDetails_status,
    recommendationReportDetails_completionTime,
    recommendationReportDetails_statusMessage,
    recommendationReportDetails_startTime,
    recommendationReportDetails_s3Keys,

    -- ** RecommendationSet
    recommendationSet_transformationTool,
    recommendationSet_targetDestination,
    recommendationSet_strategy,

    -- ** S3Object
    s3Object_s3Bucket,
    s3Object_s3key,

    -- ** SelfManageResources
    selfManageResources_targetDestination,

    -- ** ServerDetail
    serverDetail_antipatternReportStatus,
    serverDetail_dataCollectionStatus,
    serverDetail_antipatternReportS3Object,
    serverDetail_name,
    serverDetail_recommendationSet,
    serverDetail_systemInfo,
    serverDetail_applicationComponentStrategySummary,
    serverDetail_listAntipatternSeveritySummary,
    serverDetail_serverType,
    serverDetail_id,
    serverDetail_antipatternReportStatusMessage,
    serverDetail_statusMessage,
    serverDetail_lastAnalyzedTimestamp,

    -- ** ServerStrategy
    serverStrategy_status,
    serverStrategy_recommendation,
    serverStrategy_numberOfApplicationComponents,
    serverStrategy_isPreferred,

    -- ** ServerSummary
    serverSummary_serverOsType,
    serverSummary_count,

    -- ** SourceCode
    sourceCode_sourceVersion,
    sourceCode_location,
    sourceCode_versionControl,

    -- ** SourceCodeRepository
    sourceCodeRepository_branch,
    sourceCodeRepository_versionControlType,
    sourceCodeRepository_repository,

    -- ** StrategyOption
    strategyOption_targetDestination,
    strategyOption_toolName,
    strategyOption_strategy,
    strategyOption_isPreferred,

    -- ** StrategySummary
    strategySummary_count,
    strategySummary_strategy,

    -- ** SystemInfo
    systemInfo_osInfo,
    systemInfo_networkInfoList,
    systemInfo_fileSystemType,
    systemInfo_cpuArchitecture,

    -- ** TransformationTool
    transformationTool_name,
    transformationTool_tranformationToolInstallationLink,
    transformationTool_description,
  )
where

import Amazonka.MigrationHubStrategy.GetApplicationComponentDetails
import Amazonka.MigrationHubStrategy.GetApplicationComponentStrategies
import Amazonka.MigrationHubStrategy.GetAssessment
import Amazonka.MigrationHubStrategy.GetImportFileTask
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
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentDetail
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentStrategy
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentSummary
import Amazonka.MigrationHubStrategy.Types.ApplicationPreferences
import Amazonka.MigrationHubStrategy.Types.AssessmentSummary
import Amazonka.MigrationHubStrategy.Types.AssociatedApplication
import Amazonka.MigrationHubStrategy.Types.AwsManagedResources
import Amazonka.MigrationHubStrategy.Types.BusinessGoals
import Amazonka.MigrationHubStrategy.Types.Collector
import Amazonka.MigrationHubStrategy.Types.DataCollectionDetails
import Amazonka.MigrationHubStrategy.Types.DatabaseConfigDetail
import Amazonka.MigrationHubStrategy.Types.DatabaseMigrationPreference
import Amazonka.MigrationHubStrategy.Types.DatabasePreferences
import Amazonka.MigrationHubStrategy.Types.Group
import Amazonka.MigrationHubStrategy.Types.Heterogeneous
import Amazonka.MigrationHubStrategy.Types.Homogeneous
import Amazonka.MigrationHubStrategy.Types.ImportFileTaskInformation
import Amazonka.MigrationHubStrategy.Types.ManagementPreference
import Amazonka.MigrationHubStrategy.Types.NetworkInfo
import Amazonka.MigrationHubStrategy.Types.NoDatabaseMigrationPreference
import Amazonka.MigrationHubStrategy.Types.NoManagementPreference
import Amazonka.MigrationHubStrategy.Types.OSInfo
import Amazonka.MigrationHubStrategy.Types.PrioritizeBusinessGoals
import Amazonka.MigrationHubStrategy.Types.RecommendationReportDetails
import Amazonka.MigrationHubStrategy.Types.RecommendationSet
import Amazonka.MigrationHubStrategy.Types.S3Object
import Amazonka.MigrationHubStrategy.Types.SelfManageResources
import Amazonka.MigrationHubStrategy.Types.ServerDetail
import Amazonka.MigrationHubStrategy.Types.ServerStrategy
import Amazonka.MigrationHubStrategy.Types.ServerSummary
import Amazonka.MigrationHubStrategy.Types.SourceCode
import Amazonka.MigrationHubStrategy.Types.SourceCodeRepository
import Amazonka.MigrationHubStrategy.Types.StrategyOption
import Amazonka.MigrationHubStrategy.Types.StrategySummary
import Amazonka.MigrationHubStrategy.Types.SystemInfo
import Amazonka.MigrationHubStrategy.Types.TransformationTool
import Amazonka.MigrationHubStrategy.UpdateApplicationComponentConfig
import Amazonka.MigrationHubStrategy.UpdateServerConfig
