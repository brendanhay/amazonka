{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubStrategy.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,
    _ServiceLinkedRoleLockClientException,

    -- * AntipatternReportStatus
    AntipatternReportStatus (..),

    -- * AppType
    AppType (..),

    -- * ApplicationComponentCriteria
    ApplicationComponentCriteria (..),

    -- * AssessmentStatus
    AssessmentStatus (..),

    -- * AwsManagedTargetDestination
    AwsManagedTargetDestination (..),

    -- * CollectorHealth
    CollectorHealth (..),

    -- * DataSourceType
    DataSourceType (..),

    -- * DatabaseManagementPreference
    DatabaseManagementPreference (..),

    -- * GroupName
    GroupName (..),

    -- * HeterogeneousTargetDatabaseEngine
    HeterogeneousTargetDatabaseEngine (..),

    -- * HomogeneousTargetDatabaseEngine
    HomogeneousTargetDatabaseEngine (..),

    -- * ImportFileTaskStatus
    ImportFileTaskStatus (..),

    -- * InclusionStatus
    InclusionStatus (..),

    -- * NoPreferenceTargetDestination
    NoPreferenceTargetDestination (..),

    -- * OSType
    OSType (..),

    -- * OutputFormat
    OutputFormat (..),

    -- * RecommendationReportStatus
    RecommendationReportStatus (..),

    -- * ResourceSubType
    ResourceSubType (..),

    -- * RunTimeAssessmentStatus
    RunTimeAssessmentStatus (..),

    -- * SelfManageTargetDestination
    SelfManageTargetDestination (..),

    -- * ServerCriteria
    ServerCriteria (..),

    -- * ServerOsType
    ServerOsType (..),

    -- * Severity
    Severity (..),

    -- * SortOrder
    SortOrder (..),

    -- * SrcCodeOrDbAnalysisStatus
    SrcCodeOrDbAnalysisStatus (..),

    -- * Strategy
    Strategy (..),

    -- * StrategyRecommendation
    StrategyRecommendation (..),

    -- * TargetDatabaseEngine
    TargetDatabaseEngine (..),

    -- * TargetDestination
    TargetDestination (..),

    -- * TransformationToolName
    TransformationToolName (..),

    -- * VersionControl
    VersionControl (..),

    -- * AntipatternSeveritySummary
    AntipatternSeveritySummary (..),
    newAntipatternSeveritySummary,
    antipatternSeveritySummary_severity,
    antipatternSeveritySummary_count,

    -- * ApplicationComponentDetail
    ApplicationComponentDetail (..),
    newApplicationComponentDetail,
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

    -- * ApplicationComponentStrategy
    ApplicationComponentStrategy (..),
    newApplicationComponentStrategy,
    applicationComponentStrategy_status,
    applicationComponentStrategy_recommendation,
    applicationComponentStrategy_isPreferred,

    -- * ApplicationComponentSummary
    ApplicationComponentSummary (..),
    newApplicationComponentSummary,
    applicationComponentSummary_appType,
    applicationComponentSummary_count,

    -- * ApplicationPreferences
    ApplicationPreferences (..),
    newApplicationPreferences,
    applicationPreferences_managementPreference,

    -- * AssessmentSummary
    AssessmentSummary (..),
    newAssessmentSummary,
    assessmentSummary_antipatternReportStatus,
    assessmentSummary_antipatternReportS3Object,
    assessmentSummary_listApplicationComponentSummary,
    assessmentSummary_listAntipatternSeveritySummary,
    assessmentSummary_listServerStrategySummary,
    assessmentSummary_antipatternReportStatusMessage,
    assessmentSummary_listServerSummary,
    assessmentSummary_listApplicationComponentStrategySummary,
    assessmentSummary_lastAnalyzedTimestamp,

    -- * AssociatedApplication
    AssociatedApplication (..),
    newAssociatedApplication,
    associatedApplication_name,
    associatedApplication_id,

    -- * AwsManagedResources
    AwsManagedResources (..),
    newAwsManagedResources,
    awsManagedResources_targetDestination,

    -- * BusinessGoals
    BusinessGoals (..),
    newBusinessGoals,
    businessGoals_reduceOperationalOverheadWithManagedServices,
    businessGoals_licenseCostReduction,
    businessGoals_speedOfMigration,
    businessGoals_modernizeInfrastructureWithCloudNativeTechnologies,

    -- * Collector
    Collector (..),
    newCollector,
    collector_lastActivityTimeStamp,
    collector_registeredTimeStamp,
    collector_hostName,
    collector_collectorId,
    collector_collectorVersion,
    collector_collectorHealth,
    collector_ipAddress,

    -- * DataCollectionDetails
    DataCollectionDetails (..),
    newDataCollectionDetails,
    dataCollectionDetails_failed,
    dataCollectionDetails_servers,
    dataCollectionDetails_status,
    dataCollectionDetails_completionTime,
    dataCollectionDetails_success,
    dataCollectionDetails_startTime,
    dataCollectionDetails_inProgress,

    -- * DatabaseConfigDetail
    DatabaseConfigDetail (..),
    newDatabaseConfigDetail,
    databaseConfigDetail_secretName,

    -- * DatabaseMigrationPreference
    DatabaseMigrationPreference (..),
    newDatabaseMigrationPreference,
    databaseMigrationPreference_homogeneous,
    databaseMigrationPreference_heterogeneous,
    databaseMigrationPreference_noPreference,

    -- * DatabasePreferences
    DatabasePreferences (..),
    newDatabasePreferences,
    databasePreferences_databaseManagementPreference,
    databasePreferences_databaseMigrationPreference,

    -- * Group
    Group (..),
    newGroup,
    group_name,
    group_value,

    -- * Heterogeneous
    Heterogeneous (..),
    newHeterogeneous,
    heterogeneous_targetDatabaseEngine,

    -- * Homogeneous
    Homogeneous (..),
    newHomogeneous,
    homogeneous_targetDatabaseEngine,

    -- * ImportFileTaskInformation
    ImportFileTaskInformation (..),
    newImportFileTaskInformation,
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

    -- * ManagementPreference
    ManagementPreference (..),
    newManagementPreference,
    managementPreference_awsManagedResources,
    managementPreference_selfManageResources,
    managementPreference_noPreference,

    -- * NetworkInfo
    NetworkInfo (..),
    newNetworkInfo,
    networkInfo_interfaceName,
    networkInfo_ipAddress,
    networkInfo_macAddress,
    networkInfo_netMask,

    -- * NoDatabaseMigrationPreference
    NoDatabaseMigrationPreference (..),
    newNoDatabaseMigrationPreference,
    noDatabaseMigrationPreference_targetDatabaseEngine,

    -- * NoManagementPreference
    NoManagementPreference (..),
    newNoManagementPreference,
    noManagementPreference_targetDestination,

    -- * OSInfo
    OSInfo (..),
    newOSInfo,
    oSInfo_type,
    oSInfo_version,

    -- * PrioritizeBusinessGoals
    PrioritizeBusinessGoals (..),
    newPrioritizeBusinessGoals,
    prioritizeBusinessGoals_businessGoals,

    -- * RecommendationReportDetails
    RecommendationReportDetails (..),
    newRecommendationReportDetails,
    recommendationReportDetails_s3Bucket,
    recommendationReportDetails_status,
    recommendationReportDetails_completionTime,
    recommendationReportDetails_statusMessage,
    recommendationReportDetails_startTime,
    recommendationReportDetails_s3Keys,

    -- * RecommendationSet
    RecommendationSet (..),
    newRecommendationSet,
    recommendationSet_transformationTool,
    recommendationSet_targetDestination,
    recommendationSet_strategy,

    -- * S3Object
    S3Object (..),
    newS3Object,
    s3Object_s3Bucket,
    s3Object_s3key,

    -- * SelfManageResources
    SelfManageResources (..),
    newSelfManageResources,
    selfManageResources_targetDestination,

    -- * ServerDetail
    ServerDetail (..),
    newServerDetail,
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

    -- * ServerStrategy
    ServerStrategy (..),
    newServerStrategy,
    serverStrategy_status,
    serverStrategy_recommendation,
    serverStrategy_numberOfApplicationComponents,
    serverStrategy_isPreferred,

    -- * ServerSummary
    ServerSummary (..),
    newServerSummary,
    serverSummary_serverOsType,
    serverSummary_count,

    -- * SourceCode
    SourceCode (..),
    newSourceCode,
    sourceCode_sourceVersion,
    sourceCode_location,
    sourceCode_versionControl,

    -- * SourceCodeRepository
    SourceCodeRepository (..),
    newSourceCodeRepository,
    sourceCodeRepository_branch,
    sourceCodeRepository_versionControlType,
    sourceCodeRepository_repository,

    -- * StrategyOption
    StrategyOption (..),
    newStrategyOption,
    strategyOption_targetDestination,
    strategyOption_toolName,
    strategyOption_strategy,
    strategyOption_isPreferred,

    -- * StrategySummary
    StrategySummary (..),
    newStrategySummary,
    strategySummary_count,
    strategySummary_strategy,

    -- * SystemInfo
    SystemInfo (..),
    newSystemInfo,
    systemInfo_osInfo,
    systemInfo_networkInfoList,
    systemInfo_fileSystemType,
    systemInfo_cpuArchitecture,

    -- * TransformationTool
    TransformationTool (..),
    newTransformationTool,
    transformationTool_name,
    transformationTool_tranformationToolInstallationLink,
    transformationTool_description,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types.AntipatternReportStatus
import Amazonka.MigrationHubStrategy.Types.AntipatternSeveritySummary
import Amazonka.MigrationHubStrategy.Types.AppType
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentCriteria
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentDetail
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentStrategy
import Amazonka.MigrationHubStrategy.Types.ApplicationComponentSummary
import Amazonka.MigrationHubStrategy.Types.ApplicationPreferences
import Amazonka.MigrationHubStrategy.Types.AssessmentStatus
import Amazonka.MigrationHubStrategy.Types.AssessmentSummary
import Amazonka.MigrationHubStrategy.Types.AssociatedApplication
import Amazonka.MigrationHubStrategy.Types.AwsManagedResources
import Amazonka.MigrationHubStrategy.Types.AwsManagedTargetDestination
import Amazonka.MigrationHubStrategy.Types.BusinessGoals
import Amazonka.MigrationHubStrategy.Types.Collector
import Amazonka.MigrationHubStrategy.Types.CollectorHealth
import Amazonka.MigrationHubStrategy.Types.DataCollectionDetails
import Amazonka.MigrationHubStrategy.Types.DataSourceType
import Amazonka.MigrationHubStrategy.Types.DatabaseConfigDetail
import Amazonka.MigrationHubStrategy.Types.DatabaseManagementPreference
import Amazonka.MigrationHubStrategy.Types.DatabaseMigrationPreference
import Amazonka.MigrationHubStrategy.Types.DatabasePreferences
import Amazonka.MigrationHubStrategy.Types.Group
import Amazonka.MigrationHubStrategy.Types.GroupName
import Amazonka.MigrationHubStrategy.Types.Heterogeneous
import Amazonka.MigrationHubStrategy.Types.HeterogeneousTargetDatabaseEngine
import Amazonka.MigrationHubStrategy.Types.Homogeneous
import Amazonka.MigrationHubStrategy.Types.HomogeneousTargetDatabaseEngine
import Amazonka.MigrationHubStrategy.Types.ImportFileTaskInformation
import Amazonka.MigrationHubStrategy.Types.ImportFileTaskStatus
import Amazonka.MigrationHubStrategy.Types.InclusionStatus
import Amazonka.MigrationHubStrategy.Types.ManagementPreference
import Amazonka.MigrationHubStrategy.Types.NetworkInfo
import Amazonka.MigrationHubStrategy.Types.NoDatabaseMigrationPreference
import Amazonka.MigrationHubStrategy.Types.NoManagementPreference
import Amazonka.MigrationHubStrategy.Types.NoPreferenceTargetDestination
import Amazonka.MigrationHubStrategy.Types.OSInfo
import Amazonka.MigrationHubStrategy.Types.OSType
import Amazonka.MigrationHubStrategy.Types.OutputFormat
import Amazonka.MigrationHubStrategy.Types.PrioritizeBusinessGoals
import Amazonka.MigrationHubStrategy.Types.RecommendationReportDetails
import Amazonka.MigrationHubStrategy.Types.RecommendationReportStatus
import Amazonka.MigrationHubStrategy.Types.RecommendationSet
import Amazonka.MigrationHubStrategy.Types.ResourceSubType
import Amazonka.MigrationHubStrategy.Types.RunTimeAssessmentStatus
import Amazonka.MigrationHubStrategy.Types.S3Object
import Amazonka.MigrationHubStrategy.Types.SelfManageResources
import Amazonka.MigrationHubStrategy.Types.SelfManageTargetDestination
import Amazonka.MigrationHubStrategy.Types.ServerCriteria
import Amazonka.MigrationHubStrategy.Types.ServerDetail
import Amazonka.MigrationHubStrategy.Types.ServerOsType
import Amazonka.MigrationHubStrategy.Types.ServerStrategy
import Amazonka.MigrationHubStrategy.Types.ServerSummary
import Amazonka.MigrationHubStrategy.Types.Severity
import Amazonka.MigrationHubStrategy.Types.SortOrder
import Amazonka.MigrationHubStrategy.Types.SourceCode
import Amazonka.MigrationHubStrategy.Types.SourceCodeRepository
import Amazonka.MigrationHubStrategy.Types.SrcCodeOrDbAnalysisStatus
import Amazonka.MigrationHubStrategy.Types.Strategy
import Amazonka.MigrationHubStrategy.Types.StrategyOption
import Amazonka.MigrationHubStrategy.Types.StrategyRecommendation
import Amazonka.MigrationHubStrategy.Types.StrategySummary
import Amazonka.MigrationHubStrategy.Types.SystemInfo
import Amazonka.MigrationHubStrategy.Types.TargetDatabaseEngine
import Amazonka.MigrationHubStrategy.Types.TargetDestination
import Amazonka.MigrationHubStrategy.Types.TransformationTool
import Amazonka.MigrationHubStrategy.Types.TransformationToolName
import Amazonka.MigrationHubStrategy.Types.VersionControl
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-02-19@ of the Amazon Migration Hub Strategy Recommendations SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MigrationHubStrategy",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "migrationhub-strategy",
      Core.signingName = "migrationhub-strategy",
      Core.version = "2020-02-19",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "MigrationHubStrategy",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The AWS user account does not have permission to perform the action.
-- Check the AWS Identity and Access Management (IAM) policy associated
-- with this account.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The server experienced an internal error. Try again.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The AWS account has reached its quota of imports. Contact AWS Support to
-- increase the quota for this account.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The specified ID in the request is not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Exception to indicate that there is an ongoing task when a new task is
-- created. Return when once the existing tasks are complete.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request body isn\'t valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Exception to indicate that the service-linked role (SLR) is locked.
_ServiceLinkedRoleLockClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleLockClientException =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleLockClientException"
    Prelude.. Core.hasStatus 400
