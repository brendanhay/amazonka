{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _IdentityTypeNotSupportedException,
    _ResourceUnavailableException,
    _UnsupportedPricingPlanException,
    _AccessDeniedException,
    _PreconditionNotMetException,
    _QuickSightUserNotFoundException,
    _ConcurrentUpdatingException,
    _ResourceNotFoundException,
    _SessionLifetimeInMinutesInvalidException,
    _UnsupportedUserEditionException,
    _LimitExceededException,
    _InvalidNextTokenException,
    _ConflictException,
    _ThrottlingException,
    _ResourceExistsException,
    _DomainNotWhitelistedException,
    _InvalidRequestException,
    _InvalidParameterValueException,
    _InternalFailureException,

    -- * AnalysisErrorType
    AnalysisErrorType (..),

    -- * AnalysisFilterAttribute
    AnalysisFilterAttribute (..),

    -- * AssignmentStatus
    AssignmentStatus (..),

    -- * AuthenticationMethodOption
    AuthenticationMethodOption (..),

    -- * ColumnDataType
    ColumnDataType (..),

    -- * ColumnTagName
    ColumnTagName (..),

    -- * DashboardBehavior
    DashboardBehavior (..),

    -- * DashboardErrorType
    DashboardErrorType (..),

    -- * DashboardFilterAttribute
    DashboardFilterAttribute (..),

    -- * DashboardUIState
    DashboardUIState (..),

    -- * DataSetFilterAttribute
    DataSetFilterAttribute (..),

    -- * DataSetImportMode
    DataSetImportMode (..),

    -- * DataSourceErrorInfoType
    DataSourceErrorInfoType (..),

    -- * DataSourceFilterAttribute
    DataSourceFilterAttribute (..),

    -- * DataSourceType
    DataSourceType (..),

    -- * Edition
    Edition (..),

    -- * EmbeddingIdentityType
    EmbeddingIdentityType (..),

    -- * FileFormat
    FileFormat (..),

    -- * FilterOperator
    FilterOperator (..),

    -- * FolderFilterAttribute
    FolderFilterAttribute (..),

    -- * FolderType
    FolderType (..),

    -- * GeoSpatialCountryCode
    GeoSpatialCountryCode (..),

    -- * GeoSpatialDataRole
    GeoSpatialDataRole (..),

    -- * GroupFilterAttribute
    GroupFilterAttribute (..),

    -- * GroupFilterOperator
    GroupFilterOperator (..),

    -- * IdentityStore
    IdentityStore (..),

    -- * IdentityType
    IdentityType (..),

    -- * IngestionErrorType
    IngestionErrorType (..),

    -- * IngestionRequestSource
    IngestionRequestSource (..),

    -- * IngestionRequestType
    IngestionRequestType (..),

    -- * IngestionStatus
    IngestionStatus (..),

    -- * IngestionType
    IngestionType (..),

    -- * InputColumnDataType
    InputColumnDataType (..),

    -- * JoinType
    JoinType (..),

    -- * MemberType
    MemberType (..),

    -- * NamespaceErrorType
    NamespaceErrorType (..),

    -- * NamespaceStatus
    NamespaceStatus (..),

    -- * ResourceStatus
    ResourceStatus (..),

    -- * RowLevelPermissionFormatVersion
    RowLevelPermissionFormatVersion (..),

    -- * RowLevelPermissionPolicy
    RowLevelPermissionPolicy (..),

    -- * Status
    Status (..),

    -- * TemplateErrorType
    TemplateErrorType (..),

    -- * TextQualifier
    TextQualifier (..),

    -- * ThemeErrorType
    ThemeErrorType (..),

    -- * ThemeType
    ThemeType (..),

    -- * UserRole
    UserRole (..),

    -- * AccountCustomization
    AccountCustomization (..),
    newAccountCustomization,
    accountCustomization_defaultEmailCustomizationTemplate,
    accountCustomization_defaultTheme,

    -- * AccountInfo
    AccountInfo (..),
    newAccountInfo,
    accountInfo_authenticationType,
    accountInfo_notificationEmail,
    accountInfo_edition,
    accountInfo_accountSubscriptionStatus,
    accountInfo_accountName,

    -- * AccountSettings
    AccountSettings (..),
    newAccountSettings,
    accountSettings_notificationEmail,
    accountSettings_edition,
    accountSettings_accountName,
    accountSettings_defaultNamespace,
    accountSettings_terminationProtectionEnabled,
    accountSettings_publicSharingEnabled,

    -- * ActiveIAMPolicyAssignment
    ActiveIAMPolicyAssignment (..),
    newActiveIAMPolicyAssignment,
    activeIAMPolicyAssignment_policyArn,
    activeIAMPolicyAssignment_assignmentName,

    -- * AdHocFilteringOption
    AdHocFilteringOption (..),
    newAdHocFilteringOption,
    adHocFilteringOption_availabilityStatus,

    -- * AmazonElasticsearchParameters
    AmazonElasticsearchParameters (..),
    newAmazonElasticsearchParameters,
    amazonElasticsearchParameters_domain,

    -- * AmazonOpenSearchParameters
    AmazonOpenSearchParameters (..),
    newAmazonOpenSearchParameters,
    amazonOpenSearchParameters_domain,

    -- * Analysis
    Analysis (..),
    newAnalysis,
    analysis_analysisId,
    analysis_name,
    analysis_themeArn,
    analysis_createdTime,
    analysis_arn,
    analysis_status,
    analysis_lastUpdatedTime,
    analysis_errors,
    analysis_dataSetArns,
    analysis_sheets,

    -- * AnalysisError
    AnalysisError (..),
    newAnalysisError,
    analysisError_message,
    analysisError_type,

    -- * AnalysisSearchFilter
    AnalysisSearchFilter (..),
    newAnalysisSearchFilter,
    analysisSearchFilter_name,
    analysisSearchFilter_operator,
    analysisSearchFilter_value,

    -- * AnalysisSourceEntity
    AnalysisSourceEntity (..),
    newAnalysisSourceEntity,
    analysisSourceEntity_sourceTemplate,

    -- * AnalysisSourceTemplate
    AnalysisSourceTemplate (..),
    newAnalysisSourceTemplate,
    analysisSourceTemplate_dataSetReferences,
    analysisSourceTemplate_arn,

    -- * AnalysisSummary
    AnalysisSummary (..),
    newAnalysisSummary,
    analysisSummary_analysisId,
    analysisSummary_name,
    analysisSummary_createdTime,
    analysisSummary_arn,
    analysisSummary_status,
    analysisSummary_lastUpdatedTime,

    -- * AnonymousUserDashboardEmbeddingConfiguration
    AnonymousUserDashboardEmbeddingConfiguration (..),
    newAnonymousUserDashboardEmbeddingConfiguration,
    anonymousUserDashboardEmbeddingConfiguration_initialDashboardId,

    -- * AnonymousUserDashboardVisualEmbeddingConfiguration
    AnonymousUserDashboardVisualEmbeddingConfiguration (..),
    newAnonymousUserDashboardVisualEmbeddingConfiguration,
    anonymousUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId,

    -- * AnonymousUserEmbeddingExperienceConfiguration
    AnonymousUserEmbeddingExperienceConfiguration (..),
    newAnonymousUserEmbeddingExperienceConfiguration,
    anonymousUserEmbeddingExperienceConfiguration_dashboardVisual,
    anonymousUserEmbeddingExperienceConfiguration_dashboard,
    anonymousUserEmbeddingExperienceConfiguration_qSearchBar,

    -- * AnonymousUserQSearchBarEmbeddingConfiguration
    AnonymousUserQSearchBarEmbeddingConfiguration (..),
    newAnonymousUserQSearchBarEmbeddingConfiguration,
    anonymousUserQSearchBarEmbeddingConfiguration_initialTopicId,

    -- * AthenaParameters
    AthenaParameters (..),
    newAthenaParameters,
    athenaParameters_roleArn,
    athenaParameters_workGroup,

    -- * AuroraParameters
    AuroraParameters (..),
    newAuroraParameters,
    auroraParameters_host,
    auroraParameters_port,
    auroraParameters_database,

    -- * AuroraPostgreSqlParameters
    AuroraPostgreSqlParameters (..),
    newAuroraPostgreSqlParameters,
    auroraPostgreSqlParameters_host,
    auroraPostgreSqlParameters_port,
    auroraPostgreSqlParameters_database,

    -- * AwsIotAnalyticsParameters
    AwsIotAnalyticsParameters (..),
    newAwsIotAnalyticsParameters,
    awsIotAnalyticsParameters_dataSetName,

    -- * BorderStyle
    BorderStyle (..),
    newBorderStyle,
    borderStyle_show,

    -- * CalculatedColumn
    CalculatedColumn (..),
    newCalculatedColumn,
    calculatedColumn_columnName,
    calculatedColumn_columnId,
    calculatedColumn_expression,

    -- * CastColumnTypeOperation
    CastColumnTypeOperation (..),
    newCastColumnTypeOperation,
    castColumnTypeOperation_format,
    castColumnTypeOperation_columnName,
    castColumnTypeOperation_newColumnType,

    -- * ColumnDescription
    ColumnDescription (..),
    newColumnDescription,
    columnDescription_text,

    -- * ColumnGroup
    ColumnGroup (..),
    newColumnGroup,
    columnGroup_geoSpatialColumnGroup,

    -- * ColumnGroupColumnSchema
    ColumnGroupColumnSchema (..),
    newColumnGroupColumnSchema,
    columnGroupColumnSchema_name,

    -- * ColumnGroupSchema
    ColumnGroupSchema (..),
    newColumnGroupSchema,
    columnGroupSchema_columnGroupColumnSchemaList,
    columnGroupSchema_name,

    -- * ColumnLevelPermissionRule
    ColumnLevelPermissionRule (..),
    newColumnLevelPermissionRule,
    columnLevelPermissionRule_columnNames,
    columnLevelPermissionRule_principals,

    -- * ColumnSchema
    ColumnSchema (..),
    newColumnSchema,
    columnSchema_name,
    columnSchema_geographicRole,
    columnSchema_dataType,

    -- * ColumnTag
    ColumnTag (..),
    newColumnTag,
    columnTag_columnGeographicRole,
    columnTag_columnDescription,

    -- * CreateColumnsOperation
    CreateColumnsOperation (..),
    newCreateColumnsOperation,
    createColumnsOperation_columns,

    -- * CredentialPair
    CredentialPair (..),
    newCredentialPair,
    credentialPair_alternateDataSourceParameters,
    credentialPair_username,
    credentialPair_password,

    -- * CustomSql
    CustomSql (..),
    newCustomSql,
    customSql_columns,
    customSql_dataSourceArn,
    customSql_name,
    customSql_sqlQuery,

    -- * Dashboard
    Dashboard (..),
    newDashboard,
    dashboard_name,
    dashboard_createdTime,
    dashboard_arn,
    dashboard_lastUpdatedTime,
    dashboard_dashboardId,
    dashboard_version,
    dashboard_lastPublishedTime,

    -- * DashboardError
    DashboardError (..),
    newDashboardError,
    dashboardError_message,
    dashboardError_type,

    -- * DashboardPublishOptions
    DashboardPublishOptions (..),
    newDashboardPublishOptions,
    dashboardPublishOptions_adHocFilteringOption,
    dashboardPublishOptions_sheetControlsOption,
    dashboardPublishOptions_exportToCSVOption,

    -- * DashboardSearchFilter
    DashboardSearchFilter (..),
    newDashboardSearchFilter,
    dashboardSearchFilter_name,
    dashboardSearchFilter_value,
    dashboardSearchFilter_operator,

    -- * DashboardSourceEntity
    DashboardSourceEntity (..),
    newDashboardSourceEntity,
    dashboardSourceEntity_sourceTemplate,

    -- * DashboardSourceTemplate
    DashboardSourceTemplate (..),
    newDashboardSourceTemplate,
    dashboardSourceTemplate_dataSetReferences,
    dashboardSourceTemplate_arn,

    -- * DashboardSummary
    DashboardSummary (..),
    newDashboardSummary,
    dashboardSummary_name,
    dashboardSummary_createdTime,
    dashboardSummary_arn,
    dashboardSummary_lastUpdatedTime,
    dashboardSummary_publishedVersionNumber,
    dashboardSummary_dashboardId,
    dashboardSummary_lastPublishedTime,

    -- * DashboardVersion
    DashboardVersion (..),
    newDashboardVersion,
    dashboardVersion_sourceEntityArn,
    dashboardVersion_themeArn,
    dashboardVersion_createdTime,
    dashboardVersion_arn,
    dashboardVersion_status,
    dashboardVersion_description,
    dashboardVersion_errors,
    dashboardVersion_dataSetArns,
    dashboardVersion_versionNumber,
    dashboardVersion_sheets,

    -- * DashboardVersionSummary
    DashboardVersionSummary (..),
    newDashboardVersionSummary,
    dashboardVersionSummary_sourceEntityArn,
    dashboardVersionSummary_createdTime,
    dashboardVersionSummary_arn,
    dashboardVersionSummary_status,
    dashboardVersionSummary_description,
    dashboardVersionSummary_versionNumber,

    -- * DashboardVisualId
    DashboardVisualId (..),
    newDashboardVisualId,
    dashboardVisualId_dashboardId,
    dashboardVisualId_sheetId,
    dashboardVisualId_visualId,

    -- * DataColorPalette
    DataColorPalette (..),
    newDataColorPalette,
    dataColorPalette_minMaxGradient,
    dataColorPalette_emptyFillColor,
    dataColorPalette_colors,

    -- * DataSet
    DataSet (..),
    newDataSet,
    dataSet_name,
    dataSet_columnLevelPermissionRules,
    dataSet_createdTime,
    dataSet_dataSetUsageConfiguration,
    dataSet_arn,
    dataSet_outputColumns,
    dataSet_consumedSpiceCapacityInBytes,
    dataSet_rowLevelPermissionTagConfiguration,
    dataSet_lastUpdatedTime,
    dataSet_columnGroups,
    dataSet_fieldFolders,
    dataSet_rowLevelPermissionDataSet,
    dataSet_dataSetId,
    dataSet_logicalTableMap,
    dataSet_importMode,
    dataSet_physicalTableMap,

    -- * DataSetConfiguration
    DataSetConfiguration (..),
    newDataSetConfiguration,
    dataSetConfiguration_dataSetSchema,
    dataSetConfiguration_columnGroupSchemaList,
    dataSetConfiguration_placeholder,

    -- * DataSetReference
    DataSetReference (..),
    newDataSetReference,
    dataSetReference_dataSetPlaceholder,
    dataSetReference_dataSetArn,

    -- * DataSetSchema
    DataSetSchema (..),
    newDataSetSchema,
    dataSetSchema_columnSchemaList,

    -- * DataSetSearchFilter
    DataSetSearchFilter (..),
    newDataSetSearchFilter,
    dataSetSearchFilter_operator,
    dataSetSearchFilter_name,
    dataSetSearchFilter_value,

    -- * DataSetSummary
    DataSetSummary (..),
    newDataSetSummary,
    dataSetSummary_name,
    dataSetSummary_columnLevelPermissionRulesApplied,
    dataSetSummary_createdTime,
    dataSetSummary_arn,
    dataSetSummary_lastUpdatedTime,
    dataSetSummary_rowLevelPermissionDataSet,
    dataSetSummary_rowLevelPermissionTagConfigurationApplied,
    dataSetSummary_dataSetId,
    dataSetSummary_importMode,

    -- * DataSetUsageConfiguration
    DataSetUsageConfiguration (..),
    newDataSetUsageConfiguration,
    dataSetUsageConfiguration_disableUseAsDirectQuerySource,
    dataSetUsageConfiguration_disableUseAsImportedSource,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_name,
    dataSource_type,
    dataSource_dataSourceId,
    dataSource_createdTime,
    dataSource_dataSourceParameters,
    dataSource_arn,
    dataSource_status,
    dataSource_lastUpdatedTime,
    dataSource_vpcConnectionProperties,
    dataSource_sslProperties,
    dataSource_secretArn,
    dataSource_alternateDataSourceParameters,
    dataSource_errorInfo,

    -- * DataSourceCredentials
    DataSourceCredentials (..),
    newDataSourceCredentials,
    dataSourceCredentials_secretArn,
    dataSourceCredentials_credentialPair,
    dataSourceCredentials_copySourceArn,

    -- * DataSourceErrorInfo
    DataSourceErrorInfo (..),
    newDataSourceErrorInfo,
    dataSourceErrorInfo_message,
    dataSourceErrorInfo_type,

    -- * DataSourceParameters
    DataSourceParameters (..),
    newDataSourceParameters,
    dataSourceParameters_serviceNowParameters,
    dataSourceParameters_s3Parameters,
    dataSourceParameters_postgreSqlParameters,
    dataSourceParameters_mySqlParameters,
    dataSourceParameters_exasolParameters,
    dataSourceParameters_redshiftParameters,
    dataSourceParameters_awsIotAnalyticsParameters,
    dataSourceParameters_sparkParameters,
    dataSourceParameters_teradataParameters,
    dataSourceParameters_twitterParameters,
    dataSourceParameters_prestoParameters,
    dataSourceParameters_snowflakeParameters,
    dataSourceParameters_rdsParameters,
    dataSourceParameters_oracleParameters,
    dataSourceParameters_auroraPostgreSqlParameters,
    dataSourceParameters_mariaDbParameters,
    dataSourceParameters_athenaParameters,
    dataSourceParameters_amazonOpenSearchParameters,
    dataSourceParameters_databricksParameters,
    dataSourceParameters_jiraParameters,
    dataSourceParameters_amazonElasticsearchParameters,
    dataSourceParameters_sqlServerParameters,
    dataSourceParameters_auroraParameters,

    -- * DataSourceSearchFilter
    DataSourceSearchFilter (..),
    newDataSourceSearchFilter,
    dataSourceSearchFilter_operator,
    dataSourceSearchFilter_name,
    dataSourceSearchFilter_value,

    -- * DataSourceSummary
    DataSourceSummary (..),
    newDataSourceSummary,
    dataSourceSummary_name,
    dataSourceSummary_type,
    dataSourceSummary_dataSourceId,
    dataSourceSummary_createdTime,
    dataSourceSummary_arn,
    dataSourceSummary_lastUpdatedTime,

    -- * DatabricksParameters
    DatabricksParameters (..),
    newDatabricksParameters,
    databricksParameters_host,
    databricksParameters_port,
    databricksParameters_sqlEndpointPath,

    -- * DateTimeParameter
    DateTimeParameter (..),
    newDateTimeParameter,
    dateTimeParameter_name,
    dateTimeParameter_values,

    -- * DecimalParameter
    DecimalParameter (..),
    newDecimalParameter,
    decimalParameter_name,
    decimalParameter_values,

    -- * ErrorInfo
    ErrorInfo (..),
    newErrorInfo,
    errorInfo_message,
    errorInfo_type,

    -- * ExasolParameters
    ExasolParameters (..),
    newExasolParameters,
    exasolParameters_host,
    exasolParameters_port,

    -- * ExportToCSVOption
    ExportToCSVOption (..),
    newExportToCSVOption,
    exportToCSVOption_availabilityStatus,

    -- * FieldFolder
    FieldFolder (..),
    newFieldFolder,
    fieldFolder_columns,
    fieldFolder_description,

    -- * FilterOperation
    FilterOperation (..),
    newFilterOperation,
    filterOperation_conditionExpression,

    -- * Folder
    Folder (..),
    newFolder,
    folder_name,
    folder_createdTime,
    folder_arn,
    folder_lastUpdatedTime,
    folder_folderId,
    folder_folderType,
    folder_folderPath,

    -- * FolderMember
    FolderMember (..),
    newFolderMember,
    folderMember_memberId,
    folderMember_memberType,

    -- * FolderSearchFilter
    FolderSearchFilter (..),
    newFolderSearchFilter,
    folderSearchFilter_name,
    folderSearchFilter_operator,
    folderSearchFilter_value,

    -- * FolderSummary
    FolderSummary (..),
    newFolderSummary,
    folderSummary_name,
    folderSummary_createdTime,
    folderSummary_arn,
    folderSummary_lastUpdatedTime,
    folderSummary_folderId,
    folderSummary_folderType,

    -- * GeoSpatialColumnGroup
    GeoSpatialColumnGroup (..),
    newGeoSpatialColumnGroup,
    geoSpatialColumnGroup_countryCode,
    geoSpatialColumnGroup_name,
    geoSpatialColumnGroup_columns,

    -- * Group
    Group (..),
    newGroup,
    group_principalId,
    group_arn,
    group_groupName,
    group_description,

    -- * GroupMember
    GroupMember (..),
    newGroupMember,
    groupMember_memberName,
    groupMember_arn,

    -- * GroupSearchFilter
    GroupSearchFilter (..),
    newGroupSearchFilter,
    groupSearchFilter_operator,
    groupSearchFilter_name,
    groupSearchFilter_value,

    -- * GutterStyle
    GutterStyle (..),
    newGutterStyle,
    gutterStyle_show,

    -- * IAMPolicyAssignment
    IAMPolicyAssignment (..),
    newIAMPolicyAssignment,
    iAMPolicyAssignment_awsAccountId,
    iAMPolicyAssignment_identities,
    iAMPolicyAssignment_policyArn,
    iAMPolicyAssignment_assignmentName,
    iAMPolicyAssignment_assignmentId,
    iAMPolicyAssignment_assignmentStatus,

    -- * IAMPolicyAssignmentSummary
    IAMPolicyAssignmentSummary (..),
    newIAMPolicyAssignmentSummary,
    iAMPolicyAssignmentSummary_assignmentName,
    iAMPolicyAssignmentSummary_assignmentStatus,

    -- * Ingestion
    Ingestion (..),
    newIngestion,
    ingestion_requestType,
    ingestion_ingestionSizeInBytes,
    ingestion_ingestionTimeInSeconds,
    ingestion_requestSource,
    ingestion_queueInfo,
    ingestion_rowInfo,
    ingestion_ingestionId,
    ingestion_errorInfo,
    ingestion_arn,
    ingestion_ingestionStatus,
    ingestion_createdTime,

    -- * InputColumn
    InputColumn (..),
    newInputColumn,
    inputColumn_name,
    inputColumn_type,

    -- * IntegerParameter
    IntegerParameter (..),
    newIntegerParameter,
    integerParameter_name,
    integerParameter_values,

    -- * JiraParameters
    JiraParameters (..),
    newJiraParameters,
    jiraParameters_siteBaseUrl,

    -- * JoinInstruction
    JoinInstruction (..),
    newJoinInstruction,
    joinInstruction_leftJoinKeyProperties,
    joinInstruction_rightJoinKeyProperties,
    joinInstruction_leftOperand,
    joinInstruction_rightOperand,
    joinInstruction_type,
    joinInstruction_onClause,

    -- * JoinKeyProperties
    JoinKeyProperties (..),
    newJoinKeyProperties,
    joinKeyProperties_uniqueKey,

    -- * LinkSharingConfiguration
    LinkSharingConfiguration (..),
    newLinkSharingConfiguration,
    linkSharingConfiguration_permissions,

    -- * LogicalTable
    LogicalTable (..),
    newLogicalTable,
    logicalTable_dataTransforms,
    logicalTable_alias,
    logicalTable_source,

    -- * LogicalTableSource
    LogicalTableSource (..),
    newLogicalTableSource,
    logicalTableSource_physicalTableId,
    logicalTableSource_joinInstruction,
    logicalTableSource_dataSetArn,

    -- * ManifestFileLocation
    ManifestFileLocation (..),
    newManifestFileLocation,
    manifestFileLocation_bucket,
    manifestFileLocation_key,

    -- * MarginStyle
    MarginStyle (..),
    newMarginStyle,
    marginStyle_show,

    -- * MariaDbParameters
    MariaDbParameters (..),
    newMariaDbParameters,
    mariaDbParameters_host,
    mariaDbParameters_port,
    mariaDbParameters_database,

    -- * MemberIdArnPair
    MemberIdArnPair (..),
    newMemberIdArnPair,
    memberIdArnPair_memberId,
    memberIdArnPair_memberArn,

    -- * MySqlParameters
    MySqlParameters (..),
    newMySqlParameters,
    mySqlParameters_host,
    mySqlParameters_port,
    mySqlParameters_database,

    -- * NamespaceError
    NamespaceError (..),
    newNamespaceError,
    namespaceError_message,
    namespaceError_type,

    -- * NamespaceInfoV2
    NamespaceInfoV2 (..),
    newNamespaceInfoV2,
    namespaceInfoV2_creationStatus,
    namespaceInfoV2_name,
    namespaceInfoV2_namespaceError,
    namespaceInfoV2_arn,
    namespaceInfoV2_capacityRegion,
    namespaceInfoV2_identityStore,

    -- * OracleParameters
    OracleParameters (..),
    newOracleParameters,
    oracleParameters_host,
    oracleParameters_port,
    oracleParameters_database,

    -- * OutputColumn
    OutputColumn (..),
    newOutputColumn,
    outputColumn_name,
    outputColumn_type,
    outputColumn_description,

    -- * Parameters
    Parameters (..),
    newParameters,
    parameters_decimalParameters,
    parameters_dateTimeParameters,
    parameters_integerParameters,
    parameters_stringParameters,

    -- * PhysicalTable
    PhysicalTable (..),
    newPhysicalTable,
    physicalTable_s3Source,
    physicalTable_relationalTable,
    physicalTable_customSql,

    -- * PostgreSqlParameters
    PostgreSqlParameters (..),
    newPostgreSqlParameters,
    postgreSqlParameters_host,
    postgreSqlParameters_port,
    postgreSqlParameters_database,

    -- * PrestoParameters
    PrestoParameters (..),
    newPrestoParameters,
    prestoParameters_host,
    prestoParameters_port,
    prestoParameters_catalog,

    -- * ProjectOperation
    ProjectOperation (..),
    newProjectOperation,
    projectOperation_projectedColumns,

    -- * QueueInfo
    QueueInfo (..),
    newQueueInfo,
    queueInfo_waitingOnIngestion,
    queueInfo_queuedIngestion,

    -- * RdsParameters
    RdsParameters (..),
    newRdsParameters,
    rdsParameters_instanceId,
    rdsParameters_database,

    -- * RedshiftParameters
    RedshiftParameters (..),
    newRedshiftParameters,
    redshiftParameters_port,
    redshiftParameters_host,
    redshiftParameters_clusterId,
    redshiftParameters_database,

    -- * RegisteredUserDashboardEmbeddingConfiguration
    RegisteredUserDashboardEmbeddingConfiguration (..),
    newRegisteredUserDashboardEmbeddingConfiguration,
    registeredUserDashboardEmbeddingConfiguration_initialDashboardId,

    -- * RegisteredUserDashboardVisualEmbeddingConfiguration
    RegisteredUserDashboardVisualEmbeddingConfiguration (..),
    newRegisteredUserDashboardVisualEmbeddingConfiguration,
    registeredUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId,

    -- * RegisteredUserEmbeddingExperienceConfiguration
    RegisteredUserEmbeddingExperienceConfiguration (..),
    newRegisteredUserEmbeddingExperienceConfiguration,
    registeredUserEmbeddingExperienceConfiguration_dashboardVisual,
    registeredUserEmbeddingExperienceConfiguration_dashboard,
    registeredUserEmbeddingExperienceConfiguration_qSearchBar,
    registeredUserEmbeddingExperienceConfiguration_quickSightConsole,

    -- * RegisteredUserQSearchBarEmbeddingConfiguration
    RegisteredUserQSearchBarEmbeddingConfiguration (..),
    newRegisteredUserQSearchBarEmbeddingConfiguration,
    registeredUserQSearchBarEmbeddingConfiguration_initialTopicId,

    -- * RegisteredUserQuickSightConsoleEmbeddingConfiguration
    RegisteredUserQuickSightConsoleEmbeddingConfiguration (..),
    newRegisteredUserQuickSightConsoleEmbeddingConfiguration,
    registeredUserQuickSightConsoleEmbeddingConfiguration_initialPath,

    -- * RelationalTable
    RelationalTable (..),
    newRelationalTable,
    relationalTable_catalog,
    relationalTable_schema,
    relationalTable_dataSourceArn,
    relationalTable_name,
    relationalTable_inputColumns,

    -- * RenameColumnOperation
    RenameColumnOperation (..),
    newRenameColumnOperation,
    renameColumnOperation_columnName,
    renameColumnOperation_newColumnName,

    -- * ResourcePermission
    ResourcePermission (..),
    newResourcePermission,
    resourcePermission_principal,
    resourcePermission_actions,

    -- * RowInfo
    RowInfo (..),
    newRowInfo,
    rowInfo_totalRowsInDataset,
    rowInfo_rowsDropped,
    rowInfo_rowsIngested,

    -- * RowLevelPermissionDataSet
    RowLevelPermissionDataSet (..),
    newRowLevelPermissionDataSet,
    rowLevelPermissionDataSet_formatVersion,
    rowLevelPermissionDataSet_status,
    rowLevelPermissionDataSet_namespace,
    rowLevelPermissionDataSet_arn,
    rowLevelPermissionDataSet_permissionPolicy,

    -- * RowLevelPermissionTagConfiguration
    RowLevelPermissionTagConfiguration (..),
    newRowLevelPermissionTagConfiguration,
    rowLevelPermissionTagConfiguration_status,
    rowLevelPermissionTagConfiguration_tagRules,

    -- * RowLevelPermissionTagRule
    RowLevelPermissionTagRule (..),
    newRowLevelPermissionTagRule,
    rowLevelPermissionTagRule_matchAllValue,
    rowLevelPermissionTagRule_tagMultiValueDelimiter,
    rowLevelPermissionTagRule_tagKey,
    rowLevelPermissionTagRule_columnName,

    -- * S3Parameters
    S3Parameters (..),
    newS3Parameters,
    s3Parameters_manifestFileLocation,

    -- * S3Source
    S3Source (..),
    newS3Source,
    s3Source_uploadSettings,
    s3Source_dataSourceArn,
    s3Source_inputColumns,

    -- * ServiceNowParameters
    ServiceNowParameters (..),
    newServiceNowParameters,
    serviceNowParameters_siteBaseUrl,

    -- * SessionTag
    SessionTag (..),
    newSessionTag,
    sessionTag_key,
    sessionTag_value,

    -- * Sheet
    Sheet (..),
    newSheet,
    sheet_name,
    sheet_sheetId,

    -- * SheetControlsOption
    SheetControlsOption (..),
    newSheetControlsOption,
    sheetControlsOption_visibilityState,

    -- * SheetStyle
    SheetStyle (..),
    newSheetStyle,
    sheetStyle_tile,
    sheetStyle_tileLayout,

    -- * SignupResponse
    SignupResponse (..),
    newSignupResponse,
    signupResponse_directoryType,
    signupResponse_userLoginName,
    signupResponse_iAMUser,
    signupResponse_accountName,

    -- * SnowflakeParameters
    SnowflakeParameters (..),
    newSnowflakeParameters,
    snowflakeParameters_host,
    snowflakeParameters_database,
    snowflakeParameters_warehouse,

    -- * SparkParameters
    SparkParameters (..),
    newSparkParameters,
    sparkParameters_host,
    sparkParameters_port,

    -- * SqlServerParameters
    SqlServerParameters (..),
    newSqlServerParameters,
    sqlServerParameters_host,
    sqlServerParameters_port,
    sqlServerParameters_database,

    -- * SslProperties
    SslProperties (..),
    newSslProperties,
    sslProperties_disableSsl,

    -- * StringParameter
    StringParameter (..),
    newStringParameter,
    stringParameter_name,
    stringParameter_values,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagColumnOperation
    TagColumnOperation (..),
    newTagColumnOperation,
    tagColumnOperation_columnName,
    tagColumnOperation_tags,

    -- * Template
    Template (..),
    newTemplate,
    template_name,
    template_createdTime,
    template_arn,
    template_lastUpdatedTime,
    template_templateId,
    template_version,

    -- * TemplateAlias
    TemplateAlias (..),
    newTemplateAlias,
    templateAlias_templateVersionNumber,
    templateAlias_arn,
    templateAlias_aliasName,

    -- * TemplateError
    TemplateError (..),
    newTemplateError,
    templateError_message,
    templateError_type,

    -- * TemplateSourceAnalysis
    TemplateSourceAnalysis (..),
    newTemplateSourceAnalysis,
    templateSourceAnalysis_arn,
    templateSourceAnalysis_dataSetReferences,

    -- * TemplateSourceEntity
    TemplateSourceEntity (..),
    newTemplateSourceEntity,
    templateSourceEntity_sourceAnalysis,
    templateSourceEntity_sourceTemplate,

    -- * TemplateSourceTemplate
    TemplateSourceTemplate (..),
    newTemplateSourceTemplate,
    templateSourceTemplate_arn,

    -- * TemplateSummary
    TemplateSummary (..),
    newTemplateSummary,
    templateSummary_name,
    templateSummary_createdTime,
    templateSummary_latestVersionNumber,
    templateSummary_arn,
    templateSummary_lastUpdatedTime,
    templateSummary_templateId,

    -- * TemplateVersion
    TemplateVersion (..),
    newTemplateVersion,
    templateVersion_sourceEntityArn,
    templateVersion_themeArn,
    templateVersion_createdTime,
    templateVersion_status,
    templateVersion_description,
    templateVersion_errors,
    templateVersion_versionNumber,
    templateVersion_sheets,
    templateVersion_dataSetConfigurations,

    -- * TemplateVersionSummary
    TemplateVersionSummary (..),
    newTemplateVersionSummary,
    templateVersionSummary_createdTime,
    templateVersionSummary_arn,
    templateVersionSummary_status,
    templateVersionSummary_description,
    templateVersionSummary_versionNumber,

    -- * TeradataParameters
    TeradataParameters (..),
    newTeradataParameters,
    teradataParameters_host,
    teradataParameters_port,
    teradataParameters_database,

    -- * Theme
    Theme (..),
    newTheme,
    theme_name,
    theme_type,
    theme_createdTime,
    theme_arn,
    theme_lastUpdatedTime,
    theme_themeId,
    theme_version,

    -- * ThemeAlias
    ThemeAlias (..),
    newThemeAlias,
    themeAlias_arn,
    themeAlias_aliasName,
    themeAlias_themeVersionNumber,

    -- * ThemeConfiguration
    ThemeConfiguration (..),
    newThemeConfiguration,
    themeConfiguration_dataColorPalette,
    themeConfiguration_sheet,
    themeConfiguration_uIColorPalette,

    -- * ThemeError
    ThemeError (..),
    newThemeError,
    themeError_message,
    themeError_type,

    -- * ThemeSummary
    ThemeSummary (..),
    newThemeSummary,
    themeSummary_name,
    themeSummary_createdTime,
    themeSummary_latestVersionNumber,
    themeSummary_arn,
    themeSummary_lastUpdatedTime,
    themeSummary_themeId,

    -- * ThemeVersion
    ThemeVersion (..),
    newThemeVersion,
    themeVersion_createdTime,
    themeVersion_configuration,
    themeVersion_arn,
    themeVersion_status,
    themeVersion_description,
    themeVersion_errors,
    themeVersion_versionNumber,
    themeVersion_baseThemeId,

    -- * ThemeVersionSummary
    ThemeVersionSummary (..),
    newThemeVersionSummary,
    themeVersionSummary_createdTime,
    themeVersionSummary_arn,
    themeVersionSummary_status,
    themeVersionSummary_description,
    themeVersionSummary_versionNumber,

    -- * TileLayoutStyle
    TileLayoutStyle (..),
    newTileLayoutStyle,
    tileLayoutStyle_gutter,
    tileLayoutStyle_margin,

    -- * TileStyle
    TileStyle (..),
    newTileStyle,
    tileStyle_border,

    -- * TransformOperation
    TransformOperation (..),
    newTransformOperation,
    transformOperation_untagColumnOperation,
    transformOperation_createColumnsOperation,
    transformOperation_renameColumnOperation,
    transformOperation_projectOperation,
    transformOperation_tagColumnOperation,
    transformOperation_filterOperation,
    transformOperation_castColumnTypeOperation,

    -- * TwitterParameters
    TwitterParameters (..),
    newTwitterParameters,
    twitterParameters_query,
    twitterParameters_maxRows,

    -- * UIColorPalette
    UIColorPalette (..),
    newUIColorPalette,
    uIColorPalette_accentForeground,
    uIColorPalette_danger,
    uIColorPalette_dangerForeground,
    uIColorPalette_secondaryBackground,
    uIColorPalette_primaryBackground,
    uIColorPalette_warningForeground,
    uIColorPalette_dimensionForeground,
    uIColorPalette_warning,
    uIColorPalette_successForeground,
    uIColorPalette_primaryForeground,
    uIColorPalette_secondaryForeground,
    uIColorPalette_measure,
    uIColorPalette_dimension,
    uIColorPalette_accent,
    uIColorPalette_success,
    uIColorPalette_measureForeground,

    -- * UntagColumnOperation
    UntagColumnOperation (..),
    newUntagColumnOperation,
    untagColumnOperation_columnName,
    untagColumnOperation_tagNames,

    -- * UploadSettings
    UploadSettings (..),
    newUploadSettings,
    uploadSettings_containsHeader,
    uploadSettings_format,
    uploadSettings_textQualifier,
    uploadSettings_delimiter,
    uploadSettings_startFromRow,

    -- * User
    User (..),
    newUser,
    user_principalId,
    user_externalLoginFederationProviderType,
    user_active,
    user_email,
    user_userName,
    user_externalLoginFederationProviderUrl,
    user_arn,
    user_role,
    user_externalLoginId,
    user_identityType,
    user_customPermissionsName,

    -- * VpcConnectionProperties
    VpcConnectionProperties (..),
    newVpcConnectionProperties,
    vpcConnectionProperties_vpcConnectionArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AccountCustomization
import Amazonka.QuickSight.Types.AccountInfo
import Amazonka.QuickSight.Types.AccountSettings
import Amazonka.QuickSight.Types.ActiveIAMPolicyAssignment
import Amazonka.QuickSight.Types.AdHocFilteringOption
import Amazonka.QuickSight.Types.AmazonElasticsearchParameters
import Amazonka.QuickSight.Types.AmazonOpenSearchParameters
import Amazonka.QuickSight.Types.Analysis
import Amazonka.QuickSight.Types.AnalysisError
import Amazonka.QuickSight.Types.AnalysisErrorType
import Amazonka.QuickSight.Types.AnalysisFilterAttribute
import Amazonka.QuickSight.Types.AnalysisSearchFilter
import Amazonka.QuickSight.Types.AnalysisSourceEntity
import Amazonka.QuickSight.Types.AnalysisSourceTemplate
import Amazonka.QuickSight.Types.AnalysisSummary
import Amazonka.QuickSight.Types.AnonymousUserDashboardEmbeddingConfiguration
import Amazonka.QuickSight.Types.AnonymousUserDashboardVisualEmbeddingConfiguration
import Amazonka.QuickSight.Types.AnonymousUserEmbeddingExperienceConfiguration
import Amazonka.QuickSight.Types.AnonymousUserQSearchBarEmbeddingConfiguration
import Amazonka.QuickSight.Types.AssignmentStatus
import Amazonka.QuickSight.Types.AthenaParameters
import Amazonka.QuickSight.Types.AuroraParameters
import Amazonka.QuickSight.Types.AuroraPostgreSqlParameters
import Amazonka.QuickSight.Types.AuthenticationMethodOption
import Amazonka.QuickSight.Types.AwsIotAnalyticsParameters
import Amazonka.QuickSight.Types.BorderStyle
import Amazonka.QuickSight.Types.CalculatedColumn
import Amazonka.QuickSight.Types.CastColumnTypeOperation
import Amazonka.QuickSight.Types.ColumnDataType
import Amazonka.QuickSight.Types.ColumnDescription
import Amazonka.QuickSight.Types.ColumnGroup
import Amazonka.QuickSight.Types.ColumnGroupColumnSchema
import Amazonka.QuickSight.Types.ColumnGroupSchema
import Amazonka.QuickSight.Types.ColumnLevelPermissionRule
import Amazonka.QuickSight.Types.ColumnSchema
import Amazonka.QuickSight.Types.ColumnTag
import Amazonka.QuickSight.Types.ColumnTagName
import Amazonka.QuickSight.Types.CreateColumnsOperation
import Amazonka.QuickSight.Types.CredentialPair
import Amazonka.QuickSight.Types.CustomSql
import Amazonka.QuickSight.Types.Dashboard
import Amazonka.QuickSight.Types.DashboardBehavior
import Amazonka.QuickSight.Types.DashboardError
import Amazonka.QuickSight.Types.DashboardErrorType
import Amazonka.QuickSight.Types.DashboardFilterAttribute
import Amazonka.QuickSight.Types.DashboardPublishOptions
import Amazonka.QuickSight.Types.DashboardSearchFilter
import Amazonka.QuickSight.Types.DashboardSourceEntity
import Amazonka.QuickSight.Types.DashboardSourceTemplate
import Amazonka.QuickSight.Types.DashboardSummary
import Amazonka.QuickSight.Types.DashboardUIState
import Amazonka.QuickSight.Types.DashboardVersion
import Amazonka.QuickSight.Types.DashboardVersionSummary
import Amazonka.QuickSight.Types.DashboardVisualId
import Amazonka.QuickSight.Types.DataColorPalette
import Amazonka.QuickSight.Types.DataSet
import Amazonka.QuickSight.Types.DataSetConfiguration
import Amazonka.QuickSight.Types.DataSetFilterAttribute
import Amazonka.QuickSight.Types.DataSetImportMode
import Amazonka.QuickSight.Types.DataSetReference
import Amazonka.QuickSight.Types.DataSetSchema
import Amazonka.QuickSight.Types.DataSetSearchFilter
import Amazonka.QuickSight.Types.DataSetSummary
import Amazonka.QuickSight.Types.DataSetUsageConfiguration
import Amazonka.QuickSight.Types.DataSource
import Amazonka.QuickSight.Types.DataSourceCredentials
import Amazonka.QuickSight.Types.DataSourceErrorInfo
import Amazonka.QuickSight.Types.DataSourceErrorInfoType
import Amazonka.QuickSight.Types.DataSourceFilterAttribute
import Amazonka.QuickSight.Types.DataSourceParameters
import Amazonka.QuickSight.Types.DataSourceSearchFilter
import Amazonka.QuickSight.Types.DataSourceSummary
import Amazonka.QuickSight.Types.DataSourceType
import Amazonka.QuickSight.Types.DatabricksParameters
import Amazonka.QuickSight.Types.DateTimeParameter
import Amazonka.QuickSight.Types.DecimalParameter
import Amazonka.QuickSight.Types.Edition
import Amazonka.QuickSight.Types.EmbeddingIdentityType
import Amazonka.QuickSight.Types.ErrorInfo
import Amazonka.QuickSight.Types.ExasolParameters
import Amazonka.QuickSight.Types.ExportToCSVOption
import Amazonka.QuickSight.Types.FieldFolder
import Amazonka.QuickSight.Types.FileFormat
import Amazonka.QuickSight.Types.FilterOperation
import Amazonka.QuickSight.Types.FilterOperator
import Amazonka.QuickSight.Types.Folder
import Amazonka.QuickSight.Types.FolderFilterAttribute
import Amazonka.QuickSight.Types.FolderMember
import Amazonka.QuickSight.Types.FolderSearchFilter
import Amazonka.QuickSight.Types.FolderSummary
import Amazonka.QuickSight.Types.FolderType
import Amazonka.QuickSight.Types.GeoSpatialColumnGroup
import Amazonka.QuickSight.Types.GeoSpatialCountryCode
import Amazonka.QuickSight.Types.GeoSpatialDataRole
import Amazonka.QuickSight.Types.Group
import Amazonka.QuickSight.Types.GroupFilterAttribute
import Amazonka.QuickSight.Types.GroupFilterOperator
import Amazonka.QuickSight.Types.GroupMember
import Amazonka.QuickSight.Types.GroupSearchFilter
import Amazonka.QuickSight.Types.GutterStyle
import Amazonka.QuickSight.Types.IAMPolicyAssignment
import Amazonka.QuickSight.Types.IAMPolicyAssignmentSummary
import Amazonka.QuickSight.Types.IdentityStore
import Amazonka.QuickSight.Types.IdentityType
import Amazonka.QuickSight.Types.Ingestion
import Amazonka.QuickSight.Types.IngestionErrorType
import Amazonka.QuickSight.Types.IngestionRequestSource
import Amazonka.QuickSight.Types.IngestionRequestType
import Amazonka.QuickSight.Types.IngestionStatus
import Amazonka.QuickSight.Types.IngestionType
import Amazonka.QuickSight.Types.InputColumn
import Amazonka.QuickSight.Types.InputColumnDataType
import Amazonka.QuickSight.Types.IntegerParameter
import Amazonka.QuickSight.Types.JiraParameters
import Amazonka.QuickSight.Types.JoinInstruction
import Amazonka.QuickSight.Types.JoinKeyProperties
import Amazonka.QuickSight.Types.JoinType
import Amazonka.QuickSight.Types.LinkSharingConfiguration
import Amazonka.QuickSight.Types.LogicalTable
import Amazonka.QuickSight.Types.LogicalTableSource
import Amazonka.QuickSight.Types.ManifestFileLocation
import Amazonka.QuickSight.Types.MarginStyle
import Amazonka.QuickSight.Types.MariaDbParameters
import Amazonka.QuickSight.Types.MemberIdArnPair
import Amazonka.QuickSight.Types.MemberType
import Amazonka.QuickSight.Types.MySqlParameters
import Amazonka.QuickSight.Types.NamespaceError
import Amazonka.QuickSight.Types.NamespaceErrorType
import Amazonka.QuickSight.Types.NamespaceInfoV2
import Amazonka.QuickSight.Types.NamespaceStatus
import Amazonka.QuickSight.Types.OracleParameters
import Amazonka.QuickSight.Types.OutputColumn
import Amazonka.QuickSight.Types.Parameters
import Amazonka.QuickSight.Types.PhysicalTable
import Amazonka.QuickSight.Types.PostgreSqlParameters
import Amazonka.QuickSight.Types.PrestoParameters
import Amazonka.QuickSight.Types.ProjectOperation
import Amazonka.QuickSight.Types.QueueInfo
import Amazonka.QuickSight.Types.RdsParameters
import Amazonka.QuickSight.Types.RedshiftParameters
import Amazonka.QuickSight.Types.RegisteredUserDashboardEmbeddingConfiguration
import Amazonka.QuickSight.Types.RegisteredUserDashboardVisualEmbeddingConfiguration
import Amazonka.QuickSight.Types.RegisteredUserEmbeddingExperienceConfiguration
import Amazonka.QuickSight.Types.RegisteredUserQSearchBarEmbeddingConfiguration
import Amazonka.QuickSight.Types.RegisteredUserQuickSightConsoleEmbeddingConfiguration
import Amazonka.QuickSight.Types.RelationalTable
import Amazonka.QuickSight.Types.RenameColumnOperation
import Amazonka.QuickSight.Types.ResourcePermission
import Amazonka.QuickSight.Types.ResourceStatus
import Amazonka.QuickSight.Types.RowInfo
import Amazonka.QuickSight.Types.RowLevelPermissionDataSet
import Amazonka.QuickSight.Types.RowLevelPermissionFormatVersion
import Amazonka.QuickSight.Types.RowLevelPermissionPolicy
import Amazonka.QuickSight.Types.RowLevelPermissionTagConfiguration
import Amazonka.QuickSight.Types.RowLevelPermissionTagRule
import Amazonka.QuickSight.Types.S3Parameters
import Amazonka.QuickSight.Types.S3Source
import Amazonka.QuickSight.Types.ServiceNowParameters
import Amazonka.QuickSight.Types.SessionTag
import Amazonka.QuickSight.Types.Sheet
import Amazonka.QuickSight.Types.SheetControlsOption
import Amazonka.QuickSight.Types.SheetStyle
import Amazonka.QuickSight.Types.SignupResponse
import Amazonka.QuickSight.Types.SnowflakeParameters
import Amazonka.QuickSight.Types.SparkParameters
import Amazonka.QuickSight.Types.SqlServerParameters
import Amazonka.QuickSight.Types.SslProperties
import Amazonka.QuickSight.Types.Status
import Amazonka.QuickSight.Types.StringParameter
import Amazonka.QuickSight.Types.Tag
import Amazonka.QuickSight.Types.TagColumnOperation
import Amazonka.QuickSight.Types.Template
import Amazonka.QuickSight.Types.TemplateAlias
import Amazonka.QuickSight.Types.TemplateError
import Amazonka.QuickSight.Types.TemplateErrorType
import Amazonka.QuickSight.Types.TemplateSourceAnalysis
import Amazonka.QuickSight.Types.TemplateSourceEntity
import Amazonka.QuickSight.Types.TemplateSourceTemplate
import Amazonka.QuickSight.Types.TemplateSummary
import Amazonka.QuickSight.Types.TemplateVersion
import Amazonka.QuickSight.Types.TemplateVersionSummary
import Amazonka.QuickSight.Types.TeradataParameters
import Amazonka.QuickSight.Types.TextQualifier
import Amazonka.QuickSight.Types.Theme
import Amazonka.QuickSight.Types.ThemeAlias
import Amazonka.QuickSight.Types.ThemeConfiguration
import Amazonka.QuickSight.Types.ThemeError
import Amazonka.QuickSight.Types.ThemeErrorType
import Amazonka.QuickSight.Types.ThemeSummary
import Amazonka.QuickSight.Types.ThemeType
import Amazonka.QuickSight.Types.ThemeVersion
import Amazonka.QuickSight.Types.ThemeVersionSummary
import Amazonka.QuickSight.Types.TileLayoutStyle
import Amazonka.QuickSight.Types.TileStyle
import Amazonka.QuickSight.Types.TransformOperation
import Amazonka.QuickSight.Types.TwitterParameters
import Amazonka.QuickSight.Types.UIColorPalette
import Amazonka.QuickSight.Types.UntagColumnOperation
import Amazonka.QuickSight.Types.UploadSettings
import Amazonka.QuickSight.Types.User
import Amazonka.QuickSight.Types.UserRole
import Amazonka.QuickSight.Types.VpcConnectionProperties
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-04-01@ of the Amazon QuickSight SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "QuickSight",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "quicksight",
      Core.signingName = "quicksight",
      Core.version = "2018-04-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "QuickSight",
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

-- | The identity type specified isn\'t supported. Supported identity types
-- include @IAM@ and @QUICKSIGHT@.
_IdentityTypeNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdentityTypeNotSupportedException =
  Core._MatchServiceError
    defaultService
    "IdentityTypeNotSupportedException"
    Prelude.. Core.hasStatus 403

-- | This resource is currently unavailable.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | This error indicates that you are calling an embedding operation in
-- Amazon QuickSight without the required pricing plan on your Amazon Web
-- Services account. Before you can use embedding for anonymous users, a
-- QuickSight administrator needs to add capacity pricing to Amazon
-- QuickSight. You can do this on the __Manage Amazon QuickSight__ page.
--
-- After capacity pricing is added, you can use the
-- @ GetDashboardEmbedUrl @ API operation with the
-- @--identity-type ANONYMOUS@ option.
_UnsupportedPricingPlanException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedPricingPlanException =
  Core._MatchServiceError
    defaultService
    "UnsupportedPricingPlanException"
    Prelude.. Core.hasStatus 403

-- | You don\'t have access to this item. The provided credentials couldn\'t
-- be validated. You might not be authorized to carry out the request. Make
-- sure that your account is authorized to use the Amazon QuickSight
-- service, that your policies have the correct permissions, and that you
-- are using the correct access keys.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 401

-- | One or more preconditions aren\'t met.
_PreconditionNotMetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionNotMetException =
  Core._MatchServiceError
    defaultService
    "PreconditionNotMetException"
    Prelude.. Core.hasStatus 400

-- | The user with the provided name isn\'t found. This error can happen in
-- any operation that requires finding a user based on a provided user
-- name, such as @DeleteUser@, @DescribeUser@, and so on.
_QuickSightUserNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_QuickSightUserNotFoundException =
  Core._MatchServiceError
    defaultService
    "QuickSightUserNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A resource is already in a state that indicates an operation is
-- happening that must complete before a new update can be applied.
_ConcurrentUpdatingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentUpdatingException =
  Core._MatchServiceError
    defaultService
    "ConcurrentUpdatingException"
    Prelude.. Core.hasStatus 500

-- | One or more resources can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The number of minutes specified for the lifetime of a session isn\'t
-- valid. The session lifetime must be 15-600 minutes.
_SessionLifetimeInMinutesInvalidException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SessionLifetimeInMinutesInvalidException =
  Core._MatchServiceError
    defaultService
    "SessionLifetimeInMinutesInvalidException"
    Prelude.. Core.hasStatus 400

-- | This error indicates that you are calling an operation on an Amazon
-- QuickSight subscription where the edition doesn\'t include support for
-- that operation. Amazon Amazon QuickSight currently has Standard Edition
-- and Enterprise Edition. Not every operation and capability is available
-- in every edition.
_UnsupportedUserEditionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedUserEditionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedUserEditionException"
    Prelude.. Core.hasStatus 403

-- | A limit is exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409

-- | The @NextToken@ value isn\'t valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Core.hasStatus 400

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Access is throttled.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The resource specified already exists.
_ResourceExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"
    Prelude.. Core.hasStatus 409

-- | The domain specified isn\'t on the allow list. All domains for embedded
-- dashboards must be added to the approved list by an Amazon QuickSight
-- admin.
_DomainNotWhitelistedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainNotWhitelistedException =
  Core._MatchServiceError
    defaultService
    "DomainNotWhitelistedException"
    Prelude.. Core.hasStatus 403

-- | You don\'t have this feature activated for your account. To fix this
-- issue, contact Amazon Web Services support.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | One or more parameters has a value that isn\'t valid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Core.hasStatus 400

-- | An internal failure occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500
