{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QuickSight.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ResourceUnavailableException,
    _IdentityTypeNotSupportedException,
    _DomainNotWhitelistedException,
    _ConflictException,
    _UnsupportedPricingPlanException,
    _InvalidParameterValueException,
    _UnsupportedUserEditionException,
    _ThrottlingException,
    _PreconditionNotMetException,
    _InvalidNextTokenException,
    _ResourceExistsException,
    _InternalFailureException,
    _ConcurrentUpdatingException,
    _ResourceNotFoundException,
    _QuickSightUserNotFoundException,
    _LimitExceededException,
    _SessionLifetimeInMinutesInvalidException,

    -- * AnalysisErrorType
    AnalysisErrorType (..),

    -- * AnalysisFilterAttribute
    AnalysisFilterAttribute (..),

    -- * AssignmentStatus
    AssignmentStatus (..),

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

    -- * DataSetImportMode
    DataSetImportMode (..),

    -- * DataSourceErrorInfoType
    DataSourceErrorInfoType (..),

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
    accountCustomization_defaultTheme,

    -- * AccountSettings
    AccountSettings (..),
    newAccountSettings,
    accountSettings_edition,
    accountSettings_accountName,
    accountSettings_defaultNamespace,
    accountSettings_notificationEmail,

    -- * ActiveIAMPolicyAssignment
    ActiveIAMPolicyAssignment (..),
    newActiveIAMPolicyAssignment,
    activeIAMPolicyAssignment_assignmentName,
    activeIAMPolicyAssignment_policyArn,

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
    analysis_status,
    analysis_themeArn,
    analysis_dataSetArns,
    analysis_sheets,
    analysis_analysisId,
    analysis_lastUpdatedTime,
    analysis_arn,
    analysis_createdTime,
    analysis_name,
    analysis_errors,

    -- * AnalysisError
    AnalysisError (..),
    newAnalysisError,
    analysisError_type,
    analysisError_message,

    -- * AnalysisSearchFilter
    AnalysisSearchFilter (..),
    newAnalysisSearchFilter,
    analysisSearchFilter_operator,
    analysisSearchFilter_value,
    analysisSearchFilter_name,

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
    analysisSummary_status,
    analysisSummary_analysisId,
    analysisSummary_lastUpdatedTime,
    analysisSummary_arn,
    analysisSummary_createdTime,
    analysisSummary_name,

    -- * AnonymousUserDashboardEmbeddingConfiguration
    AnonymousUserDashboardEmbeddingConfiguration (..),
    newAnonymousUserDashboardEmbeddingConfiguration,
    anonymousUserDashboardEmbeddingConfiguration_initialDashboardId,

    -- * AnonymousUserEmbeddingExperienceConfiguration
    AnonymousUserEmbeddingExperienceConfiguration (..),
    newAnonymousUserEmbeddingExperienceConfiguration,
    anonymousUserEmbeddingExperienceConfiguration_dashboard,

    -- * AthenaParameters
    AthenaParameters (..),
    newAthenaParameters,
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
    columnLevelPermissionRule_principals,
    columnLevelPermissionRule_columnNames,

    -- * ColumnSchema
    ColumnSchema (..),
    newColumnSchema,
    columnSchema_geographicRole,
    columnSchema_name,
    columnSchema_dataType,

    -- * ColumnTag
    ColumnTag (..),
    newColumnTag,
    columnTag_columnDescription,
    columnTag_columnGeographicRole,

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
    dashboard_lastUpdatedTime,
    dashboard_arn,
    dashboard_createdTime,
    dashboard_dashboardId,
    dashboard_lastPublishedTime,
    dashboard_name,
    dashboard_version,

    -- * DashboardError
    DashboardError (..),
    newDashboardError,
    dashboardError_type,
    dashboardError_message,

    -- * DashboardPublishOptions
    DashboardPublishOptions (..),
    newDashboardPublishOptions,
    dashboardPublishOptions_adHocFilteringOption,
    dashboardPublishOptions_exportToCSVOption,
    dashboardPublishOptions_sheetControlsOption,

    -- * DashboardSearchFilter
    DashboardSearchFilter (..),
    newDashboardSearchFilter,
    dashboardSearchFilter_value,
    dashboardSearchFilter_name,
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
    dashboardSummary_lastUpdatedTime,
    dashboardSummary_arn,
    dashboardSummary_createdTime,
    dashboardSummary_dashboardId,
    dashboardSummary_publishedVersionNumber,
    dashboardSummary_lastPublishedTime,
    dashboardSummary_name,

    -- * DashboardVersion
    DashboardVersion (..),
    newDashboardVersion,
    dashboardVersion_status,
    dashboardVersion_themeArn,
    dashboardVersion_dataSetArns,
    dashboardVersion_sheets,
    dashboardVersion_arn,
    dashboardVersion_createdTime,
    dashboardVersion_sourceEntityArn,
    dashboardVersion_versionNumber,
    dashboardVersion_errors,
    dashboardVersion_description,

    -- * DashboardVersionSummary
    DashboardVersionSummary (..),
    newDashboardVersionSummary,
    dashboardVersionSummary_status,
    dashboardVersionSummary_arn,
    dashboardVersionSummary_createdTime,
    dashboardVersionSummary_sourceEntityArn,
    dashboardVersionSummary_versionNumber,
    dashboardVersionSummary_description,

    -- * DataColorPalette
    DataColorPalette (..),
    newDataColorPalette,
    dataColorPalette_minMaxGradient,
    dataColorPalette_emptyFillColor,
    dataColorPalette_colors,

    -- * DataSet
    DataSet (..),
    newDataSet,
    dataSet_fieldFolders,
    dataSet_columnGroups,
    dataSet_lastUpdatedTime,
    dataSet_arn,
    dataSet_createdTime,
    dataSet_consumedSpiceCapacityInBytes,
    dataSet_importMode,
    dataSet_physicalTableMap,
    dataSet_dataSetId,
    dataSet_name,
    dataSet_dataSetUsageConfiguration,
    dataSet_outputColumns,
    dataSet_rowLevelPermissionTagConfiguration,
    dataSet_rowLevelPermissionDataSet,
    dataSet_columnLevelPermissionRules,
    dataSet_logicalTableMap,

    -- * DataSetConfiguration
    DataSetConfiguration (..),
    newDataSetConfiguration,
    dataSetConfiguration_columnGroupSchemaList,
    dataSetConfiguration_dataSetSchema,
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

    -- * DataSetSummary
    DataSetSummary (..),
    newDataSetSummary,
    dataSetSummary_lastUpdatedTime,
    dataSetSummary_arn,
    dataSetSummary_createdTime,
    dataSetSummary_rowLevelPermissionTagConfigurationApplied,
    dataSetSummary_columnLevelPermissionRulesApplied,
    dataSetSummary_importMode,
    dataSetSummary_dataSetId,
    dataSetSummary_name,
    dataSetSummary_rowLevelPermissionDataSet,

    -- * DataSetUsageConfiguration
    DataSetUsageConfiguration (..),
    newDataSetUsageConfiguration,
    dataSetUsageConfiguration_disableUseAsImportedSource,
    dataSetUsageConfiguration_disableUseAsDirectQuerySource,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_status,
    dataSource_dataSourceParameters,
    dataSource_lastUpdatedTime,
    dataSource_arn,
    dataSource_createdTime,
    dataSource_sslProperties,
    dataSource_dataSourceId,
    dataSource_name,
    dataSource_alternateDataSourceParameters,
    dataSource_vpcConnectionProperties,
    dataSource_type,
    dataSource_errorInfo,

    -- * DataSourceCredentials
    DataSourceCredentials (..),
    newDataSourceCredentials,
    dataSourceCredentials_copySourceArn,
    dataSourceCredentials_credentialPair,

    -- * DataSourceErrorInfo
    DataSourceErrorInfo (..),
    newDataSourceErrorInfo,
    dataSourceErrorInfo_type,
    dataSourceErrorInfo_message,

    -- * DataSourceParameters
    DataSourceParameters (..),
    newDataSourceParameters,
    dataSourceParameters_rdsParameters,
    dataSourceParameters_s3Parameters,
    dataSourceParameters_athenaParameters,
    dataSourceParameters_teradataParameters,
    dataSourceParameters_auroraParameters,
    dataSourceParameters_awsIotAnalyticsParameters,
    dataSourceParameters_amazonOpenSearchParameters,
    dataSourceParameters_sqlServerParameters,
    dataSourceParameters_redshiftParameters,
    dataSourceParameters_postgreSqlParameters,
    dataSourceParameters_oracleParameters,
    dataSourceParameters_amazonElasticsearchParameters,
    dataSourceParameters_twitterParameters,
    dataSourceParameters_mariaDbParameters,
    dataSourceParameters_jiraParameters,
    dataSourceParameters_snowflakeParameters,
    dataSourceParameters_prestoParameters,
    dataSourceParameters_mySqlParameters,
    dataSourceParameters_auroraPostgreSqlParameters,
    dataSourceParameters_sparkParameters,
    dataSourceParameters_serviceNowParameters,

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
    errorInfo_type,
    errorInfo_message,

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
    folder_lastUpdatedTime,
    folder_arn,
    folder_createdTime,
    folder_folderId,
    folder_name,
    folder_folderPath,
    folder_folderType,

    -- * FolderMember
    FolderMember (..),
    newFolderMember,
    folderMember_memberId,
    folderMember_memberType,

    -- * FolderSearchFilter
    FolderSearchFilter (..),
    newFolderSearchFilter,
    folderSearchFilter_operator,
    folderSearchFilter_value,
    folderSearchFilter_name,

    -- * FolderSummary
    FolderSummary (..),
    newFolderSummary,
    folderSummary_lastUpdatedTime,
    folderSummary_arn,
    folderSummary_createdTime,
    folderSummary_folderId,
    folderSummary_name,
    folderSummary_folderType,

    -- * GeoSpatialColumnGroup
    GeoSpatialColumnGroup (..),
    newGeoSpatialColumnGroup,
    geoSpatialColumnGroup_name,
    geoSpatialColumnGroup_countryCode,
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

    -- * GutterStyle
    GutterStyle (..),
    newGutterStyle,
    gutterStyle_show,

    -- * IAMPolicyAssignment
    IAMPolicyAssignment (..),
    newIAMPolicyAssignment,
    iAMPolicyAssignment_assignmentName,
    iAMPolicyAssignment_awsAccountId,
    iAMPolicyAssignment_assignmentStatus,
    iAMPolicyAssignment_policyArn,
    iAMPolicyAssignment_identities,
    iAMPolicyAssignment_assignmentId,

    -- * IAMPolicyAssignmentSummary
    IAMPolicyAssignmentSummary (..),
    newIAMPolicyAssignmentSummary,
    iAMPolicyAssignmentSummary_assignmentName,
    iAMPolicyAssignmentSummary_assignmentStatus,

    -- * Ingestion
    Ingestion (..),
    newIngestion,
    ingestion_requestSource,
    ingestion_queueInfo,
    ingestion_ingestionTimeInSeconds,
    ingestion_requestType,
    ingestion_ingestionSizeInBytes,
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
    joinInstruction_rightJoinKeyProperties,
    joinInstruction_leftJoinKeyProperties,
    joinInstruction_leftOperand,
    joinInstruction_rightOperand,
    joinInstruction_type,
    joinInstruction_onClause,

    -- * JoinKeyProperties
    JoinKeyProperties (..),
    newJoinKeyProperties,
    joinKeyProperties_uniqueKey,

    -- * LogicalTable
    LogicalTable (..),
    newLogicalTable,
    logicalTable_dataTransforms,
    logicalTable_alias,
    logicalTable_source,

    -- * LogicalTableSource
    LogicalTableSource (..),
    newLogicalTableSource,
    logicalTableSource_dataSetArn,
    logicalTableSource_joinInstruction,
    logicalTableSource_physicalTableId,

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
    namespaceError_type,
    namespaceError_message,

    -- * NamespaceInfoV2
    NamespaceInfoV2 (..),
    newNamespaceInfoV2,
    namespaceInfoV2_capacityRegion,
    namespaceInfoV2_arn,
    namespaceInfoV2_creationStatus,
    namespaceInfoV2_name,
    namespaceInfoV2_namespaceError,
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
    parameters_dateTimeParameters,
    parameters_decimalParameters,
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
    redshiftParameters_clusterId,
    redshiftParameters_host,
    redshiftParameters_port,
    redshiftParameters_database,

    -- * RegisteredUserDashboardEmbeddingConfiguration
    RegisteredUserDashboardEmbeddingConfiguration (..),
    newRegisteredUserDashboardEmbeddingConfiguration,
    registeredUserDashboardEmbeddingConfiguration_initialDashboardId,

    -- * RegisteredUserEmbeddingExperienceConfiguration
    RegisteredUserEmbeddingExperienceConfiguration (..),
    newRegisteredUserEmbeddingExperienceConfiguration,
    registeredUserEmbeddingExperienceConfiguration_dashboard,
    registeredUserEmbeddingExperienceConfiguration_quickSightConsole,
    registeredUserEmbeddingExperienceConfiguration_qSearchBar,

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
    rowInfo_rowsIngested,
    rowInfo_totalRowsInDataset,
    rowInfo_rowsDropped,

    -- * RowLevelPermissionDataSet
    RowLevelPermissionDataSet (..),
    newRowLevelPermissionDataSet,
    rowLevelPermissionDataSet_status,
    rowLevelPermissionDataSet_namespace,
    rowLevelPermissionDataSet_formatVersion,
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
    rowLevelPermissionTagRule_tagMultiValueDelimiter,
    rowLevelPermissionTagRule_matchAllValue,
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
    sheetStyle_tileLayout,
    sheetStyle_tile,

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
    template_lastUpdatedTime,
    template_arn,
    template_createdTime,
    template_templateId,
    template_name,
    template_version,

    -- * TemplateAlias
    TemplateAlias (..),
    newTemplateAlias,
    templateAlias_arn,
    templateAlias_aliasName,
    templateAlias_templateVersionNumber,

    -- * TemplateError
    TemplateError (..),
    newTemplateError,
    templateError_type,
    templateError_message,

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
    templateSummary_lastUpdatedTime,
    templateSummary_latestVersionNumber,
    templateSummary_arn,
    templateSummary_createdTime,
    templateSummary_templateId,
    templateSummary_name,

    -- * TemplateVersion
    TemplateVersion (..),
    newTemplateVersion,
    templateVersion_status,
    templateVersion_themeArn,
    templateVersion_sheets,
    templateVersion_createdTime,
    templateVersion_sourceEntityArn,
    templateVersion_dataSetConfigurations,
    templateVersion_versionNumber,
    templateVersion_errors,
    templateVersion_description,

    -- * TemplateVersionSummary
    TemplateVersionSummary (..),
    newTemplateVersionSummary,
    templateVersionSummary_status,
    templateVersionSummary_arn,
    templateVersionSummary_createdTime,
    templateVersionSummary_versionNumber,
    templateVersionSummary_description,

    -- * TeradataParameters
    TeradataParameters (..),
    newTeradataParameters,
    teradataParameters_host,
    teradataParameters_port,
    teradataParameters_database,

    -- * Theme
    Theme (..),
    newTheme,
    theme_themeId,
    theme_lastUpdatedTime,
    theme_arn,
    theme_createdTime,
    theme_name,
    theme_version,
    theme_type,

    -- * ThemeAlias
    ThemeAlias (..),
    newThemeAlias,
    themeAlias_arn,
    themeAlias_themeVersionNumber,
    themeAlias_aliasName,

    -- * ThemeConfiguration
    ThemeConfiguration (..),
    newThemeConfiguration,
    themeConfiguration_uIColorPalette,
    themeConfiguration_sheet,
    themeConfiguration_dataColorPalette,

    -- * ThemeError
    ThemeError (..),
    newThemeError,
    themeError_type,
    themeError_message,

    -- * ThemeSummary
    ThemeSummary (..),
    newThemeSummary,
    themeSummary_themeId,
    themeSummary_lastUpdatedTime,
    themeSummary_latestVersionNumber,
    themeSummary_arn,
    themeSummary_createdTime,
    themeSummary_name,

    -- * ThemeVersion
    ThemeVersion (..),
    newThemeVersion,
    themeVersion_status,
    themeVersion_arn,
    themeVersion_createdTime,
    themeVersion_versionNumber,
    themeVersion_configuration,
    themeVersion_errors,
    themeVersion_description,
    themeVersion_baseThemeId,

    -- * ThemeVersionSummary
    ThemeVersionSummary (..),
    newThemeVersionSummary,
    themeVersionSummary_status,
    themeVersionSummary_arn,
    themeVersionSummary_createdTime,
    themeVersionSummary_versionNumber,
    themeVersionSummary_description,

    -- * TileLayoutStyle
    TileLayoutStyle (..),
    newTileLayoutStyle,
    tileLayoutStyle_margin,
    tileLayoutStyle_gutter,

    -- * TileStyle
    TileStyle (..),
    newTileStyle,
    tileStyle_border,

    -- * TransformOperation
    TransformOperation (..),
    newTransformOperation,
    transformOperation_castColumnTypeOperation,
    transformOperation_tagColumnOperation,
    transformOperation_createColumnsOperation,
    transformOperation_untagColumnOperation,
    transformOperation_filterOperation,
    transformOperation_projectOperation,
    transformOperation_renameColumnOperation,

    -- * TwitterParameters
    TwitterParameters (..),
    newTwitterParameters,
    twitterParameters_query,
    twitterParameters_maxRows,

    -- * UIColorPalette
    UIColorPalette (..),
    newUIColorPalette,
    uIColorPalette_secondaryBackground,
    uIColorPalette_successForeground,
    uIColorPalette_dimension,
    uIColorPalette_dimensionForeground,
    uIColorPalette_success,
    uIColorPalette_accent,
    uIColorPalette_secondaryForeground,
    uIColorPalette_primaryForeground,
    uIColorPalette_warning,
    uIColorPalette_primaryBackground,
    uIColorPalette_accentForeground,
    uIColorPalette_measure,
    uIColorPalette_dangerForeground,
    uIColorPalette_measureForeground,
    uIColorPalette_danger,
    uIColorPalette_warningForeground,

    -- * UntagColumnOperation
    UntagColumnOperation (..),
    newUntagColumnOperation,
    untagColumnOperation_columnName,
    untagColumnOperation_tagNames,

    -- * UploadSettings
    UploadSettings (..),
    newUploadSettings,
    uploadSettings_startFromRow,
    uploadSettings_containsHeader,
    uploadSettings_textQualifier,
    uploadSettings_format,
    uploadSettings_delimiter,

    -- * User
    User (..),
    newUser,
    user_email,
    user_principalId,
    user_arn,
    user_userName,
    user_identityType,
    user_externalLoginFederationProviderUrl,
    user_customPermissionsName,
    user_active,
    user_role,
    user_externalLoginFederationProviderType,
    user_externalLoginId,

    -- * VpcConnectionProperties
    VpcConnectionProperties (..),
    newVpcConnectionProperties,
    vpcConnectionProperties_vpcConnectionArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.AccountCustomization
import Network.AWS.QuickSight.Types.AccountSettings
import Network.AWS.QuickSight.Types.ActiveIAMPolicyAssignment
import Network.AWS.QuickSight.Types.AdHocFilteringOption
import Network.AWS.QuickSight.Types.AmazonElasticsearchParameters
import Network.AWS.QuickSight.Types.AmazonOpenSearchParameters
import Network.AWS.QuickSight.Types.Analysis
import Network.AWS.QuickSight.Types.AnalysisError
import Network.AWS.QuickSight.Types.AnalysisErrorType
import Network.AWS.QuickSight.Types.AnalysisFilterAttribute
import Network.AWS.QuickSight.Types.AnalysisSearchFilter
import Network.AWS.QuickSight.Types.AnalysisSourceEntity
import Network.AWS.QuickSight.Types.AnalysisSourceTemplate
import Network.AWS.QuickSight.Types.AnalysisSummary
import Network.AWS.QuickSight.Types.AnonymousUserDashboardEmbeddingConfiguration
import Network.AWS.QuickSight.Types.AnonymousUserEmbeddingExperienceConfiguration
import Network.AWS.QuickSight.Types.AssignmentStatus
import Network.AWS.QuickSight.Types.AthenaParameters
import Network.AWS.QuickSight.Types.AuroraParameters
import Network.AWS.QuickSight.Types.AuroraPostgreSqlParameters
import Network.AWS.QuickSight.Types.AwsIotAnalyticsParameters
import Network.AWS.QuickSight.Types.BorderStyle
import Network.AWS.QuickSight.Types.CalculatedColumn
import Network.AWS.QuickSight.Types.CastColumnTypeOperation
import Network.AWS.QuickSight.Types.ColumnDataType
import Network.AWS.QuickSight.Types.ColumnDescription
import Network.AWS.QuickSight.Types.ColumnGroup
import Network.AWS.QuickSight.Types.ColumnGroupColumnSchema
import Network.AWS.QuickSight.Types.ColumnGroupSchema
import Network.AWS.QuickSight.Types.ColumnLevelPermissionRule
import Network.AWS.QuickSight.Types.ColumnSchema
import Network.AWS.QuickSight.Types.ColumnTag
import Network.AWS.QuickSight.Types.ColumnTagName
import Network.AWS.QuickSight.Types.CreateColumnsOperation
import Network.AWS.QuickSight.Types.CredentialPair
import Network.AWS.QuickSight.Types.CustomSql
import Network.AWS.QuickSight.Types.Dashboard
import Network.AWS.QuickSight.Types.DashboardBehavior
import Network.AWS.QuickSight.Types.DashboardError
import Network.AWS.QuickSight.Types.DashboardErrorType
import Network.AWS.QuickSight.Types.DashboardFilterAttribute
import Network.AWS.QuickSight.Types.DashboardPublishOptions
import Network.AWS.QuickSight.Types.DashboardSearchFilter
import Network.AWS.QuickSight.Types.DashboardSourceEntity
import Network.AWS.QuickSight.Types.DashboardSourceTemplate
import Network.AWS.QuickSight.Types.DashboardSummary
import Network.AWS.QuickSight.Types.DashboardUIState
import Network.AWS.QuickSight.Types.DashboardVersion
import Network.AWS.QuickSight.Types.DashboardVersionSummary
import Network.AWS.QuickSight.Types.DataColorPalette
import Network.AWS.QuickSight.Types.DataSet
import Network.AWS.QuickSight.Types.DataSetConfiguration
import Network.AWS.QuickSight.Types.DataSetImportMode
import Network.AWS.QuickSight.Types.DataSetReference
import Network.AWS.QuickSight.Types.DataSetSchema
import Network.AWS.QuickSight.Types.DataSetSummary
import Network.AWS.QuickSight.Types.DataSetUsageConfiguration
import Network.AWS.QuickSight.Types.DataSource
import Network.AWS.QuickSight.Types.DataSourceCredentials
import Network.AWS.QuickSight.Types.DataSourceErrorInfo
import Network.AWS.QuickSight.Types.DataSourceErrorInfoType
import Network.AWS.QuickSight.Types.DataSourceParameters
import Network.AWS.QuickSight.Types.DataSourceType
import Network.AWS.QuickSight.Types.DateTimeParameter
import Network.AWS.QuickSight.Types.DecimalParameter
import Network.AWS.QuickSight.Types.Edition
import Network.AWS.QuickSight.Types.EmbeddingIdentityType
import Network.AWS.QuickSight.Types.ErrorInfo
import Network.AWS.QuickSight.Types.ExportToCSVOption
import Network.AWS.QuickSight.Types.FieldFolder
import Network.AWS.QuickSight.Types.FileFormat
import Network.AWS.QuickSight.Types.FilterOperation
import Network.AWS.QuickSight.Types.FilterOperator
import Network.AWS.QuickSight.Types.Folder
import Network.AWS.QuickSight.Types.FolderFilterAttribute
import Network.AWS.QuickSight.Types.FolderMember
import Network.AWS.QuickSight.Types.FolderSearchFilter
import Network.AWS.QuickSight.Types.FolderSummary
import Network.AWS.QuickSight.Types.FolderType
import Network.AWS.QuickSight.Types.GeoSpatialColumnGroup
import Network.AWS.QuickSight.Types.GeoSpatialCountryCode
import Network.AWS.QuickSight.Types.GeoSpatialDataRole
import Network.AWS.QuickSight.Types.Group
import Network.AWS.QuickSight.Types.GroupMember
import Network.AWS.QuickSight.Types.GutterStyle
import Network.AWS.QuickSight.Types.IAMPolicyAssignment
import Network.AWS.QuickSight.Types.IAMPolicyAssignmentSummary
import Network.AWS.QuickSight.Types.IdentityStore
import Network.AWS.QuickSight.Types.IdentityType
import Network.AWS.QuickSight.Types.Ingestion
import Network.AWS.QuickSight.Types.IngestionErrorType
import Network.AWS.QuickSight.Types.IngestionRequestSource
import Network.AWS.QuickSight.Types.IngestionRequestType
import Network.AWS.QuickSight.Types.IngestionStatus
import Network.AWS.QuickSight.Types.IngestionType
import Network.AWS.QuickSight.Types.InputColumn
import Network.AWS.QuickSight.Types.InputColumnDataType
import Network.AWS.QuickSight.Types.IntegerParameter
import Network.AWS.QuickSight.Types.JiraParameters
import Network.AWS.QuickSight.Types.JoinInstruction
import Network.AWS.QuickSight.Types.JoinKeyProperties
import Network.AWS.QuickSight.Types.JoinType
import Network.AWS.QuickSight.Types.LogicalTable
import Network.AWS.QuickSight.Types.LogicalTableSource
import Network.AWS.QuickSight.Types.ManifestFileLocation
import Network.AWS.QuickSight.Types.MarginStyle
import Network.AWS.QuickSight.Types.MariaDbParameters
import Network.AWS.QuickSight.Types.MemberIdArnPair
import Network.AWS.QuickSight.Types.MemberType
import Network.AWS.QuickSight.Types.MySqlParameters
import Network.AWS.QuickSight.Types.NamespaceError
import Network.AWS.QuickSight.Types.NamespaceErrorType
import Network.AWS.QuickSight.Types.NamespaceInfoV2
import Network.AWS.QuickSight.Types.NamespaceStatus
import Network.AWS.QuickSight.Types.OracleParameters
import Network.AWS.QuickSight.Types.OutputColumn
import Network.AWS.QuickSight.Types.Parameters
import Network.AWS.QuickSight.Types.PhysicalTable
import Network.AWS.QuickSight.Types.PostgreSqlParameters
import Network.AWS.QuickSight.Types.PrestoParameters
import Network.AWS.QuickSight.Types.ProjectOperation
import Network.AWS.QuickSight.Types.QueueInfo
import Network.AWS.QuickSight.Types.RdsParameters
import Network.AWS.QuickSight.Types.RedshiftParameters
import Network.AWS.QuickSight.Types.RegisteredUserDashboardEmbeddingConfiguration
import Network.AWS.QuickSight.Types.RegisteredUserEmbeddingExperienceConfiguration
import Network.AWS.QuickSight.Types.RegisteredUserQSearchBarEmbeddingConfiguration
import Network.AWS.QuickSight.Types.RegisteredUserQuickSightConsoleEmbeddingConfiguration
import Network.AWS.QuickSight.Types.RelationalTable
import Network.AWS.QuickSight.Types.RenameColumnOperation
import Network.AWS.QuickSight.Types.ResourcePermission
import Network.AWS.QuickSight.Types.ResourceStatus
import Network.AWS.QuickSight.Types.RowInfo
import Network.AWS.QuickSight.Types.RowLevelPermissionDataSet
import Network.AWS.QuickSight.Types.RowLevelPermissionFormatVersion
import Network.AWS.QuickSight.Types.RowLevelPermissionPolicy
import Network.AWS.QuickSight.Types.RowLevelPermissionTagConfiguration
import Network.AWS.QuickSight.Types.RowLevelPermissionTagRule
import Network.AWS.QuickSight.Types.S3Parameters
import Network.AWS.QuickSight.Types.S3Source
import Network.AWS.QuickSight.Types.ServiceNowParameters
import Network.AWS.QuickSight.Types.SessionTag
import Network.AWS.QuickSight.Types.Sheet
import Network.AWS.QuickSight.Types.SheetControlsOption
import Network.AWS.QuickSight.Types.SheetStyle
import Network.AWS.QuickSight.Types.SnowflakeParameters
import Network.AWS.QuickSight.Types.SparkParameters
import Network.AWS.QuickSight.Types.SqlServerParameters
import Network.AWS.QuickSight.Types.SslProperties
import Network.AWS.QuickSight.Types.Status
import Network.AWS.QuickSight.Types.StringParameter
import Network.AWS.QuickSight.Types.Tag
import Network.AWS.QuickSight.Types.TagColumnOperation
import Network.AWS.QuickSight.Types.Template
import Network.AWS.QuickSight.Types.TemplateAlias
import Network.AWS.QuickSight.Types.TemplateError
import Network.AWS.QuickSight.Types.TemplateErrorType
import Network.AWS.QuickSight.Types.TemplateSourceAnalysis
import Network.AWS.QuickSight.Types.TemplateSourceEntity
import Network.AWS.QuickSight.Types.TemplateSourceTemplate
import Network.AWS.QuickSight.Types.TemplateSummary
import Network.AWS.QuickSight.Types.TemplateVersion
import Network.AWS.QuickSight.Types.TemplateVersionSummary
import Network.AWS.QuickSight.Types.TeradataParameters
import Network.AWS.QuickSight.Types.TextQualifier
import Network.AWS.QuickSight.Types.Theme
import Network.AWS.QuickSight.Types.ThemeAlias
import Network.AWS.QuickSight.Types.ThemeConfiguration
import Network.AWS.QuickSight.Types.ThemeError
import Network.AWS.QuickSight.Types.ThemeErrorType
import Network.AWS.QuickSight.Types.ThemeSummary
import Network.AWS.QuickSight.Types.ThemeType
import Network.AWS.QuickSight.Types.ThemeVersion
import Network.AWS.QuickSight.Types.ThemeVersionSummary
import Network.AWS.QuickSight.Types.TileLayoutStyle
import Network.AWS.QuickSight.Types.TileStyle
import Network.AWS.QuickSight.Types.TransformOperation
import Network.AWS.QuickSight.Types.TwitterParameters
import Network.AWS.QuickSight.Types.UIColorPalette
import Network.AWS.QuickSight.Types.UntagColumnOperation
import Network.AWS.QuickSight.Types.UploadSettings
import Network.AWS.QuickSight.Types.User
import Network.AWS.QuickSight.Types.UserRole
import Network.AWS.QuickSight.Types.VpcConnectionProperties
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-04-01@ of the Amazon QuickSight SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "QuickSight",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "quicksight",
      Core._serviceSigningName = "quicksight",
      Core._serviceVersion = "2018-04-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "QuickSight",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

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

-- | This resource is currently unavailable.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The identity type specified isn\'t supported. Supported identity types
-- include @IAM@ and @QUICKSIGHT@.
_IdentityTypeNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdentityTypeNotSupportedException =
  Core._MatchServiceError
    defaultService
    "IdentityTypeNotSupportedException"
    Prelude.. Core.hasStatus 403

-- | The domain specified isn\'t on the allow list. All domains for embedded
-- dashboards must be added to the approved list by an Amazon QuickSight
-- admin.
_DomainNotWhitelistedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainNotWhitelistedException =
  Core._MatchServiceError
    defaultService
    "DomainNotWhitelistedException"
    Prelude.. Core.hasStatus 403

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | This error indicates that you are calling an embedding operation in
-- Amazon QuickSight without the required pricing plan on your Amazon Web
-- Services account. Before you can use embedding for anonymous users, a
-- Amazon QuickSight administrator needs to add capacity pricing to Amazon
-- QuickSight. You can do this on the __Manage Amazon QuickSight__ page.
--
-- After capacity pricing is added, you can use the GetDashboardEmbedUrl
-- API operation with the @--identity-type ANONYMOUS@ option.
_UnsupportedPricingPlanException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedPricingPlanException =
  Core._MatchServiceError
    defaultService
    "UnsupportedPricingPlanException"
    Prelude.. Core.hasStatus 403

-- | One or more parameters has a value that isn\'t valid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
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

-- | Access is throttled.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | One or more preconditions aren\'t met.
_PreconditionNotMetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionNotMetException =
  Core._MatchServiceError
    defaultService
    "PreconditionNotMetException"
    Prelude.. Core.hasStatus 400

-- | The @NextToken@ value isn\'t valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Core.hasStatus 400

-- | The resource specified already exists.
_ResourceExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"
    Prelude.. Core.hasStatus 409

-- | An internal failure occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

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

-- | The user with the provided name isn\'t found. This error can happen in
-- any operation that requires finding a user based on a provided user
-- name, such as @DeleteUser@, @DescribeUser@, and so on.
_QuickSightUserNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_QuickSightUserNotFoundException =
  Core._MatchServiceError
    defaultService
    "QuickSightUserNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A limit is exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409

-- | The number of minutes specified for the lifetime of a session isn\'t
-- valid. The session lifetime must be 15-600 minutes.
_SessionLifetimeInMinutesInvalidException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SessionLifetimeInMinutesInvalidException =
  Core._MatchServiceError
    defaultService
    "SessionLifetimeInMinutesInvalidException"
    Prelude.. Core.hasStatus 400
