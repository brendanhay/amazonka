{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConcurrentUpdatingException,
    _ConflictException,
    _DomainNotWhitelistedException,
    _IdentityTypeNotSupportedException,
    _InternalFailureException,
    _InvalidNextTokenException,
    _InvalidParameterValueException,
    _InvalidRequestException,
    _LimitExceededException,
    _PreconditionNotMetException,
    _QuickSightUserNotFoundException,
    _ResourceExistsException,
    _ResourceNotFoundException,
    _ResourceUnavailableException,
    _SessionLifetimeInMinutesInvalidException,
    _ThrottlingException,
    _UnsupportedPricingPlanException,
    _UnsupportedUserEditionException,

    -- * AnalysisErrorType
    AnalysisErrorType (..),

    -- * AnalysisFilterAttribute
    AnalysisFilterAttribute (..),

    -- * AnchorOption
    AnchorOption (..),

    -- * ArcThickness
    ArcThickness (..),

    -- * ArcThicknessOptions
    ArcThicknessOptions (..),

    -- * AssignmentStatus
    AssignmentStatus (..),

    -- * AuthenticationMethodOption
    AuthenticationMethodOption (..),

    -- * AxisBinding
    AxisBinding (..),

    -- * BarChartOrientation
    BarChartOrientation (..),

    -- * BarsArrangement
    BarsArrangement (..),

    -- * BaseMapStyleType
    BaseMapStyleType (..),

    -- * BoxPlotFillStyle
    BoxPlotFillStyle (..),

    -- * CategoricalAggregationFunction
    CategoricalAggregationFunction (..),

    -- * CategoryFilterMatchOperator
    CategoryFilterMatchOperator (..),

    -- * CategoryFilterSelectAllOptions
    CategoryFilterSelectAllOptions (..),

    -- * ColorFillType
    ColorFillType (..),

    -- * ColumnDataType
    ColumnDataType (..),

    -- * ColumnRole
    ColumnRole (..),

    -- * ColumnTagName
    ColumnTagName (..),

    -- * ComparisonMethod
    ComparisonMethod (..),

    -- * ConditionalFormattingIconDisplayOption
    ConditionalFormattingIconDisplayOption (..),

    -- * ConditionalFormattingIconSetType
    ConditionalFormattingIconSetType (..),

    -- * CrossDatasetTypes
    CrossDatasetTypes (..),

    -- * CustomContentImageScalingConfiguration
    CustomContentImageScalingConfiguration (..),

    -- * CustomContentType
    CustomContentType (..),

    -- * DashboardBehavior
    DashboardBehavior (..),

    -- * DashboardErrorType
    DashboardErrorType (..),

    -- * DashboardFilterAttribute
    DashboardFilterAttribute (..),

    -- * DashboardUIState
    DashboardUIState (..),

    -- * DataLabelContent
    DataLabelContent (..),

    -- * DataLabelOverlap
    DataLabelOverlap (..),

    -- * DataLabelPosition
    DataLabelPosition (..),

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

    -- * DateAggregationFunction
    DateAggregationFunction (..),

    -- * Edition
    Edition (..),

    -- * EmbeddingIdentityType
    EmbeddingIdentityType (..),

    -- * FileFormat
    FileFormat (..),

    -- * FilterNullOption
    FilterNullOption (..),

    -- * FilterOperator
    FilterOperator (..),

    -- * FilterVisualScope
    FilterVisualScope (..),

    -- * FolderFilterAttribute
    FolderFilterAttribute (..),

    -- * FolderType
    FolderType (..),

    -- * FontDecoration
    FontDecoration (..),

    -- * FontStyle
    FontStyle (..),

    -- * FontWeightName
    FontWeightName (..),

    -- * ForecastComputationSeasonality
    ForecastComputationSeasonality (..),

    -- * FunnelChartMeasureDataLabelStyle
    FunnelChartMeasureDataLabelStyle (..),

    -- * GeoSpatialCountryCode
    GeoSpatialCountryCode (..),

    -- * GeoSpatialDataRole
    GeoSpatialDataRole (..),

    -- * GeospatialSelectedPointStyle
    GeospatialSelectedPointStyle (..),

    -- * GroupFilterAttribute
    GroupFilterAttribute (..),

    -- * GroupFilterOperator
    GroupFilterOperator (..),

    -- * HistogramBinType
    HistogramBinType (..),

    -- * HorizontalTextAlignment
    HorizontalTextAlignment (..),

    -- * Icon
    Icon (..),

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

    -- * LayoutElementType
    LayoutElementType (..),

    -- * LegendPosition
    LegendPosition (..),

    -- * LineChartLineStyle
    LineChartLineStyle (..),

    -- * LineChartMarkerShape
    LineChartMarkerShape (..),

    -- * LineChartType
    LineChartType (..),

    -- * LineInterpolation
    LineInterpolation (..),

    -- * MapZoomMode
    MapZoomMode (..),

    -- * MaximumMinimumComputationType
    MaximumMinimumComputationType (..),

    -- * MemberType
    MemberType (..),

    -- * MissingDataTreatmentOption
    MissingDataTreatmentOption (..),

    -- * NamespaceErrorType
    NamespaceErrorType (..),

    -- * NamespaceStatus
    NamespaceStatus (..),

    -- * NegativeValueDisplayMode
    NegativeValueDisplayMode (..),

    -- * NumberScale
    NumberScale (..),

    -- * NumericEqualityMatchOperator
    NumericEqualityMatchOperator (..),

    -- * NumericFilterSelectAllOptions
    NumericFilterSelectAllOptions (..),

    -- * NumericSeparatorSymbol
    NumericSeparatorSymbol (..),

    -- * OtherCategories
    OtherCategories (..),

    -- * PanelBorderStyle
    PanelBorderStyle (..),

    -- * PaperOrientation
    PaperOrientation (..),

    -- * PaperSize
    PaperSize (..),

    -- * ParameterValueType
    ParameterValueType (..),

    -- * PivotTableConditionalFormattingScopeRole
    PivotTableConditionalFormattingScopeRole (..),

    -- * PivotTableMetricPlacement
    PivotTableMetricPlacement (..),

    -- * PivotTableSubtotalLevel
    PivotTableSubtotalLevel (..),

    -- * PrimaryValueDisplayType
    PrimaryValueDisplayType (..),

    -- * ReferenceLineLabelHorizontalPosition
    ReferenceLineLabelHorizontalPosition (..),

    -- * ReferenceLineLabelVerticalPosition
    ReferenceLineLabelVerticalPosition (..),

    -- * ReferenceLinePatternType
    ReferenceLinePatternType (..),

    -- * ReferenceLineValueLabelRelativePosition
    ReferenceLineValueLabelRelativePosition (..),

    -- * RelativeDateType
    RelativeDateType (..),

    -- * RelativeFontSize
    RelativeFontSize (..),

    -- * ResizeOption
    ResizeOption (..),

    -- * ResourceStatus
    ResourceStatus (..),

    -- * RowLevelPermissionFormatVersion
    RowLevelPermissionFormatVersion (..),

    -- * RowLevelPermissionPolicy
    RowLevelPermissionPolicy (..),

    -- * SectionPageBreakStatus
    SectionPageBreakStatus (..),

    -- * SelectAllValueOptions
    SelectAllValueOptions (..),

    -- * SelectedFieldOptions
    SelectedFieldOptions (..),

    -- * SelectedTooltipType
    SelectedTooltipType (..),

    -- * SheetContentType
    SheetContentType (..),

    -- * SheetControlDateTimePickerType
    SheetControlDateTimePickerType (..),

    -- * SheetControlListType
    SheetControlListType (..),

    -- * SheetControlSliderType
    SheetControlSliderType (..),

    -- * SimpleNumericalAggregationFunction
    SimpleNumericalAggregationFunction (..),

    -- * SortDirection
    SortDirection (..),

    -- * Status
    Status (..),

    -- * TableBorderStyle
    TableBorderStyle (..),

    -- * TableCellImageScalingConfiguration
    TableCellImageScalingConfiguration (..),

    -- * TableFieldIconSetType
    TableFieldIconSetType (..),

    -- * TableOrientation
    TableOrientation (..),

    -- * TableTotalsPlacement
    TableTotalsPlacement (..),

    -- * TableTotalsScrollStatus
    TableTotalsScrollStatus (..),

    -- * TargetVisualOptions
    TargetVisualOptions (..),

    -- * TemplateErrorType
    TemplateErrorType (..),

    -- * TextQualifier
    TextQualifier (..),

    -- * TextWrap
    TextWrap (..),

    -- * ThemeErrorType
    ThemeErrorType (..),

    -- * ThemeType
    ThemeType (..),

    -- * TimeGranularity
    TimeGranularity (..),

    -- * TooltipTitleType
    TooltipTitleType (..),

    -- * TopBottomComputationType
    TopBottomComputationType (..),

    -- * TopBottomSortOrder
    TopBottomSortOrder (..),

    -- * URLTargetConfiguration
    URLTargetConfiguration (..),

    -- * UserRole
    UserRole (..),

    -- * ValueWhenUnsetOption
    ValueWhenUnsetOption (..),

    -- * VerticalTextAlignment
    VerticalTextAlignment (..),

    -- * Visibility
    Visibility (..),

    -- * VisualCustomActionTrigger
    VisualCustomActionTrigger (..),

    -- * WidgetStatus
    WidgetStatus (..),

    -- * WordCloudCloudLayout
    WordCloudCloudLayout (..),

    -- * WordCloudWordCasing
    WordCloudWordCasing (..),

    -- * WordCloudWordOrientation
    WordCloudWordOrientation (..),

    -- * WordCloudWordPadding
    WordCloudWordPadding (..),

    -- * WordCloudWordScaling
    WordCloudWordScaling (..),

    -- * AccountCustomization
    AccountCustomization (..),
    newAccountCustomization,
    accountCustomization_defaultEmailCustomizationTemplate,
    accountCustomization_defaultTheme,

    -- * AccountInfo
    AccountInfo (..),
    newAccountInfo,
    accountInfo_accountName,
    accountInfo_accountSubscriptionStatus,
    accountInfo_authenticationType,
    accountInfo_edition,
    accountInfo_notificationEmail,

    -- * AccountSettings
    AccountSettings (..),
    newAccountSettings,
    accountSettings_accountName,
    accountSettings_defaultNamespace,
    accountSettings_edition,
    accountSettings_notificationEmail,
    accountSettings_publicSharingEnabled,
    accountSettings_terminationProtectionEnabled,

    -- * ActiveIAMPolicyAssignment
    ActiveIAMPolicyAssignment (..),
    newActiveIAMPolicyAssignment,
    activeIAMPolicyAssignment_assignmentName,
    activeIAMPolicyAssignment_policyArn,

    -- * AdHocFilteringOption
    AdHocFilteringOption (..),
    newAdHocFilteringOption,
    adHocFilteringOption_availabilityStatus,

    -- * AggregationFunction
    AggregationFunction (..),
    newAggregationFunction,
    aggregationFunction_categoricalAggregationFunction,
    aggregationFunction_dateAggregationFunction,
    aggregationFunction_numericalAggregationFunction,

    -- * AggregationSortConfiguration
    AggregationSortConfiguration (..),
    newAggregationSortConfiguration,
    aggregationSortConfiguration_column,
    aggregationSortConfiguration_sortDirection,
    aggregationSortConfiguration_aggregationFunction,

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
    analysis_arn,
    analysis_createdTime,
    analysis_dataSetArns,
    analysis_errors,
    analysis_lastUpdatedTime,
    analysis_name,
    analysis_sheets,
    analysis_status,
    analysis_themeArn,

    -- * AnalysisDefaults
    AnalysisDefaults (..),
    newAnalysisDefaults,
    analysisDefaults_defaultNewSheetConfiguration,

    -- * AnalysisDefinition
    AnalysisDefinition (..),
    newAnalysisDefinition,
    analysisDefinition_analysisDefaults,
    analysisDefinition_calculatedFields,
    analysisDefinition_columnConfigurations,
    analysisDefinition_filterGroups,
    analysisDefinition_parameterDeclarations,
    analysisDefinition_sheets,
    analysisDefinition_dataSetIdentifierDeclarations,

    -- * AnalysisError
    AnalysisError (..),
    newAnalysisError,
    analysisError_message,
    analysisError_type,
    analysisError_violatedEntities,

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
    analysisSummary_arn,
    analysisSummary_createdTime,
    analysisSummary_lastUpdatedTime,
    analysisSummary_name,
    analysisSummary_status,

    -- * AnchorDateConfiguration
    AnchorDateConfiguration (..),
    newAnchorDateConfiguration,
    anchorDateConfiguration_anchorOption,
    anchorDateConfiguration_parameterName,

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
    anonymousUserEmbeddingExperienceConfiguration_dashboard,
    anonymousUserEmbeddingExperienceConfiguration_dashboardVisual,
    anonymousUserEmbeddingExperienceConfiguration_qSearchBar,

    -- * AnonymousUserQSearchBarEmbeddingConfiguration
    AnonymousUserQSearchBarEmbeddingConfiguration (..),
    newAnonymousUserQSearchBarEmbeddingConfiguration,
    anonymousUserQSearchBarEmbeddingConfiguration_initialTopicId,

    -- * ArcAxisConfiguration
    ArcAxisConfiguration (..),
    newArcAxisConfiguration,
    arcAxisConfiguration_range,
    arcAxisConfiguration_reserveRange,

    -- * ArcAxisDisplayRange
    ArcAxisDisplayRange (..),
    newArcAxisDisplayRange,
    arcAxisDisplayRange_max,
    arcAxisDisplayRange_min,

    -- * ArcConfiguration
    ArcConfiguration (..),
    newArcConfiguration,
    arcConfiguration_arcAngle,
    arcConfiguration_arcThickness,

    -- * ArcOptions
    ArcOptions (..),
    newArcOptions,
    arcOptions_arcThickness,

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

    -- * AxisDataOptions
    AxisDataOptions (..),
    newAxisDataOptions,
    axisDataOptions_dateAxisOptions,
    axisDataOptions_numericAxisOptions,

    -- * AxisDisplayDataDrivenRange
    AxisDisplayDataDrivenRange (..),
    newAxisDisplayDataDrivenRange,

    -- * AxisDisplayMinMaxRange
    AxisDisplayMinMaxRange (..),
    newAxisDisplayMinMaxRange,
    axisDisplayMinMaxRange_maximum,
    axisDisplayMinMaxRange_minimum,

    -- * AxisDisplayOptions
    AxisDisplayOptions (..),
    newAxisDisplayOptions,
    axisDisplayOptions_axisLineVisibility,
    axisDisplayOptions_axisOffset,
    axisDisplayOptions_dataOptions,
    axisDisplayOptions_gridLineVisibility,
    axisDisplayOptions_scrollbarOptions,
    axisDisplayOptions_tickLabelOptions,

    -- * AxisDisplayRange
    AxisDisplayRange (..),
    newAxisDisplayRange,
    axisDisplayRange_dataDriven,
    axisDisplayRange_minMax,

    -- * AxisLabelOptions
    AxisLabelOptions (..),
    newAxisLabelOptions,
    axisLabelOptions_applyTo,
    axisLabelOptions_customLabel,
    axisLabelOptions_fontConfiguration,

    -- * AxisLabelReferenceOptions
    AxisLabelReferenceOptions (..),
    newAxisLabelReferenceOptions,
    axisLabelReferenceOptions_fieldId,
    axisLabelReferenceOptions_column,

    -- * AxisLinearScale
    AxisLinearScale (..),
    newAxisLinearScale,
    axisLinearScale_stepCount,
    axisLinearScale_stepSize,

    -- * AxisLogarithmicScale
    AxisLogarithmicScale (..),
    newAxisLogarithmicScale,
    axisLogarithmicScale_base,

    -- * AxisScale
    AxisScale (..),
    newAxisScale,
    axisScale_linear,
    axisScale_logarithmic,

    -- * AxisTickLabelOptions
    AxisTickLabelOptions (..),
    newAxisTickLabelOptions,
    axisTickLabelOptions_labelOptions,
    axisTickLabelOptions_rotationAngle,

    -- * BarChartAggregatedFieldWells
    BarChartAggregatedFieldWells (..),
    newBarChartAggregatedFieldWells,
    barChartAggregatedFieldWells_category,
    barChartAggregatedFieldWells_colors,
    barChartAggregatedFieldWells_smallMultiples,
    barChartAggregatedFieldWells_values,

    -- * BarChartConfiguration
    BarChartConfiguration (..),
    newBarChartConfiguration,
    barChartConfiguration_barsArrangement,
    barChartConfiguration_categoryAxis,
    barChartConfiguration_categoryLabelOptions,
    barChartConfiguration_colorLabelOptions,
    barChartConfiguration_contributionAnalysisDefaults,
    barChartConfiguration_dataLabels,
    barChartConfiguration_fieldWells,
    barChartConfiguration_legend,
    barChartConfiguration_orientation,
    barChartConfiguration_referenceLines,
    barChartConfiguration_smallMultiplesOptions,
    barChartConfiguration_sortConfiguration,
    barChartConfiguration_tooltip,
    barChartConfiguration_valueAxis,
    barChartConfiguration_valueLabelOptions,
    barChartConfiguration_visualPalette,

    -- * BarChartFieldWells
    BarChartFieldWells (..),
    newBarChartFieldWells,
    barChartFieldWells_barChartAggregatedFieldWells,

    -- * BarChartSortConfiguration
    BarChartSortConfiguration (..),
    newBarChartSortConfiguration,
    barChartSortConfiguration_categoryItemsLimit,
    barChartSortConfiguration_categorySort,
    barChartSortConfiguration_colorItemsLimit,
    barChartSortConfiguration_colorSort,
    barChartSortConfiguration_smallMultiplesLimitConfiguration,
    barChartSortConfiguration_smallMultiplesSort,

    -- * BarChartVisual
    BarChartVisual (..),
    newBarChartVisual,
    barChartVisual_actions,
    barChartVisual_chartConfiguration,
    barChartVisual_columnHierarchies,
    barChartVisual_subtitle,
    barChartVisual_title,
    barChartVisual_visualId,

    -- * BinCountOptions
    BinCountOptions (..),
    newBinCountOptions,
    binCountOptions_value,

    -- * BinWidthOptions
    BinWidthOptions (..),
    newBinWidthOptions,
    binWidthOptions_binCountLimit,
    binWidthOptions_value,

    -- * BodySectionConfiguration
    BodySectionConfiguration (..),
    newBodySectionConfiguration,
    bodySectionConfiguration_pageBreakConfiguration,
    bodySectionConfiguration_style,
    bodySectionConfiguration_sectionId,
    bodySectionConfiguration_content,

    -- * BodySectionContent
    BodySectionContent (..),
    newBodySectionContent,
    bodySectionContent_layout,

    -- * BorderStyle
    BorderStyle (..),
    newBorderStyle,
    borderStyle_show,

    -- * BoxPlotAggregatedFieldWells
    BoxPlotAggregatedFieldWells (..),
    newBoxPlotAggregatedFieldWells,
    boxPlotAggregatedFieldWells_groupBy,
    boxPlotAggregatedFieldWells_values,

    -- * BoxPlotChartConfiguration
    BoxPlotChartConfiguration (..),
    newBoxPlotChartConfiguration,
    boxPlotChartConfiguration_boxPlotOptions,
    boxPlotChartConfiguration_categoryAxis,
    boxPlotChartConfiguration_categoryLabelOptions,
    boxPlotChartConfiguration_fieldWells,
    boxPlotChartConfiguration_legend,
    boxPlotChartConfiguration_primaryYAxisDisplayOptions,
    boxPlotChartConfiguration_primaryYAxisLabelOptions,
    boxPlotChartConfiguration_referenceLines,
    boxPlotChartConfiguration_sortConfiguration,
    boxPlotChartConfiguration_tooltip,
    boxPlotChartConfiguration_visualPalette,

    -- * BoxPlotFieldWells
    BoxPlotFieldWells (..),
    newBoxPlotFieldWells,
    boxPlotFieldWells_boxPlotAggregatedFieldWells,

    -- * BoxPlotOptions
    BoxPlotOptions (..),
    newBoxPlotOptions,
    boxPlotOptions_allDataPointsVisibility,
    boxPlotOptions_outlierVisibility,
    boxPlotOptions_styleOptions,

    -- * BoxPlotSortConfiguration
    BoxPlotSortConfiguration (..),
    newBoxPlotSortConfiguration,
    boxPlotSortConfiguration_categorySort,
    boxPlotSortConfiguration_paginationConfiguration,

    -- * BoxPlotStyleOptions
    BoxPlotStyleOptions (..),
    newBoxPlotStyleOptions,
    boxPlotStyleOptions_fillStyle,

    -- * BoxPlotVisual
    BoxPlotVisual (..),
    newBoxPlotVisual,
    boxPlotVisual_actions,
    boxPlotVisual_chartConfiguration,
    boxPlotVisual_columnHierarchies,
    boxPlotVisual_subtitle,
    boxPlotVisual_title,
    boxPlotVisual_visualId,

    -- * CalculatedColumn
    CalculatedColumn (..),
    newCalculatedColumn,
    calculatedColumn_columnName,
    calculatedColumn_columnId,
    calculatedColumn_expression,

    -- * CalculatedField
    CalculatedField (..),
    newCalculatedField,
    calculatedField_dataSetIdentifier,
    calculatedField_name,
    calculatedField_expression,

    -- * CalculatedMeasureField
    CalculatedMeasureField (..),
    newCalculatedMeasureField,
    calculatedMeasureField_fieldId,
    calculatedMeasureField_expression,

    -- * CascadingControlConfiguration
    CascadingControlConfiguration (..),
    newCascadingControlConfiguration,
    cascadingControlConfiguration_sourceControls,

    -- * CascadingControlSource
    CascadingControlSource (..),
    newCascadingControlSource,
    cascadingControlSource_columnToMatch,
    cascadingControlSource_sourceSheetControlId,

    -- * CastColumnTypeOperation
    CastColumnTypeOperation (..),
    newCastColumnTypeOperation,
    castColumnTypeOperation_format,
    castColumnTypeOperation_columnName,
    castColumnTypeOperation_newColumnType,

    -- * CategoricalDimensionField
    CategoricalDimensionField (..),
    newCategoricalDimensionField,
    categoricalDimensionField_formatConfiguration,
    categoricalDimensionField_hierarchyId,
    categoricalDimensionField_fieldId,
    categoricalDimensionField_column,

    -- * CategoricalMeasureField
    CategoricalMeasureField (..),
    newCategoricalMeasureField,
    categoricalMeasureField_aggregationFunction,
    categoricalMeasureField_formatConfiguration,
    categoricalMeasureField_fieldId,
    categoricalMeasureField_column,

    -- * CategoryDrillDownFilter
    CategoryDrillDownFilter (..),
    newCategoryDrillDownFilter,
    categoryDrillDownFilter_column,
    categoryDrillDownFilter_categoryValues,

    -- * CategoryFilter
    CategoryFilter (..),
    newCategoryFilter,
    categoryFilter_configuration,
    categoryFilter_filterId,
    categoryFilter_column,

    -- * CategoryFilterConfiguration
    CategoryFilterConfiguration (..),
    newCategoryFilterConfiguration,
    categoryFilterConfiguration_customFilterConfiguration,
    categoryFilterConfiguration_customFilterListConfiguration,
    categoryFilterConfiguration_filterListConfiguration,

    -- * ChartAxisLabelOptions
    ChartAxisLabelOptions (..),
    newChartAxisLabelOptions,
    chartAxisLabelOptions_axisLabelOptions,
    chartAxisLabelOptions_sortIconVisibility,
    chartAxisLabelOptions_visibility,

    -- * ClusterMarker
    ClusterMarker (..),
    newClusterMarker,
    clusterMarker_simpleClusterMarker,

    -- * ClusterMarkerConfiguration
    ClusterMarkerConfiguration (..),
    newClusterMarkerConfiguration,
    clusterMarkerConfiguration_clusterMarker,

    -- * ColorScale
    ColorScale (..),
    newColorScale,
    colorScale_nullValueColor,
    colorScale_colors,
    colorScale_colorFillType,

    -- * ColumnConfiguration
    ColumnConfiguration (..),
    newColumnConfiguration,
    columnConfiguration_formatConfiguration,
    columnConfiguration_role,
    columnConfiguration_column,

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

    -- * ColumnHierarchy
    ColumnHierarchy (..),
    newColumnHierarchy,
    columnHierarchy_dateTimeHierarchy,
    columnHierarchy_explicitHierarchy,
    columnHierarchy_predefinedHierarchy,

    -- * ColumnIdentifier
    ColumnIdentifier (..),
    newColumnIdentifier,
    columnIdentifier_dataSetIdentifier,
    columnIdentifier_columnName,

    -- * ColumnLevelPermissionRule
    ColumnLevelPermissionRule (..),
    newColumnLevelPermissionRule,
    columnLevelPermissionRule_columnNames,
    columnLevelPermissionRule_principals,

    -- * ColumnSchema
    ColumnSchema (..),
    newColumnSchema,
    columnSchema_dataType,
    columnSchema_geographicRole,
    columnSchema_name,

    -- * ColumnSort
    ColumnSort (..),
    newColumnSort,
    columnSort_aggregationFunction,
    columnSort_sortBy,
    columnSort_direction,

    -- * ColumnTag
    ColumnTag (..),
    newColumnTag,
    columnTag_columnDescription,
    columnTag_columnGeographicRole,

    -- * ColumnTooltipItem
    ColumnTooltipItem (..),
    newColumnTooltipItem,
    columnTooltipItem_aggregation,
    columnTooltipItem_label,
    columnTooltipItem_visibility,
    columnTooltipItem_column,

    -- * ComboChartAggregatedFieldWells
    ComboChartAggregatedFieldWells (..),
    newComboChartAggregatedFieldWells,
    comboChartAggregatedFieldWells_barValues,
    comboChartAggregatedFieldWells_category,
    comboChartAggregatedFieldWells_colors,
    comboChartAggregatedFieldWells_lineValues,

    -- * ComboChartConfiguration
    ComboChartConfiguration (..),
    newComboChartConfiguration,
    comboChartConfiguration_barDataLabels,
    comboChartConfiguration_barsArrangement,
    comboChartConfiguration_categoryAxis,
    comboChartConfiguration_categoryLabelOptions,
    comboChartConfiguration_colorLabelOptions,
    comboChartConfiguration_fieldWells,
    comboChartConfiguration_legend,
    comboChartConfiguration_lineDataLabels,
    comboChartConfiguration_primaryYAxisDisplayOptions,
    comboChartConfiguration_primaryYAxisLabelOptions,
    comboChartConfiguration_referenceLines,
    comboChartConfiguration_secondaryYAxisDisplayOptions,
    comboChartConfiguration_secondaryYAxisLabelOptions,
    comboChartConfiguration_sortConfiguration,
    comboChartConfiguration_tooltip,
    comboChartConfiguration_visualPalette,

    -- * ComboChartFieldWells
    ComboChartFieldWells (..),
    newComboChartFieldWells,
    comboChartFieldWells_comboChartAggregatedFieldWells,

    -- * ComboChartSortConfiguration
    ComboChartSortConfiguration (..),
    newComboChartSortConfiguration,
    comboChartSortConfiguration_categoryItemsLimit,
    comboChartSortConfiguration_categorySort,
    comboChartSortConfiguration_colorItemsLimit,
    comboChartSortConfiguration_colorSort,

    -- * ComboChartVisual
    ComboChartVisual (..),
    newComboChartVisual,
    comboChartVisual_actions,
    comboChartVisual_chartConfiguration,
    comboChartVisual_columnHierarchies,
    comboChartVisual_subtitle,
    comboChartVisual_title,
    comboChartVisual_visualId,

    -- * ComparisonConfiguration
    ComparisonConfiguration (..),
    newComparisonConfiguration,
    comparisonConfiguration_comparisonFormat,
    comparisonConfiguration_comparisonMethod,

    -- * ComparisonFormatConfiguration
    ComparisonFormatConfiguration (..),
    newComparisonFormatConfiguration,
    comparisonFormatConfiguration_numberDisplayFormatConfiguration,
    comparisonFormatConfiguration_percentageDisplayFormatConfiguration,

    -- * Computation
    Computation (..),
    newComputation,
    computation_forecast,
    computation_growthRate,
    computation_maximumMinimum,
    computation_metricComparison,
    computation_periodOverPeriod,
    computation_periodToDate,
    computation_topBottomMovers,
    computation_topBottomRanked,
    computation_totalAggregation,
    computation_uniqueValues,

    -- * ConditionalFormattingColor
    ConditionalFormattingColor (..),
    newConditionalFormattingColor,
    conditionalFormattingColor_gradient,
    conditionalFormattingColor_solid,

    -- * ConditionalFormattingCustomIconCondition
    ConditionalFormattingCustomIconCondition (..),
    newConditionalFormattingCustomIconCondition,
    conditionalFormattingCustomIconCondition_color,
    conditionalFormattingCustomIconCondition_displayConfiguration,
    conditionalFormattingCustomIconCondition_expression,
    conditionalFormattingCustomIconCondition_iconOptions,

    -- * ConditionalFormattingCustomIconOptions
    ConditionalFormattingCustomIconOptions (..),
    newConditionalFormattingCustomIconOptions,
    conditionalFormattingCustomIconOptions_icon,
    conditionalFormattingCustomIconOptions_unicodeIcon,

    -- * ConditionalFormattingGradientColor
    ConditionalFormattingGradientColor (..),
    newConditionalFormattingGradientColor,
    conditionalFormattingGradientColor_expression,
    conditionalFormattingGradientColor_color,

    -- * ConditionalFormattingIcon
    ConditionalFormattingIcon (..),
    newConditionalFormattingIcon,
    conditionalFormattingIcon_customCondition,
    conditionalFormattingIcon_iconSet,

    -- * ConditionalFormattingIconDisplayConfiguration
    ConditionalFormattingIconDisplayConfiguration (..),
    newConditionalFormattingIconDisplayConfiguration,
    conditionalFormattingIconDisplayConfiguration_iconDisplayOption,

    -- * ConditionalFormattingIconSet
    ConditionalFormattingIconSet (..),
    newConditionalFormattingIconSet,
    conditionalFormattingIconSet_iconSetType,
    conditionalFormattingIconSet_expression,

    -- * ConditionalFormattingSolidColor
    ConditionalFormattingSolidColor (..),
    newConditionalFormattingSolidColor,
    conditionalFormattingSolidColor_color,
    conditionalFormattingSolidColor_expression,

    -- * ContributionAnalysisDefault
    ContributionAnalysisDefault (..),
    newContributionAnalysisDefault,
    contributionAnalysisDefault_measureFieldId,
    contributionAnalysisDefault_contributorDimensions,

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

    -- * CurrencyDisplayFormatConfiguration
    CurrencyDisplayFormatConfiguration (..),
    newCurrencyDisplayFormatConfiguration,
    currencyDisplayFormatConfiguration_decimalPlacesConfiguration,
    currencyDisplayFormatConfiguration_negativeValueConfiguration,
    currencyDisplayFormatConfiguration_nullValueFormatConfiguration,
    currencyDisplayFormatConfiguration_numberScale,
    currencyDisplayFormatConfiguration_prefix,
    currencyDisplayFormatConfiguration_separatorConfiguration,
    currencyDisplayFormatConfiguration_suffix,
    currencyDisplayFormatConfiguration_symbol,

    -- * CustomActionFilterOperation
    CustomActionFilterOperation (..),
    newCustomActionFilterOperation,
    customActionFilterOperation_selectedFieldsConfiguration,
    customActionFilterOperation_targetVisualsConfiguration,

    -- * CustomActionNavigationOperation
    CustomActionNavigationOperation (..),
    newCustomActionNavigationOperation,
    customActionNavigationOperation_localNavigationConfiguration,

    -- * CustomActionSetParametersOperation
    CustomActionSetParametersOperation (..),
    newCustomActionSetParametersOperation,
    customActionSetParametersOperation_parameterValueConfigurations,

    -- * CustomActionURLOperation
    CustomActionURLOperation (..),
    newCustomActionURLOperation,
    customActionURLOperation_uRLTemplate,
    customActionURLOperation_uRLTarget,

    -- * CustomContentConfiguration
    CustomContentConfiguration (..),
    newCustomContentConfiguration,
    customContentConfiguration_contentType,
    customContentConfiguration_contentUrl,
    customContentConfiguration_imageScaling,

    -- * CustomContentVisual
    CustomContentVisual (..),
    newCustomContentVisual,
    customContentVisual_actions,
    customContentVisual_chartConfiguration,
    customContentVisual_subtitle,
    customContentVisual_title,
    customContentVisual_visualId,
    customContentVisual_dataSetIdentifier,

    -- * CustomFilterConfiguration
    CustomFilterConfiguration (..),
    newCustomFilterConfiguration,
    customFilterConfiguration_categoryValue,
    customFilterConfiguration_parameterName,
    customFilterConfiguration_selectAllOptions,
    customFilterConfiguration_matchOperator,
    customFilterConfiguration_nullOption,

    -- * CustomFilterListConfiguration
    CustomFilterListConfiguration (..),
    newCustomFilterListConfiguration,
    customFilterListConfiguration_categoryValues,
    customFilterListConfiguration_selectAllOptions,
    customFilterListConfiguration_matchOperator,
    customFilterListConfiguration_nullOption,

    -- * CustomNarrativeOptions
    CustomNarrativeOptions (..),
    newCustomNarrativeOptions,
    customNarrativeOptions_narrative,

    -- * CustomParameterValues
    CustomParameterValues (..),
    newCustomParameterValues,
    customParameterValues_dateTimeValues,
    customParameterValues_decimalValues,
    customParameterValues_integerValues,
    customParameterValues_stringValues,

    -- * CustomSql
    CustomSql (..),
    newCustomSql,
    customSql_columns,
    customSql_dataSourceArn,
    customSql_name,
    customSql_sqlQuery,

    -- * CustomValuesConfiguration
    CustomValuesConfiguration (..),
    newCustomValuesConfiguration,
    customValuesConfiguration_includeNullValue,
    customValuesConfiguration_customValues,

    -- * Dashboard
    Dashboard (..),
    newDashboard,
    dashboard_arn,
    dashboard_createdTime,
    dashboard_dashboardId,
    dashboard_lastPublishedTime,
    dashboard_lastUpdatedTime,
    dashboard_name,
    dashboard_version,

    -- * DashboardError
    DashboardError (..),
    newDashboardError,
    dashboardError_message,
    dashboardError_type,
    dashboardError_violatedEntities,

    -- * DashboardPublishOptions
    DashboardPublishOptions (..),
    newDashboardPublishOptions,
    dashboardPublishOptions_adHocFilteringOption,
    dashboardPublishOptions_exportToCSVOption,
    dashboardPublishOptions_sheetControlsOption,
    dashboardPublishOptions_visualPublishOptions,

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
    dashboardSummary_arn,
    dashboardSummary_createdTime,
    dashboardSummary_dashboardId,
    dashboardSummary_lastPublishedTime,
    dashboardSummary_lastUpdatedTime,
    dashboardSummary_name,
    dashboardSummary_publishedVersionNumber,

    -- * DashboardVersion
    DashboardVersion (..),
    newDashboardVersion,
    dashboardVersion_arn,
    dashboardVersion_createdTime,
    dashboardVersion_dataSetArns,
    dashboardVersion_description,
    dashboardVersion_errors,
    dashboardVersion_sheets,
    dashboardVersion_sourceEntityArn,
    dashboardVersion_status,
    dashboardVersion_themeArn,
    dashboardVersion_versionNumber,

    -- * DashboardVersionDefinition
    DashboardVersionDefinition (..),
    newDashboardVersionDefinition,
    dashboardVersionDefinition_analysisDefaults,
    dashboardVersionDefinition_calculatedFields,
    dashboardVersionDefinition_columnConfigurations,
    dashboardVersionDefinition_filterGroups,
    dashboardVersionDefinition_parameterDeclarations,
    dashboardVersionDefinition_sheets,
    dashboardVersionDefinition_dataSetIdentifierDeclarations,

    -- * DashboardVersionSummary
    DashboardVersionSummary (..),
    newDashboardVersionSummary,
    dashboardVersionSummary_arn,
    dashboardVersionSummary_createdTime,
    dashboardVersionSummary_description,
    dashboardVersionSummary_sourceEntityArn,
    dashboardVersionSummary_status,
    dashboardVersionSummary_versionNumber,

    -- * DashboardVisualId
    DashboardVisualId (..),
    newDashboardVisualId,
    dashboardVisualId_dashboardId,
    dashboardVisualId_sheetId,
    dashboardVisualId_visualId,

    -- * DashboardVisualPublishOptions
    DashboardVisualPublishOptions (..),
    newDashboardVisualPublishOptions,
    dashboardVisualPublishOptions_exportHiddenFieldsOption,

    -- * DataColor
    DataColor (..),
    newDataColor,
    dataColor_color,
    dataColor_dataValue,

    -- * DataColorPalette
    DataColorPalette (..),
    newDataColorPalette,
    dataColorPalette_colors,
    dataColorPalette_emptyFillColor,
    dataColorPalette_minMaxGradient,

    -- * DataFieldSeriesItem
    DataFieldSeriesItem (..),
    newDataFieldSeriesItem,
    dataFieldSeriesItem_fieldValue,
    dataFieldSeriesItem_settings,
    dataFieldSeriesItem_fieldId,
    dataFieldSeriesItem_axisBinding,

    -- * DataLabelOptions
    DataLabelOptions (..),
    newDataLabelOptions,
    dataLabelOptions_categoryLabelVisibility,
    dataLabelOptions_dataLabelTypes,
    dataLabelOptions_labelColor,
    dataLabelOptions_labelContent,
    dataLabelOptions_labelFontConfiguration,
    dataLabelOptions_measureLabelVisibility,
    dataLabelOptions_overlap,
    dataLabelOptions_position,
    dataLabelOptions_visibility,

    -- * DataLabelType
    DataLabelType (..),
    newDataLabelType,
    dataLabelType_dataPathLabelType,
    dataLabelType_fieldLabelType,
    dataLabelType_maximumLabelType,
    dataLabelType_minimumLabelType,
    dataLabelType_rangeEndsLabelType,

    -- * DataPathColor
    DataPathColor (..),
    newDataPathColor,
    dataPathColor_timeGranularity,
    dataPathColor_element,
    dataPathColor_color,

    -- * DataPathLabelType
    DataPathLabelType (..),
    newDataPathLabelType,
    dataPathLabelType_fieldId,
    dataPathLabelType_fieldValue,
    dataPathLabelType_visibility,

    -- * DataPathSort
    DataPathSort (..),
    newDataPathSort,
    dataPathSort_direction,
    dataPathSort_sortPaths,

    -- * DataPathValue
    DataPathValue (..),
    newDataPathValue,
    dataPathValue_fieldId,
    dataPathValue_fieldValue,

    -- * DataSet
    DataSet (..),
    newDataSet,
    dataSet_arn,
    dataSet_columnGroups,
    dataSet_columnLevelPermissionRules,
    dataSet_consumedSpiceCapacityInBytes,
    dataSet_createdTime,
    dataSet_dataSetId,
    dataSet_dataSetUsageConfiguration,
    dataSet_fieldFolders,
    dataSet_importMode,
    dataSet_lastUpdatedTime,
    dataSet_logicalTableMap,
    dataSet_name,
    dataSet_outputColumns,
    dataSet_physicalTableMap,
    dataSet_rowLevelPermissionDataSet,
    dataSet_rowLevelPermissionTagConfiguration,

    -- * DataSetConfiguration
    DataSetConfiguration (..),
    newDataSetConfiguration,
    dataSetConfiguration_columnGroupSchemaList,
    dataSetConfiguration_dataSetSchema,
    dataSetConfiguration_placeholder,

    -- * DataSetIdentifierDeclaration
    DataSetIdentifierDeclaration (..),
    newDataSetIdentifierDeclaration,
    dataSetIdentifierDeclaration_identifier,
    dataSetIdentifierDeclaration_dataSetArn,

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
    dataSetSummary_arn,
    dataSetSummary_columnLevelPermissionRulesApplied,
    dataSetSummary_createdTime,
    dataSetSummary_dataSetId,
    dataSetSummary_importMode,
    dataSetSummary_lastUpdatedTime,
    dataSetSummary_name,
    dataSetSummary_rowLevelPermissionDataSet,
    dataSetSummary_rowLevelPermissionTagConfigurationApplied,

    -- * DataSetUsageConfiguration
    DataSetUsageConfiguration (..),
    newDataSetUsageConfiguration,
    dataSetUsageConfiguration_disableUseAsDirectQuerySource,
    dataSetUsageConfiguration_disableUseAsImportedSource,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_alternateDataSourceParameters,
    dataSource_arn,
    dataSource_createdTime,
    dataSource_dataSourceId,
    dataSource_dataSourceParameters,
    dataSource_errorInfo,
    dataSource_lastUpdatedTime,
    dataSource_name,
    dataSource_secretArn,
    dataSource_sslProperties,
    dataSource_status,
    dataSource_type,
    dataSource_vpcConnectionProperties,

    -- * DataSourceCredentials
    DataSourceCredentials (..),
    newDataSourceCredentials,
    dataSourceCredentials_copySourceArn,
    dataSourceCredentials_credentialPair,
    dataSourceCredentials_secretArn,

    -- * DataSourceErrorInfo
    DataSourceErrorInfo (..),
    newDataSourceErrorInfo,
    dataSourceErrorInfo_message,
    dataSourceErrorInfo_type,

    -- * DataSourceParameters
    DataSourceParameters (..),
    newDataSourceParameters,
    dataSourceParameters_amazonElasticsearchParameters,
    dataSourceParameters_amazonOpenSearchParameters,
    dataSourceParameters_athenaParameters,
    dataSourceParameters_auroraParameters,
    dataSourceParameters_auroraPostgreSqlParameters,
    dataSourceParameters_awsIotAnalyticsParameters,
    dataSourceParameters_databricksParameters,
    dataSourceParameters_exasolParameters,
    dataSourceParameters_jiraParameters,
    dataSourceParameters_mariaDbParameters,
    dataSourceParameters_mySqlParameters,
    dataSourceParameters_oracleParameters,
    dataSourceParameters_postgreSqlParameters,
    dataSourceParameters_prestoParameters,
    dataSourceParameters_rdsParameters,
    dataSourceParameters_redshiftParameters,
    dataSourceParameters_s3Parameters,
    dataSourceParameters_serviceNowParameters,
    dataSourceParameters_snowflakeParameters,
    dataSourceParameters_sparkParameters,
    dataSourceParameters_sqlServerParameters,
    dataSourceParameters_teradataParameters,
    dataSourceParameters_twitterParameters,

    -- * DataSourceSearchFilter
    DataSourceSearchFilter (..),
    newDataSourceSearchFilter,
    dataSourceSearchFilter_operator,
    dataSourceSearchFilter_name,
    dataSourceSearchFilter_value,

    -- * DataSourceSummary
    DataSourceSummary (..),
    newDataSourceSummary,
    dataSourceSummary_arn,
    dataSourceSummary_createdTime,
    dataSourceSummary_dataSourceId,
    dataSourceSummary_lastUpdatedTime,
    dataSourceSummary_name,
    dataSourceSummary_type,

    -- * DatabricksParameters
    DatabricksParameters (..),
    newDatabricksParameters,
    databricksParameters_host,
    databricksParameters_port,
    databricksParameters_sqlEndpointPath,

    -- * DateAxisOptions
    DateAxisOptions (..),
    newDateAxisOptions,
    dateAxisOptions_missingDateVisibility,

    -- * DateDimensionField
    DateDimensionField (..),
    newDateDimensionField,
    dateDimensionField_dateGranularity,
    dateDimensionField_formatConfiguration,
    dateDimensionField_hierarchyId,
    dateDimensionField_fieldId,
    dateDimensionField_column,

    -- * DateMeasureField
    DateMeasureField (..),
    newDateMeasureField,
    dateMeasureField_aggregationFunction,
    dateMeasureField_formatConfiguration,
    dateMeasureField_fieldId,
    dateMeasureField_column,

    -- * DateTimeDefaultValues
    DateTimeDefaultValues (..),
    newDateTimeDefaultValues,
    dateTimeDefaultValues_dynamicValue,
    dateTimeDefaultValues_rollingDate,
    dateTimeDefaultValues_staticValues,

    -- * DateTimeFormatConfiguration
    DateTimeFormatConfiguration (..),
    newDateTimeFormatConfiguration,
    dateTimeFormatConfiguration_dateTimeFormat,
    dateTimeFormatConfiguration_nullValueFormatConfiguration,
    dateTimeFormatConfiguration_numericFormatConfiguration,

    -- * DateTimeHierarchy
    DateTimeHierarchy (..),
    newDateTimeHierarchy,
    dateTimeHierarchy_drillDownFilters,
    dateTimeHierarchy_hierarchyId,

    -- * DateTimeParameter
    DateTimeParameter (..),
    newDateTimeParameter,
    dateTimeParameter_name,
    dateTimeParameter_values,

    -- * DateTimeParameterDeclaration
    DateTimeParameterDeclaration (..),
    newDateTimeParameterDeclaration,
    dateTimeParameterDeclaration_defaultValues,
    dateTimeParameterDeclaration_timeGranularity,
    dateTimeParameterDeclaration_valueWhenUnset,
    dateTimeParameterDeclaration_name,

    -- * DateTimePickerControlDisplayOptions
    DateTimePickerControlDisplayOptions (..),
    newDateTimePickerControlDisplayOptions,
    dateTimePickerControlDisplayOptions_dateTimeFormat,
    dateTimePickerControlDisplayOptions_titleOptions,

    -- * DateTimeValueWhenUnsetConfiguration
    DateTimeValueWhenUnsetConfiguration (..),
    newDateTimeValueWhenUnsetConfiguration,
    dateTimeValueWhenUnsetConfiguration_customValue,
    dateTimeValueWhenUnsetConfiguration_valueWhenUnsetOption,

    -- * DecimalDefaultValues
    DecimalDefaultValues (..),
    newDecimalDefaultValues,
    decimalDefaultValues_dynamicValue,
    decimalDefaultValues_staticValues,

    -- * DecimalParameter
    DecimalParameter (..),
    newDecimalParameter,
    decimalParameter_name,
    decimalParameter_values,

    -- * DecimalParameterDeclaration
    DecimalParameterDeclaration (..),
    newDecimalParameterDeclaration,
    decimalParameterDeclaration_defaultValues,
    decimalParameterDeclaration_valueWhenUnset,
    decimalParameterDeclaration_parameterValueType,
    decimalParameterDeclaration_name,

    -- * DecimalPlacesConfiguration
    DecimalPlacesConfiguration (..),
    newDecimalPlacesConfiguration,
    decimalPlacesConfiguration_decimalPlaces,

    -- * DecimalValueWhenUnsetConfiguration
    DecimalValueWhenUnsetConfiguration (..),
    newDecimalValueWhenUnsetConfiguration,
    decimalValueWhenUnsetConfiguration_customValue,
    decimalValueWhenUnsetConfiguration_valueWhenUnsetOption,

    -- * DefaultFreeFormLayoutConfiguration
    DefaultFreeFormLayoutConfiguration (..),
    newDefaultFreeFormLayoutConfiguration,
    defaultFreeFormLayoutConfiguration_canvasSizeOptions,

    -- * DefaultGridLayoutConfiguration
    DefaultGridLayoutConfiguration (..),
    newDefaultGridLayoutConfiguration,
    defaultGridLayoutConfiguration_canvasSizeOptions,

    -- * DefaultInteractiveLayoutConfiguration
    DefaultInteractiveLayoutConfiguration (..),
    newDefaultInteractiveLayoutConfiguration,
    defaultInteractiveLayoutConfiguration_freeForm,
    defaultInteractiveLayoutConfiguration_grid,

    -- * DefaultNewSheetConfiguration
    DefaultNewSheetConfiguration (..),
    newDefaultNewSheetConfiguration,
    defaultNewSheetConfiguration_interactiveLayoutConfiguration,
    defaultNewSheetConfiguration_paginatedLayoutConfiguration,
    defaultNewSheetConfiguration_sheetContentType,

    -- * DefaultPaginatedLayoutConfiguration
    DefaultPaginatedLayoutConfiguration (..),
    newDefaultPaginatedLayoutConfiguration,
    defaultPaginatedLayoutConfiguration_sectionBased,

    -- * DefaultSectionBasedLayoutConfiguration
    DefaultSectionBasedLayoutConfiguration (..),
    newDefaultSectionBasedLayoutConfiguration,
    defaultSectionBasedLayoutConfiguration_canvasSizeOptions,

    -- * DestinationParameterValueConfiguration
    DestinationParameterValueConfiguration (..),
    newDestinationParameterValueConfiguration,
    destinationParameterValueConfiguration_customValuesConfiguration,
    destinationParameterValueConfiguration_selectAllValueOptions,
    destinationParameterValueConfiguration_sourceField,
    destinationParameterValueConfiguration_sourceParameterName,

    -- * DimensionField
    DimensionField (..),
    newDimensionField,
    dimensionField_categoricalDimensionField,
    dimensionField_dateDimensionField,
    dimensionField_numericalDimensionField,

    -- * DonutCenterOptions
    DonutCenterOptions (..),
    newDonutCenterOptions,
    donutCenterOptions_labelVisibility,

    -- * DonutOptions
    DonutOptions (..),
    newDonutOptions,
    donutOptions_arcOptions,
    donutOptions_donutCenterOptions,

    -- * DrillDownFilter
    DrillDownFilter (..),
    newDrillDownFilter,
    drillDownFilter_categoryFilter,
    drillDownFilter_numericEqualityFilter,
    drillDownFilter_timeRangeFilter,

    -- * DropDownControlDisplayOptions
    DropDownControlDisplayOptions (..),
    newDropDownControlDisplayOptions,
    dropDownControlDisplayOptions_selectAllOptions,
    dropDownControlDisplayOptions_titleOptions,

    -- * DynamicDefaultValue
    DynamicDefaultValue (..),
    newDynamicDefaultValue,
    dynamicDefaultValue_groupNameColumn,
    dynamicDefaultValue_userNameColumn,
    dynamicDefaultValue_defaultValueColumn,

    -- * EmptyVisual
    EmptyVisual (..),
    newEmptyVisual,
    emptyVisual_actions,
    emptyVisual_visualId,
    emptyVisual_dataSetIdentifier,

    -- * Entity
    Entity (..),
    newEntity,
    entity_path,

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

    -- * ExcludePeriodConfiguration
    ExcludePeriodConfiguration (..),
    newExcludePeriodConfiguration,
    excludePeriodConfiguration_status,
    excludePeriodConfiguration_amount,
    excludePeriodConfiguration_granularity,

    -- * ExplicitHierarchy
    ExplicitHierarchy (..),
    newExplicitHierarchy,
    explicitHierarchy_drillDownFilters,
    explicitHierarchy_hierarchyId,
    explicitHierarchy_columns,

    -- * ExportHiddenFieldsOption
    ExportHiddenFieldsOption (..),
    newExportHiddenFieldsOption,
    exportHiddenFieldsOption_availabilityStatus,

    -- * ExportToCSVOption
    ExportToCSVOption (..),
    newExportToCSVOption,
    exportToCSVOption_availabilityStatus,

    -- * FieldBasedTooltip
    FieldBasedTooltip (..),
    newFieldBasedTooltip,
    fieldBasedTooltip_aggregationVisibility,
    fieldBasedTooltip_tooltipFields,
    fieldBasedTooltip_tooltipTitleType,

    -- * FieldFolder
    FieldFolder (..),
    newFieldFolder,
    fieldFolder_columns,
    fieldFolder_description,

    -- * FieldLabelType
    FieldLabelType (..),
    newFieldLabelType,
    fieldLabelType_fieldId,
    fieldLabelType_visibility,

    -- * FieldSeriesItem
    FieldSeriesItem (..),
    newFieldSeriesItem,
    fieldSeriesItem_settings,
    fieldSeriesItem_fieldId,
    fieldSeriesItem_axisBinding,

    -- * FieldSort
    FieldSort (..),
    newFieldSort,
    fieldSort_fieldId,
    fieldSort_direction,

    -- * FieldSortOptions
    FieldSortOptions (..),
    newFieldSortOptions,
    fieldSortOptions_columnSort,
    fieldSortOptions_fieldSort,

    -- * FieldTooltipItem
    FieldTooltipItem (..),
    newFieldTooltipItem,
    fieldTooltipItem_label,
    fieldTooltipItem_visibility,
    fieldTooltipItem_fieldId,

    -- * FilledMapAggregatedFieldWells
    FilledMapAggregatedFieldWells (..),
    newFilledMapAggregatedFieldWells,
    filledMapAggregatedFieldWells_geospatial,
    filledMapAggregatedFieldWells_values,

    -- * FilledMapConditionalFormatting
    FilledMapConditionalFormatting (..),
    newFilledMapConditionalFormatting,
    filledMapConditionalFormatting_conditionalFormattingOptions,

    -- * FilledMapConditionalFormattingOption
    FilledMapConditionalFormattingOption (..),
    newFilledMapConditionalFormattingOption,
    filledMapConditionalFormattingOption_shape,

    -- * FilledMapConfiguration
    FilledMapConfiguration (..),
    newFilledMapConfiguration,
    filledMapConfiguration_fieldWells,
    filledMapConfiguration_legend,
    filledMapConfiguration_mapStyleOptions,
    filledMapConfiguration_sortConfiguration,
    filledMapConfiguration_tooltip,
    filledMapConfiguration_windowOptions,

    -- * FilledMapFieldWells
    FilledMapFieldWells (..),
    newFilledMapFieldWells,
    filledMapFieldWells_filledMapAggregatedFieldWells,

    -- * FilledMapShapeConditionalFormatting
    FilledMapShapeConditionalFormatting (..),
    newFilledMapShapeConditionalFormatting,
    filledMapShapeConditionalFormatting_format,
    filledMapShapeConditionalFormatting_fieldId,

    -- * FilledMapSortConfiguration
    FilledMapSortConfiguration (..),
    newFilledMapSortConfiguration,
    filledMapSortConfiguration_categorySort,

    -- * FilledMapVisual
    FilledMapVisual (..),
    newFilledMapVisual,
    filledMapVisual_actions,
    filledMapVisual_chartConfiguration,
    filledMapVisual_columnHierarchies,
    filledMapVisual_conditionalFormatting,
    filledMapVisual_subtitle,
    filledMapVisual_title,
    filledMapVisual_visualId,

    -- * Filter
    Filter (..),
    newFilter,
    filter_categoryFilter,
    filter_numericEqualityFilter,
    filter_numericRangeFilter,
    filter_relativeDatesFilter,
    filter_timeEqualityFilter,
    filter_timeRangeFilter,
    filter_topBottomFilter,

    -- * FilterControl
    FilterControl (..),
    newFilterControl,
    filterControl_dateTimePicker,
    filterControl_dropdown,
    filterControl_list,
    filterControl_relativeDateTime,
    filterControl_slider,
    filterControl_textArea,
    filterControl_textField,

    -- * FilterDateTimePickerControl
    FilterDateTimePickerControl (..),
    newFilterDateTimePickerControl,
    filterDateTimePickerControl_displayOptions,
    filterDateTimePickerControl_type,
    filterDateTimePickerControl_filterControlId,
    filterDateTimePickerControl_title,
    filterDateTimePickerControl_sourceFilterId,

    -- * FilterDropDownControl
    FilterDropDownControl (..),
    newFilterDropDownControl,
    filterDropDownControl_cascadingControlConfiguration,
    filterDropDownControl_displayOptions,
    filterDropDownControl_selectableValues,
    filterDropDownControl_type,
    filterDropDownControl_filterControlId,
    filterDropDownControl_title,
    filterDropDownControl_sourceFilterId,

    -- * FilterGroup
    FilterGroup (..),
    newFilterGroup,
    filterGroup_status,
    filterGroup_filterGroupId,
    filterGroup_filters,
    filterGroup_scopeConfiguration,
    filterGroup_crossDataset,

    -- * FilterListConfiguration
    FilterListConfiguration (..),
    newFilterListConfiguration,
    filterListConfiguration_categoryValues,
    filterListConfiguration_selectAllOptions,
    filterListConfiguration_matchOperator,

    -- * FilterListControl
    FilterListControl (..),
    newFilterListControl,
    filterListControl_cascadingControlConfiguration,
    filterListControl_displayOptions,
    filterListControl_selectableValues,
    filterListControl_type,
    filterListControl_filterControlId,
    filterListControl_title,
    filterListControl_sourceFilterId,

    -- * FilterOperation
    FilterOperation (..),
    newFilterOperation,
    filterOperation_conditionExpression,

    -- * FilterOperationSelectedFieldsConfiguration
    FilterOperationSelectedFieldsConfiguration (..),
    newFilterOperationSelectedFieldsConfiguration,
    filterOperationSelectedFieldsConfiguration_selectedFieldOptions,
    filterOperationSelectedFieldsConfiguration_selectedFields,

    -- * FilterOperationTargetVisualsConfiguration
    FilterOperationTargetVisualsConfiguration (..),
    newFilterOperationTargetVisualsConfiguration,
    filterOperationTargetVisualsConfiguration_sameSheetTargetVisualConfiguration,

    -- * FilterRelativeDateTimeControl
    FilterRelativeDateTimeControl (..),
    newFilterRelativeDateTimeControl,
    filterRelativeDateTimeControl_displayOptions,
    filterRelativeDateTimeControl_filterControlId,
    filterRelativeDateTimeControl_title,
    filterRelativeDateTimeControl_sourceFilterId,

    -- * FilterScopeConfiguration
    FilterScopeConfiguration (..),
    newFilterScopeConfiguration,
    filterScopeConfiguration_selectedSheets,

    -- * FilterSelectableValues
    FilterSelectableValues (..),
    newFilterSelectableValues,
    filterSelectableValues_values,

    -- * FilterSliderControl
    FilterSliderControl (..),
    newFilterSliderControl,
    filterSliderControl_displayOptions,
    filterSliderControl_type,
    filterSliderControl_filterControlId,
    filterSliderControl_title,
    filterSliderControl_sourceFilterId,
    filterSliderControl_maximumValue,
    filterSliderControl_minimumValue,
    filterSliderControl_stepSize,

    -- * FilterTextAreaControl
    FilterTextAreaControl (..),
    newFilterTextAreaControl,
    filterTextAreaControl_delimiter,
    filterTextAreaControl_displayOptions,
    filterTextAreaControl_filterControlId,
    filterTextAreaControl_title,
    filterTextAreaControl_sourceFilterId,

    -- * FilterTextFieldControl
    FilterTextFieldControl (..),
    newFilterTextFieldControl,
    filterTextFieldControl_displayOptions,
    filterTextFieldControl_filterControlId,
    filterTextFieldControl_title,
    filterTextFieldControl_sourceFilterId,

    -- * Folder
    Folder (..),
    newFolder,
    folder_arn,
    folder_createdTime,
    folder_folderId,
    folder_folderPath,
    folder_folderType,
    folder_lastUpdatedTime,
    folder_name,

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
    folderSummary_arn,
    folderSummary_createdTime,
    folderSummary_folderId,
    folderSummary_folderType,
    folderSummary_lastUpdatedTime,
    folderSummary_name,

    -- * Font
    Font (..),
    newFont,
    font_fontFamily,

    -- * FontConfiguration
    FontConfiguration (..),
    newFontConfiguration,
    fontConfiguration_fontColor,
    fontConfiguration_fontDecoration,
    fontConfiguration_fontSize,
    fontConfiguration_fontStyle,
    fontConfiguration_fontWeight,

    -- * FontSize
    FontSize (..),
    newFontSize,
    fontSize_relative,

    -- * FontWeight
    FontWeight (..),
    newFontWeight,
    fontWeight_name,

    -- * ForecastComputation
    ForecastComputation (..),
    newForecastComputation,
    forecastComputation_customSeasonalityValue,
    forecastComputation_lowerBoundary,
    forecastComputation_name,
    forecastComputation_periodsBackward,
    forecastComputation_periodsForward,
    forecastComputation_predictionInterval,
    forecastComputation_seasonality,
    forecastComputation_upperBoundary,
    forecastComputation_value,
    forecastComputation_computationId,
    forecastComputation_time,

    -- * ForecastConfiguration
    ForecastConfiguration (..),
    newForecastConfiguration,
    forecastConfiguration_forecastProperties,
    forecastConfiguration_scenario,

    -- * ForecastScenario
    ForecastScenario (..),
    newForecastScenario,
    forecastScenario_whatIfPointScenario,
    forecastScenario_whatIfRangeScenario,

    -- * FormatConfiguration
    FormatConfiguration (..),
    newFormatConfiguration,
    formatConfiguration_dateTimeFormatConfiguration,
    formatConfiguration_numberFormatConfiguration,
    formatConfiguration_stringFormatConfiguration,

    -- * FreeFormLayoutCanvasSizeOptions
    FreeFormLayoutCanvasSizeOptions (..),
    newFreeFormLayoutCanvasSizeOptions,
    freeFormLayoutCanvasSizeOptions_screenCanvasSizeOptions,

    -- * FreeFormLayoutConfiguration
    FreeFormLayoutConfiguration (..),
    newFreeFormLayoutConfiguration,
    freeFormLayoutConfiguration_canvasSizeOptions,
    freeFormLayoutConfiguration_elements,

    -- * FreeFormLayoutElement
    FreeFormLayoutElement (..),
    newFreeFormLayoutElement,
    freeFormLayoutElement_backgroundStyle,
    freeFormLayoutElement_borderStyle,
    freeFormLayoutElement_loadingAnimation,
    freeFormLayoutElement_renderingRules,
    freeFormLayoutElement_selectedBorderStyle,
    freeFormLayoutElement_visibility,
    freeFormLayoutElement_elementId,
    freeFormLayoutElement_elementType,
    freeFormLayoutElement_xAxisLocation,
    freeFormLayoutElement_yAxisLocation,
    freeFormLayoutElement_width,
    freeFormLayoutElement_height,

    -- * FreeFormLayoutElementBackgroundStyle
    FreeFormLayoutElementBackgroundStyle (..),
    newFreeFormLayoutElementBackgroundStyle,
    freeFormLayoutElementBackgroundStyle_color,
    freeFormLayoutElementBackgroundStyle_visibility,

    -- * FreeFormLayoutElementBorderStyle
    FreeFormLayoutElementBorderStyle (..),
    newFreeFormLayoutElementBorderStyle,
    freeFormLayoutElementBorderStyle_color,
    freeFormLayoutElementBorderStyle_visibility,

    -- * FreeFormLayoutScreenCanvasSizeOptions
    FreeFormLayoutScreenCanvasSizeOptions (..),
    newFreeFormLayoutScreenCanvasSizeOptions,
    freeFormLayoutScreenCanvasSizeOptions_optimizedViewPortWidth,

    -- * FreeFormSectionLayoutConfiguration
    FreeFormSectionLayoutConfiguration (..),
    newFreeFormSectionLayoutConfiguration,
    freeFormSectionLayoutConfiguration_elements,

    -- * FunnelChartAggregatedFieldWells
    FunnelChartAggregatedFieldWells (..),
    newFunnelChartAggregatedFieldWells,
    funnelChartAggregatedFieldWells_category,
    funnelChartAggregatedFieldWells_values,

    -- * FunnelChartConfiguration
    FunnelChartConfiguration (..),
    newFunnelChartConfiguration,
    funnelChartConfiguration_categoryLabelOptions,
    funnelChartConfiguration_dataLabelOptions,
    funnelChartConfiguration_fieldWells,
    funnelChartConfiguration_sortConfiguration,
    funnelChartConfiguration_tooltip,
    funnelChartConfiguration_valueLabelOptions,
    funnelChartConfiguration_visualPalette,

    -- * FunnelChartDataLabelOptions
    FunnelChartDataLabelOptions (..),
    newFunnelChartDataLabelOptions,
    funnelChartDataLabelOptions_categoryLabelVisibility,
    funnelChartDataLabelOptions_labelColor,
    funnelChartDataLabelOptions_labelFontConfiguration,
    funnelChartDataLabelOptions_measureDataLabelStyle,
    funnelChartDataLabelOptions_measureLabelVisibility,
    funnelChartDataLabelOptions_position,
    funnelChartDataLabelOptions_visibility,

    -- * FunnelChartFieldWells
    FunnelChartFieldWells (..),
    newFunnelChartFieldWells,
    funnelChartFieldWells_funnelChartAggregatedFieldWells,

    -- * FunnelChartSortConfiguration
    FunnelChartSortConfiguration (..),
    newFunnelChartSortConfiguration,
    funnelChartSortConfiguration_categoryItemsLimit,
    funnelChartSortConfiguration_categorySort,

    -- * FunnelChartVisual
    FunnelChartVisual (..),
    newFunnelChartVisual,
    funnelChartVisual_actions,
    funnelChartVisual_chartConfiguration,
    funnelChartVisual_columnHierarchies,
    funnelChartVisual_subtitle,
    funnelChartVisual_title,
    funnelChartVisual_visualId,

    -- * GaugeChartArcConditionalFormatting
    GaugeChartArcConditionalFormatting (..),
    newGaugeChartArcConditionalFormatting,
    gaugeChartArcConditionalFormatting_foregroundColor,

    -- * GaugeChartConditionalFormatting
    GaugeChartConditionalFormatting (..),
    newGaugeChartConditionalFormatting,
    gaugeChartConditionalFormatting_conditionalFormattingOptions,

    -- * GaugeChartConditionalFormattingOption
    GaugeChartConditionalFormattingOption (..),
    newGaugeChartConditionalFormattingOption,
    gaugeChartConditionalFormattingOption_arc,
    gaugeChartConditionalFormattingOption_primaryValue,

    -- * GaugeChartConfiguration
    GaugeChartConfiguration (..),
    newGaugeChartConfiguration,
    gaugeChartConfiguration_dataLabels,
    gaugeChartConfiguration_fieldWells,
    gaugeChartConfiguration_gaugeChartOptions,
    gaugeChartConfiguration_tooltipOptions,
    gaugeChartConfiguration_visualPalette,

    -- * GaugeChartFieldWells
    GaugeChartFieldWells (..),
    newGaugeChartFieldWells,
    gaugeChartFieldWells_targetValues,
    gaugeChartFieldWells_values,

    -- * GaugeChartOptions
    GaugeChartOptions (..),
    newGaugeChartOptions,
    gaugeChartOptions_arc,
    gaugeChartOptions_arcAxis,
    gaugeChartOptions_comparison,
    gaugeChartOptions_primaryValueDisplayType,
    gaugeChartOptions_primaryValueFontConfiguration,

    -- * GaugeChartPrimaryValueConditionalFormatting
    GaugeChartPrimaryValueConditionalFormatting (..),
    newGaugeChartPrimaryValueConditionalFormatting,
    gaugeChartPrimaryValueConditionalFormatting_icon,
    gaugeChartPrimaryValueConditionalFormatting_textColor,

    -- * GaugeChartVisual
    GaugeChartVisual (..),
    newGaugeChartVisual,
    gaugeChartVisual_actions,
    gaugeChartVisual_chartConfiguration,
    gaugeChartVisual_conditionalFormatting,
    gaugeChartVisual_subtitle,
    gaugeChartVisual_title,
    gaugeChartVisual_visualId,

    -- * GeoSpatialColumnGroup
    GeoSpatialColumnGroup (..),
    newGeoSpatialColumnGroup,
    geoSpatialColumnGroup_countryCode,
    geoSpatialColumnGroup_name,
    geoSpatialColumnGroup_columns,

    -- * GeospatialCoordinateBounds
    GeospatialCoordinateBounds (..),
    newGeospatialCoordinateBounds,
    geospatialCoordinateBounds_north,
    geospatialCoordinateBounds_south,
    geospatialCoordinateBounds_west,
    geospatialCoordinateBounds_east,

    -- * GeospatialMapAggregatedFieldWells
    GeospatialMapAggregatedFieldWells (..),
    newGeospatialMapAggregatedFieldWells,
    geospatialMapAggregatedFieldWells_colors,
    geospatialMapAggregatedFieldWells_geospatial,
    geospatialMapAggregatedFieldWells_values,

    -- * GeospatialMapConfiguration
    GeospatialMapConfiguration (..),
    newGeospatialMapConfiguration,
    geospatialMapConfiguration_fieldWells,
    geospatialMapConfiguration_legend,
    geospatialMapConfiguration_mapStyleOptions,
    geospatialMapConfiguration_pointStyleOptions,
    geospatialMapConfiguration_tooltip,
    geospatialMapConfiguration_visualPalette,
    geospatialMapConfiguration_windowOptions,

    -- * GeospatialMapFieldWells
    GeospatialMapFieldWells (..),
    newGeospatialMapFieldWells,
    geospatialMapFieldWells_geospatialMapAggregatedFieldWells,

    -- * GeospatialMapStyleOptions
    GeospatialMapStyleOptions (..),
    newGeospatialMapStyleOptions,
    geospatialMapStyleOptions_baseMapStyle,

    -- * GeospatialMapVisual
    GeospatialMapVisual (..),
    newGeospatialMapVisual,
    geospatialMapVisual_actions,
    geospatialMapVisual_chartConfiguration,
    geospatialMapVisual_columnHierarchies,
    geospatialMapVisual_subtitle,
    geospatialMapVisual_title,
    geospatialMapVisual_visualId,

    -- * GeospatialPointStyleOptions
    GeospatialPointStyleOptions (..),
    newGeospatialPointStyleOptions,
    geospatialPointStyleOptions_clusterMarkerConfiguration,
    geospatialPointStyleOptions_selectedPointStyle,

    -- * GeospatialWindowOptions
    GeospatialWindowOptions (..),
    newGeospatialWindowOptions,
    geospatialWindowOptions_bounds,
    geospatialWindowOptions_mapZoomMode,

    -- * GlobalTableBorderOptions
    GlobalTableBorderOptions (..),
    newGlobalTableBorderOptions,
    globalTableBorderOptions_sideSpecificBorder,
    globalTableBorderOptions_uniformBorder,

    -- * GradientColor
    GradientColor (..),
    newGradientColor,
    gradientColor_stops,

    -- * GradientStop
    GradientStop (..),
    newGradientStop,
    gradientStop_color,
    gradientStop_dataValue,
    gradientStop_gradientOffset,

    -- * GridLayoutCanvasSizeOptions
    GridLayoutCanvasSizeOptions (..),
    newGridLayoutCanvasSizeOptions,
    gridLayoutCanvasSizeOptions_screenCanvasSizeOptions,

    -- * GridLayoutConfiguration
    GridLayoutConfiguration (..),
    newGridLayoutConfiguration,
    gridLayoutConfiguration_canvasSizeOptions,
    gridLayoutConfiguration_elements,

    -- * GridLayoutElement
    GridLayoutElement (..),
    newGridLayoutElement,
    gridLayoutElement_columnIndex,
    gridLayoutElement_rowIndex,
    gridLayoutElement_elementId,
    gridLayoutElement_elementType,
    gridLayoutElement_columnSpan,
    gridLayoutElement_rowSpan,

    -- * GridLayoutScreenCanvasSizeOptions
    GridLayoutScreenCanvasSizeOptions (..),
    newGridLayoutScreenCanvasSizeOptions,
    gridLayoutScreenCanvasSizeOptions_optimizedViewPortWidth,
    gridLayoutScreenCanvasSizeOptions_resizeOption,

    -- * Group
    Group (..),
    newGroup,
    group_arn,
    group_description,
    group_groupName,
    group_principalId,

    -- * GroupMember
    GroupMember (..),
    newGroupMember,
    groupMember_arn,
    groupMember_memberName,

    -- * GroupSearchFilter
    GroupSearchFilter (..),
    newGroupSearchFilter,
    groupSearchFilter_operator,
    groupSearchFilter_name,
    groupSearchFilter_value,

    -- * GrowthRateComputation
    GrowthRateComputation (..),
    newGrowthRateComputation,
    growthRateComputation_name,
    growthRateComputation_periodSize,
    growthRateComputation_value,
    growthRateComputation_computationId,
    growthRateComputation_time,

    -- * GutterStyle
    GutterStyle (..),
    newGutterStyle,
    gutterStyle_show,

    -- * HeaderFooterSectionConfiguration
    HeaderFooterSectionConfiguration (..),
    newHeaderFooterSectionConfiguration,
    headerFooterSectionConfiguration_style,
    headerFooterSectionConfiguration_sectionId,
    headerFooterSectionConfiguration_layout,

    -- * HeatMapAggregatedFieldWells
    HeatMapAggregatedFieldWells (..),
    newHeatMapAggregatedFieldWells,
    heatMapAggregatedFieldWells_columns,
    heatMapAggregatedFieldWells_rows,
    heatMapAggregatedFieldWells_values,

    -- * HeatMapConfiguration
    HeatMapConfiguration (..),
    newHeatMapConfiguration,
    heatMapConfiguration_colorScale,
    heatMapConfiguration_columnLabelOptions,
    heatMapConfiguration_dataLabels,
    heatMapConfiguration_fieldWells,
    heatMapConfiguration_legend,
    heatMapConfiguration_rowLabelOptions,
    heatMapConfiguration_sortConfiguration,
    heatMapConfiguration_tooltip,

    -- * HeatMapFieldWells
    HeatMapFieldWells (..),
    newHeatMapFieldWells,
    heatMapFieldWells_heatMapAggregatedFieldWells,

    -- * HeatMapSortConfiguration
    HeatMapSortConfiguration (..),
    newHeatMapSortConfiguration,
    heatMapSortConfiguration_heatMapColumnItemsLimitConfiguration,
    heatMapSortConfiguration_heatMapColumnSort,
    heatMapSortConfiguration_heatMapRowItemsLimitConfiguration,
    heatMapSortConfiguration_heatMapRowSort,

    -- * HeatMapVisual
    HeatMapVisual (..),
    newHeatMapVisual,
    heatMapVisual_actions,
    heatMapVisual_chartConfiguration,
    heatMapVisual_columnHierarchies,
    heatMapVisual_subtitle,
    heatMapVisual_title,
    heatMapVisual_visualId,

    -- * HistogramAggregatedFieldWells
    HistogramAggregatedFieldWells (..),
    newHistogramAggregatedFieldWells,
    histogramAggregatedFieldWells_values,

    -- * HistogramBinOptions
    HistogramBinOptions (..),
    newHistogramBinOptions,
    histogramBinOptions_binCount,
    histogramBinOptions_binWidth,
    histogramBinOptions_selectedBinType,
    histogramBinOptions_startValue,

    -- * HistogramConfiguration
    HistogramConfiguration (..),
    newHistogramConfiguration,
    histogramConfiguration_binOptions,
    histogramConfiguration_dataLabels,
    histogramConfiguration_fieldWells,
    histogramConfiguration_tooltip,
    histogramConfiguration_visualPalette,
    histogramConfiguration_xAxisDisplayOptions,
    histogramConfiguration_xAxisLabelOptions,
    histogramConfiguration_yAxisDisplayOptions,

    -- * HistogramFieldWells
    HistogramFieldWells (..),
    newHistogramFieldWells,
    histogramFieldWells_histogramAggregatedFieldWells,

    -- * HistogramVisual
    HistogramVisual (..),
    newHistogramVisual,
    histogramVisual_actions,
    histogramVisual_chartConfiguration,
    histogramVisual_subtitle,
    histogramVisual_title,
    histogramVisual_visualId,

    -- * IAMPolicyAssignment
    IAMPolicyAssignment (..),
    newIAMPolicyAssignment,
    iAMPolicyAssignment_assignmentId,
    iAMPolicyAssignment_assignmentName,
    iAMPolicyAssignment_assignmentStatus,
    iAMPolicyAssignment_awsAccountId,
    iAMPolicyAssignment_identities,
    iAMPolicyAssignment_policyArn,

    -- * IAMPolicyAssignmentSummary
    IAMPolicyAssignmentSummary (..),
    newIAMPolicyAssignmentSummary,
    iAMPolicyAssignmentSummary_assignmentName,
    iAMPolicyAssignmentSummary_assignmentStatus,

    -- * Ingestion
    Ingestion (..),
    newIngestion,
    ingestion_errorInfo,
    ingestion_ingestionId,
    ingestion_ingestionSizeInBytes,
    ingestion_ingestionTimeInSeconds,
    ingestion_queueInfo,
    ingestion_requestSource,
    ingestion_requestType,
    ingestion_rowInfo,
    ingestion_arn,
    ingestion_ingestionStatus,
    ingestion_createdTime,

    -- * InputColumn
    InputColumn (..),
    newInputColumn,
    inputColumn_name,
    inputColumn_type,

    -- * InsightConfiguration
    InsightConfiguration (..),
    newInsightConfiguration,
    insightConfiguration_computations,
    insightConfiguration_customNarrative,

    -- * InsightVisual
    InsightVisual (..),
    newInsightVisual,
    insightVisual_actions,
    insightVisual_insightConfiguration,
    insightVisual_subtitle,
    insightVisual_title,
    insightVisual_visualId,
    insightVisual_dataSetIdentifier,

    -- * IntegerDefaultValues
    IntegerDefaultValues (..),
    newIntegerDefaultValues,
    integerDefaultValues_dynamicValue,
    integerDefaultValues_staticValues,

    -- * IntegerParameter
    IntegerParameter (..),
    newIntegerParameter,
    integerParameter_name,
    integerParameter_values,

    -- * IntegerParameterDeclaration
    IntegerParameterDeclaration (..),
    newIntegerParameterDeclaration,
    integerParameterDeclaration_defaultValues,
    integerParameterDeclaration_valueWhenUnset,
    integerParameterDeclaration_parameterValueType,
    integerParameterDeclaration_name,

    -- * IntegerValueWhenUnsetConfiguration
    IntegerValueWhenUnsetConfiguration (..),
    newIntegerValueWhenUnsetConfiguration,
    integerValueWhenUnsetConfiguration_customValue,
    integerValueWhenUnsetConfiguration_valueWhenUnsetOption,

    -- * ItemsLimitConfiguration
    ItemsLimitConfiguration (..),
    newItemsLimitConfiguration,
    itemsLimitConfiguration_itemsLimit,
    itemsLimitConfiguration_otherCategories,

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

    -- * KPIConditionalFormatting
    KPIConditionalFormatting (..),
    newKPIConditionalFormatting,
    kPIConditionalFormatting_conditionalFormattingOptions,

    -- * KPIConditionalFormattingOption
    KPIConditionalFormattingOption (..),
    newKPIConditionalFormattingOption,
    kPIConditionalFormattingOption_primaryValue,
    kPIConditionalFormattingOption_progressBar,

    -- * KPIConfiguration
    KPIConfiguration (..),
    newKPIConfiguration,
    kPIConfiguration_fieldWells,
    kPIConfiguration_kPIOptions,
    kPIConfiguration_sortConfiguration,

    -- * KPIFieldWells
    KPIFieldWells (..),
    newKPIFieldWells,
    kPIFieldWells_targetValues,
    kPIFieldWells_trendGroups,
    kPIFieldWells_values,

    -- * KPIOptions
    KPIOptions (..),
    newKPIOptions,
    kPIOptions_comparison,
    kPIOptions_primaryValueDisplayType,
    kPIOptions_primaryValueFontConfiguration,
    kPIOptions_progressBar,
    kPIOptions_secondaryValue,
    kPIOptions_secondaryValueFontConfiguration,
    kPIOptions_trendArrows,

    -- * KPIPrimaryValueConditionalFormatting
    KPIPrimaryValueConditionalFormatting (..),
    newKPIPrimaryValueConditionalFormatting,
    kPIPrimaryValueConditionalFormatting_icon,
    kPIPrimaryValueConditionalFormatting_textColor,

    -- * KPIProgressBarConditionalFormatting
    KPIProgressBarConditionalFormatting (..),
    newKPIProgressBarConditionalFormatting,
    kPIProgressBarConditionalFormatting_foregroundColor,

    -- * KPISortConfiguration
    KPISortConfiguration (..),
    newKPISortConfiguration,
    kPISortConfiguration_trendGroupSort,

    -- * KPIVisual
    KPIVisual (..),
    newKPIVisual,
    kPIVisual_actions,
    kPIVisual_chartConfiguration,
    kPIVisual_columnHierarchies,
    kPIVisual_conditionalFormatting,
    kPIVisual_subtitle,
    kPIVisual_title,
    kPIVisual_visualId,

    -- * LabelOptions
    LabelOptions (..),
    newLabelOptions,
    labelOptions_customLabel,
    labelOptions_fontConfiguration,
    labelOptions_visibility,

    -- * Layout
    Layout (..),
    newLayout,
    layout_configuration,

    -- * LayoutConfiguration
    LayoutConfiguration (..),
    newLayoutConfiguration,
    layoutConfiguration_freeFormLayout,
    layoutConfiguration_gridLayout,
    layoutConfiguration_sectionBasedLayout,

    -- * LegendOptions
    LegendOptions (..),
    newLegendOptions,
    legendOptions_height,
    legendOptions_position,
    legendOptions_title,
    legendOptions_visibility,
    legendOptions_width,

    -- * LineChartAggregatedFieldWells
    LineChartAggregatedFieldWells (..),
    newLineChartAggregatedFieldWells,
    lineChartAggregatedFieldWells_category,
    lineChartAggregatedFieldWells_colors,
    lineChartAggregatedFieldWells_smallMultiples,
    lineChartAggregatedFieldWells_values,

    -- * LineChartConfiguration
    LineChartConfiguration (..),
    newLineChartConfiguration,
    lineChartConfiguration_contributionAnalysisDefaults,
    lineChartConfiguration_dataLabels,
    lineChartConfiguration_defaultSeriesSettings,
    lineChartConfiguration_fieldWells,
    lineChartConfiguration_forecastConfigurations,
    lineChartConfiguration_legend,
    lineChartConfiguration_primaryYAxisDisplayOptions,
    lineChartConfiguration_primaryYAxisLabelOptions,
    lineChartConfiguration_referenceLines,
    lineChartConfiguration_secondaryYAxisDisplayOptions,
    lineChartConfiguration_secondaryYAxisLabelOptions,
    lineChartConfiguration_series,
    lineChartConfiguration_smallMultiplesOptions,
    lineChartConfiguration_sortConfiguration,
    lineChartConfiguration_tooltip,
    lineChartConfiguration_type,
    lineChartConfiguration_visualPalette,
    lineChartConfiguration_xAxisDisplayOptions,
    lineChartConfiguration_xAxisLabelOptions,

    -- * LineChartDefaultSeriesSettings
    LineChartDefaultSeriesSettings (..),
    newLineChartDefaultSeriesSettings,
    lineChartDefaultSeriesSettings_axisBinding,
    lineChartDefaultSeriesSettings_lineStyleSettings,
    lineChartDefaultSeriesSettings_markerStyleSettings,

    -- * LineChartFieldWells
    LineChartFieldWells (..),
    newLineChartFieldWells,
    lineChartFieldWells_lineChartAggregatedFieldWells,

    -- * LineChartLineStyleSettings
    LineChartLineStyleSettings (..),
    newLineChartLineStyleSettings,
    lineChartLineStyleSettings_lineInterpolation,
    lineChartLineStyleSettings_lineStyle,
    lineChartLineStyleSettings_lineVisibility,
    lineChartLineStyleSettings_lineWidth,

    -- * LineChartMarkerStyleSettings
    LineChartMarkerStyleSettings (..),
    newLineChartMarkerStyleSettings,
    lineChartMarkerStyleSettings_markerColor,
    lineChartMarkerStyleSettings_markerShape,
    lineChartMarkerStyleSettings_markerSize,
    lineChartMarkerStyleSettings_markerVisibility,

    -- * LineChartSeriesSettings
    LineChartSeriesSettings (..),
    newLineChartSeriesSettings,
    lineChartSeriesSettings_lineStyleSettings,
    lineChartSeriesSettings_markerStyleSettings,

    -- * LineChartSortConfiguration
    LineChartSortConfiguration (..),
    newLineChartSortConfiguration,
    lineChartSortConfiguration_categoryItemsLimitConfiguration,
    lineChartSortConfiguration_categorySort,
    lineChartSortConfiguration_colorItemsLimitConfiguration,
    lineChartSortConfiguration_smallMultiplesLimitConfiguration,
    lineChartSortConfiguration_smallMultiplesSort,

    -- * LineChartVisual
    LineChartVisual (..),
    newLineChartVisual,
    lineChartVisual_actions,
    lineChartVisual_chartConfiguration,
    lineChartVisual_columnHierarchies,
    lineChartVisual_subtitle,
    lineChartVisual_title,
    lineChartVisual_visualId,

    -- * LineSeriesAxisDisplayOptions
    LineSeriesAxisDisplayOptions (..),
    newLineSeriesAxisDisplayOptions,
    lineSeriesAxisDisplayOptions_axisOptions,
    lineSeriesAxisDisplayOptions_missingDataConfigurations,

    -- * LinkSharingConfiguration
    LinkSharingConfiguration (..),
    newLinkSharingConfiguration,
    linkSharingConfiguration_permissions,

    -- * ListControlDisplayOptions
    ListControlDisplayOptions (..),
    newListControlDisplayOptions,
    listControlDisplayOptions_searchOptions,
    listControlDisplayOptions_selectAllOptions,
    listControlDisplayOptions_titleOptions,

    -- * ListControlSearchOptions
    ListControlSearchOptions (..),
    newListControlSearchOptions,
    listControlSearchOptions_visibility,

    -- * ListControlSelectAllOptions
    ListControlSelectAllOptions (..),
    newListControlSelectAllOptions,
    listControlSelectAllOptions_visibility,

    -- * LoadingAnimation
    LoadingAnimation (..),
    newLoadingAnimation,
    loadingAnimation_visibility,

    -- * LocalNavigationConfiguration
    LocalNavigationConfiguration (..),
    newLocalNavigationConfiguration,
    localNavigationConfiguration_targetSheetId,

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

    -- * LongFormatText
    LongFormatText (..),
    newLongFormatText,
    longFormatText_plainText,
    longFormatText_richText,

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

    -- * MaximumLabelType
    MaximumLabelType (..),
    newMaximumLabelType,
    maximumLabelType_visibility,

    -- * MaximumMinimumComputation
    MaximumMinimumComputation (..),
    newMaximumMinimumComputation,
    maximumMinimumComputation_name,
    maximumMinimumComputation_value,
    maximumMinimumComputation_computationId,
    maximumMinimumComputation_time,
    maximumMinimumComputation_type,

    -- * MeasureField
    MeasureField (..),
    newMeasureField,
    measureField_calculatedMeasureField,
    measureField_categoricalMeasureField,
    measureField_dateMeasureField,
    measureField_numericalMeasureField,

    -- * MemberIdArnPair
    MemberIdArnPair (..),
    newMemberIdArnPair,
    memberIdArnPair_memberArn,
    memberIdArnPair_memberId,

    -- * MetricComparisonComputation
    MetricComparisonComputation (..),
    newMetricComparisonComputation,
    metricComparisonComputation_name,
    metricComparisonComputation_computationId,
    metricComparisonComputation_time,
    metricComparisonComputation_fromValue,
    metricComparisonComputation_targetValue,

    -- * MinimumLabelType
    MinimumLabelType (..),
    newMinimumLabelType,
    minimumLabelType_visibility,

    -- * MissingDataConfiguration
    MissingDataConfiguration (..),
    newMissingDataConfiguration,
    missingDataConfiguration_treatmentOption,

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
    namespaceInfoV2_arn,
    namespaceInfoV2_capacityRegion,
    namespaceInfoV2_creationStatus,
    namespaceInfoV2_identityStore,
    namespaceInfoV2_name,
    namespaceInfoV2_namespaceError,

    -- * NegativeValueConfiguration
    NegativeValueConfiguration (..),
    newNegativeValueConfiguration,
    negativeValueConfiguration_displayMode,

    -- * NullValueFormatConfiguration
    NullValueFormatConfiguration (..),
    newNullValueFormatConfiguration,
    nullValueFormatConfiguration_nullString,

    -- * NumberDisplayFormatConfiguration
    NumberDisplayFormatConfiguration (..),
    newNumberDisplayFormatConfiguration,
    numberDisplayFormatConfiguration_decimalPlacesConfiguration,
    numberDisplayFormatConfiguration_negativeValueConfiguration,
    numberDisplayFormatConfiguration_nullValueFormatConfiguration,
    numberDisplayFormatConfiguration_numberScale,
    numberDisplayFormatConfiguration_prefix,
    numberDisplayFormatConfiguration_separatorConfiguration,
    numberDisplayFormatConfiguration_suffix,

    -- * NumberFormatConfiguration
    NumberFormatConfiguration (..),
    newNumberFormatConfiguration,
    numberFormatConfiguration_formatConfiguration,

    -- * NumericAxisOptions
    NumericAxisOptions (..),
    newNumericAxisOptions,
    numericAxisOptions_range,
    numericAxisOptions_scale,

    -- * NumericEqualityDrillDownFilter
    NumericEqualityDrillDownFilter (..),
    newNumericEqualityDrillDownFilter,
    numericEqualityDrillDownFilter_column,
    numericEqualityDrillDownFilter_value,

    -- * NumericEqualityFilter
    NumericEqualityFilter (..),
    newNumericEqualityFilter,
    numericEqualityFilter_aggregationFunction,
    numericEqualityFilter_parameterName,
    numericEqualityFilter_selectAllOptions,
    numericEqualityFilter_value,
    numericEqualityFilter_filterId,
    numericEqualityFilter_column,
    numericEqualityFilter_matchOperator,
    numericEqualityFilter_nullOption,

    -- * NumericFormatConfiguration
    NumericFormatConfiguration (..),
    newNumericFormatConfiguration,
    numericFormatConfiguration_currencyDisplayFormatConfiguration,
    numericFormatConfiguration_numberDisplayFormatConfiguration,
    numericFormatConfiguration_percentageDisplayFormatConfiguration,

    -- * NumericRangeFilter
    NumericRangeFilter (..),
    newNumericRangeFilter,
    numericRangeFilter_aggregationFunction,
    numericRangeFilter_includeMaximum,
    numericRangeFilter_includeMinimum,
    numericRangeFilter_rangeMaximum,
    numericRangeFilter_rangeMinimum,
    numericRangeFilter_selectAllOptions,
    numericRangeFilter_filterId,
    numericRangeFilter_column,
    numericRangeFilter_nullOption,

    -- * NumericRangeFilterValue
    NumericRangeFilterValue (..),
    newNumericRangeFilterValue,
    numericRangeFilterValue_parameter,
    numericRangeFilterValue_staticValue,

    -- * NumericSeparatorConfiguration
    NumericSeparatorConfiguration (..),
    newNumericSeparatorConfiguration,
    numericSeparatorConfiguration_decimalSeparator,
    numericSeparatorConfiguration_thousandsSeparator,

    -- * NumericalAggregationFunction
    NumericalAggregationFunction (..),
    newNumericalAggregationFunction,
    numericalAggregationFunction_percentileAggregation,
    numericalAggregationFunction_simpleNumericalAggregation,

    -- * NumericalDimensionField
    NumericalDimensionField (..),
    newNumericalDimensionField,
    numericalDimensionField_formatConfiguration,
    numericalDimensionField_hierarchyId,
    numericalDimensionField_fieldId,
    numericalDimensionField_column,

    -- * NumericalMeasureField
    NumericalMeasureField (..),
    newNumericalMeasureField,
    numericalMeasureField_aggregationFunction,
    numericalMeasureField_formatConfiguration,
    numericalMeasureField_fieldId,
    numericalMeasureField_column,

    -- * OracleParameters
    OracleParameters (..),
    newOracleParameters,
    oracleParameters_host,
    oracleParameters_port,
    oracleParameters_database,

    -- * OutputColumn
    OutputColumn (..),
    newOutputColumn,
    outputColumn_description,
    outputColumn_name,
    outputColumn_type,

    -- * PaginationConfiguration
    PaginationConfiguration (..),
    newPaginationConfiguration,
    paginationConfiguration_pageSize,
    paginationConfiguration_pageNumber,

    -- * PanelConfiguration
    PanelConfiguration (..),
    newPanelConfiguration,
    panelConfiguration_backgroundColor,
    panelConfiguration_backgroundVisibility,
    panelConfiguration_borderColor,
    panelConfiguration_borderStyle,
    panelConfiguration_borderThickness,
    panelConfiguration_borderVisibility,
    panelConfiguration_gutterSpacing,
    panelConfiguration_gutterVisibility,
    panelConfiguration_title,

    -- * PanelTitleOptions
    PanelTitleOptions (..),
    newPanelTitleOptions,
    panelTitleOptions_fontConfiguration,
    panelTitleOptions_horizontalTextAlignment,
    panelTitleOptions_visibility,

    -- * ParameterControl
    ParameterControl (..),
    newParameterControl,
    parameterControl_dateTimePicker,
    parameterControl_dropdown,
    parameterControl_list,
    parameterControl_slider,
    parameterControl_textArea,
    parameterControl_textField,

    -- * ParameterDateTimePickerControl
    ParameterDateTimePickerControl (..),
    newParameterDateTimePickerControl,
    parameterDateTimePickerControl_displayOptions,
    parameterDateTimePickerControl_parameterControlId,
    parameterDateTimePickerControl_title,
    parameterDateTimePickerControl_sourceParameterName,

    -- * ParameterDeclaration
    ParameterDeclaration (..),
    newParameterDeclaration,
    parameterDeclaration_dateTimeParameterDeclaration,
    parameterDeclaration_decimalParameterDeclaration,
    parameterDeclaration_integerParameterDeclaration,
    parameterDeclaration_stringParameterDeclaration,

    -- * ParameterDropDownControl
    ParameterDropDownControl (..),
    newParameterDropDownControl,
    parameterDropDownControl_cascadingControlConfiguration,
    parameterDropDownControl_displayOptions,
    parameterDropDownControl_selectableValues,
    parameterDropDownControl_type,
    parameterDropDownControl_parameterControlId,
    parameterDropDownControl_title,
    parameterDropDownControl_sourceParameterName,

    -- * ParameterListControl
    ParameterListControl (..),
    newParameterListControl,
    parameterListControl_cascadingControlConfiguration,
    parameterListControl_displayOptions,
    parameterListControl_selectableValues,
    parameterListControl_type,
    parameterListControl_parameterControlId,
    parameterListControl_title,
    parameterListControl_sourceParameterName,

    -- * ParameterSelectableValues
    ParameterSelectableValues (..),
    newParameterSelectableValues,
    parameterSelectableValues_linkToDataSetColumn,
    parameterSelectableValues_values,

    -- * ParameterSliderControl
    ParameterSliderControl (..),
    newParameterSliderControl,
    parameterSliderControl_displayOptions,
    parameterSliderControl_parameterControlId,
    parameterSliderControl_title,
    parameterSliderControl_sourceParameterName,
    parameterSliderControl_maximumValue,
    parameterSliderControl_minimumValue,
    parameterSliderControl_stepSize,

    -- * ParameterTextAreaControl
    ParameterTextAreaControl (..),
    newParameterTextAreaControl,
    parameterTextAreaControl_delimiter,
    parameterTextAreaControl_displayOptions,
    parameterTextAreaControl_parameterControlId,
    parameterTextAreaControl_title,
    parameterTextAreaControl_sourceParameterName,

    -- * ParameterTextFieldControl
    ParameterTextFieldControl (..),
    newParameterTextFieldControl,
    parameterTextFieldControl_displayOptions,
    parameterTextFieldControl_parameterControlId,
    parameterTextFieldControl_title,
    parameterTextFieldControl_sourceParameterName,

    -- * Parameters
    Parameters (..),
    newParameters,
    parameters_dateTimeParameters,
    parameters_decimalParameters,
    parameters_integerParameters,
    parameters_stringParameters,

    -- * PercentVisibleRange
    PercentVisibleRange (..),
    newPercentVisibleRange,
    percentVisibleRange_from,
    percentVisibleRange_to,

    -- * PercentageDisplayFormatConfiguration
    PercentageDisplayFormatConfiguration (..),
    newPercentageDisplayFormatConfiguration,
    percentageDisplayFormatConfiguration_decimalPlacesConfiguration,
    percentageDisplayFormatConfiguration_negativeValueConfiguration,
    percentageDisplayFormatConfiguration_nullValueFormatConfiguration,
    percentageDisplayFormatConfiguration_prefix,
    percentageDisplayFormatConfiguration_separatorConfiguration,
    percentageDisplayFormatConfiguration_suffix,

    -- * PercentileAggregation
    PercentileAggregation (..),
    newPercentileAggregation,
    percentileAggregation_percentileValue,

    -- * PeriodOverPeriodComputation
    PeriodOverPeriodComputation (..),
    newPeriodOverPeriodComputation,
    periodOverPeriodComputation_name,
    periodOverPeriodComputation_value,
    periodOverPeriodComputation_computationId,
    periodOverPeriodComputation_time,

    -- * PeriodToDateComputation
    PeriodToDateComputation (..),
    newPeriodToDateComputation,
    periodToDateComputation_name,
    periodToDateComputation_periodTimeGranularity,
    periodToDateComputation_value,
    periodToDateComputation_computationId,
    periodToDateComputation_time,

    -- * PhysicalTable
    PhysicalTable (..),
    newPhysicalTable,
    physicalTable_customSql,
    physicalTable_relationalTable,
    physicalTable_s3Source,

    -- * PieChartAggregatedFieldWells
    PieChartAggregatedFieldWells (..),
    newPieChartAggregatedFieldWells,
    pieChartAggregatedFieldWells_category,
    pieChartAggregatedFieldWells_smallMultiples,
    pieChartAggregatedFieldWells_values,

    -- * PieChartConfiguration
    PieChartConfiguration (..),
    newPieChartConfiguration,
    pieChartConfiguration_categoryLabelOptions,
    pieChartConfiguration_contributionAnalysisDefaults,
    pieChartConfiguration_dataLabels,
    pieChartConfiguration_donutOptions,
    pieChartConfiguration_fieldWells,
    pieChartConfiguration_legend,
    pieChartConfiguration_smallMultiplesOptions,
    pieChartConfiguration_sortConfiguration,
    pieChartConfiguration_tooltip,
    pieChartConfiguration_valueLabelOptions,
    pieChartConfiguration_visualPalette,

    -- * PieChartFieldWells
    PieChartFieldWells (..),
    newPieChartFieldWells,
    pieChartFieldWells_pieChartAggregatedFieldWells,

    -- * PieChartSortConfiguration
    PieChartSortConfiguration (..),
    newPieChartSortConfiguration,
    pieChartSortConfiguration_categoryItemsLimit,
    pieChartSortConfiguration_categorySort,
    pieChartSortConfiguration_smallMultiplesLimitConfiguration,
    pieChartSortConfiguration_smallMultiplesSort,

    -- * PieChartVisual
    PieChartVisual (..),
    newPieChartVisual,
    pieChartVisual_actions,
    pieChartVisual_chartConfiguration,
    pieChartVisual_columnHierarchies,
    pieChartVisual_subtitle,
    pieChartVisual_title,
    pieChartVisual_visualId,

    -- * PivotFieldSortOptions
    PivotFieldSortOptions (..),
    newPivotFieldSortOptions,
    pivotFieldSortOptions_fieldId,
    pivotFieldSortOptions_sortBy,

    -- * PivotTableAggregatedFieldWells
    PivotTableAggregatedFieldWells (..),
    newPivotTableAggregatedFieldWells,
    pivotTableAggregatedFieldWells_columns,
    pivotTableAggregatedFieldWells_rows,
    pivotTableAggregatedFieldWells_values,

    -- * PivotTableCellConditionalFormatting
    PivotTableCellConditionalFormatting (..),
    newPivotTableCellConditionalFormatting,
    pivotTableCellConditionalFormatting_scope,
    pivotTableCellConditionalFormatting_textFormat,
    pivotTableCellConditionalFormatting_fieldId,

    -- * PivotTableConditionalFormatting
    PivotTableConditionalFormatting (..),
    newPivotTableConditionalFormatting,
    pivotTableConditionalFormatting_conditionalFormattingOptions,

    -- * PivotTableConditionalFormattingOption
    PivotTableConditionalFormattingOption (..),
    newPivotTableConditionalFormattingOption,
    pivotTableConditionalFormattingOption_cell,

    -- * PivotTableConditionalFormattingScope
    PivotTableConditionalFormattingScope (..),
    newPivotTableConditionalFormattingScope,
    pivotTableConditionalFormattingScope_role,

    -- * PivotTableConfiguration
    PivotTableConfiguration (..),
    newPivotTableConfiguration,
    pivotTableConfiguration_fieldOptions,
    pivotTableConfiguration_fieldWells,
    pivotTableConfiguration_paginatedReportOptions,
    pivotTableConfiguration_sortConfiguration,
    pivotTableConfiguration_tableOptions,
    pivotTableConfiguration_totalOptions,

    -- * PivotTableDataPathOption
    PivotTableDataPathOption (..),
    newPivotTableDataPathOption,
    pivotTableDataPathOption_width,
    pivotTableDataPathOption_dataPathList,

    -- * PivotTableFieldOption
    PivotTableFieldOption (..),
    newPivotTableFieldOption,
    pivotTableFieldOption_customLabel,
    pivotTableFieldOption_visibility,
    pivotTableFieldOption_fieldId,

    -- * PivotTableFieldOptions
    PivotTableFieldOptions (..),
    newPivotTableFieldOptions,
    pivotTableFieldOptions_dataPathOptions,
    pivotTableFieldOptions_selectedFieldOptions,

    -- * PivotTableFieldSubtotalOptions
    PivotTableFieldSubtotalOptions (..),
    newPivotTableFieldSubtotalOptions,
    pivotTableFieldSubtotalOptions_fieldId,

    -- * PivotTableFieldWells
    PivotTableFieldWells (..),
    newPivotTableFieldWells,
    pivotTableFieldWells_pivotTableAggregatedFieldWells,

    -- * PivotTableOptions
    PivotTableOptions (..),
    newPivotTableOptions,
    pivotTableOptions_cellStyle,
    pivotTableOptions_columnHeaderStyle,
    pivotTableOptions_columnNamesVisibility,
    pivotTableOptions_metricPlacement,
    pivotTableOptions_rowAlternateColorOptions,
    pivotTableOptions_rowFieldNamesStyle,
    pivotTableOptions_rowHeaderStyle,
    pivotTableOptions_singleMetricVisibility,
    pivotTableOptions_toggleButtonsVisibility,

    -- * PivotTablePaginatedReportOptions
    PivotTablePaginatedReportOptions (..),
    newPivotTablePaginatedReportOptions,
    pivotTablePaginatedReportOptions_overflowColumnHeaderVisibility,
    pivotTablePaginatedReportOptions_verticalOverflowVisibility,

    -- * PivotTableSortBy
    PivotTableSortBy (..),
    newPivotTableSortBy,
    pivotTableSortBy_column,
    pivotTableSortBy_dataPath,
    pivotTableSortBy_field,

    -- * PivotTableSortConfiguration
    PivotTableSortConfiguration (..),
    newPivotTableSortConfiguration,
    pivotTableSortConfiguration_fieldSortOptions,

    -- * PivotTableTotalOptions
    PivotTableTotalOptions (..),
    newPivotTableTotalOptions,
    pivotTableTotalOptions_columnSubtotalOptions,
    pivotTableTotalOptions_columnTotalOptions,
    pivotTableTotalOptions_rowSubtotalOptions,
    pivotTableTotalOptions_rowTotalOptions,

    -- * PivotTableVisual
    PivotTableVisual (..),
    newPivotTableVisual,
    pivotTableVisual_actions,
    pivotTableVisual_chartConfiguration,
    pivotTableVisual_conditionalFormatting,
    pivotTableVisual_subtitle,
    pivotTableVisual_title,
    pivotTableVisual_visualId,

    -- * PivotTotalOptions
    PivotTotalOptions (..),
    newPivotTotalOptions,
    pivotTotalOptions_customLabel,
    pivotTotalOptions_metricHeaderCellStyle,
    pivotTotalOptions_placement,
    pivotTotalOptions_scrollStatus,
    pivotTotalOptions_totalCellStyle,
    pivotTotalOptions_totalsVisibility,
    pivotTotalOptions_valueCellStyle,

    -- * PostgreSqlParameters
    PostgreSqlParameters (..),
    newPostgreSqlParameters,
    postgreSqlParameters_host,
    postgreSqlParameters_port,
    postgreSqlParameters_database,

    -- * PredefinedHierarchy
    PredefinedHierarchy (..),
    newPredefinedHierarchy,
    predefinedHierarchy_drillDownFilters,
    predefinedHierarchy_hierarchyId,
    predefinedHierarchy_columns,

    -- * PrestoParameters
    PrestoParameters (..),
    newPrestoParameters,
    prestoParameters_host,
    prestoParameters_port,
    prestoParameters_catalog,

    -- * ProgressBarOptions
    ProgressBarOptions (..),
    newProgressBarOptions,
    progressBarOptions_visibility,

    -- * ProjectOperation
    ProjectOperation (..),
    newProjectOperation,
    projectOperation_projectedColumns,

    -- * QueueInfo
    QueueInfo (..),
    newQueueInfo,
    queueInfo_waitingOnIngestion,
    queueInfo_queuedIngestion,

    -- * RangeEndsLabelType
    RangeEndsLabelType (..),
    newRangeEndsLabelType,
    rangeEndsLabelType_visibility,

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

    -- * ReferenceLine
    ReferenceLine (..),
    newReferenceLine,
    referenceLine_labelConfiguration,
    referenceLine_status,
    referenceLine_styleConfiguration,
    referenceLine_dataConfiguration,

    -- * ReferenceLineCustomLabelConfiguration
    ReferenceLineCustomLabelConfiguration (..),
    newReferenceLineCustomLabelConfiguration,
    referenceLineCustomLabelConfiguration_customLabel,

    -- * ReferenceLineDataConfiguration
    ReferenceLineDataConfiguration (..),
    newReferenceLineDataConfiguration,
    referenceLineDataConfiguration_axisBinding,
    referenceLineDataConfiguration_dynamicConfiguration,
    referenceLineDataConfiguration_staticConfiguration,

    -- * ReferenceLineDynamicDataConfiguration
    ReferenceLineDynamicDataConfiguration (..),
    newReferenceLineDynamicDataConfiguration,
    referenceLineDynamicDataConfiguration_column,
    referenceLineDynamicDataConfiguration_measureAggregationFunction,
    referenceLineDynamicDataConfiguration_calculation,

    -- * ReferenceLineLabelConfiguration
    ReferenceLineLabelConfiguration (..),
    newReferenceLineLabelConfiguration,
    referenceLineLabelConfiguration_customLabelConfiguration,
    referenceLineLabelConfiguration_fontColor,
    referenceLineLabelConfiguration_fontConfiguration,
    referenceLineLabelConfiguration_horizontalPosition,
    referenceLineLabelConfiguration_valueLabelConfiguration,
    referenceLineLabelConfiguration_verticalPosition,

    -- * ReferenceLineStaticDataConfiguration
    ReferenceLineStaticDataConfiguration (..),
    newReferenceLineStaticDataConfiguration,
    referenceLineStaticDataConfiguration_value,

    -- * ReferenceLineStyleConfiguration
    ReferenceLineStyleConfiguration (..),
    newReferenceLineStyleConfiguration,
    referenceLineStyleConfiguration_color,
    referenceLineStyleConfiguration_pattern,

    -- * ReferenceLineValueLabelConfiguration
    ReferenceLineValueLabelConfiguration (..),
    newReferenceLineValueLabelConfiguration,
    referenceLineValueLabelConfiguration_formatConfiguration,
    referenceLineValueLabelConfiguration_relativePosition,

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
    registeredUserEmbeddingExperienceConfiguration_dashboard,
    registeredUserEmbeddingExperienceConfiguration_dashboardVisual,
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

    -- * RelativeDateTimeControlDisplayOptions
    RelativeDateTimeControlDisplayOptions (..),
    newRelativeDateTimeControlDisplayOptions,
    relativeDateTimeControlDisplayOptions_dateTimeFormat,
    relativeDateTimeControlDisplayOptions_titleOptions,

    -- * RelativeDatesFilter
    RelativeDatesFilter (..),
    newRelativeDatesFilter,
    relativeDatesFilter_excludePeriodConfiguration,
    relativeDatesFilter_minimumGranularity,
    relativeDatesFilter_parameterName,
    relativeDatesFilter_relativeDateValue,
    relativeDatesFilter_filterId,
    relativeDatesFilter_column,
    relativeDatesFilter_anchorDateConfiguration,
    relativeDatesFilter_timeGranularity,
    relativeDatesFilter_relativeDateType,
    relativeDatesFilter_nullOption,

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

    -- * RollingDateConfiguration
    RollingDateConfiguration (..),
    newRollingDateConfiguration,
    rollingDateConfiguration_dataSetIdentifier,
    rollingDateConfiguration_expression,

    -- * RowAlternateColorOptions
    RowAlternateColorOptions (..),
    newRowAlternateColorOptions,
    rowAlternateColorOptions_rowAlternateColors,
    rowAlternateColorOptions_status,

    -- * RowInfo
    RowInfo (..),
    newRowInfo,
    rowInfo_rowsDropped,
    rowInfo_rowsIngested,
    rowInfo_totalRowsInDataset,

    -- * RowLevelPermissionDataSet
    RowLevelPermissionDataSet (..),
    newRowLevelPermissionDataSet,
    rowLevelPermissionDataSet_formatVersion,
    rowLevelPermissionDataSet_namespace,
    rowLevelPermissionDataSet_status,
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

    -- * SameSheetTargetVisualConfiguration
    SameSheetTargetVisualConfiguration (..),
    newSameSheetTargetVisualConfiguration,
    sameSheetTargetVisualConfiguration_targetVisualOptions,
    sameSheetTargetVisualConfiguration_targetVisuals,

    -- * SankeyDiagramAggregatedFieldWells
    SankeyDiagramAggregatedFieldWells (..),
    newSankeyDiagramAggregatedFieldWells,
    sankeyDiagramAggregatedFieldWells_destination,
    sankeyDiagramAggregatedFieldWells_source,
    sankeyDiagramAggregatedFieldWells_weight,

    -- * SankeyDiagramChartConfiguration
    SankeyDiagramChartConfiguration (..),
    newSankeyDiagramChartConfiguration,
    sankeyDiagramChartConfiguration_dataLabels,
    sankeyDiagramChartConfiguration_fieldWells,
    sankeyDiagramChartConfiguration_sortConfiguration,

    -- * SankeyDiagramFieldWells
    SankeyDiagramFieldWells (..),
    newSankeyDiagramFieldWells,
    sankeyDiagramFieldWells_sankeyDiagramAggregatedFieldWells,

    -- * SankeyDiagramSortConfiguration
    SankeyDiagramSortConfiguration (..),
    newSankeyDiagramSortConfiguration,
    sankeyDiagramSortConfiguration_destinationItemsLimit,
    sankeyDiagramSortConfiguration_sourceItemsLimit,
    sankeyDiagramSortConfiguration_weightSort,

    -- * SankeyDiagramVisual
    SankeyDiagramVisual (..),
    newSankeyDiagramVisual,
    sankeyDiagramVisual_actions,
    sankeyDiagramVisual_chartConfiguration,
    sankeyDiagramVisual_subtitle,
    sankeyDiagramVisual_title,
    sankeyDiagramVisual_visualId,

    -- * ScatterPlotCategoricallyAggregatedFieldWells
    ScatterPlotCategoricallyAggregatedFieldWells (..),
    newScatterPlotCategoricallyAggregatedFieldWells,
    scatterPlotCategoricallyAggregatedFieldWells_category,
    scatterPlotCategoricallyAggregatedFieldWells_size,
    scatterPlotCategoricallyAggregatedFieldWells_xAxis,
    scatterPlotCategoricallyAggregatedFieldWells_yAxis,

    -- * ScatterPlotConfiguration
    ScatterPlotConfiguration (..),
    newScatterPlotConfiguration,
    scatterPlotConfiguration_dataLabels,
    scatterPlotConfiguration_fieldWells,
    scatterPlotConfiguration_legend,
    scatterPlotConfiguration_tooltip,
    scatterPlotConfiguration_visualPalette,
    scatterPlotConfiguration_xAxisDisplayOptions,
    scatterPlotConfiguration_xAxisLabelOptions,
    scatterPlotConfiguration_yAxisDisplayOptions,
    scatterPlotConfiguration_yAxisLabelOptions,

    -- * ScatterPlotFieldWells
    ScatterPlotFieldWells (..),
    newScatterPlotFieldWells,
    scatterPlotFieldWells_scatterPlotCategoricallyAggregatedFieldWells,
    scatterPlotFieldWells_scatterPlotUnaggregatedFieldWells,

    -- * ScatterPlotUnaggregatedFieldWells
    ScatterPlotUnaggregatedFieldWells (..),
    newScatterPlotUnaggregatedFieldWells,
    scatterPlotUnaggregatedFieldWells_size,
    scatterPlotUnaggregatedFieldWells_xAxis,
    scatterPlotUnaggregatedFieldWells_yAxis,

    -- * ScatterPlotVisual
    ScatterPlotVisual (..),
    newScatterPlotVisual,
    scatterPlotVisual_actions,
    scatterPlotVisual_chartConfiguration,
    scatterPlotVisual_columnHierarchies,
    scatterPlotVisual_subtitle,
    scatterPlotVisual_title,
    scatterPlotVisual_visualId,

    -- * ScrollBarOptions
    ScrollBarOptions (..),
    newScrollBarOptions,
    scrollBarOptions_visibility,
    scrollBarOptions_visibleRange,

    -- * SecondaryValueOptions
    SecondaryValueOptions (..),
    newSecondaryValueOptions,
    secondaryValueOptions_visibility,

    -- * SectionAfterPageBreak
    SectionAfterPageBreak (..),
    newSectionAfterPageBreak,
    sectionAfterPageBreak_status,

    -- * SectionBasedLayoutCanvasSizeOptions
    SectionBasedLayoutCanvasSizeOptions (..),
    newSectionBasedLayoutCanvasSizeOptions,
    sectionBasedLayoutCanvasSizeOptions_paperCanvasSizeOptions,

    -- * SectionBasedLayoutConfiguration
    SectionBasedLayoutConfiguration (..),
    newSectionBasedLayoutConfiguration,
    sectionBasedLayoutConfiguration_headerSections,
    sectionBasedLayoutConfiguration_bodySections,
    sectionBasedLayoutConfiguration_footerSections,
    sectionBasedLayoutConfiguration_canvasSizeOptions,

    -- * SectionBasedLayoutPaperCanvasSizeOptions
    SectionBasedLayoutPaperCanvasSizeOptions (..),
    newSectionBasedLayoutPaperCanvasSizeOptions,
    sectionBasedLayoutPaperCanvasSizeOptions_paperMargin,
    sectionBasedLayoutPaperCanvasSizeOptions_paperOrientation,
    sectionBasedLayoutPaperCanvasSizeOptions_paperSize,

    -- * SectionLayoutConfiguration
    SectionLayoutConfiguration (..),
    newSectionLayoutConfiguration,
    sectionLayoutConfiguration_freeFormLayout,

    -- * SectionPageBreakConfiguration
    SectionPageBreakConfiguration (..),
    newSectionPageBreakConfiguration,
    sectionPageBreakConfiguration_after,

    -- * SectionStyle
    SectionStyle (..),
    newSectionStyle,
    sectionStyle_height,
    sectionStyle_padding,

    -- * SelectedSheetsFilterScopeConfiguration
    SelectedSheetsFilterScopeConfiguration (..),
    newSelectedSheetsFilterScopeConfiguration,
    selectedSheetsFilterScopeConfiguration_sheetVisualScopingConfigurations,

    -- * SeriesItem
    SeriesItem (..),
    newSeriesItem,
    seriesItem_dataFieldSeriesItem,
    seriesItem_fieldSeriesItem,

    -- * ServiceNowParameters
    ServiceNowParameters (..),
    newServiceNowParameters,
    serviceNowParameters_siteBaseUrl,

    -- * SessionTag
    SessionTag (..),
    newSessionTag,
    sessionTag_key,
    sessionTag_value,

    -- * SetParameterValueConfiguration
    SetParameterValueConfiguration (..),
    newSetParameterValueConfiguration,
    setParameterValueConfiguration_destinationParameterName,
    setParameterValueConfiguration_value,

    -- * ShapeConditionalFormat
    ShapeConditionalFormat (..),
    newShapeConditionalFormat,
    shapeConditionalFormat_backgroundColor,

    -- * Sheet
    Sheet (..),
    newSheet,
    sheet_name,
    sheet_sheetId,

    -- * SheetControlLayout
    SheetControlLayout (..),
    newSheetControlLayout,
    sheetControlLayout_configuration,

    -- * SheetControlLayoutConfiguration
    SheetControlLayoutConfiguration (..),
    newSheetControlLayoutConfiguration,
    sheetControlLayoutConfiguration_gridLayout,

    -- * SheetControlsOption
    SheetControlsOption (..),
    newSheetControlsOption,
    sheetControlsOption_visibilityState,

    -- * SheetDefinition
    SheetDefinition (..),
    newSheetDefinition,
    sheetDefinition_contentType,
    sheetDefinition_description,
    sheetDefinition_filterControls,
    sheetDefinition_layouts,
    sheetDefinition_name,
    sheetDefinition_parameterControls,
    sheetDefinition_sheetControlLayouts,
    sheetDefinition_textBoxes,
    sheetDefinition_title,
    sheetDefinition_visuals,
    sheetDefinition_sheetId,

    -- * SheetElementConfigurationOverrides
    SheetElementConfigurationOverrides (..),
    newSheetElementConfigurationOverrides,
    sheetElementConfigurationOverrides_visibility,

    -- * SheetElementRenderingRule
    SheetElementRenderingRule (..),
    newSheetElementRenderingRule,
    sheetElementRenderingRule_expression,
    sheetElementRenderingRule_configurationOverrides,

    -- * SheetStyle
    SheetStyle (..),
    newSheetStyle,
    sheetStyle_tile,
    sheetStyle_tileLayout,

    -- * SheetTextBox
    SheetTextBox (..),
    newSheetTextBox,
    sheetTextBox_content,
    sheetTextBox_sheetTextBoxId,

    -- * SheetVisualScopingConfiguration
    SheetVisualScopingConfiguration (..),
    newSheetVisualScopingConfiguration,
    sheetVisualScopingConfiguration_visualIds,
    sheetVisualScopingConfiguration_sheetId,
    sheetVisualScopingConfiguration_scope,

    -- * ShortFormatText
    ShortFormatText (..),
    newShortFormatText,
    shortFormatText_plainText,
    shortFormatText_richText,

    -- * SignupResponse
    SignupResponse (..),
    newSignupResponse,
    signupResponse_iAMUser,
    signupResponse_accountName,
    signupResponse_directoryType,
    signupResponse_userLoginName,

    -- * SimpleClusterMarker
    SimpleClusterMarker (..),
    newSimpleClusterMarker,
    simpleClusterMarker_color,

    -- * SliderControlDisplayOptions
    SliderControlDisplayOptions (..),
    newSliderControlDisplayOptions,
    sliderControlDisplayOptions_titleOptions,

    -- * SmallMultiplesOptions
    SmallMultiplesOptions (..),
    newSmallMultiplesOptions,
    smallMultiplesOptions_maxVisibleColumns,
    smallMultiplesOptions_maxVisibleRows,
    smallMultiplesOptions_panelConfiguration,

    -- * SnowflakeParameters
    SnowflakeParameters (..),
    newSnowflakeParameters,
    snowflakeParameters_host,
    snowflakeParameters_database,
    snowflakeParameters_warehouse,

    -- * Spacing
    Spacing (..),
    newSpacing,
    spacing_bottom,
    spacing_left,
    spacing_right,
    spacing_top,

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

    -- * StringDefaultValues
    StringDefaultValues (..),
    newStringDefaultValues,
    stringDefaultValues_dynamicValue,
    stringDefaultValues_staticValues,

    -- * StringFormatConfiguration
    StringFormatConfiguration (..),
    newStringFormatConfiguration,
    stringFormatConfiguration_nullValueFormatConfiguration,
    stringFormatConfiguration_numericFormatConfiguration,

    -- * StringParameter
    StringParameter (..),
    newStringParameter,
    stringParameter_name,
    stringParameter_values,

    -- * StringParameterDeclaration
    StringParameterDeclaration (..),
    newStringParameterDeclaration,
    stringParameterDeclaration_defaultValues,
    stringParameterDeclaration_valueWhenUnset,
    stringParameterDeclaration_parameterValueType,
    stringParameterDeclaration_name,

    -- * StringValueWhenUnsetConfiguration
    StringValueWhenUnsetConfiguration (..),
    newStringValueWhenUnsetConfiguration,
    stringValueWhenUnsetConfiguration_customValue,
    stringValueWhenUnsetConfiguration_valueWhenUnsetOption,

    -- * SubtotalOptions
    SubtotalOptions (..),
    newSubtotalOptions,
    subtotalOptions_customLabel,
    subtotalOptions_fieldLevel,
    subtotalOptions_fieldLevelOptions,
    subtotalOptions_metricHeaderCellStyle,
    subtotalOptions_totalCellStyle,
    subtotalOptions_totalsVisibility,
    subtotalOptions_valueCellStyle,

    -- * TableAggregatedFieldWells
    TableAggregatedFieldWells (..),
    newTableAggregatedFieldWells,
    tableAggregatedFieldWells_groupBy,
    tableAggregatedFieldWells_values,

    -- * TableBorderOptions
    TableBorderOptions (..),
    newTableBorderOptions,
    tableBorderOptions_color,
    tableBorderOptions_style,
    tableBorderOptions_thickness,

    -- * TableCellConditionalFormatting
    TableCellConditionalFormatting (..),
    newTableCellConditionalFormatting,
    tableCellConditionalFormatting_textFormat,
    tableCellConditionalFormatting_fieldId,

    -- * TableCellImageSizingConfiguration
    TableCellImageSizingConfiguration (..),
    newTableCellImageSizingConfiguration,
    tableCellImageSizingConfiguration_tableCellImageScalingConfiguration,

    -- * TableCellStyle
    TableCellStyle (..),
    newTableCellStyle,
    tableCellStyle_backgroundColor,
    tableCellStyle_border,
    tableCellStyle_fontConfiguration,
    tableCellStyle_height,
    tableCellStyle_horizontalTextAlignment,
    tableCellStyle_textWrap,
    tableCellStyle_verticalTextAlignment,
    tableCellStyle_visibility,

    -- * TableConditionalFormatting
    TableConditionalFormatting (..),
    newTableConditionalFormatting,
    tableConditionalFormatting_conditionalFormattingOptions,

    -- * TableConditionalFormattingOption
    TableConditionalFormattingOption (..),
    newTableConditionalFormattingOption,
    tableConditionalFormattingOption_cell,
    tableConditionalFormattingOption_row,

    -- * TableConfiguration
    TableConfiguration (..),
    newTableConfiguration,
    tableConfiguration_fieldOptions,
    tableConfiguration_fieldWells,
    tableConfiguration_paginatedReportOptions,
    tableConfiguration_sortConfiguration,
    tableConfiguration_tableOptions,
    tableConfiguration_totalOptions,

    -- * TableFieldCustomIconContent
    TableFieldCustomIconContent (..),
    newTableFieldCustomIconContent,
    tableFieldCustomIconContent_icon,

    -- * TableFieldCustomTextContent
    TableFieldCustomTextContent (..),
    newTableFieldCustomTextContent,
    tableFieldCustomTextContent_value,
    tableFieldCustomTextContent_fontConfiguration,

    -- * TableFieldImageConfiguration
    TableFieldImageConfiguration (..),
    newTableFieldImageConfiguration,
    tableFieldImageConfiguration_sizingOptions,

    -- * TableFieldLinkConfiguration
    TableFieldLinkConfiguration (..),
    newTableFieldLinkConfiguration,
    tableFieldLinkConfiguration_target,
    tableFieldLinkConfiguration_content,

    -- * TableFieldLinkContentConfiguration
    TableFieldLinkContentConfiguration (..),
    newTableFieldLinkContentConfiguration,
    tableFieldLinkContentConfiguration_customIconContent,
    tableFieldLinkContentConfiguration_customTextContent,

    -- * TableFieldOption
    TableFieldOption (..),
    newTableFieldOption,
    tableFieldOption_customLabel,
    tableFieldOption_uRLStyling,
    tableFieldOption_visibility,
    tableFieldOption_width,
    tableFieldOption_fieldId,

    -- * TableFieldOptions
    TableFieldOptions (..),
    newTableFieldOptions,
    tableFieldOptions_order,
    tableFieldOptions_selectedFieldOptions,

    -- * TableFieldURLConfiguration
    TableFieldURLConfiguration (..),
    newTableFieldURLConfiguration,
    tableFieldURLConfiguration_imageConfiguration,
    tableFieldURLConfiguration_linkConfiguration,

    -- * TableFieldWells
    TableFieldWells (..),
    newTableFieldWells,
    tableFieldWells_tableAggregatedFieldWells,
    tableFieldWells_tableUnaggregatedFieldWells,

    -- * TableOptions
    TableOptions (..),
    newTableOptions,
    tableOptions_cellStyle,
    tableOptions_headerStyle,
    tableOptions_orientation,
    tableOptions_rowAlternateColorOptions,

    -- * TablePaginatedReportOptions
    TablePaginatedReportOptions (..),
    newTablePaginatedReportOptions,
    tablePaginatedReportOptions_overflowColumnHeaderVisibility,
    tablePaginatedReportOptions_verticalOverflowVisibility,

    -- * TableRowConditionalFormatting
    TableRowConditionalFormatting (..),
    newTableRowConditionalFormatting,
    tableRowConditionalFormatting_backgroundColor,
    tableRowConditionalFormatting_textColor,

    -- * TableSideBorderOptions
    TableSideBorderOptions (..),
    newTableSideBorderOptions,
    tableSideBorderOptions_bottom,
    tableSideBorderOptions_innerHorizontal,
    tableSideBorderOptions_innerVertical,
    tableSideBorderOptions_left,
    tableSideBorderOptions_right,
    tableSideBorderOptions_top,

    -- * TableSortConfiguration
    TableSortConfiguration (..),
    newTableSortConfiguration,
    tableSortConfiguration_paginationConfiguration,
    tableSortConfiguration_rowSort,

    -- * TableUnaggregatedFieldWells
    TableUnaggregatedFieldWells (..),
    newTableUnaggregatedFieldWells,
    tableUnaggregatedFieldWells_values,

    -- * TableVisual
    TableVisual (..),
    newTableVisual,
    tableVisual_actions,
    tableVisual_chartConfiguration,
    tableVisual_conditionalFormatting,
    tableVisual_subtitle,
    tableVisual_title,
    tableVisual_visualId,

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
    template_arn,
    template_createdTime,
    template_lastUpdatedTime,
    template_name,
    template_templateId,
    template_version,

    -- * TemplateAlias
    TemplateAlias (..),
    newTemplateAlias,
    templateAlias_aliasName,
    templateAlias_arn,
    templateAlias_templateVersionNumber,

    -- * TemplateError
    TemplateError (..),
    newTemplateError,
    templateError_message,
    templateError_type,
    templateError_violatedEntities,

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
    templateSummary_arn,
    templateSummary_createdTime,
    templateSummary_lastUpdatedTime,
    templateSummary_latestVersionNumber,
    templateSummary_name,
    templateSummary_templateId,

    -- * TemplateVersion
    TemplateVersion (..),
    newTemplateVersion,
    templateVersion_createdTime,
    templateVersion_dataSetConfigurations,
    templateVersion_description,
    templateVersion_errors,
    templateVersion_sheets,
    templateVersion_sourceEntityArn,
    templateVersion_status,
    templateVersion_themeArn,
    templateVersion_versionNumber,

    -- * TemplateVersionDefinition
    TemplateVersionDefinition (..),
    newTemplateVersionDefinition,
    templateVersionDefinition_analysisDefaults,
    templateVersionDefinition_calculatedFields,
    templateVersionDefinition_columnConfigurations,
    templateVersionDefinition_filterGroups,
    templateVersionDefinition_parameterDeclarations,
    templateVersionDefinition_sheets,
    templateVersionDefinition_dataSetConfigurations,

    -- * TemplateVersionSummary
    TemplateVersionSummary (..),
    newTemplateVersionSummary,
    templateVersionSummary_arn,
    templateVersionSummary_createdTime,
    templateVersionSummary_description,
    templateVersionSummary_status,
    templateVersionSummary_versionNumber,

    -- * TeradataParameters
    TeradataParameters (..),
    newTeradataParameters,
    teradataParameters_host,
    teradataParameters_port,
    teradataParameters_database,

    -- * TextAreaControlDisplayOptions
    TextAreaControlDisplayOptions (..),
    newTextAreaControlDisplayOptions,
    textAreaControlDisplayOptions_placeholderOptions,
    textAreaControlDisplayOptions_titleOptions,

    -- * TextConditionalFormat
    TextConditionalFormat (..),
    newTextConditionalFormat,
    textConditionalFormat_backgroundColor,
    textConditionalFormat_icon,
    textConditionalFormat_textColor,

    -- * TextControlPlaceholderOptions
    TextControlPlaceholderOptions (..),
    newTextControlPlaceholderOptions,
    textControlPlaceholderOptions_visibility,

    -- * TextFieldControlDisplayOptions
    TextFieldControlDisplayOptions (..),
    newTextFieldControlDisplayOptions,
    textFieldControlDisplayOptions_placeholderOptions,
    textFieldControlDisplayOptions_titleOptions,

    -- * Theme
    Theme (..),
    newTheme,
    theme_arn,
    theme_createdTime,
    theme_lastUpdatedTime,
    theme_name,
    theme_themeId,
    theme_type,
    theme_version,

    -- * ThemeAlias
    ThemeAlias (..),
    newThemeAlias,
    themeAlias_aliasName,
    themeAlias_arn,
    themeAlias_themeVersionNumber,

    -- * ThemeConfiguration
    ThemeConfiguration (..),
    newThemeConfiguration,
    themeConfiguration_dataColorPalette,
    themeConfiguration_sheet,
    themeConfiguration_typography,
    themeConfiguration_uIColorPalette,

    -- * ThemeError
    ThemeError (..),
    newThemeError,
    themeError_message,
    themeError_type,

    -- * ThemeSummary
    ThemeSummary (..),
    newThemeSummary,
    themeSummary_arn,
    themeSummary_createdTime,
    themeSummary_lastUpdatedTime,
    themeSummary_latestVersionNumber,
    themeSummary_name,
    themeSummary_themeId,

    -- * ThemeVersion
    ThemeVersion (..),
    newThemeVersion,
    themeVersion_arn,
    themeVersion_baseThemeId,
    themeVersion_configuration,
    themeVersion_createdTime,
    themeVersion_description,
    themeVersion_errors,
    themeVersion_status,
    themeVersion_versionNumber,

    -- * ThemeVersionSummary
    ThemeVersionSummary (..),
    newThemeVersionSummary,
    themeVersionSummary_arn,
    themeVersionSummary_createdTime,
    themeVersionSummary_description,
    themeVersionSummary_status,
    themeVersionSummary_versionNumber,

    -- * ThousandSeparatorOptions
    ThousandSeparatorOptions (..),
    newThousandSeparatorOptions,
    thousandSeparatorOptions_symbol,
    thousandSeparatorOptions_visibility,

    -- * TileLayoutStyle
    TileLayoutStyle (..),
    newTileLayoutStyle,
    tileLayoutStyle_gutter,
    tileLayoutStyle_margin,

    -- * TileStyle
    TileStyle (..),
    newTileStyle,
    tileStyle_border,

    -- * TimeBasedForecastProperties
    TimeBasedForecastProperties (..),
    newTimeBasedForecastProperties,
    timeBasedForecastProperties_lowerBoundary,
    timeBasedForecastProperties_periodsBackward,
    timeBasedForecastProperties_periodsForward,
    timeBasedForecastProperties_predictionInterval,
    timeBasedForecastProperties_seasonality,
    timeBasedForecastProperties_upperBoundary,

    -- * TimeEqualityFilter
    TimeEqualityFilter (..),
    newTimeEqualityFilter,
    timeEqualityFilter_parameterName,
    timeEqualityFilter_timeGranularity,
    timeEqualityFilter_value,
    timeEqualityFilter_filterId,
    timeEqualityFilter_column,

    -- * TimeRangeDrillDownFilter
    TimeRangeDrillDownFilter (..),
    newTimeRangeDrillDownFilter,
    timeRangeDrillDownFilter_column,
    timeRangeDrillDownFilter_rangeMinimum,
    timeRangeDrillDownFilter_rangeMaximum,
    timeRangeDrillDownFilter_timeGranularity,

    -- * TimeRangeFilter
    TimeRangeFilter (..),
    newTimeRangeFilter,
    timeRangeFilter_excludePeriodConfiguration,
    timeRangeFilter_includeMaximum,
    timeRangeFilter_includeMinimum,
    timeRangeFilter_rangeMaximumValue,
    timeRangeFilter_rangeMinimumValue,
    timeRangeFilter_timeGranularity,
    timeRangeFilter_filterId,
    timeRangeFilter_column,
    timeRangeFilter_nullOption,

    -- * TimeRangeFilterValue
    TimeRangeFilterValue (..),
    newTimeRangeFilterValue,
    timeRangeFilterValue_parameter,
    timeRangeFilterValue_rollingDate,
    timeRangeFilterValue_staticValue,

    -- * TooltipItem
    TooltipItem (..),
    newTooltipItem,
    tooltipItem_columnTooltipItem,
    tooltipItem_fieldTooltipItem,

    -- * TooltipOptions
    TooltipOptions (..),
    newTooltipOptions,
    tooltipOptions_fieldBasedTooltip,
    tooltipOptions_selectedTooltipType,
    tooltipOptions_tooltipVisibility,

    -- * TopBottomFilter
    TopBottomFilter (..),
    newTopBottomFilter,
    topBottomFilter_limit,
    topBottomFilter_parameterName,
    topBottomFilter_timeGranularity,
    topBottomFilter_filterId,
    topBottomFilter_column,
    topBottomFilter_aggregationSortConfigurations,

    -- * TopBottomMoversComputation
    TopBottomMoversComputation (..),
    newTopBottomMoversComputation,
    topBottomMoversComputation_moverSize,
    topBottomMoversComputation_name,
    topBottomMoversComputation_sortOrder,
    topBottomMoversComputation_value,
    topBottomMoversComputation_computationId,
    topBottomMoversComputation_time,
    topBottomMoversComputation_category,
    topBottomMoversComputation_type,

    -- * TopBottomRankedComputation
    TopBottomRankedComputation (..),
    newTopBottomRankedComputation,
    topBottomRankedComputation_name,
    topBottomRankedComputation_resultSize,
    topBottomRankedComputation_value,
    topBottomRankedComputation_computationId,
    topBottomRankedComputation_category,
    topBottomRankedComputation_type,

    -- * TotalAggregationComputation
    TotalAggregationComputation (..),
    newTotalAggregationComputation,
    totalAggregationComputation_name,
    totalAggregationComputation_computationId,
    totalAggregationComputation_value,

    -- * TotalOptions
    TotalOptions (..),
    newTotalOptions,
    totalOptions_customLabel,
    totalOptions_placement,
    totalOptions_scrollStatus,
    totalOptions_totalCellStyle,
    totalOptions_totalsVisibility,

    -- * TransformOperation
    TransformOperation (..),
    newTransformOperation,
    transformOperation_castColumnTypeOperation,
    transformOperation_createColumnsOperation,
    transformOperation_filterOperation,
    transformOperation_projectOperation,
    transformOperation_renameColumnOperation,
    transformOperation_tagColumnOperation,
    transformOperation_untagColumnOperation,

    -- * TreeMapAggregatedFieldWells
    TreeMapAggregatedFieldWells (..),
    newTreeMapAggregatedFieldWells,
    treeMapAggregatedFieldWells_colors,
    treeMapAggregatedFieldWells_groups,
    treeMapAggregatedFieldWells_sizes,

    -- * TreeMapConfiguration
    TreeMapConfiguration (..),
    newTreeMapConfiguration,
    treeMapConfiguration_colorLabelOptions,
    treeMapConfiguration_colorScale,
    treeMapConfiguration_dataLabels,
    treeMapConfiguration_fieldWells,
    treeMapConfiguration_groupLabelOptions,
    treeMapConfiguration_legend,
    treeMapConfiguration_sizeLabelOptions,
    treeMapConfiguration_sortConfiguration,
    treeMapConfiguration_tooltip,

    -- * TreeMapFieldWells
    TreeMapFieldWells (..),
    newTreeMapFieldWells,
    treeMapFieldWells_treeMapAggregatedFieldWells,

    -- * TreeMapSortConfiguration
    TreeMapSortConfiguration (..),
    newTreeMapSortConfiguration,
    treeMapSortConfiguration_treeMapGroupItemsLimitConfiguration,
    treeMapSortConfiguration_treeMapSort,

    -- * TreeMapVisual
    TreeMapVisual (..),
    newTreeMapVisual,
    treeMapVisual_actions,
    treeMapVisual_chartConfiguration,
    treeMapVisual_columnHierarchies,
    treeMapVisual_subtitle,
    treeMapVisual_title,
    treeMapVisual_visualId,

    -- * TrendArrowOptions
    TrendArrowOptions (..),
    newTrendArrowOptions,
    trendArrowOptions_visibility,

    -- * TwitterParameters
    TwitterParameters (..),
    newTwitterParameters,
    twitterParameters_query,
    twitterParameters_maxRows,

    -- * Typography
    Typography (..),
    newTypography,
    typography_fontFamilies,

    -- * UIColorPalette
    UIColorPalette (..),
    newUIColorPalette,
    uIColorPalette_accent,
    uIColorPalette_accentForeground,
    uIColorPalette_danger,
    uIColorPalette_dangerForeground,
    uIColorPalette_dimension,
    uIColorPalette_dimensionForeground,
    uIColorPalette_measure,
    uIColorPalette_measureForeground,
    uIColorPalette_primaryBackground,
    uIColorPalette_primaryForeground,
    uIColorPalette_secondaryBackground,
    uIColorPalette_secondaryForeground,
    uIColorPalette_success,
    uIColorPalette_successForeground,
    uIColorPalette_warning,
    uIColorPalette_warningForeground,

    -- * UnaggregatedField
    UnaggregatedField (..),
    newUnaggregatedField,
    unaggregatedField_formatConfiguration,
    unaggregatedField_fieldId,
    unaggregatedField_column,

    -- * UniqueValuesComputation
    UniqueValuesComputation (..),
    newUniqueValuesComputation,
    uniqueValuesComputation_name,
    uniqueValuesComputation_computationId,
    uniqueValuesComputation_category,

    -- * UntagColumnOperation
    UntagColumnOperation (..),
    newUntagColumnOperation,
    untagColumnOperation_columnName,
    untagColumnOperation_tagNames,

    -- * UploadSettings
    UploadSettings (..),
    newUploadSettings,
    uploadSettings_containsHeader,
    uploadSettings_delimiter,
    uploadSettings_format,
    uploadSettings_startFromRow,
    uploadSettings_textQualifier,

    -- * User
    User (..),
    newUser,
    user_active,
    user_arn,
    user_customPermissionsName,
    user_email,
    user_externalLoginFederationProviderType,
    user_externalLoginFederationProviderUrl,
    user_externalLoginId,
    user_identityType,
    user_principalId,
    user_role,
    user_userName,

    -- * VisibleRangeOptions
    VisibleRangeOptions (..),
    newVisibleRangeOptions,
    visibleRangeOptions_percentRange,

    -- * Visual
    Visual (..),
    newVisual,
    visual_barChartVisual,
    visual_boxPlotVisual,
    visual_comboChartVisual,
    visual_customContentVisual,
    visual_emptyVisual,
    visual_filledMapVisual,
    visual_funnelChartVisual,
    visual_gaugeChartVisual,
    visual_geospatialMapVisual,
    visual_heatMapVisual,
    visual_histogramVisual,
    visual_insightVisual,
    visual_kPIVisual,
    visual_lineChartVisual,
    visual_pieChartVisual,
    visual_pivotTableVisual,
    visual_sankeyDiagramVisual,
    visual_scatterPlotVisual,
    visual_tableVisual,
    visual_treeMapVisual,
    visual_waterfallVisual,
    visual_wordCloudVisual,

    -- * VisualCustomAction
    VisualCustomAction (..),
    newVisualCustomAction,
    visualCustomAction_status,
    visualCustomAction_customActionId,
    visualCustomAction_name,
    visualCustomAction_trigger,
    visualCustomAction_actionOperations,

    -- * VisualCustomActionOperation
    VisualCustomActionOperation (..),
    newVisualCustomActionOperation,
    visualCustomActionOperation_filterOperation,
    visualCustomActionOperation_navigationOperation,
    visualCustomActionOperation_setParametersOperation,
    visualCustomActionOperation_uRLOperation,

    -- * VisualPalette
    VisualPalette (..),
    newVisualPalette,
    visualPalette_chartColor,
    visualPalette_colorMap,

    -- * VisualSubtitleLabelOptions
    VisualSubtitleLabelOptions (..),
    newVisualSubtitleLabelOptions,
    visualSubtitleLabelOptions_formatText,
    visualSubtitleLabelOptions_visibility,

    -- * VisualTitleLabelOptions
    VisualTitleLabelOptions (..),
    newVisualTitleLabelOptions,
    visualTitleLabelOptions_formatText,
    visualTitleLabelOptions_visibility,

    -- * VpcConnectionProperties
    VpcConnectionProperties (..),
    newVpcConnectionProperties,
    vpcConnectionProperties_vpcConnectionArn,

    -- * WaterfallChartAggregatedFieldWells
    WaterfallChartAggregatedFieldWells (..),
    newWaterfallChartAggregatedFieldWells,
    waterfallChartAggregatedFieldWells_breakdowns,
    waterfallChartAggregatedFieldWells_categories,
    waterfallChartAggregatedFieldWells_values,

    -- * WaterfallChartConfiguration
    WaterfallChartConfiguration (..),
    newWaterfallChartConfiguration,
    waterfallChartConfiguration_categoryAxisDisplayOptions,
    waterfallChartConfiguration_categoryAxisLabelOptions,
    waterfallChartConfiguration_dataLabels,
    waterfallChartConfiguration_fieldWells,
    waterfallChartConfiguration_legend,
    waterfallChartConfiguration_primaryYAxisDisplayOptions,
    waterfallChartConfiguration_primaryYAxisLabelOptions,
    waterfallChartConfiguration_sortConfiguration,
    waterfallChartConfiguration_visualPalette,
    waterfallChartConfiguration_waterfallChartOptions,

    -- * WaterfallChartFieldWells
    WaterfallChartFieldWells (..),
    newWaterfallChartFieldWells,
    waterfallChartFieldWells_waterfallChartAggregatedFieldWells,

    -- * WaterfallChartOptions
    WaterfallChartOptions (..),
    newWaterfallChartOptions,
    waterfallChartOptions_totalBarLabel,

    -- * WaterfallChartSortConfiguration
    WaterfallChartSortConfiguration (..),
    newWaterfallChartSortConfiguration,
    waterfallChartSortConfiguration_breakdownItemsLimit,
    waterfallChartSortConfiguration_categorySort,

    -- * WaterfallVisual
    WaterfallVisual (..),
    newWaterfallVisual,
    waterfallVisual_actions,
    waterfallVisual_chartConfiguration,
    waterfallVisual_columnHierarchies,
    waterfallVisual_subtitle,
    waterfallVisual_title,
    waterfallVisual_visualId,

    -- * WhatIfPointScenario
    WhatIfPointScenario (..),
    newWhatIfPointScenario,
    whatIfPointScenario_date,
    whatIfPointScenario_value,

    -- * WhatIfRangeScenario
    WhatIfRangeScenario (..),
    newWhatIfRangeScenario,
    whatIfRangeScenario_startDate,
    whatIfRangeScenario_endDate,
    whatIfRangeScenario_value,

    -- * WordCloudAggregatedFieldWells
    WordCloudAggregatedFieldWells (..),
    newWordCloudAggregatedFieldWells,
    wordCloudAggregatedFieldWells_groupBy,
    wordCloudAggregatedFieldWells_size,

    -- * WordCloudChartConfiguration
    WordCloudChartConfiguration (..),
    newWordCloudChartConfiguration,
    wordCloudChartConfiguration_categoryLabelOptions,
    wordCloudChartConfiguration_fieldWells,
    wordCloudChartConfiguration_sortConfiguration,
    wordCloudChartConfiguration_wordCloudOptions,

    -- * WordCloudFieldWells
    WordCloudFieldWells (..),
    newWordCloudFieldWells,
    wordCloudFieldWells_wordCloudAggregatedFieldWells,

    -- * WordCloudOptions
    WordCloudOptions (..),
    newWordCloudOptions,
    wordCloudOptions_cloudLayout,
    wordCloudOptions_maximumStringLength,
    wordCloudOptions_wordCasing,
    wordCloudOptions_wordOrientation,
    wordCloudOptions_wordPadding,
    wordCloudOptions_wordScaling,

    -- * WordCloudSortConfiguration
    WordCloudSortConfiguration (..),
    newWordCloudSortConfiguration,
    wordCloudSortConfiguration_categoryItemsLimit,
    wordCloudSortConfiguration_categorySort,

    -- * WordCloudVisual
    WordCloudVisual (..),
    newWordCloudVisual,
    wordCloudVisual_actions,
    wordCloudVisual_chartConfiguration,
    wordCloudVisual_columnHierarchies,
    wordCloudVisual_subtitle,
    wordCloudVisual_title,
    wordCloudVisual_visualId,
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
import Amazonka.QuickSight.Types.AggregationFunction
import Amazonka.QuickSight.Types.AggregationSortConfiguration
import Amazonka.QuickSight.Types.AmazonElasticsearchParameters
import Amazonka.QuickSight.Types.AmazonOpenSearchParameters
import Amazonka.QuickSight.Types.Analysis
import Amazonka.QuickSight.Types.AnalysisDefaults
import Amazonka.QuickSight.Types.AnalysisDefinition
import Amazonka.QuickSight.Types.AnalysisError
import Amazonka.QuickSight.Types.AnalysisErrorType
import Amazonka.QuickSight.Types.AnalysisFilterAttribute
import Amazonka.QuickSight.Types.AnalysisSearchFilter
import Amazonka.QuickSight.Types.AnalysisSourceEntity
import Amazonka.QuickSight.Types.AnalysisSourceTemplate
import Amazonka.QuickSight.Types.AnalysisSummary
import Amazonka.QuickSight.Types.AnchorDateConfiguration
import Amazonka.QuickSight.Types.AnchorOption
import Amazonka.QuickSight.Types.AnonymousUserDashboardEmbeddingConfiguration
import Amazonka.QuickSight.Types.AnonymousUserDashboardVisualEmbeddingConfiguration
import Amazonka.QuickSight.Types.AnonymousUserEmbeddingExperienceConfiguration
import Amazonka.QuickSight.Types.AnonymousUserQSearchBarEmbeddingConfiguration
import Amazonka.QuickSight.Types.ArcAxisConfiguration
import Amazonka.QuickSight.Types.ArcAxisDisplayRange
import Amazonka.QuickSight.Types.ArcConfiguration
import Amazonka.QuickSight.Types.ArcOptions
import Amazonka.QuickSight.Types.ArcThickness
import Amazonka.QuickSight.Types.ArcThicknessOptions
import Amazonka.QuickSight.Types.AssignmentStatus
import Amazonka.QuickSight.Types.AthenaParameters
import Amazonka.QuickSight.Types.AuroraParameters
import Amazonka.QuickSight.Types.AuroraPostgreSqlParameters
import Amazonka.QuickSight.Types.AuthenticationMethodOption
import Amazonka.QuickSight.Types.AwsIotAnalyticsParameters
import Amazonka.QuickSight.Types.AxisBinding
import Amazonka.QuickSight.Types.AxisDataOptions
import Amazonka.QuickSight.Types.AxisDisplayDataDrivenRange
import Amazonka.QuickSight.Types.AxisDisplayMinMaxRange
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.AxisDisplayRange
import Amazonka.QuickSight.Types.AxisLabelOptions
import Amazonka.QuickSight.Types.AxisLabelReferenceOptions
import Amazonka.QuickSight.Types.AxisLinearScale
import Amazonka.QuickSight.Types.AxisLogarithmicScale
import Amazonka.QuickSight.Types.AxisScale
import Amazonka.QuickSight.Types.AxisTickLabelOptions
import Amazonka.QuickSight.Types.BarChartAggregatedFieldWells
import Amazonka.QuickSight.Types.BarChartConfiguration
import Amazonka.QuickSight.Types.BarChartFieldWells
import Amazonka.QuickSight.Types.BarChartOrientation
import Amazonka.QuickSight.Types.BarChartSortConfiguration
import Amazonka.QuickSight.Types.BarChartVisual
import Amazonka.QuickSight.Types.BarsArrangement
import Amazonka.QuickSight.Types.BaseMapStyleType
import Amazonka.QuickSight.Types.BinCountOptions
import Amazonka.QuickSight.Types.BinWidthOptions
import Amazonka.QuickSight.Types.BodySectionConfiguration
import Amazonka.QuickSight.Types.BodySectionContent
import Amazonka.QuickSight.Types.BorderStyle
import Amazonka.QuickSight.Types.BoxPlotAggregatedFieldWells
import Amazonka.QuickSight.Types.BoxPlotChartConfiguration
import Amazonka.QuickSight.Types.BoxPlotFieldWells
import Amazonka.QuickSight.Types.BoxPlotFillStyle
import Amazonka.QuickSight.Types.BoxPlotOptions
import Amazonka.QuickSight.Types.BoxPlotSortConfiguration
import Amazonka.QuickSight.Types.BoxPlotStyleOptions
import Amazonka.QuickSight.Types.BoxPlotVisual
import Amazonka.QuickSight.Types.CalculatedColumn
import Amazonka.QuickSight.Types.CalculatedField
import Amazonka.QuickSight.Types.CalculatedMeasureField
import Amazonka.QuickSight.Types.CascadingControlConfiguration
import Amazonka.QuickSight.Types.CascadingControlSource
import Amazonka.QuickSight.Types.CastColumnTypeOperation
import Amazonka.QuickSight.Types.CategoricalAggregationFunction
import Amazonka.QuickSight.Types.CategoricalDimensionField
import Amazonka.QuickSight.Types.CategoricalMeasureField
import Amazonka.QuickSight.Types.CategoryDrillDownFilter
import Amazonka.QuickSight.Types.CategoryFilter
import Amazonka.QuickSight.Types.CategoryFilterConfiguration
import Amazonka.QuickSight.Types.CategoryFilterMatchOperator
import Amazonka.QuickSight.Types.CategoryFilterSelectAllOptions
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.ClusterMarker
import Amazonka.QuickSight.Types.ClusterMarkerConfiguration
import Amazonka.QuickSight.Types.ColorFillType
import Amazonka.QuickSight.Types.ColorScale
import Amazonka.QuickSight.Types.ColumnConfiguration
import Amazonka.QuickSight.Types.ColumnDataType
import Amazonka.QuickSight.Types.ColumnDescription
import Amazonka.QuickSight.Types.ColumnGroup
import Amazonka.QuickSight.Types.ColumnGroupColumnSchema
import Amazonka.QuickSight.Types.ColumnGroupSchema
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.ColumnLevelPermissionRule
import Amazonka.QuickSight.Types.ColumnRole
import Amazonka.QuickSight.Types.ColumnSchema
import Amazonka.QuickSight.Types.ColumnSort
import Amazonka.QuickSight.Types.ColumnTag
import Amazonka.QuickSight.Types.ColumnTagName
import Amazonka.QuickSight.Types.ColumnTooltipItem
import Amazonka.QuickSight.Types.ComboChartAggregatedFieldWells
import Amazonka.QuickSight.Types.ComboChartConfiguration
import Amazonka.QuickSight.Types.ComboChartFieldWells
import Amazonka.QuickSight.Types.ComboChartSortConfiguration
import Amazonka.QuickSight.Types.ComboChartVisual
import Amazonka.QuickSight.Types.ComparisonConfiguration
import Amazonka.QuickSight.Types.ComparisonFormatConfiguration
import Amazonka.QuickSight.Types.ComparisonMethod
import Amazonka.QuickSight.Types.Computation
import Amazonka.QuickSight.Types.ConditionalFormattingColor
import Amazonka.QuickSight.Types.ConditionalFormattingCustomIconCondition
import Amazonka.QuickSight.Types.ConditionalFormattingCustomIconOptions
import Amazonka.QuickSight.Types.ConditionalFormattingGradientColor
import Amazonka.QuickSight.Types.ConditionalFormattingIcon
import Amazonka.QuickSight.Types.ConditionalFormattingIconDisplayConfiguration
import Amazonka.QuickSight.Types.ConditionalFormattingIconDisplayOption
import Amazonka.QuickSight.Types.ConditionalFormattingIconSet
import Amazonka.QuickSight.Types.ConditionalFormattingIconSetType
import Amazonka.QuickSight.Types.ConditionalFormattingSolidColor
import Amazonka.QuickSight.Types.ContributionAnalysisDefault
import Amazonka.QuickSight.Types.CreateColumnsOperation
import Amazonka.QuickSight.Types.CredentialPair
import Amazonka.QuickSight.Types.CrossDatasetTypes
import Amazonka.QuickSight.Types.CurrencyDisplayFormatConfiguration
import Amazonka.QuickSight.Types.CustomActionFilterOperation
import Amazonka.QuickSight.Types.CustomActionNavigationOperation
import Amazonka.QuickSight.Types.CustomActionSetParametersOperation
import Amazonka.QuickSight.Types.CustomActionURLOperation
import Amazonka.QuickSight.Types.CustomContentConfiguration
import Amazonka.QuickSight.Types.CustomContentImageScalingConfiguration
import Amazonka.QuickSight.Types.CustomContentType
import Amazonka.QuickSight.Types.CustomContentVisual
import Amazonka.QuickSight.Types.CustomFilterConfiguration
import Amazonka.QuickSight.Types.CustomFilterListConfiguration
import Amazonka.QuickSight.Types.CustomNarrativeOptions
import Amazonka.QuickSight.Types.CustomParameterValues
import Amazonka.QuickSight.Types.CustomSql
import Amazonka.QuickSight.Types.CustomValuesConfiguration
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
import Amazonka.QuickSight.Types.DashboardVersionDefinition
import Amazonka.QuickSight.Types.DashboardVersionSummary
import Amazonka.QuickSight.Types.DashboardVisualId
import Amazonka.QuickSight.Types.DashboardVisualPublishOptions
import Amazonka.QuickSight.Types.DataColor
import Amazonka.QuickSight.Types.DataColorPalette
import Amazonka.QuickSight.Types.DataFieldSeriesItem
import Amazonka.QuickSight.Types.DataLabelContent
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.DataLabelOverlap
import Amazonka.QuickSight.Types.DataLabelPosition
import Amazonka.QuickSight.Types.DataLabelType
import Amazonka.QuickSight.Types.DataPathColor
import Amazonka.QuickSight.Types.DataPathLabelType
import Amazonka.QuickSight.Types.DataPathSort
import Amazonka.QuickSight.Types.DataPathValue
import Amazonka.QuickSight.Types.DataSet
import Amazonka.QuickSight.Types.DataSetConfiguration
import Amazonka.QuickSight.Types.DataSetFilterAttribute
import Amazonka.QuickSight.Types.DataSetIdentifierDeclaration
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
import Amazonka.QuickSight.Types.DateAggregationFunction
import Amazonka.QuickSight.Types.DateAxisOptions
import Amazonka.QuickSight.Types.DateDimensionField
import Amazonka.QuickSight.Types.DateMeasureField
import Amazonka.QuickSight.Types.DateTimeDefaultValues
import Amazonka.QuickSight.Types.DateTimeFormatConfiguration
import Amazonka.QuickSight.Types.DateTimeHierarchy
import Amazonka.QuickSight.Types.DateTimeParameter
import Amazonka.QuickSight.Types.DateTimeParameterDeclaration
import Amazonka.QuickSight.Types.DateTimePickerControlDisplayOptions
import Amazonka.QuickSight.Types.DateTimeValueWhenUnsetConfiguration
import Amazonka.QuickSight.Types.DecimalDefaultValues
import Amazonka.QuickSight.Types.DecimalParameter
import Amazonka.QuickSight.Types.DecimalParameterDeclaration
import Amazonka.QuickSight.Types.DecimalPlacesConfiguration
import Amazonka.QuickSight.Types.DecimalValueWhenUnsetConfiguration
import Amazonka.QuickSight.Types.DefaultFreeFormLayoutConfiguration
import Amazonka.QuickSight.Types.DefaultGridLayoutConfiguration
import Amazonka.QuickSight.Types.DefaultInteractiveLayoutConfiguration
import Amazonka.QuickSight.Types.DefaultNewSheetConfiguration
import Amazonka.QuickSight.Types.DefaultPaginatedLayoutConfiguration
import Amazonka.QuickSight.Types.DefaultSectionBasedLayoutConfiguration
import Amazonka.QuickSight.Types.DestinationParameterValueConfiguration
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.DonutCenterOptions
import Amazonka.QuickSight.Types.DonutOptions
import Amazonka.QuickSight.Types.DrillDownFilter
import Amazonka.QuickSight.Types.DropDownControlDisplayOptions
import Amazonka.QuickSight.Types.DynamicDefaultValue
import Amazonka.QuickSight.Types.Edition
import Amazonka.QuickSight.Types.EmbeddingIdentityType
import Amazonka.QuickSight.Types.EmptyVisual
import Amazonka.QuickSight.Types.Entity
import Amazonka.QuickSight.Types.ErrorInfo
import Amazonka.QuickSight.Types.ExasolParameters
import Amazonka.QuickSight.Types.ExcludePeriodConfiguration
import Amazonka.QuickSight.Types.ExplicitHierarchy
import Amazonka.QuickSight.Types.ExportHiddenFieldsOption
import Amazonka.QuickSight.Types.ExportToCSVOption
import Amazonka.QuickSight.Types.FieldBasedTooltip
import Amazonka.QuickSight.Types.FieldFolder
import Amazonka.QuickSight.Types.FieldLabelType
import Amazonka.QuickSight.Types.FieldSeriesItem
import Amazonka.QuickSight.Types.FieldSort
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.FieldTooltipItem
import Amazonka.QuickSight.Types.FileFormat
import Amazonka.QuickSight.Types.FilledMapAggregatedFieldWells
import Amazonka.QuickSight.Types.FilledMapConditionalFormatting
import Amazonka.QuickSight.Types.FilledMapConditionalFormattingOption
import Amazonka.QuickSight.Types.FilledMapConfiguration
import Amazonka.QuickSight.Types.FilledMapFieldWells
import Amazonka.QuickSight.Types.FilledMapShapeConditionalFormatting
import Amazonka.QuickSight.Types.FilledMapSortConfiguration
import Amazonka.QuickSight.Types.FilledMapVisual
import Amazonka.QuickSight.Types.Filter
import Amazonka.QuickSight.Types.FilterControl
import Amazonka.QuickSight.Types.FilterDateTimePickerControl
import Amazonka.QuickSight.Types.FilterDropDownControl
import Amazonka.QuickSight.Types.FilterGroup
import Amazonka.QuickSight.Types.FilterListConfiguration
import Amazonka.QuickSight.Types.FilterListControl
import Amazonka.QuickSight.Types.FilterNullOption
import Amazonka.QuickSight.Types.FilterOperation
import Amazonka.QuickSight.Types.FilterOperationSelectedFieldsConfiguration
import Amazonka.QuickSight.Types.FilterOperationTargetVisualsConfiguration
import Amazonka.QuickSight.Types.FilterOperator
import Amazonka.QuickSight.Types.FilterRelativeDateTimeControl
import Amazonka.QuickSight.Types.FilterScopeConfiguration
import Amazonka.QuickSight.Types.FilterSelectableValues
import Amazonka.QuickSight.Types.FilterSliderControl
import Amazonka.QuickSight.Types.FilterTextAreaControl
import Amazonka.QuickSight.Types.FilterTextFieldControl
import Amazonka.QuickSight.Types.FilterVisualScope
import Amazonka.QuickSight.Types.Folder
import Amazonka.QuickSight.Types.FolderFilterAttribute
import Amazonka.QuickSight.Types.FolderMember
import Amazonka.QuickSight.Types.FolderSearchFilter
import Amazonka.QuickSight.Types.FolderSummary
import Amazonka.QuickSight.Types.FolderType
import Amazonka.QuickSight.Types.Font
import Amazonka.QuickSight.Types.FontConfiguration
import Amazonka.QuickSight.Types.FontDecoration
import Amazonka.QuickSight.Types.FontSize
import Amazonka.QuickSight.Types.FontStyle
import Amazonka.QuickSight.Types.FontWeight
import Amazonka.QuickSight.Types.FontWeightName
import Amazonka.QuickSight.Types.ForecastComputation
import Amazonka.QuickSight.Types.ForecastComputationSeasonality
import Amazonka.QuickSight.Types.ForecastConfiguration
import Amazonka.QuickSight.Types.ForecastScenario
import Amazonka.QuickSight.Types.FormatConfiguration
import Amazonka.QuickSight.Types.FreeFormLayoutCanvasSizeOptions
import Amazonka.QuickSight.Types.FreeFormLayoutConfiguration
import Amazonka.QuickSight.Types.FreeFormLayoutElement
import Amazonka.QuickSight.Types.FreeFormLayoutElementBackgroundStyle
import Amazonka.QuickSight.Types.FreeFormLayoutElementBorderStyle
import Amazonka.QuickSight.Types.FreeFormLayoutScreenCanvasSizeOptions
import Amazonka.QuickSight.Types.FreeFormSectionLayoutConfiguration
import Amazonka.QuickSight.Types.FunnelChartAggregatedFieldWells
import Amazonka.QuickSight.Types.FunnelChartConfiguration
import Amazonka.QuickSight.Types.FunnelChartDataLabelOptions
import Amazonka.QuickSight.Types.FunnelChartFieldWells
import Amazonka.QuickSight.Types.FunnelChartMeasureDataLabelStyle
import Amazonka.QuickSight.Types.FunnelChartSortConfiguration
import Amazonka.QuickSight.Types.FunnelChartVisual
import Amazonka.QuickSight.Types.GaugeChartArcConditionalFormatting
import Amazonka.QuickSight.Types.GaugeChartConditionalFormatting
import Amazonka.QuickSight.Types.GaugeChartConditionalFormattingOption
import Amazonka.QuickSight.Types.GaugeChartConfiguration
import Amazonka.QuickSight.Types.GaugeChartFieldWells
import Amazonka.QuickSight.Types.GaugeChartOptions
import Amazonka.QuickSight.Types.GaugeChartPrimaryValueConditionalFormatting
import Amazonka.QuickSight.Types.GaugeChartVisual
import Amazonka.QuickSight.Types.GeoSpatialColumnGroup
import Amazonka.QuickSight.Types.GeoSpatialCountryCode
import Amazonka.QuickSight.Types.GeoSpatialDataRole
import Amazonka.QuickSight.Types.GeospatialCoordinateBounds
import Amazonka.QuickSight.Types.GeospatialMapAggregatedFieldWells
import Amazonka.QuickSight.Types.GeospatialMapConfiguration
import Amazonka.QuickSight.Types.GeospatialMapFieldWells
import Amazonka.QuickSight.Types.GeospatialMapStyleOptions
import Amazonka.QuickSight.Types.GeospatialMapVisual
import Amazonka.QuickSight.Types.GeospatialPointStyleOptions
import Amazonka.QuickSight.Types.GeospatialSelectedPointStyle
import Amazonka.QuickSight.Types.GeospatialWindowOptions
import Amazonka.QuickSight.Types.GlobalTableBorderOptions
import Amazonka.QuickSight.Types.GradientColor
import Amazonka.QuickSight.Types.GradientStop
import Amazonka.QuickSight.Types.GridLayoutCanvasSizeOptions
import Amazonka.QuickSight.Types.GridLayoutConfiguration
import Amazonka.QuickSight.Types.GridLayoutElement
import Amazonka.QuickSight.Types.GridLayoutScreenCanvasSizeOptions
import Amazonka.QuickSight.Types.Group
import Amazonka.QuickSight.Types.GroupFilterAttribute
import Amazonka.QuickSight.Types.GroupFilterOperator
import Amazonka.QuickSight.Types.GroupMember
import Amazonka.QuickSight.Types.GroupSearchFilter
import Amazonka.QuickSight.Types.GrowthRateComputation
import Amazonka.QuickSight.Types.GutterStyle
import Amazonka.QuickSight.Types.HeaderFooterSectionConfiguration
import Amazonka.QuickSight.Types.HeatMapAggregatedFieldWells
import Amazonka.QuickSight.Types.HeatMapConfiguration
import Amazonka.QuickSight.Types.HeatMapFieldWells
import Amazonka.QuickSight.Types.HeatMapSortConfiguration
import Amazonka.QuickSight.Types.HeatMapVisual
import Amazonka.QuickSight.Types.HistogramAggregatedFieldWells
import Amazonka.QuickSight.Types.HistogramBinOptions
import Amazonka.QuickSight.Types.HistogramBinType
import Amazonka.QuickSight.Types.HistogramConfiguration
import Amazonka.QuickSight.Types.HistogramFieldWells
import Amazonka.QuickSight.Types.HistogramVisual
import Amazonka.QuickSight.Types.HorizontalTextAlignment
import Amazonka.QuickSight.Types.IAMPolicyAssignment
import Amazonka.QuickSight.Types.IAMPolicyAssignmentSummary
import Amazonka.QuickSight.Types.Icon
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
import Amazonka.QuickSight.Types.InsightConfiguration
import Amazonka.QuickSight.Types.InsightVisual
import Amazonka.QuickSight.Types.IntegerDefaultValues
import Amazonka.QuickSight.Types.IntegerParameter
import Amazonka.QuickSight.Types.IntegerParameterDeclaration
import Amazonka.QuickSight.Types.IntegerValueWhenUnsetConfiguration
import Amazonka.QuickSight.Types.ItemsLimitConfiguration
import Amazonka.QuickSight.Types.JiraParameters
import Amazonka.QuickSight.Types.JoinInstruction
import Amazonka.QuickSight.Types.JoinKeyProperties
import Amazonka.QuickSight.Types.JoinType
import Amazonka.QuickSight.Types.KPIConditionalFormatting
import Amazonka.QuickSight.Types.KPIConditionalFormattingOption
import Amazonka.QuickSight.Types.KPIConfiguration
import Amazonka.QuickSight.Types.KPIFieldWells
import Amazonka.QuickSight.Types.KPIOptions
import Amazonka.QuickSight.Types.KPIPrimaryValueConditionalFormatting
import Amazonka.QuickSight.Types.KPIProgressBarConditionalFormatting
import Amazonka.QuickSight.Types.KPISortConfiguration
import Amazonka.QuickSight.Types.KPIVisual
import Amazonka.QuickSight.Types.LabelOptions
import Amazonka.QuickSight.Types.Layout
import Amazonka.QuickSight.Types.LayoutConfiguration
import Amazonka.QuickSight.Types.LayoutElementType
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.LegendPosition
import Amazonka.QuickSight.Types.LineChartAggregatedFieldWells
import Amazonka.QuickSight.Types.LineChartConfiguration
import Amazonka.QuickSight.Types.LineChartDefaultSeriesSettings
import Amazonka.QuickSight.Types.LineChartFieldWells
import Amazonka.QuickSight.Types.LineChartLineStyle
import Amazonka.QuickSight.Types.LineChartLineStyleSettings
import Amazonka.QuickSight.Types.LineChartMarkerShape
import Amazonka.QuickSight.Types.LineChartMarkerStyleSettings
import Amazonka.QuickSight.Types.LineChartSeriesSettings
import Amazonka.QuickSight.Types.LineChartSortConfiguration
import Amazonka.QuickSight.Types.LineChartType
import Amazonka.QuickSight.Types.LineChartVisual
import Amazonka.QuickSight.Types.LineInterpolation
import Amazonka.QuickSight.Types.LineSeriesAxisDisplayOptions
import Amazonka.QuickSight.Types.LinkSharingConfiguration
import Amazonka.QuickSight.Types.ListControlDisplayOptions
import Amazonka.QuickSight.Types.ListControlSearchOptions
import Amazonka.QuickSight.Types.ListControlSelectAllOptions
import Amazonka.QuickSight.Types.LoadingAnimation
import Amazonka.QuickSight.Types.LocalNavigationConfiguration
import Amazonka.QuickSight.Types.LogicalTable
import Amazonka.QuickSight.Types.LogicalTableSource
import Amazonka.QuickSight.Types.LongFormatText
import Amazonka.QuickSight.Types.ManifestFileLocation
import Amazonka.QuickSight.Types.MapZoomMode
import Amazonka.QuickSight.Types.MarginStyle
import Amazonka.QuickSight.Types.MariaDbParameters
import Amazonka.QuickSight.Types.MaximumLabelType
import Amazonka.QuickSight.Types.MaximumMinimumComputation
import Amazonka.QuickSight.Types.MaximumMinimumComputationType
import Amazonka.QuickSight.Types.MeasureField
import Amazonka.QuickSight.Types.MemberIdArnPair
import Amazonka.QuickSight.Types.MemberType
import Amazonka.QuickSight.Types.MetricComparisonComputation
import Amazonka.QuickSight.Types.MinimumLabelType
import Amazonka.QuickSight.Types.MissingDataConfiguration
import Amazonka.QuickSight.Types.MissingDataTreatmentOption
import Amazonka.QuickSight.Types.MySqlParameters
import Amazonka.QuickSight.Types.NamespaceError
import Amazonka.QuickSight.Types.NamespaceErrorType
import Amazonka.QuickSight.Types.NamespaceInfoV2
import Amazonka.QuickSight.Types.NamespaceStatus
import Amazonka.QuickSight.Types.NegativeValueConfiguration
import Amazonka.QuickSight.Types.NegativeValueDisplayMode
import Amazonka.QuickSight.Types.NullValueFormatConfiguration
import Amazonka.QuickSight.Types.NumberDisplayFormatConfiguration
import Amazonka.QuickSight.Types.NumberFormatConfiguration
import Amazonka.QuickSight.Types.NumberScale
import Amazonka.QuickSight.Types.NumericAxisOptions
import Amazonka.QuickSight.Types.NumericEqualityDrillDownFilter
import Amazonka.QuickSight.Types.NumericEqualityFilter
import Amazonka.QuickSight.Types.NumericEqualityMatchOperator
import Amazonka.QuickSight.Types.NumericFilterSelectAllOptions
import Amazonka.QuickSight.Types.NumericFormatConfiguration
import Amazonka.QuickSight.Types.NumericRangeFilter
import Amazonka.QuickSight.Types.NumericRangeFilterValue
import Amazonka.QuickSight.Types.NumericSeparatorConfiguration
import Amazonka.QuickSight.Types.NumericSeparatorSymbol
import Amazonka.QuickSight.Types.NumericalAggregationFunction
import Amazonka.QuickSight.Types.NumericalDimensionField
import Amazonka.QuickSight.Types.NumericalMeasureField
import Amazonka.QuickSight.Types.OracleParameters
import Amazonka.QuickSight.Types.OtherCategories
import Amazonka.QuickSight.Types.OutputColumn
import Amazonka.QuickSight.Types.PaginationConfiguration
import Amazonka.QuickSight.Types.PanelBorderStyle
import Amazonka.QuickSight.Types.PanelConfiguration
import Amazonka.QuickSight.Types.PanelTitleOptions
import Amazonka.QuickSight.Types.PaperOrientation
import Amazonka.QuickSight.Types.PaperSize
import Amazonka.QuickSight.Types.ParameterControl
import Amazonka.QuickSight.Types.ParameterDateTimePickerControl
import Amazonka.QuickSight.Types.ParameterDeclaration
import Amazonka.QuickSight.Types.ParameterDropDownControl
import Amazonka.QuickSight.Types.ParameterListControl
import Amazonka.QuickSight.Types.ParameterSelectableValues
import Amazonka.QuickSight.Types.ParameterSliderControl
import Amazonka.QuickSight.Types.ParameterTextAreaControl
import Amazonka.QuickSight.Types.ParameterTextFieldControl
import Amazonka.QuickSight.Types.ParameterValueType
import Amazonka.QuickSight.Types.Parameters
import Amazonka.QuickSight.Types.PercentVisibleRange
import Amazonka.QuickSight.Types.PercentageDisplayFormatConfiguration
import Amazonka.QuickSight.Types.PercentileAggregation
import Amazonka.QuickSight.Types.PeriodOverPeriodComputation
import Amazonka.QuickSight.Types.PeriodToDateComputation
import Amazonka.QuickSight.Types.PhysicalTable
import Amazonka.QuickSight.Types.PieChartAggregatedFieldWells
import Amazonka.QuickSight.Types.PieChartConfiguration
import Amazonka.QuickSight.Types.PieChartFieldWells
import Amazonka.QuickSight.Types.PieChartSortConfiguration
import Amazonka.QuickSight.Types.PieChartVisual
import Amazonka.QuickSight.Types.PivotFieldSortOptions
import Amazonka.QuickSight.Types.PivotTableAggregatedFieldWells
import Amazonka.QuickSight.Types.PivotTableCellConditionalFormatting
import Amazonka.QuickSight.Types.PivotTableConditionalFormatting
import Amazonka.QuickSight.Types.PivotTableConditionalFormattingOption
import Amazonka.QuickSight.Types.PivotTableConditionalFormattingScope
import Amazonka.QuickSight.Types.PivotTableConditionalFormattingScopeRole
import Amazonka.QuickSight.Types.PivotTableConfiguration
import Amazonka.QuickSight.Types.PivotTableDataPathOption
import Amazonka.QuickSight.Types.PivotTableFieldOption
import Amazonka.QuickSight.Types.PivotTableFieldOptions
import Amazonka.QuickSight.Types.PivotTableFieldSubtotalOptions
import Amazonka.QuickSight.Types.PivotTableFieldWells
import Amazonka.QuickSight.Types.PivotTableMetricPlacement
import Amazonka.QuickSight.Types.PivotTableOptions
import Amazonka.QuickSight.Types.PivotTablePaginatedReportOptions
import Amazonka.QuickSight.Types.PivotTableSortBy
import Amazonka.QuickSight.Types.PivotTableSortConfiguration
import Amazonka.QuickSight.Types.PivotTableSubtotalLevel
import Amazonka.QuickSight.Types.PivotTableTotalOptions
import Amazonka.QuickSight.Types.PivotTableVisual
import Amazonka.QuickSight.Types.PivotTotalOptions
import Amazonka.QuickSight.Types.PostgreSqlParameters
import Amazonka.QuickSight.Types.PredefinedHierarchy
import Amazonka.QuickSight.Types.PrestoParameters
import Amazonka.QuickSight.Types.PrimaryValueDisplayType
import Amazonka.QuickSight.Types.ProgressBarOptions
import Amazonka.QuickSight.Types.ProjectOperation
import Amazonka.QuickSight.Types.QueueInfo
import Amazonka.QuickSight.Types.RangeEndsLabelType
import Amazonka.QuickSight.Types.RdsParameters
import Amazonka.QuickSight.Types.RedshiftParameters
import Amazonka.QuickSight.Types.ReferenceLine
import Amazonka.QuickSight.Types.ReferenceLineCustomLabelConfiguration
import Amazonka.QuickSight.Types.ReferenceLineDataConfiguration
import Amazonka.QuickSight.Types.ReferenceLineDynamicDataConfiguration
import Amazonka.QuickSight.Types.ReferenceLineLabelConfiguration
import Amazonka.QuickSight.Types.ReferenceLineLabelHorizontalPosition
import Amazonka.QuickSight.Types.ReferenceLineLabelVerticalPosition
import Amazonka.QuickSight.Types.ReferenceLinePatternType
import Amazonka.QuickSight.Types.ReferenceLineStaticDataConfiguration
import Amazonka.QuickSight.Types.ReferenceLineStyleConfiguration
import Amazonka.QuickSight.Types.ReferenceLineValueLabelConfiguration
import Amazonka.QuickSight.Types.ReferenceLineValueLabelRelativePosition
import Amazonka.QuickSight.Types.RegisteredUserDashboardEmbeddingConfiguration
import Amazonka.QuickSight.Types.RegisteredUserDashboardVisualEmbeddingConfiguration
import Amazonka.QuickSight.Types.RegisteredUserEmbeddingExperienceConfiguration
import Amazonka.QuickSight.Types.RegisteredUserQSearchBarEmbeddingConfiguration
import Amazonka.QuickSight.Types.RegisteredUserQuickSightConsoleEmbeddingConfiguration
import Amazonka.QuickSight.Types.RelationalTable
import Amazonka.QuickSight.Types.RelativeDateTimeControlDisplayOptions
import Amazonka.QuickSight.Types.RelativeDateType
import Amazonka.QuickSight.Types.RelativeDatesFilter
import Amazonka.QuickSight.Types.RelativeFontSize
import Amazonka.QuickSight.Types.RenameColumnOperation
import Amazonka.QuickSight.Types.ResizeOption
import Amazonka.QuickSight.Types.ResourcePermission
import Amazonka.QuickSight.Types.ResourceStatus
import Amazonka.QuickSight.Types.RollingDateConfiguration
import Amazonka.QuickSight.Types.RowAlternateColorOptions
import Amazonka.QuickSight.Types.RowInfo
import Amazonka.QuickSight.Types.RowLevelPermissionDataSet
import Amazonka.QuickSight.Types.RowLevelPermissionFormatVersion
import Amazonka.QuickSight.Types.RowLevelPermissionPolicy
import Amazonka.QuickSight.Types.RowLevelPermissionTagConfiguration
import Amazonka.QuickSight.Types.RowLevelPermissionTagRule
import Amazonka.QuickSight.Types.S3Parameters
import Amazonka.QuickSight.Types.S3Source
import Amazonka.QuickSight.Types.SameSheetTargetVisualConfiguration
import Amazonka.QuickSight.Types.SankeyDiagramAggregatedFieldWells
import Amazonka.QuickSight.Types.SankeyDiagramChartConfiguration
import Amazonka.QuickSight.Types.SankeyDiagramFieldWells
import Amazonka.QuickSight.Types.SankeyDiagramSortConfiguration
import Amazonka.QuickSight.Types.SankeyDiagramVisual
import Amazonka.QuickSight.Types.ScatterPlotCategoricallyAggregatedFieldWells
import Amazonka.QuickSight.Types.ScatterPlotConfiguration
import Amazonka.QuickSight.Types.ScatterPlotFieldWells
import Amazonka.QuickSight.Types.ScatterPlotUnaggregatedFieldWells
import Amazonka.QuickSight.Types.ScatterPlotVisual
import Amazonka.QuickSight.Types.ScrollBarOptions
import Amazonka.QuickSight.Types.SecondaryValueOptions
import Amazonka.QuickSight.Types.SectionAfterPageBreak
import Amazonka.QuickSight.Types.SectionBasedLayoutCanvasSizeOptions
import Amazonka.QuickSight.Types.SectionBasedLayoutConfiguration
import Amazonka.QuickSight.Types.SectionBasedLayoutPaperCanvasSizeOptions
import Amazonka.QuickSight.Types.SectionLayoutConfiguration
import Amazonka.QuickSight.Types.SectionPageBreakConfiguration
import Amazonka.QuickSight.Types.SectionPageBreakStatus
import Amazonka.QuickSight.Types.SectionStyle
import Amazonka.QuickSight.Types.SelectAllValueOptions
import Amazonka.QuickSight.Types.SelectedFieldOptions
import Amazonka.QuickSight.Types.SelectedSheetsFilterScopeConfiguration
import Amazonka.QuickSight.Types.SelectedTooltipType
import Amazonka.QuickSight.Types.SeriesItem
import Amazonka.QuickSight.Types.ServiceNowParameters
import Amazonka.QuickSight.Types.SessionTag
import Amazonka.QuickSight.Types.SetParameterValueConfiguration
import Amazonka.QuickSight.Types.ShapeConditionalFormat
import Amazonka.QuickSight.Types.Sheet
import Amazonka.QuickSight.Types.SheetContentType
import Amazonka.QuickSight.Types.SheetControlDateTimePickerType
import Amazonka.QuickSight.Types.SheetControlLayout
import Amazonka.QuickSight.Types.SheetControlLayoutConfiguration
import Amazonka.QuickSight.Types.SheetControlListType
import Amazonka.QuickSight.Types.SheetControlSliderType
import Amazonka.QuickSight.Types.SheetControlsOption
import Amazonka.QuickSight.Types.SheetDefinition
import Amazonka.QuickSight.Types.SheetElementConfigurationOverrides
import Amazonka.QuickSight.Types.SheetElementRenderingRule
import Amazonka.QuickSight.Types.SheetStyle
import Amazonka.QuickSight.Types.SheetTextBox
import Amazonka.QuickSight.Types.SheetVisualScopingConfiguration
import Amazonka.QuickSight.Types.ShortFormatText
import Amazonka.QuickSight.Types.SignupResponse
import Amazonka.QuickSight.Types.SimpleClusterMarker
import Amazonka.QuickSight.Types.SimpleNumericalAggregationFunction
import Amazonka.QuickSight.Types.SliderControlDisplayOptions
import Amazonka.QuickSight.Types.SmallMultiplesOptions
import Amazonka.QuickSight.Types.SnowflakeParameters
import Amazonka.QuickSight.Types.SortDirection
import Amazonka.QuickSight.Types.Spacing
import Amazonka.QuickSight.Types.SparkParameters
import Amazonka.QuickSight.Types.SqlServerParameters
import Amazonka.QuickSight.Types.SslProperties
import Amazonka.QuickSight.Types.Status
import Amazonka.QuickSight.Types.StringDefaultValues
import Amazonka.QuickSight.Types.StringFormatConfiguration
import Amazonka.QuickSight.Types.StringParameter
import Amazonka.QuickSight.Types.StringParameterDeclaration
import Amazonka.QuickSight.Types.StringValueWhenUnsetConfiguration
import Amazonka.QuickSight.Types.SubtotalOptions
import Amazonka.QuickSight.Types.TableAggregatedFieldWells
import Amazonka.QuickSight.Types.TableBorderOptions
import Amazonka.QuickSight.Types.TableBorderStyle
import Amazonka.QuickSight.Types.TableCellConditionalFormatting
import Amazonka.QuickSight.Types.TableCellImageScalingConfiguration
import Amazonka.QuickSight.Types.TableCellImageSizingConfiguration
import Amazonka.QuickSight.Types.TableCellStyle
import Amazonka.QuickSight.Types.TableConditionalFormatting
import Amazonka.QuickSight.Types.TableConditionalFormattingOption
import Amazonka.QuickSight.Types.TableConfiguration
import Amazonka.QuickSight.Types.TableFieldCustomIconContent
import Amazonka.QuickSight.Types.TableFieldCustomTextContent
import Amazonka.QuickSight.Types.TableFieldIconSetType
import Amazonka.QuickSight.Types.TableFieldImageConfiguration
import Amazonka.QuickSight.Types.TableFieldLinkConfiguration
import Amazonka.QuickSight.Types.TableFieldLinkContentConfiguration
import Amazonka.QuickSight.Types.TableFieldOption
import Amazonka.QuickSight.Types.TableFieldOptions
import Amazonka.QuickSight.Types.TableFieldURLConfiguration
import Amazonka.QuickSight.Types.TableFieldWells
import Amazonka.QuickSight.Types.TableOptions
import Amazonka.QuickSight.Types.TableOrientation
import Amazonka.QuickSight.Types.TablePaginatedReportOptions
import Amazonka.QuickSight.Types.TableRowConditionalFormatting
import Amazonka.QuickSight.Types.TableSideBorderOptions
import Amazonka.QuickSight.Types.TableSortConfiguration
import Amazonka.QuickSight.Types.TableTotalsPlacement
import Amazonka.QuickSight.Types.TableTotalsScrollStatus
import Amazonka.QuickSight.Types.TableUnaggregatedFieldWells
import Amazonka.QuickSight.Types.TableVisual
import Amazonka.QuickSight.Types.Tag
import Amazonka.QuickSight.Types.TagColumnOperation
import Amazonka.QuickSight.Types.TargetVisualOptions
import Amazonka.QuickSight.Types.Template
import Amazonka.QuickSight.Types.TemplateAlias
import Amazonka.QuickSight.Types.TemplateError
import Amazonka.QuickSight.Types.TemplateErrorType
import Amazonka.QuickSight.Types.TemplateSourceAnalysis
import Amazonka.QuickSight.Types.TemplateSourceEntity
import Amazonka.QuickSight.Types.TemplateSourceTemplate
import Amazonka.QuickSight.Types.TemplateSummary
import Amazonka.QuickSight.Types.TemplateVersion
import Amazonka.QuickSight.Types.TemplateVersionDefinition
import Amazonka.QuickSight.Types.TemplateVersionSummary
import Amazonka.QuickSight.Types.TeradataParameters
import Amazonka.QuickSight.Types.TextAreaControlDisplayOptions
import Amazonka.QuickSight.Types.TextConditionalFormat
import Amazonka.QuickSight.Types.TextControlPlaceholderOptions
import Amazonka.QuickSight.Types.TextFieldControlDisplayOptions
import Amazonka.QuickSight.Types.TextQualifier
import Amazonka.QuickSight.Types.TextWrap
import Amazonka.QuickSight.Types.Theme
import Amazonka.QuickSight.Types.ThemeAlias
import Amazonka.QuickSight.Types.ThemeConfiguration
import Amazonka.QuickSight.Types.ThemeError
import Amazonka.QuickSight.Types.ThemeErrorType
import Amazonka.QuickSight.Types.ThemeSummary
import Amazonka.QuickSight.Types.ThemeType
import Amazonka.QuickSight.Types.ThemeVersion
import Amazonka.QuickSight.Types.ThemeVersionSummary
import Amazonka.QuickSight.Types.ThousandSeparatorOptions
import Amazonka.QuickSight.Types.TileLayoutStyle
import Amazonka.QuickSight.Types.TileStyle
import Amazonka.QuickSight.Types.TimeBasedForecastProperties
import Amazonka.QuickSight.Types.TimeEqualityFilter
import Amazonka.QuickSight.Types.TimeGranularity
import Amazonka.QuickSight.Types.TimeRangeDrillDownFilter
import Amazonka.QuickSight.Types.TimeRangeFilter
import Amazonka.QuickSight.Types.TimeRangeFilterValue
import Amazonka.QuickSight.Types.TooltipItem
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.TooltipTitleType
import Amazonka.QuickSight.Types.TopBottomComputationType
import Amazonka.QuickSight.Types.TopBottomFilter
import Amazonka.QuickSight.Types.TopBottomMoversComputation
import Amazonka.QuickSight.Types.TopBottomRankedComputation
import Amazonka.QuickSight.Types.TopBottomSortOrder
import Amazonka.QuickSight.Types.TotalAggregationComputation
import Amazonka.QuickSight.Types.TotalOptions
import Amazonka.QuickSight.Types.TransformOperation
import Amazonka.QuickSight.Types.TreeMapAggregatedFieldWells
import Amazonka.QuickSight.Types.TreeMapConfiguration
import Amazonka.QuickSight.Types.TreeMapFieldWells
import Amazonka.QuickSight.Types.TreeMapSortConfiguration
import Amazonka.QuickSight.Types.TreeMapVisual
import Amazonka.QuickSight.Types.TrendArrowOptions
import Amazonka.QuickSight.Types.TwitterParameters
import Amazonka.QuickSight.Types.Typography
import Amazonka.QuickSight.Types.UIColorPalette
import Amazonka.QuickSight.Types.URLTargetConfiguration
import Amazonka.QuickSight.Types.UnaggregatedField
import Amazonka.QuickSight.Types.UniqueValuesComputation
import Amazonka.QuickSight.Types.UntagColumnOperation
import Amazonka.QuickSight.Types.UploadSettings
import Amazonka.QuickSight.Types.User
import Amazonka.QuickSight.Types.UserRole
import Amazonka.QuickSight.Types.ValueWhenUnsetOption
import Amazonka.QuickSight.Types.VerticalTextAlignment
import Amazonka.QuickSight.Types.Visibility
import Amazonka.QuickSight.Types.VisibleRangeOptions
import Amazonka.QuickSight.Types.Visual
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualCustomActionOperation
import Amazonka.QuickSight.Types.VisualCustomActionTrigger
import Amazonka.QuickSight.Types.VisualPalette
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions
import Amazonka.QuickSight.Types.VpcConnectionProperties
import Amazonka.QuickSight.Types.WaterfallChartAggregatedFieldWells
import Amazonka.QuickSight.Types.WaterfallChartConfiguration
import Amazonka.QuickSight.Types.WaterfallChartFieldWells
import Amazonka.QuickSight.Types.WaterfallChartOptions
import Amazonka.QuickSight.Types.WaterfallChartSortConfiguration
import Amazonka.QuickSight.Types.WaterfallVisual
import Amazonka.QuickSight.Types.WhatIfPointScenario
import Amazonka.QuickSight.Types.WhatIfRangeScenario
import Amazonka.QuickSight.Types.WidgetStatus
import Amazonka.QuickSight.Types.WordCloudAggregatedFieldWells
import Amazonka.QuickSight.Types.WordCloudChartConfiguration
import Amazonka.QuickSight.Types.WordCloudCloudLayout
import Amazonka.QuickSight.Types.WordCloudFieldWells
import Amazonka.QuickSight.Types.WordCloudOptions
import Amazonka.QuickSight.Types.WordCloudSortConfiguration
import Amazonka.QuickSight.Types.WordCloudVisual
import Amazonka.QuickSight.Types.WordCloudWordCasing
import Amazonka.QuickSight.Types.WordCloudWordOrientation
import Amazonka.QuickSight.Types.WordCloudWordPadding
import Amazonka.QuickSight.Types.WordCloudWordScaling
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
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You don\'t have access to this item. The provided credentials couldn\'t
-- be validated. You might not be authorized to carry out the request. Make
-- sure that your account is authorized to use the Amazon QuickSight
-- service, that your policies have the correct permissions, and that you
-- are using the correct access keys.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 401

-- | A resource is already in a state that indicates an operation is
-- happening that must complete before a new update can be applied.
_ConcurrentUpdatingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConcurrentUpdatingException =
  Core._MatchServiceError
    defaultService
    "ConcurrentUpdatingException"
    Prelude.. Core.hasStatus 500

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The domain specified isn\'t on the allow list. All domains for embedded
-- dashboards must be added to the approved list by an Amazon QuickSight
-- admin.
_DomainNotWhitelistedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_DomainNotWhitelistedException =
  Core._MatchServiceError
    defaultService
    "DomainNotWhitelistedException"
    Prelude.. Core.hasStatus 403

-- | The identity type specified isn\'t supported. Supported identity types
-- include @IAM@ and @QUICKSIGHT@.
_IdentityTypeNotSupportedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_IdentityTypeNotSupportedException =
  Core._MatchServiceError
    defaultService
    "IdentityTypeNotSupportedException"
    Prelude.. Core.hasStatus 403

-- | An internal failure occurred.
_InternalFailureException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | The @NextToken@ value isn\'t valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Core.hasStatus 400

-- | One or more parameters has a value that isn\'t valid.
_InvalidParameterValueException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Core.hasStatus 400

-- | You don\'t have this feature activated for your account. To fix this
-- issue, contact Amazon Web Services support.
_InvalidRequestException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | A limit is exceeded.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409

-- | One or more preconditions aren\'t met.
_PreconditionNotMetException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PreconditionNotMetException =
  Core._MatchServiceError
    defaultService
    "PreconditionNotMetException"
    Prelude.. Core.hasStatus 400

-- | The user with the provided name isn\'t found. This error can happen in
-- any operation that requires finding a user based on a provided user
-- name, such as @DeleteUser@, @DescribeUser@, and so on.
_QuickSightUserNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_QuickSightUserNotFoundException =
  Core._MatchServiceError
    defaultService
    "QuickSightUserNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The resource specified already exists.
_ResourceExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"
    Prelude.. Core.hasStatus 409

-- | One or more resources can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | This resource is currently unavailable.
_ResourceUnavailableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The number of minutes specified for the lifetime of a session isn\'t
-- valid. The session lifetime must be 15-600 minutes.
_SessionLifetimeInMinutesInvalidException :: Core.AsError a => Lens.Fold a Core.ServiceError
_SessionLifetimeInMinutesInvalidException =
  Core._MatchServiceError
    defaultService
    "SessionLifetimeInMinutesInvalidException"
    Prelude.. Core.hasStatus 400

-- | Access is throttled.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | This error indicates that you are calling an embedding operation in
-- Amazon QuickSight without the required pricing plan on your Amazon Web
-- Services account. Before you can use embedding for anonymous users, a
-- QuickSight administrator needs to add capacity pricing to Amazon
-- QuickSight. You can do this on the __Manage Amazon QuickSight__ page.
--
-- After capacity pricing is added, you can use the
-- @ GetDashboardEmbedUrl @ API operation with the
-- @--identity-type ANONYMOUS@ option.
_UnsupportedPricingPlanException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedPricingPlanException =
  Core._MatchServiceError
    defaultService
    "UnsupportedPricingPlanException"
    Prelude.. Core.hasStatus 403

-- | This error indicates that you are calling an operation on an Amazon
-- QuickSight subscription where the edition doesn\'t include support for
-- that operation. Amazon Amazon QuickSight currently has Standard Edition
-- and Enterprise Edition. Not every operation and capability is available
-- in every edition.
_UnsupportedUserEditionException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedUserEditionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedUserEditionException"
    Prelude.. Core.hasStatus 403
