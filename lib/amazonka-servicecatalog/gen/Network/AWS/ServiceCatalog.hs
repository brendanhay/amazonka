{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Service Catalog__
--
-- <https://aws.amazon.com/servicecatalog/ AWS Service Catalog> enables organizations to create and manage catalogs of IT services that are approved for AWS. To get the most out of this documentation, you should be familiar with the terminology discussed in <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/what-is_concepts.html AWS Service Catalog Concepts> .
module Network.AWS.ServiceCatalog
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidParametersException
    _InvalidParametersException,

    -- ** DuplicateResourceException
    _DuplicateResourceException,

    -- ** OperationNotSupportedException
    _OperationNotSupportedException,

    -- ** TagOptionNotMigratedException
    _TagOptionNotMigratedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ImportAsProvisionedProduct
    module Network.AWS.ServiceCatalog.ImportAsProvisionedProduct,

    -- ** DeleteConstraint
    module Network.AWS.ServiceCatalog.DeleteConstraint,

    -- ** UpdateConstraint
    module Network.AWS.ServiceCatalog.UpdateConstraint,

    -- ** CreateProvisionedProductPlan
    module Network.AWS.ServiceCatalog.CreateProvisionedProductPlan,

    -- ** ExecuteProvisionedProductServiceAction
    module Network.AWS.ServiceCatalog.ExecuteProvisionedProductServiceAction,

    -- ** CreateProduct
    module Network.AWS.ServiceCatalog.CreateProduct,

    -- ** DescribeCopyProductStatus
    module Network.AWS.ServiceCatalog.DescribeCopyProductStatus,

    -- ** CreateServiceAction
    module Network.AWS.ServiceCatalog.CreateServiceAction,

    -- ** TerminateProvisionedProduct
    module Network.AWS.ServiceCatalog.TerminateProvisionedProduct,

    -- ** UpdateProvisionedProduct
    module Network.AWS.ServiceCatalog.UpdateProvisionedProduct,

    -- ** DescribeProvisioningArtifact
    module Network.AWS.ServiceCatalog.DescribeProvisioningArtifact,

    -- ** AssociateServiceActionWithProvisioningArtifact
    module Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact,

    -- ** ListRecordHistory (Paginated)
    module Network.AWS.ServiceCatalog.ListRecordHistory,

    -- ** DescribeProvisionedProductPlan
    module Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan,

    -- ** AssociateTagOptionWithResource
    module Network.AWS.ServiceCatalog.AssociateTagOptionWithResource,

    -- ** CreateTagOption
    module Network.AWS.ServiceCatalog.CreateTagOption,

    -- ** ListBudgetsForResource
    module Network.AWS.ServiceCatalog.ListBudgetsForResource,

    -- ** DisassociateProductFromPortfolio
    module Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio,

    -- ** ListConstraintsForPortfolio (Paginated)
    module Network.AWS.ServiceCatalog.ListConstraintsForPortfolio,

    -- ** DescribeRecord
    module Network.AWS.ServiceCatalog.DescribeRecord,

    -- ** EnableAWSOrganizationsAccess
    module Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess,

    -- ** DescribeConstraint
    module Network.AWS.ServiceCatalog.DescribeConstraint,

    -- ** CreateProvisioningArtifact
    module Network.AWS.ServiceCatalog.CreateProvisioningArtifact,

    -- ** ListPortfolios (Paginated)
    module Network.AWS.ServiceCatalog.ListPortfolios,

    -- ** DisassociateBudgetFromResource
    module Network.AWS.ServiceCatalog.DisassociateBudgetFromResource,

    -- ** DescribeProductView
    module Network.AWS.ServiceCatalog.DescribeProductView,

    -- ** CreatePortfolioShare
    module Network.AWS.ServiceCatalog.CreatePortfolioShare,

    -- ** ListProvisioningArtifacts
    module Network.AWS.ServiceCatalog.ListProvisioningArtifacts,

    -- ** ListServiceActionsForProvisioningArtifact (Paginated)
    module Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact,

    -- ** SearchProducts
    module Network.AWS.ServiceCatalog.SearchProducts,

    -- ** DescribeServiceActionExecutionParameters
    module Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters,

    -- ** SearchProvisionedProducts
    module Network.AWS.ServiceCatalog.SearchProvisionedProducts,

    -- ** ListStackInstancesForProvisionedProduct
    module Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct,

    -- ** DescribeServiceAction
    module Network.AWS.ServiceCatalog.DescribeServiceAction,

    -- ** DescribeProduct
    module Network.AWS.ServiceCatalog.DescribeProduct,

    -- ** DeleteProvisionedProductPlan
    module Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan,

    -- ** GetProvisionedProductOutputs
    module Network.AWS.ServiceCatalog.GetProvisionedProductOutputs,

    -- ** CreateConstraint
    module Network.AWS.ServiceCatalog.CreateConstraint,

    -- ** ListProvisionedProductPlans (Paginated)
    module Network.AWS.ServiceCatalog.ListProvisionedProductPlans,

    -- ** ListPortfolioAccess
    module Network.AWS.ServiceCatalog.ListPortfolioAccess,

    -- ** BatchDisassociateServiceActionFromProvisioningArtifact
    module Network.AWS.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact,

    -- ** DisassociatePrincipalFromPortfolio
    module Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio,

    -- ** DescribeTagOption
    module Network.AWS.ServiceCatalog.DescribeTagOption,

    -- ** DisassociateTagOptionFromResource
    module Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource,

    -- ** DescribePortfolio
    module Network.AWS.ServiceCatalog.DescribePortfolio,

    -- ** AssociateProductWithPortfolio
    module Network.AWS.ServiceCatalog.AssociateProductWithPortfolio,

    -- ** ListAcceptedPortfolioShares (Paginated)
    module Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares,

    -- ** ExecuteProvisionedProductPlan
    module Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan,

    -- ** AcceptPortfolioShare
    module Network.AWS.ServiceCatalog.AcceptPortfolioShare,

    -- ** ScanProvisionedProducts (Paginated)
    module Network.AWS.ServiceCatalog.ScanProvisionedProducts,

    -- ** ListOrganizationPortfolioAccess (Paginated)
    module Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess,

    -- ** ListPrincipalsForPortfolio (Paginated)
    module Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio,

    -- ** DeleteProduct
    module Network.AWS.ServiceCatalog.DeleteProduct,

    -- ** UpdateProduct
    module Network.AWS.ServiceCatalog.UpdateProduct,

    -- ** ListServiceActions (Paginated)
    module Network.AWS.ServiceCatalog.ListServiceActions,

    -- ** ProvisionProduct
    module Network.AWS.ServiceCatalog.ProvisionProduct,

    -- ** DeleteServiceAction
    module Network.AWS.ServiceCatalog.DeleteServiceAction,

    -- ** UpdateServiceAction
    module Network.AWS.ServiceCatalog.UpdateServiceAction,

    -- ** DisableAWSOrganizationsAccess
    module Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess,

    -- ** RejectPortfolioShare
    module Network.AWS.ServiceCatalog.RejectPortfolioShare,

    -- ** DisassociateServiceActionFromProvisioningArtifact
    module Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact,

    -- ** DeleteTagOption
    module Network.AWS.ServiceCatalog.DeleteTagOption,

    -- ** UpdateTagOption
    module Network.AWS.ServiceCatalog.UpdateTagOption,

    -- ** ListTagOptions (Paginated)
    module Network.AWS.ServiceCatalog.ListTagOptions,

    -- ** UpdateProvisionedProductProperties
    module Network.AWS.ServiceCatalog.UpdateProvisionedProductProperties,

    -- ** SearchProductsAsAdmin (Paginated)
    module Network.AWS.ServiceCatalog.SearchProductsAsAdmin,

    -- ** DeletePortfolio
    module Network.AWS.ServiceCatalog.DeletePortfolio,

    -- ** UpdatePortfolio
    module Network.AWS.ServiceCatalog.UpdatePortfolio,

    -- ** ListPortfoliosForProduct (Paginated)
    module Network.AWS.ServiceCatalog.ListPortfoliosForProduct,

    -- ** GetAWSOrganizationsAccessStatus
    module Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus,

    -- ** DescribeProductAsAdmin
    module Network.AWS.ServiceCatalog.DescribeProductAsAdmin,

    -- ** BatchAssociateServiceActionWithProvisioningArtifact
    module Network.AWS.ServiceCatalog.BatchAssociateServiceActionWithProvisioningArtifact,

    -- ** DescribeProvisioningParameters
    module Network.AWS.ServiceCatalog.DescribeProvisioningParameters,

    -- ** AssociatePrincipalWithPortfolio
    module Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio,

    -- ** DescribeProvisionedProduct
    module Network.AWS.ServiceCatalog.DescribeProvisionedProduct,

    -- ** CopyProduct
    module Network.AWS.ServiceCatalog.CopyProduct,

    -- ** DescribePortfolioShareStatus
    module Network.AWS.ServiceCatalog.DescribePortfolioShareStatus,

    -- ** UpdateProvisioningArtifact
    module Network.AWS.ServiceCatalog.UpdateProvisioningArtifact,

    -- ** DeletePortfolioShare
    module Network.AWS.ServiceCatalog.DeletePortfolioShare,

    -- ** DeleteProvisioningArtifact
    module Network.AWS.ServiceCatalog.DeleteProvisioningArtifact,

    -- ** ListProvisioningArtifactsForServiceAction (Paginated)
    module Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction,

    -- ** CreatePortfolio
    module Network.AWS.ServiceCatalog.CreatePortfolio,

    -- ** ListLaunchPaths (Paginated)
    module Network.AWS.ServiceCatalog.ListLaunchPaths,

    -- ** ListResourcesForTagOption (Paginated)
    module Network.AWS.ServiceCatalog.ListResourcesForTagOption,

    -- ** AssociateBudgetWithResource
    module Network.AWS.ServiceCatalog.AssociateBudgetWithResource,

    -- * Types

    -- ** ResourceChange
    ResourceChange (..),
    mkResourceChange,
    rcAction,
    rcDetails,
    rcLogicalResourceId,
    rcPhysicalResourceId,
    rcReplacement,
    rcResourceType,
    rcScope,

    -- ** OutputValue
    OutputValue (..),

    -- ** CloudWatchDashboard
    CloudWatchDashboard (..),
    mkCloudWatchDashboard,
    cwdName,

    -- ** ExecutionParameterKey
    ExecutionParameterKey (..),

    -- ** IdempotencyToken
    IdempotencyToken (..),

    -- ** RequestStatus
    RequestStatus (..),

    -- ** LogicalResourceId
    LogicalResourceId (..),

    -- ** ServiceActionName
    ServiceActionName (..),

    -- ** RecordDetail
    RecordDetail (..),
    mkRecordDetail,
    rdCreatedTime,
    rdLaunchRoleArn,
    rdPathId,
    rdProductId,
    rdProvisionedProductId,
    rdProvisionedProductName,
    rdProvisionedProductType,
    rdProvisioningArtifactId,
    rdRecordErrors,
    rdRecordId,
    rdRecordTags,
    rdRecordType,
    rdStatus,
    rdUpdatedTime,

    -- ** LaunchPathSummary
    LaunchPathSummary (..),
    mkLaunchPathSummary,
    lpsConstraintSummaries,
    lpsId,
    lpsName,
    lpsTags,

    -- ** ConstraintDetail
    ConstraintDetail (..),
    mkConstraintDetail,
    cdConstraintId,
    cdDescription,
    cdOwner,
    cdPortfolioId,
    cdProductId,
    cdType,

    -- ** TagOptionId
    TagOptionId (..),

    -- ** ProvisionedProductNameOrArn
    ProvisionedProductNameOrArn (..),

    -- ** ProvisionedProductName
    ProvisionedProductName (..),

    -- ** RecordTag
    RecordTag (..),
    mkRecordTag,
    rtKey,
    rtValue,

    -- ** ShareDetails
    ShareDetails (..),
    mkShareDetails,
    sdShareErrors,
    sdSuccessfulShares,

    -- ** ConstraintParameters
    ConstraintParameters (..),

    -- ** ProductViewOwner
    ProductViewOwner (..),

    -- ** ServiceActionAssociationErrorCode
    ServiceActionAssociationErrorCode (..),

    -- ** ProvisionedProductAttribute
    ProvisionedProductAttribute (..),
    mkProvisionedProductAttribute,
    ppaArn,
    ppaCreatedTime,
    ppaId,
    ppaIdempotencyToken,
    ppaLastProvisioningRecordId,
    ppaLastRecordId,
    ppaLastSuccessfulProvisioningRecordId,
    ppaName,
    ppaPhysicalId,
    ppaProductId,
    ppaProductName,
    ppaProvisioningArtifactId,
    ppaProvisioningArtifactName,
    ppaStatus,
    ppaStatusMessage,
    ppaTags,
    ppaType,
    ppaUserArn,
    ppaUserArnSession,

    -- ** ProvisionedProductPlanName
    ProvisionedProductPlanName (..),

    -- ** ServiceActionSummary
    ServiceActionSummary (..),
    mkServiceActionSummary,
    sasDefinitionType,
    sasDescription,
    sasId,
    sasName,

    -- ** ProvisioningArtifactType
    ProvisioningArtifactType (..),

    -- ** ProvisioningArtifactPreferences
    ProvisioningArtifactPreferences (..),
    mkProvisioningArtifactPreferences,
    papStackSetAccounts,
    papStackSetRegions,

    -- ** PortfolioDisplayName
    PortfolioDisplayName (..),

    -- ** CausingEntity
    CausingEntity (..),

    -- ** AttributeValue
    AttributeValue (..),

    -- ** ResourceId
    ResourceId (..),

    -- ** PortfolioShareType
    PortfolioShareType (..),

    -- ** ProvisioningArtifactPropertyName
    ProvisioningArtifactPropertyName (..),

    -- ** ServiceActionDetail
    ServiceActionDetail (..),
    mkServiceActionDetail,
    sadDefinition,
    sadServiceActionSummary,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** TagOptionValue
    TagOptionValue (..),

    -- ** CopyProductStatus
    CopyProductStatus (..),

    -- ** PhysicalResourceId
    PhysicalResourceId (..),

    -- ** ProductViewSummary
    ProductViewSummary (..),
    mkProductViewSummary,
    pvsDistributor,
    pvsHasDefaultPath,
    pvsId,
    pvsName,
    pvsOwner,
    pvsProductId,
    pvsShortDescription,
    pvsSupportDescription,
    pvsSupportEmail,
    pvsSupportUrl,
    pvsType,

    -- ** ProvisionedProductDetail
    ProvisionedProductDetail (..),
    mkProvisionedProductDetail,
    ppdArn,
    ppdCreatedTime,
    ppdId,
    ppdIdempotencyToken,
    ppdLastProvisioningRecordId,
    ppdLastRecordId,
    ppdLastSuccessfulProvisioningRecordId,
    ppdLaunchRoleArn,
    ppdName,
    ppdProductId,
    ppdProvisioningArtifactId,
    ppdStatus,
    ppdStatusMessage,
    ppdType,

    -- ** ProvisioningArtifactInfoValue
    ProvisioningArtifactInfoValue (..),

    -- ** RecordOutput
    RecordOutput (..),
    mkRecordOutput,
    roDescription,
    roOutputKey,
    roOutputValue,

    -- ** ProvisioningArtifactView
    ProvisioningArtifactView (..),
    mkProvisioningArtifactView,
    pavProductViewSummary,
    pavProvisioningArtifact,

    -- ** ResourceType
    ResourceType (..),

    -- ** ProvisionedProductPlanDetails
    ProvisionedProductPlanDetails (..),
    mkProvisionedProductPlanDetails,
    pppdCreatedTime,
    pppdNotificationArns,
    pppdPathId,
    pppdPlanId,
    pppdPlanName,
    pppdPlanType,
    pppdProductId,
    pppdProvisionProductId,
    pppdProvisionProductName,
    pppdProvisioningArtifactId,
    pppdProvisioningParameters,
    pppdStatus,
    pppdStatusMessage,
    pppdTags,
    pppdUpdatedTime,

    -- ** PrincipalType
    PrincipalType (..),

    -- ** ParameterValue
    ParameterValue (..),

    -- ** ProductViewAggregationType
    ProductViewAggregationType (..),

    -- ** AccessLevelFilterKey
    AccessLevelFilterKey (..),

    -- ** ServiceActionDescription
    ServiceActionDescription (..),

    -- ** PortfolioName
    PortfolioName (..),

    -- ** TagOptionSummary
    TagOptionSummary (..),
    mkTagOptionSummary,
    tosKey,
    tosValues,

    -- ** RecordType
    RecordType (..),

    -- ** ConstraintType
    ConstraintType (..),

    -- ** RecordError
    RecordError (..),
    mkRecordError,
    reCode,
    reDescription,

    -- ** ChangeAction
    ChangeAction (..),

    -- ** AccessStatus
    AccessStatus (..),

    -- ** InstructionType
    InstructionType (..),

    -- ** TagOptionDetail
    TagOptionDetail (..),
    mkTagOptionDetail,
    todActive,
    todId,
    todKey,
    todValue,

    -- ** OrganizationNodeValue
    OrganizationNodeValue (..),

    -- ** ProvisioningArtifactName
    ProvisioningArtifactName (..),

    -- ** ProvisionedProductPlanType
    ProvisionedProductPlanType (..),

    -- ** RequiresRecreation
    RequiresRecreation (..),

    -- ** PropertyKey
    PropertyKey (..),

    -- ** ProvisioningArtifactOutput
    ProvisioningArtifactOutput (..),
    mkProvisioningArtifactOutput,
    paoDescription,
    paoKey,

    -- ** LaunchPath
    LaunchPath (..),
    mkLaunchPath,
    lpId,
    lpName,

    -- ** Error
    Error (..),

    -- ** ProvisioningParameter
    ProvisioningParameter (..),
    mkProvisioningParameter,
    ppKey,
    ppValue,

    -- ** ExecutionParameter
    ExecutionParameter (..),
    mkExecutionParameter,
    epDefaultValues,
    epName,
    epType,

    -- ** OrganizationNodeType
    OrganizationNodeType (..),

    -- ** ProvisionedProductType
    ProvisionedProductType (..),

    -- ** PortfolioDetail
    PortfolioDetail (..),
    mkPortfolioDetail,
    pdARN,
    pdCreatedTime,
    pdDescription,
    pdDisplayName,
    pdId,
    pdProviderName,

    -- ** ConstraintDescription
    ConstraintDescription (..),

    -- ** ProvisioningArtifactSummary
    ProvisioningArtifactSummary (..),
    mkProvisioningArtifactSummary,
    pasCreatedTime,
    pasDescription,
    pasId,
    pasName,
    pasProvisioningArtifactMetadata,

    -- ** ProductType
    ProductType (..),

    -- ** ResourceDetail
    ResourceDetail (..),
    mkResourceDetail,
    rARN,
    rCreatedTime,
    rDescription,
    rId,
    rName,

    -- ** ProvisioningArtifactParameter
    ProvisioningArtifactParameter (..),
    mkProvisioningArtifactParameter,
    papDefaultValue,
    papDescription,
    papIsNoEcho,
    papParameterConstraints,
    papParameterKey,
    papParameterType,

    -- ** ResourceDetailId
    ResourceDetailId (..),

    -- ** ParameterKey
    ParameterKey (..),

    -- ** ResourceAttribute
    ResourceAttribute (..),

    -- ** CopyOption
    CopyOption (..),

    -- ** TagOptionKey
    TagOptionKey (..),

    -- ** EvaluationType
    EvaluationType (..),

    -- ** ProvisioningArtifactInfoKey
    ProvisioningArtifactInfoKey (..),

    -- ** ParameterType
    ParameterType (..),

    -- ** ProvisioningArtifactDescription
    ProvisioningArtifactDescription (..),

    -- ** UpdateProvisioningParameter
    UpdateProvisioningParameter (..),
    mkUpdateProvisioningParameter,
    uppKey,
    uppUsePreviousValue,
    uppValue,

    -- ** ConstraintSummary
    ConstraintSummary (..),
    mkConstraintSummary,
    csDescription,
    csType,

    -- ** ServiceActionAssociationErrorMessage
    ServiceActionAssociationErrorMessage (..),

    -- ** SearchFilterKey
    SearchFilterKey (..),

    -- ** ServiceActionDefinitionKey
    ServiceActionDefinitionKey (..),

    -- ** ShareStatus
    ShareStatus (..),

    -- ** FailedServiceActionAssociation
    FailedServiceActionAssociation (..),
    mkFailedServiceActionAssociation,
    fsaaErrorCode,
    fsaaErrorMessage,
    fsaaProductId,
    fsaaProvisioningArtifactId,
    fsaaServiceActionId,

    -- ** AccountId
    AccountId (..),

    -- ** BudgetDetail
    BudgetDetail (..),
    mkBudgetDetail,
    bdBudgetName,

    -- ** PropertyValue
    PropertyValue (..),

    -- ** ProvisionedProductPlanSummary
    ProvisionedProductPlanSummary (..),
    mkProvisionedProductPlanSummary,
    pppsPlanId,
    pppsPlanName,
    pppsPlanType,
    pppsProvisionProductId,
    pppsProvisionProductName,
    pppsProvisioningArtifactId,

    -- ** SupportUrl
    SupportUrl (..),

    -- ** ProvisionedProductPlanStatus
    ProvisionedProductPlanStatus (..),

    -- ** ProvisionedProductStatusMessage
    ProvisionedProductStatusMessage (..),

    -- ** UserArn
    UserArn (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** PortfolioDescription
    PortfolioDescription (..),

    -- ** ProvisioningArtifact
    ProvisioningArtifact (..),
    mkProvisioningArtifact,
    paCreatedTime,
    paDescription,
    paGuidance,
    paId,
    paName,

    -- ** ParameterConstraints
    ParameterConstraints (..),
    mkParameterConstraints,
    pcAllowedValues,

    -- ** PrincipalARN
    PrincipalARN (..),

    -- ** ResourceARN
    ResourceARN (..),

    -- ** ProductViewSortBy
    ProductViewSortBy (..),

    -- ** StackInstance
    StackInstance (..),
    mkStackInstance,
    siAccount,
    siRegion,
    siStackInstanceStatus,

    -- ** ProductViewDetail
    ProductViewDetail (..),
    mkProductViewDetail,
    pvdCreatedTime,
    pvdProductARN,
    pvdProductViewSummary,
    pvdStatus,

    -- ** StatusMessage
    StatusMessage (..),

    -- ** ProvisionedProductStatus
    ProvisionedProductStatus (..),

    -- ** Principal
    Principal (..),
    mkPrincipal,
    pPrincipalARN,
    pPrincipalType,

    -- ** AcceptLanguage
    AcceptLanguage (..),

    -- ** AccessLevelFilter
    AccessLevelFilter (..),
    mkAccessLevelFilter,
    alfKey,
    alfValue,

    -- ** ServiceActionAssociation
    ServiceActionAssociation (..),
    mkServiceActionAssociation,
    saaServiceActionId,
    saaProductId,
    saaProvisioningArtifactId,

    -- ** BudgetName
    BudgetName (..),

    -- ** UserArnSession
    UserArnSession (..),

    -- ** StatusDetail
    StatusDetail (..),

    -- ** Id
    Id (..),

    -- ** ProvisionedProductViewFilterValue
    ProvisionedProductViewFilterValue (..),

    -- ** ProvisionedProductViewFilterBy
    ProvisionedProductViewFilterBy (..),

    -- ** SearchFilterValue
    SearchFilterValue (..),

    -- ** ResourceChangeDetail
    ResourceChangeDetail (..),
    mkResourceChangeDetail,
    rcdCausingEntity,
    rcdEvaluation,
    rcdTarget,

    -- ** ProductViewFilterValue
    ProductViewFilterValue (..),

    -- ** UsageInstruction
    UsageInstruction (..),
    mkUsageInstruction,
    uiType,
    uiValue,

    -- ** ShareError
    ShareError (..),
    mkShareError,
    seAccounts,
    seError,
    seMessage,

    -- ** ServiceActionDefinitionValue
    ServiceActionDefinitionValue (..),

    -- ** TagKey
    TagKey (..),

    -- ** ProductViewFilterBy
    ProductViewFilterBy (..),

    -- ** DefaultValue
    DefaultValue (..),

    -- ** Region
    Region (..),

    -- ** NotificationArn
    NotificationArn (..),

    -- ** PropertyName
    PropertyName (..),

    -- ** ExecutionParameterValue
    ExecutionParameterValue (..),

    -- ** OutputKey
    OutputKey (..),

    -- ** UpdateProvisioningPreferences
    UpdateProvisioningPreferences (..),
    mkUpdateProvisioningPreferences,
    uppStackSetAccounts,
    uppStackSetFailureToleranceCount,
    uppStackSetFailureTolerancePercentage,
    uppStackSetMaxConcurrencyCount,
    uppStackSetMaxConcurrencyPercentage,
    uppStackSetOperationType,
    uppStackSetRegions,

    -- ** PageToken
    PageToken (..),

    -- ** PhysicalId
    PhysicalId (..),

    -- ** ListTagOptionsFilters
    ListTagOptionsFilters (..),
    mkListTagOptionsFilters,
    ltofActive,
    ltofKey,
    ltofValue,

    -- ** SupportEmail
    SupportEmail (..),

    -- ** ProvisioningArtifactPropertyValue
    ProvisioningArtifactPropertyValue (..),

    -- ** Message
    Message (..),

    -- ** StackInstanceStatus
    StackInstanceStatus (..),

    -- ** OrganizationNode
    OrganizationNode (..),
    mkOrganizationNode,
    onType,
    onValue,

    -- ** AllowedValue
    AllowedValue (..),

    -- ** ListRecordHistorySearchFilter
    ListRecordHistorySearchFilter (..),
    mkListRecordHistorySearchFilter,
    lrhsfKey,
    lrhsfValue,

    -- ** Replacement
    Replacement (..),

    -- ** ProvisionedProductId
    ProvisionedProductId (..),

    -- ** Description
    Description (..),

    -- ** ProductSource
    ProductSource (..),

    -- ** ProviderName
    ProviderName (..),

    -- ** ProvisioningArtifactProperties
    ProvisioningArtifactProperties (..),
    mkProvisioningArtifactProperties,
    pInfo,
    pDescription,
    pDisableTemplateValidation,
    pName,
    pType,

    -- ** ProvisioningArtifactDetail
    ProvisioningArtifactDetail (..),
    mkProvisioningArtifactDetail,
    padActive,
    padCreatedTime,
    padDescription,
    padGuidance,
    padId,
    padName,
    padType,

    -- ** ServiceActionDefinitionType
    ServiceActionDefinitionType (..),

    -- ** ResourceTargetDefinition
    ResourceTargetDefinition (..),
    mkResourceTargetDefinition,
    rtdAttribute,
    rtdName,
    rtdRequiresRecreation,

    -- ** RecordStatus
    RecordStatus (..),

    -- ** ProductViewAggregationValue
    ProductViewAggregationValue (..),
    mkProductViewAggregationValue,
    pvavApproximateCount,
    pvavValue,

    -- ** SupportDescription
    SupportDescription (..),

    -- ** ProvisioningArtifactGuidance
    ProvisioningArtifactGuidance (..),

    -- ** ProvisioningPreferences
    ProvisioningPreferences (..),
    mkProvisioningPreferences,
    ppStackSetAccounts,
    ppStackSetFailureToleranceCount,
    ppStackSetFailureTolerancePercentage,
    ppStackSetMaxConcurrencyCount,
    ppStackSetMaxConcurrencyPercentage,
    ppStackSetRegions,

    -- ** StackSetOperationType
    StackSetOperationType (..),

    -- ** NextPageToken
    NextPageToken (..),

    -- ** Name
    Name (..),

    -- ** Distributor
    Distributor (..),

    -- ** Owner
    Owner (..),

    -- ** PortfolioId
    PortfolioId (..),

    -- ** RecordId
    RecordId (..),

    -- ** PathId
    PathId (..),

    -- ** PathName
    PathName (..),

    -- ** ProductId
    ProductId (..),

    -- ** ProductName
    ProductName (..),

    -- ** ProvisioningArtifactId
    ProvisioningArtifactId (..),

    -- ** LaunchRoleArn
    LaunchRoleArn (..),

    -- ** ConstraintId
    ConstraintId (..),

    -- ** Type
    Type (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** PlanId
    PlanId (..),

    -- ** ProvisionProductId
    ProvisionProductId (..),

    -- ** LastProvisioningRecordId
    LastProvisioningRecordId (..),

    -- ** LastRecordId
    LastRecordId (..),

    -- ** LastSuccessfulProvisioningRecordId
    LastSuccessfulProvisioningRecordId (..),

    -- ** ShortDescription
    ShortDescription (..),

    -- ** OrganizationParentId
    OrganizationParentId (..),

    -- ** Code
    Code (..),

    -- ** ServiceActionId
    ServiceActionId (..),

    -- ** SortBy
    SortBy (..),

    -- ** ARN
    ARN (..),

    -- ** CopyProductToken
    CopyProductToken (..),

    -- ** PortfolioShareToken
    PortfolioShareToken (..),

    -- ** SourceProductArn
    SourceProductArn (..),

    -- ** TargetProductName
    TargetProductName (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.AcceptPortfolioShare
import Network.AWS.ServiceCatalog.AssociateBudgetWithResource
import Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
import Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
import Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
import Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
import Network.AWS.ServiceCatalog.BatchAssociateServiceActionWithProvisioningArtifact
import Network.AWS.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
import Network.AWS.ServiceCatalog.CopyProduct
import Network.AWS.ServiceCatalog.CreateConstraint
import Network.AWS.ServiceCatalog.CreatePortfolio
import Network.AWS.ServiceCatalog.CreatePortfolioShare
import Network.AWS.ServiceCatalog.CreateProduct
import Network.AWS.ServiceCatalog.CreateProvisionedProductPlan
import Network.AWS.ServiceCatalog.CreateProvisioningArtifact
import Network.AWS.ServiceCatalog.CreateServiceAction
import Network.AWS.ServiceCatalog.CreateTagOption
import Network.AWS.ServiceCatalog.DeleteConstraint
import Network.AWS.ServiceCatalog.DeletePortfolio
import Network.AWS.ServiceCatalog.DeletePortfolioShare
import Network.AWS.ServiceCatalog.DeleteProduct
import Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
import Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
import Network.AWS.ServiceCatalog.DeleteServiceAction
import Network.AWS.ServiceCatalog.DeleteTagOption
import Network.AWS.ServiceCatalog.DescribeConstraint
import Network.AWS.ServiceCatalog.DescribeCopyProductStatus
import Network.AWS.ServiceCatalog.DescribePortfolio
import Network.AWS.ServiceCatalog.DescribePortfolioShareStatus
import Network.AWS.ServiceCatalog.DescribeProduct
import Network.AWS.ServiceCatalog.DescribeProductAsAdmin
import Network.AWS.ServiceCatalog.DescribeProductView
import Network.AWS.ServiceCatalog.DescribeProvisionedProduct
import Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
import Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
import Network.AWS.ServiceCatalog.DescribeProvisioningParameters
import Network.AWS.ServiceCatalog.DescribeRecord
import Network.AWS.ServiceCatalog.DescribeServiceAction
import Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters
import Network.AWS.ServiceCatalog.DescribeTagOption
import Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess
import Network.AWS.ServiceCatalog.DisassociateBudgetFromResource
import Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
import Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio
import Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
import Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
import Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess
import Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
import Network.AWS.ServiceCatalog.ExecuteProvisionedProductServiceAction
import Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
import Network.AWS.ServiceCatalog.GetProvisionedProductOutputs
import Network.AWS.ServiceCatalog.ImportAsProvisionedProduct
import Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
import Network.AWS.ServiceCatalog.ListBudgetsForResource
import Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
import Network.AWS.ServiceCatalog.ListLaunchPaths
import Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess
import Network.AWS.ServiceCatalog.ListPortfolioAccess
import Network.AWS.ServiceCatalog.ListPortfolios
import Network.AWS.ServiceCatalog.ListPortfoliosForProduct
import Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
import Network.AWS.ServiceCatalog.ListProvisionedProductPlans
import Network.AWS.ServiceCatalog.ListProvisioningArtifacts
import Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction
import Network.AWS.ServiceCatalog.ListRecordHistory
import Network.AWS.ServiceCatalog.ListResourcesForTagOption
import Network.AWS.ServiceCatalog.ListServiceActions
import Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact
import Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct
import Network.AWS.ServiceCatalog.ListTagOptions
import Network.AWS.ServiceCatalog.ProvisionProduct
import Network.AWS.ServiceCatalog.RejectPortfolioShare
import Network.AWS.ServiceCatalog.ScanProvisionedProducts
import Network.AWS.ServiceCatalog.SearchProducts
import Network.AWS.ServiceCatalog.SearchProductsAsAdmin
import Network.AWS.ServiceCatalog.SearchProvisionedProducts
import Network.AWS.ServiceCatalog.TerminateProvisionedProduct
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.UpdateConstraint
import Network.AWS.ServiceCatalog.UpdatePortfolio
import Network.AWS.ServiceCatalog.UpdateProduct
import Network.AWS.ServiceCatalog.UpdateProvisionedProduct
import Network.AWS.ServiceCatalog.UpdateProvisionedProductProperties
import Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
import Network.AWS.ServiceCatalog.UpdateServiceAction
import Network.AWS.ServiceCatalog.UpdateTagOption
import Network.AWS.ServiceCatalog.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ServiceCatalog'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
