{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ServiceCatalog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-12-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Service Catalog
--
-- <http://aws.amazon.com/servicecatalog Service Catalog> enables
-- organizations to create and manage catalogs of IT services that are
-- approved for Amazon Web Services. To get the most out of this
-- documentation, you should be familiar with the terminology discussed in
-- <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/what-is_concepts.html Service Catalog Concepts>.
module Amazonka.ServiceCatalog
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DuplicateResourceException
    _DuplicateResourceException,

    -- ** InvalidParametersException
    _InvalidParametersException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** OperationNotSupportedException
    _OperationNotSupportedException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TagOptionNotMigratedException
    _TagOptionNotMigratedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptPortfolioShare
    AcceptPortfolioShare (AcceptPortfolioShare'),
    newAcceptPortfolioShare,
    AcceptPortfolioShareResponse (AcceptPortfolioShareResponse'),
    newAcceptPortfolioShareResponse,

    -- ** AssociateBudgetWithResource
    AssociateBudgetWithResource (AssociateBudgetWithResource'),
    newAssociateBudgetWithResource,
    AssociateBudgetWithResourceResponse (AssociateBudgetWithResourceResponse'),
    newAssociateBudgetWithResourceResponse,

    -- ** AssociatePrincipalWithPortfolio
    AssociatePrincipalWithPortfolio (AssociatePrincipalWithPortfolio'),
    newAssociatePrincipalWithPortfolio,
    AssociatePrincipalWithPortfolioResponse (AssociatePrincipalWithPortfolioResponse'),
    newAssociatePrincipalWithPortfolioResponse,

    -- ** AssociateProductWithPortfolio
    AssociateProductWithPortfolio (AssociateProductWithPortfolio'),
    newAssociateProductWithPortfolio,
    AssociateProductWithPortfolioResponse (AssociateProductWithPortfolioResponse'),
    newAssociateProductWithPortfolioResponse,

    -- ** AssociateServiceActionWithProvisioningArtifact
    AssociateServiceActionWithProvisioningArtifact (AssociateServiceActionWithProvisioningArtifact'),
    newAssociateServiceActionWithProvisioningArtifact,
    AssociateServiceActionWithProvisioningArtifactResponse (AssociateServiceActionWithProvisioningArtifactResponse'),
    newAssociateServiceActionWithProvisioningArtifactResponse,

    -- ** AssociateTagOptionWithResource
    AssociateTagOptionWithResource (AssociateTagOptionWithResource'),
    newAssociateTagOptionWithResource,
    AssociateTagOptionWithResourceResponse (AssociateTagOptionWithResourceResponse'),
    newAssociateTagOptionWithResourceResponse,

    -- ** BatchAssociateServiceActionWithProvisioningArtifact
    BatchAssociateServiceActionWithProvisioningArtifact (BatchAssociateServiceActionWithProvisioningArtifact'),
    newBatchAssociateServiceActionWithProvisioningArtifact,
    BatchAssociateServiceActionWithProvisioningArtifactResponse (BatchAssociateServiceActionWithProvisioningArtifactResponse'),
    newBatchAssociateServiceActionWithProvisioningArtifactResponse,

    -- ** BatchDisassociateServiceActionFromProvisioningArtifact
    BatchDisassociateServiceActionFromProvisioningArtifact (BatchDisassociateServiceActionFromProvisioningArtifact'),
    newBatchDisassociateServiceActionFromProvisioningArtifact,
    BatchDisassociateServiceActionFromProvisioningArtifactResponse (BatchDisassociateServiceActionFromProvisioningArtifactResponse'),
    newBatchDisassociateServiceActionFromProvisioningArtifactResponse,

    -- ** CopyProduct
    CopyProduct (CopyProduct'),
    newCopyProduct,
    CopyProductResponse (CopyProductResponse'),
    newCopyProductResponse,

    -- ** CreateConstraint
    CreateConstraint (CreateConstraint'),
    newCreateConstraint,
    CreateConstraintResponse (CreateConstraintResponse'),
    newCreateConstraintResponse,

    -- ** CreatePortfolio
    CreatePortfolio (CreatePortfolio'),
    newCreatePortfolio,
    CreatePortfolioResponse (CreatePortfolioResponse'),
    newCreatePortfolioResponse,

    -- ** CreatePortfolioShare
    CreatePortfolioShare (CreatePortfolioShare'),
    newCreatePortfolioShare,
    CreatePortfolioShareResponse (CreatePortfolioShareResponse'),
    newCreatePortfolioShareResponse,

    -- ** CreateProduct
    CreateProduct (CreateProduct'),
    newCreateProduct,
    CreateProductResponse (CreateProductResponse'),
    newCreateProductResponse,

    -- ** CreateProvisionedProductPlan
    CreateProvisionedProductPlan (CreateProvisionedProductPlan'),
    newCreateProvisionedProductPlan,
    CreateProvisionedProductPlanResponse (CreateProvisionedProductPlanResponse'),
    newCreateProvisionedProductPlanResponse,

    -- ** CreateProvisioningArtifact
    CreateProvisioningArtifact (CreateProvisioningArtifact'),
    newCreateProvisioningArtifact,
    CreateProvisioningArtifactResponse (CreateProvisioningArtifactResponse'),
    newCreateProvisioningArtifactResponse,

    -- ** CreateServiceAction
    CreateServiceAction (CreateServiceAction'),
    newCreateServiceAction,
    CreateServiceActionResponse (CreateServiceActionResponse'),
    newCreateServiceActionResponse,

    -- ** CreateTagOption
    CreateTagOption (CreateTagOption'),
    newCreateTagOption,
    CreateTagOptionResponse (CreateTagOptionResponse'),
    newCreateTagOptionResponse,

    -- ** DeleteConstraint
    DeleteConstraint (DeleteConstraint'),
    newDeleteConstraint,
    DeleteConstraintResponse (DeleteConstraintResponse'),
    newDeleteConstraintResponse,

    -- ** DeletePortfolio
    DeletePortfolio (DeletePortfolio'),
    newDeletePortfolio,
    DeletePortfolioResponse (DeletePortfolioResponse'),
    newDeletePortfolioResponse,

    -- ** DeletePortfolioShare
    DeletePortfolioShare (DeletePortfolioShare'),
    newDeletePortfolioShare,
    DeletePortfolioShareResponse (DeletePortfolioShareResponse'),
    newDeletePortfolioShareResponse,

    -- ** DeleteProduct
    DeleteProduct (DeleteProduct'),
    newDeleteProduct,
    DeleteProductResponse (DeleteProductResponse'),
    newDeleteProductResponse,

    -- ** DeleteProvisionedProductPlan
    DeleteProvisionedProductPlan (DeleteProvisionedProductPlan'),
    newDeleteProvisionedProductPlan,
    DeleteProvisionedProductPlanResponse (DeleteProvisionedProductPlanResponse'),
    newDeleteProvisionedProductPlanResponse,

    -- ** DeleteProvisioningArtifact
    DeleteProvisioningArtifact (DeleteProvisioningArtifact'),
    newDeleteProvisioningArtifact,
    DeleteProvisioningArtifactResponse (DeleteProvisioningArtifactResponse'),
    newDeleteProvisioningArtifactResponse,

    -- ** DeleteServiceAction
    DeleteServiceAction (DeleteServiceAction'),
    newDeleteServiceAction,
    DeleteServiceActionResponse (DeleteServiceActionResponse'),
    newDeleteServiceActionResponse,

    -- ** DeleteTagOption
    DeleteTagOption (DeleteTagOption'),
    newDeleteTagOption,
    DeleteTagOptionResponse (DeleteTagOptionResponse'),
    newDeleteTagOptionResponse,

    -- ** DescribeConstraint
    DescribeConstraint (DescribeConstraint'),
    newDescribeConstraint,
    DescribeConstraintResponse (DescribeConstraintResponse'),
    newDescribeConstraintResponse,

    -- ** DescribeCopyProductStatus
    DescribeCopyProductStatus (DescribeCopyProductStatus'),
    newDescribeCopyProductStatus,
    DescribeCopyProductStatusResponse (DescribeCopyProductStatusResponse'),
    newDescribeCopyProductStatusResponse,

    -- ** DescribePortfolio
    DescribePortfolio (DescribePortfolio'),
    newDescribePortfolio,
    DescribePortfolioResponse (DescribePortfolioResponse'),
    newDescribePortfolioResponse,

    -- ** DescribePortfolioShareStatus
    DescribePortfolioShareStatus (DescribePortfolioShareStatus'),
    newDescribePortfolioShareStatus,
    DescribePortfolioShareStatusResponse (DescribePortfolioShareStatusResponse'),
    newDescribePortfolioShareStatusResponse,

    -- ** DescribePortfolioShares
    DescribePortfolioShares (DescribePortfolioShares'),
    newDescribePortfolioShares,
    DescribePortfolioSharesResponse (DescribePortfolioSharesResponse'),
    newDescribePortfolioSharesResponse,

    -- ** DescribeProduct
    DescribeProduct (DescribeProduct'),
    newDescribeProduct,
    DescribeProductResponse (DescribeProductResponse'),
    newDescribeProductResponse,

    -- ** DescribeProductAsAdmin
    DescribeProductAsAdmin (DescribeProductAsAdmin'),
    newDescribeProductAsAdmin,
    DescribeProductAsAdminResponse (DescribeProductAsAdminResponse'),
    newDescribeProductAsAdminResponse,

    -- ** DescribeProductView
    DescribeProductView (DescribeProductView'),
    newDescribeProductView,
    DescribeProductViewResponse (DescribeProductViewResponse'),
    newDescribeProductViewResponse,

    -- ** DescribeProvisionedProduct
    DescribeProvisionedProduct (DescribeProvisionedProduct'),
    newDescribeProvisionedProduct,
    DescribeProvisionedProductResponse (DescribeProvisionedProductResponse'),
    newDescribeProvisionedProductResponse,

    -- ** DescribeProvisionedProductPlan
    DescribeProvisionedProductPlan (DescribeProvisionedProductPlan'),
    newDescribeProvisionedProductPlan,
    DescribeProvisionedProductPlanResponse (DescribeProvisionedProductPlanResponse'),
    newDescribeProvisionedProductPlanResponse,

    -- ** DescribeProvisioningArtifact
    DescribeProvisioningArtifact (DescribeProvisioningArtifact'),
    newDescribeProvisioningArtifact,
    DescribeProvisioningArtifactResponse (DescribeProvisioningArtifactResponse'),
    newDescribeProvisioningArtifactResponse,

    -- ** DescribeProvisioningParameters
    DescribeProvisioningParameters (DescribeProvisioningParameters'),
    newDescribeProvisioningParameters,
    DescribeProvisioningParametersResponse (DescribeProvisioningParametersResponse'),
    newDescribeProvisioningParametersResponse,

    -- ** DescribeRecord
    DescribeRecord (DescribeRecord'),
    newDescribeRecord,
    DescribeRecordResponse (DescribeRecordResponse'),
    newDescribeRecordResponse,

    -- ** DescribeServiceAction
    DescribeServiceAction (DescribeServiceAction'),
    newDescribeServiceAction,
    DescribeServiceActionResponse (DescribeServiceActionResponse'),
    newDescribeServiceActionResponse,

    -- ** DescribeServiceActionExecutionParameters
    DescribeServiceActionExecutionParameters (DescribeServiceActionExecutionParameters'),
    newDescribeServiceActionExecutionParameters,
    DescribeServiceActionExecutionParametersResponse (DescribeServiceActionExecutionParametersResponse'),
    newDescribeServiceActionExecutionParametersResponse,

    -- ** DescribeTagOption
    DescribeTagOption (DescribeTagOption'),
    newDescribeTagOption,
    DescribeTagOptionResponse (DescribeTagOptionResponse'),
    newDescribeTagOptionResponse,

    -- ** DisableAWSOrganizationsAccess
    DisableAWSOrganizationsAccess (DisableAWSOrganizationsAccess'),
    newDisableAWSOrganizationsAccess,
    DisableAWSOrganizationsAccessResponse (DisableAWSOrganizationsAccessResponse'),
    newDisableAWSOrganizationsAccessResponse,

    -- ** DisassociateBudgetFromResource
    DisassociateBudgetFromResource (DisassociateBudgetFromResource'),
    newDisassociateBudgetFromResource,
    DisassociateBudgetFromResourceResponse (DisassociateBudgetFromResourceResponse'),
    newDisassociateBudgetFromResourceResponse,

    -- ** DisassociatePrincipalFromPortfolio
    DisassociatePrincipalFromPortfolio (DisassociatePrincipalFromPortfolio'),
    newDisassociatePrincipalFromPortfolio,
    DisassociatePrincipalFromPortfolioResponse (DisassociatePrincipalFromPortfolioResponse'),
    newDisassociatePrincipalFromPortfolioResponse,

    -- ** DisassociateProductFromPortfolio
    DisassociateProductFromPortfolio (DisassociateProductFromPortfolio'),
    newDisassociateProductFromPortfolio,
    DisassociateProductFromPortfolioResponse (DisassociateProductFromPortfolioResponse'),
    newDisassociateProductFromPortfolioResponse,

    -- ** DisassociateServiceActionFromProvisioningArtifact
    DisassociateServiceActionFromProvisioningArtifact (DisassociateServiceActionFromProvisioningArtifact'),
    newDisassociateServiceActionFromProvisioningArtifact,
    DisassociateServiceActionFromProvisioningArtifactResponse (DisassociateServiceActionFromProvisioningArtifactResponse'),
    newDisassociateServiceActionFromProvisioningArtifactResponse,

    -- ** DisassociateTagOptionFromResource
    DisassociateTagOptionFromResource (DisassociateTagOptionFromResource'),
    newDisassociateTagOptionFromResource,
    DisassociateTagOptionFromResourceResponse (DisassociateTagOptionFromResourceResponse'),
    newDisassociateTagOptionFromResourceResponse,

    -- ** EnableAWSOrganizationsAccess
    EnableAWSOrganizationsAccess (EnableAWSOrganizationsAccess'),
    newEnableAWSOrganizationsAccess,
    EnableAWSOrganizationsAccessResponse (EnableAWSOrganizationsAccessResponse'),
    newEnableAWSOrganizationsAccessResponse,

    -- ** ExecuteProvisionedProductPlan
    ExecuteProvisionedProductPlan (ExecuteProvisionedProductPlan'),
    newExecuteProvisionedProductPlan,
    ExecuteProvisionedProductPlanResponse (ExecuteProvisionedProductPlanResponse'),
    newExecuteProvisionedProductPlanResponse,

    -- ** ExecuteProvisionedProductServiceAction
    ExecuteProvisionedProductServiceAction (ExecuteProvisionedProductServiceAction'),
    newExecuteProvisionedProductServiceAction,
    ExecuteProvisionedProductServiceActionResponse (ExecuteProvisionedProductServiceActionResponse'),
    newExecuteProvisionedProductServiceActionResponse,

    -- ** GetAWSOrganizationsAccessStatus
    GetAWSOrganizationsAccessStatus (GetAWSOrganizationsAccessStatus'),
    newGetAWSOrganizationsAccessStatus,
    GetAWSOrganizationsAccessStatusResponse (GetAWSOrganizationsAccessStatusResponse'),
    newGetAWSOrganizationsAccessStatusResponse,

    -- ** GetProvisionedProductOutputs
    GetProvisionedProductOutputs (GetProvisionedProductOutputs'),
    newGetProvisionedProductOutputs,
    GetProvisionedProductOutputsResponse (GetProvisionedProductOutputsResponse'),
    newGetProvisionedProductOutputsResponse,

    -- ** ImportAsProvisionedProduct
    ImportAsProvisionedProduct (ImportAsProvisionedProduct'),
    newImportAsProvisionedProduct,
    ImportAsProvisionedProductResponse (ImportAsProvisionedProductResponse'),
    newImportAsProvisionedProductResponse,

    -- ** ListAcceptedPortfolioShares (Paginated)
    ListAcceptedPortfolioShares (ListAcceptedPortfolioShares'),
    newListAcceptedPortfolioShares,
    ListAcceptedPortfolioSharesResponse (ListAcceptedPortfolioSharesResponse'),
    newListAcceptedPortfolioSharesResponse,

    -- ** ListBudgetsForResource
    ListBudgetsForResource (ListBudgetsForResource'),
    newListBudgetsForResource,
    ListBudgetsForResourceResponse (ListBudgetsForResourceResponse'),
    newListBudgetsForResourceResponse,

    -- ** ListConstraintsForPortfolio (Paginated)
    ListConstraintsForPortfolio (ListConstraintsForPortfolio'),
    newListConstraintsForPortfolio,
    ListConstraintsForPortfolioResponse (ListConstraintsForPortfolioResponse'),
    newListConstraintsForPortfolioResponse,

    -- ** ListLaunchPaths (Paginated)
    ListLaunchPaths (ListLaunchPaths'),
    newListLaunchPaths,
    ListLaunchPathsResponse (ListLaunchPathsResponse'),
    newListLaunchPathsResponse,

    -- ** ListOrganizationPortfolioAccess (Paginated)
    ListOrganizationPortfolioAccess (ListOrganizationPortfolioAccess'),
    newListOrganizationPortfolioAccess,
    ListOrganizationPortfolioAccessResponse (ListOrganizationPortfolioAccessResponse'),
    newListOrganizationPortfolioAccessResponse,

    -- ** ListPortfolioAccess
    ListPortfolioAccess (ListPortfolioAccess'),
    newListPortfolioAccess,
    ListPortfolioAccessResponse (ListPortfolioAccessResponse'),
    newListPortfolioAccessResponse,

    -- ** ListPortfolios (Paginated)
    ListPortfolios (ListPortfolios'),
    newListPortfolios,
    ListPortfoliosResponse (ListPortfoliosResponse'),
    newListPortfoliosResponse,

    -- ** ListPortfoliosForProduct (Paginated)
    ListPortfoliosForProduct (ListPortfoliosForProduct'),
    newListPortfoliosForProduct,
    ListPortfoliosForProductResponse (ListPortfoliosForProductResponse'),
    newListPortfoliosForProductResponse,

    -- ** ListPrincipalsForPortfolio (Paginated)
    ListPrincipalsForPortfolio (ListPrincipalsForPortfolio'),
    newListPrincipalsForPortfolio,
    ListPrincipalsForPortfolioResponse (ListPrincipalsForPortfolioResponse'),
    newListPrincipalsForPortfolioResponse,

    -- ** ListProvisionedProductPlans (Paginated)
    ListProvisionedProductPlans (ListProvisionedProductPlans'),
    newListProvisionedProductPlans,
    ListProvisionedProductPlansResponse (ListProvisionedProductPlansResponse'),
    newListProvisionedProductPlansResponse,

    -- ** ListProvisioningArtifacts
    ListProvisioningArtifacts (ListProvisioningArtifacts'),
    newListProvisioningArtifacts,
    ListProvisioningArtifactsResponse (ListProvisioningArtifactsResponse'),
    newListProvisioningArtifactsResponse,

    -- ** ListProvisioningArtifactsForServiceAction (Paginated)
    ListProvisioningArtifactsForServiceAction (ListProvisioningArtifactsForServiceAction'),
    newListProvisioningArtifactsForServiceAction,
    ListProvisioningArtifactsForServiceActionResponse (ListProvisioningArtifactsForServiceActionResponse'),
    newListProvisioningArtifactsForServiceActionResponse,

    -- ** ListRecordHistory (Paginated)
    ListRecordHistory (ListRecordHistory'),
    newListRecordHistory,
    ListRecordHistoryResponse (ListRecordHistoryResponse'),
    newListRecordHistoryResponse,

    -- ** ListResourcesForTagOption (Paginated)
    ListResourcesForTagOption (ListResourcesForTagOption'),
    newListResourcesForTagOption,
    ListResourcesForTagOptionResponse (ListResourcesForTagOptionResponse'),
    newListResourcesForTagOptionResponse,

    -- ** ListServiceActions (Paginated)
    ListServiceActions (ListServiceActions'),
    newListServiceActions,
    ListServiceActionsResponse (ListServiceActionsResponse'),
    newListServiceActionsResponse,

    -- ** ListServiceActionsForProvisioningArtifact (Paginated)
    ListServiceActionsForProvisioningArtifact (ListServiceActionsForProvisioningArtifact'),
    newListServiceActionsForProvisioningArtifact,
    ListServiceActionsForProvisioningArtifactResponse (ListServiceActionsForProvisioningArtifactResponse'),
    newListServiceActionsForProvisioningArtifactResponse,

    -- ** ListStackInstancesForProvisionedProduct
    ListStackInstancesForProvisionedProduct (ListStackInstancesForProvisionedProduct'),
    newListStackInstancesForProvisionedProduct,
    ListStackInstancesForProvisionedProductResponse (ListStackInstancesForProvisionedProductResponse'),
    newListStackInstancesForProvisionedProductResponse,

    -- ** ListTagOptions (Paginated)
    ListTagOptions (ListTagOptions'),
    newListTagOptions,
    ListTagOptionsResponse (ListTagOptionsResponse'),
    newListTagOptionsResponse,

    -- ** ProvisionProduct
    ProvisionProduct (ProvisionProduct'),
    newProvisionProduct,
    ProvisionProductResponse (ProvisionProductResponse'),
    newProvisionProductResponse,

    -- ** RejectPortfolioShare
    RejectPortfolioShare (RejectPortfolioShare'),
    newRejectPortfolioShare,
    RejectPortfolioShareResponse (RejectPortfolioShareResponse'),
    newRejectPortfolioShareResponse,

    -- ** ScanProvisionedProducts (Paginated)
    ScanProvisionedProducts (ScanProvisionedProducts'),
    newScanProvisionedProducts,
    ScanProvisionedProductsResponse (ScanProvisionedProductsResponse'),
    newScanProvisionedProductsResponse,

    -- ** SearchProducts
    SearchProducts (SearchProducts'),
    newSearchProducts,
    SearchProductsResponse (SearchProductsResponse'),
    newSearchProductsResponse,

    -- ** SearchProductsAsAdmin (Paginated)
    SearchProductsAsAdmin (SearchProductsAsAdmin'),
    newSearchProductsAsAdmin,
    SearchProductsAsAdminResponse (SearchProductsAsAdminResponse'),
    newSearchProductsAsAdminResponse,

    -- ** SearchProvisionedProducts
    SearchProvisionedProducts (SearchProvisionedProducts'),
    newSearchProvisionedProducts,
    SearchProvisionedProductsResponse (SearchProvisionedProductsResponse'),
    newSearchProvisionedProductsResponse,

    -- ** TerminateProvisionedProduct
    TerminateProvisionedProduct (TerminateProvisionedProduct'),
    newTerminateProvisionedProduct,
    TerminateProvisionedProductResponse (TerminateProvisionedProductResponse'),
    newTerminateProvisionedProductResponse,

    -- ** UpdateConstraint
    UpdateConstraint (UpdateConstraint'),
    newUpdateConstraint,
    UpdateConstraintResponse (UpdateConstraintResponse'),
    newUpdateConstraintResponse,

    -- ** UpdatePortfolio
    UpdatePortfolio (UpdatePortfolio'),
    newUpdatePortfolio,
    UpdatePortfolioResponse (UpdatePortfolioResponse'),
    newUpdatePortfolioResponse,

    -- ** UpdatePortfolioShare
    UpdatePortfolioShare (UpdatePortfolioShare'),
    newUpdatePortfolioShare,
    UpdatePortfolioShareResponse (UpdatePortfolioShareResponse'),
    newUpdatePortfolioShareResponse,

    -- ** UpdateProduct
    UpdateProduct (UpdateProduct'),
    newUpdateProduct,
    UpdateProductResponse (UpdateProductResponse'),
    newUpdateProductResponse,

    -- ** UpdateProvisionedProduct
    UpdateProvisionedProduct (UpdateProvisionedProduct'),
    newUpdateProvisionedProduct,
    UpdateProvisionedProductResponse (UpdateProvisionedProductResponse'),
    newUpdateProvisionedProductResponse,

    -- ** UpdateProvisionedProductProperties
    UpdateProvisionedProductProperties (UpdateProvisionedProductProperties'),
    newUpdateProvisionedProductProperties,
    UpdateProvisionedProductPropertiesResponse (UpdateProvisionedProductPropertiesResponse'),
    newUpdateProvisionedProductPropertiesResponse,

    -- ** UpdateProvisioningArtifact
    UpdateProvisioningArtifact (UpdateProvisioningArtifact'),
    newUpdateProvisioningArtifact,
    UpdateProvisioningArtifactResponse (UpdateProvisioningArtifactResponse'),
    newUpdateProvisioningArtifactResponse,

    -- ** UpdateServiceAction
    UpdateServiceAction (UpdateServiceAction'),
    newUpdateServiceAction,
    UpdateServiceActionResponse (UpdateServiceActionResponse'),
    newUpdateServiceActionResponse,

    -- ** UpdateTagOption
    UpdateTagOption (UpdateTagOption'),
    newUpdateTagOption,
    UpdateTagOptionResponse (UpdateTagOptionResponse'),
    newUpdateTagOptionResponse,

    -- * Types

    -- ** AccessLevelFilterKey
    AccessLevelFilterKey (..),

    -- ** AccessStatus
    AccessStatus (..),

    -- ** ChangeAction
    ChangeAction (..),

    -- ** CopyOption
    CopyOption (..),

    -- ** CopyProductStatus
    CopyProductStatus (..),

    -- ** DescribePortfolioShareType
    DescribePortfolioShareType (..),

    -- ** EvaluationType
    EvaluationType (..),

    -- ** LastSyncStatus
    LastSyncStatus (..),

    -- ** OrganizationNodeType
    OrganizationNodeType (..),

    -- ** PortfolioShareType
    PortfolioShareType (..),

    -- ** PrincipalType
    PrincipalType (..),

    -- ** ProductSource
    ProductSource (..),

    -- ** ProductType
    ProductType (..),

    -- ** ProductViewFilterBy
    ProductViewFilterBy (..),

    -- ** ProductViewSortBy
    ProductViewSortBy (..),

    -- ** PropertyKey
    PropertyKey (..),

    -- ** ProvisionedProductPlanStatus
    ProvisionedProductPlanStatus (..),

    -- ** ProvisionedProductPlanType
    ProvisionedProductPlanType (..),

    -- ** ProvisionedProductStatus
    ProvisionedProductStatus (..),

    -- ** ProvisionedProductViewFilterBy
    ProvisionedProductViewFilterBy (..),

    -- ** ProvisioningArtifactGuidance
    ProvisioningArtifactGuidance (..),

    -- ** ProvisioningArtifactPropertyName
    ProvisioningArtifactPropertyName (..),

    -- ** ProvisioningArtifactType
    ProvisioningArtifactType (..),

    -- ** RecordStatus
    RecordStatus (..),

    -- ** Replacement
    Replacement (..),

    -- ** RequestStatus
    RequestStatus (..),

    -- ** RequiresRecreation
    RequiresRecreation (..),

    -- ** ResourceAttribute
    ResourceAttribute (..),

    -- ** ServiceActionAssociationErrorCode
    ServiceActionAssociationErrorCode (..),

    -- ** ServiceActionDefinitionKey
    ServiceActionDefinitionKey (..),

    -- ** ServiceActionDefinitionType
    ServiceActionDefinitionType (..),

    -- ** ShareStatus
    ShareStatus (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** SourceType
    SourceType (..),

    -- ** StackInstanceStatus
    StackInstanceStatus (..),

    -- ** StackSetOperationType
    StackSetOperationType (..),

    -- ** AccessLevelFilter
    AccessLevelFilter (AccessLevelFilter'),
    newAccessLevelFilter,

    -- ** BudgetDetail
    BudgetDetail (BudgetDetail'),
    newBudgetDetail,

    -- ** CloudWatchDashboard
    CloudWatchDashboard (CloudWatchDashboard'),
    newCloudWatchDashboard,

    -- ** CodeStarParameters
    CodeStarParameters (CodeStarParameters'),
    newCodeStarParameters,

    -- ** ConstraintDetail
    ConstraintDetail (ConstraintDetail'),
    newConstraintDetail,

    -- ** ConstraintSummary
    ConstraintSummary (ConstraintSummary'),
    newConstraintSummary,

    -- ** ExecutionParameter
    ExecutionParameter (ExecutionParameter'),
    newExecutionParameter,

    -- ** FailedServiceActionAssociation
    FailedServiceActionAssociation (FailedServiceActionAssociation'),
    newFailedServiceActionAssociation,

    -- ** LastSync
    LastSync (LastSync'),
    newLastSync,

    -- ** LaunchPath
    LaunchPath (LaunchPath'),
    newLaunchPath,

    -- ** LaunchPathSummary
    LaunchPathSummary (LaunchPathSummary'),
    newLaunchPathSummary,

    -- ** ListRecordHistorySearchFilter
    ListRecordHistorySearchFilter (ListRecordHistorySearchFilter'),
    newListRecordHistorySearchFilter,

    -- ** ListTagOptionsFilters
    ListTagOptionsFilters (ListTagOptionsFilters'),
    newListTagOptionsFilters,

    -- ** OrganizationNode
    OrganizationNode (OrganizationNode'),
    newOrganizationNode,

    -- ** ParameterConstraints
    ParameterConstraints (ParameterConstraints'),
    newParameterConstraints,

    -- ** PortfolioDetail
    PortfolioDetail (PortfolioDetail'),
    newPortfolioDetail,

    -- ** PortfolioShareDetail
    PortfolioShareDetail (PortfolioShareDetail'),
    newPortfolioShareDetail,

    -- ** Principal
    Principal (Principal'),
    newPrincipal,

    -- ** ProductViewAggregationValue
    ProductViewAggregationValue (ProductViewAggregationValue'),
    newProductViewAggregationValue,

    -- ** ProductViewDetail
    ProductViewDetail (ProductViewDetail'),
    newProductViewDetail,

    -- ** ProductViewSummary
    ProductViewSummary (ProductViewSummary'),
    newProductViewSummary,

    -- ** ProvisionedProductAttribute
    ProvisionedProductAttribute (ProvisionedProductAttribute'),
    newProvisionedProductAttribute,

    -- ** ProvisionedProductDetail
    ProvisionedProductDetail (ProvisionedProductDetail'),
    newProvisionedProductDetail,

    -- ** ProvisionedProductPlanDetails
    ProvisionedProductPlanDetails (ProvisionedProductPlanDetails'),
    newProvisionedProductPlanDetails,

    -- ** ProvisionedProductPlanSummary
    ProvisionedProductPlanSummary (ProvisionedProductPlanSummary'),
    newProvisionedProductPlanSummary,

    -- ** ProvisioningArtifact
    ProvisioningArtifact (ProvisioningArtifact'),
    newProvisioningArtifact,

    -- ** ProvisioningArtifactDetail
    ProvisioningArtifactDetail (ProvisioningArtifactDetail'),
    newProvisioningArtifactDetail,

    -- ** ProvisioningArtifactOutput
    ProvisioningArtifactOutput (ProvisioningArtifactOutput'),
    newProvisioningArtifactOutput,

    -- ** ProvisioningArtifactParameter
    ProvisioningArtifactParameter (ProvisioningArtifactParameter'),
    newProvisioningArtifactParameter,

    -- ** ProvisioningArtifactPreferences
    ProvisioningArtifactPreferences (ProvisioningArtifactPreferences'),
    newProvisioningArtifactPreferences,

    -- ** ProvisioningArtifactProperties
    ProvisioningArtifactProperties (ProvisioningArtifactProperties'),
    newProvisioningArtifactProperties,

    -- ** ProvisioningArtifactSummary
    ProvisioningArtifactSummary (ProvisioningArtifactSummary'),
    newProvisioningArtifactSummary,

    -- ** ProvisioningArtifactView
    ProvisioningArtifactView (ProvisioningArtifactView'),
    newProvisioningArtifactView,

    -- ** ProvisioningParameter
    ProvisioningParameter (ProvisioningParameter'),
    newProvisioningParameter,

    -- ** ProvisioningPreferences
    ProvisioningPreferences (ProvisioningPreferences'),
    newProvisioningPreferences,

    -- ** RecordDetail
    RecordDetail (RecordDetail'),
    newRecordDetail,

    -- ** RecordError
    RecordError (RecordError'),
    newRecordError,

    -- ** RecordOutput
    RecordOutput (RecordOutput'),
    newRecordOutput,

    -- ** RecordTag
    RecordTag (RecordTag'),
    newRecordTag,

    -- ** ResourceChange
    ResourceChange (ResourceChange'),
    newResourceChange,

    -- ** ResourceChangeDetail
    ResourceChangeDetail (ResourceChangeDetail'),
    newResourceChangeDetail,

    -- ** ResourceDetail
    ResourceDetail (ResourceDetail'),
    newResourceDetail,

    -- ** ResourceTargetDefinition
    ResourceTargetDefinition (ResourceTargetDefinition'),
    newResourceTargetDefinition,

    -- ** ServiceActionAssociation
    ServiceActionAssociation (ServiceActionAssociation'),
    newServiceActionAssociation,

    -- ** ServiceActionDetail
    ServiceActionDetail (ServiceActionDetail'),
    newServiceActionDetail,

    -- ** ServiceActionSummary
    ServiceActionSummary (ServiceActionSummary'),
    newServiceActionSummary,

    -- ** ShareDetails
    ShareDetails (ShareDetails'),
    newShareDetails,

    -- ** ShareError
    ShareError (ShareError'),
    newShareError,

    -- ** SourceConnection
    SourceConnection (SourceConnection'),
    newSourceConnection,

    -- ** SourceConnectionDetail
    SourceConnectionDetail (SourceConnectionDetail'),
    newSourceConnectionDetail,

    -- ** SourceConnectionParameters
    SourceConnectionParameters (SourceConnectionParameters'),
    newSourceConnectionParameters,

    -- ** StackInstance
    StackInstance (StackInstance'),
    newStackInstance,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagOptionDetail
    TagOptionDetail (TagOptionDetail'),
    newTagOptionDetail,

    -- ** TagOptionSummary
    TagOptionSummary (TagOptionSummary'),
    newTagOptionSummary,

    -- ** UpdateProvisioningParameter
    UpdateProvisioningParameter (UpdateProvisioningParameter'),
    newUpdateProvisioningParameter,

    -- ** UpdateProvisioningPreferences
    UpdateProvisioningPreferences (UpdateProvisioningPreferences'),
    newUpdateProvisioningPreferences,

    -- ** UsageInstruction
    UsageInstruction (UsageInstruction'),
    newUsageInstruction,
  )
where

import Amazonka.ServiceCatalog.AcceptPortfolioShare
import Amazonka.ServiceCatalog.AssociateBudgetWithResource
import Amazonka.ServiceCatalog.AssociatePrincipalWithPortfolio
import Amazonka.ServiceCatalog.AssociateProductWithPortfolio
import Amazonka.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
import Amazonka.ServiceCatalog.AssociateTagOptionWithResource
import Amazonka.ServiceCatalog.BatchAssociateServiceActionWithProvisioningArtifact
import Amazonka.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
import Amazonka.ServiceCatalog.CopyProduct
import Amazonka.ServiceCatalog.CreateConstraint
import Amazonka.ServiceCatalog.CreatePortfolio
import Amazonka.ServiceCatalog.CreatePortfolioShare
import Amazonka.ServiceCatalog.CreateProduct
import Amazonka.ServiceCatalog.CreateProvisionedProductPlan
import Amazonka.ServiceCatalog.CreateProvisioningArtifact
import Amazonka.ServiceCatalog.CreateServiceAction
import Amazonka.ServiceCatalog.CreateTagOption
import Amazonka.ServiceCatalog.DeleteConstraint
import Amazonka.ServiceCatalog.DeletePortfolio
import Amazonka.ServiceCatalog.DeletePortfolioShare
import Amazonka.ServiceCatalog.DeleteProduct
import Amazonka.ServiceCatalog.DeleteProvisionedProductPlan
import Amazonka.ServiceCatalog.DeleteProvisioningArtifact
import Amazonka.ServiceCatalog.DeleteServiceAction
import Amazonka.ServiceCatalog.DeleteTagOption
import Amazonka.ServiceCatalog.DescribeConstraint
import Amazonka.ServiceCatalog.DescribeCopyProductStatus
import Amazonka.ServiceCatalog.DescribePortfolio
import Amazonka.ServiceCatalog.DescribePortfolioShareStatus
import Amazonka.ServiceCatalog.DescribePortfolioShares
import Amazonka.ServiceCatalog.DescribeProduct
import Amazonka.ServiceCatalog.DescribeProductAsAdmin
import Amazonka.ServiceCatalog.DescribeProductView
import Amazonka.ServiceCatalog.DescribeProvisionedProduct
import Amazonka.ServiceCatalog.DescribeProvisionedProductPlan
import Amazonka.ServiceCatalog.DescribeProvisioningArtifact
import Amazonka.ServiceCatalog.DescribeProvisioningParameters
import Amazonka.ServiceCatalog.DescribeRecord
import Amazonka.ServiceCatalog.DescribeServiceAction
import Amazonka.ServiceCatalog.DescribeServiceActionExecutionParameters
import Amazonka.ServiceCatalog.DescribeTagOption
import Amazonka.ServiceCatalog.DisableAWSOrganizationsAccess
import Amazonka.ServiceCatalog.DisassociateBudgetFromResource
import Amazonka.ServiceCatalog.DisassociatePrincipalFromPortfolio
import Amazonka.ServiceCatalog.DisassociateProductFromPortfolio
import Amazonka.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
import Amazonka.ServiceCatalog.DisassociateTagOptionFromResource
import Amazonka.ServiceCatalog.EnableAWSOrganizationsAccess
import Amazonka.ServiceCatalog.ExecuteProvisionedProductPlan
import Amazonka.ServiceCatalog.ExecuteProvisionedProductServiceAction
import Amazonka.ServiceCatalog.GetAWSOrganizationsAccessStatus
import Amazonka.ServiceCatalog.GetProvisionedProductOutputs
import Amazonka.ServiceCatalog.ImportAsProvisionedProduct
import Amazonka.ServiceCatalog.Lens
import Amazonka.ServiceCatalog.ListAcceptedPortfolioShares
import Amazonka.ServiceCatalog.ListBudgetsForResource
import Amazonka.ServiceCatalog.ListConstraintsForPortfolio
import Amazonka.ServiceCatalog.ListLaunchPaths
import Amazonka.ServiceCatalog.ListOrganizationPortfolioAccess
import Amazonka.ServiceCatalog.ListPortfolioAccess
import Amazonka.ServiceCatalog.ListPortfolios
import Amazonka.ServiceCatalog.ListPortfoliosForProduct
import Amazonka.ServiceCatalog.ListPrincipalsForPortfolio
import Amazonka.ServiceCatalog.ListProvisionedProductPlans
import Amazonka.ServiceCatalog.ListProvisioningArtifacts
import Amazonka.ServiceCatalog.ListProvisioningArtifactsForServiceAction
import Amazonka.ServiceCatalog.ListRecordHistory
import Amazonka.ServiceCatalog.ListResourcesForTagOption
import Amazonka.ServiceCatalog.ListServiceActions
import Amazonka.ServiceCatalog.ListServiceActionsForProvisioningArtifact
import Amazonka.ServiceCatalog.ListStackInstancesForProvisionedProduct
import Amazonka.ServiceCatalog.ListTagOptions
import Amazonka.ServiceCatalog.ProvisionProduct
import Amazonka.ServiceCatalog.RejectPortfolioShare
import Amazonka.ServiceCatalog.ScanProvisionedProducts
import Amazonka.ServiceCatalog.SearchProducts
import Amazonka.ServiceCatalog.SearchProductsAsAdmin
import Amazonka.ServiceCatalog.SearchProvisionedProducts
import Amazonka.ServiceCatalog.TerminateProvisionedProduct
import Amazonka.ServiceCatalog.Types
import Amazonka.ServiceCatalog.UpdateConstraint
import Amazonka.ServiceCatalog.UpdatePortfolio
import Amazonka.ServiceCatalog.UpdatePortfolioShare
import Amazonka.ServiceCatalog.UpdateProduct
import Amazonka.ServiceCatalog.UpdateProvisionedProduct
import Amazonka.ServiceCatalog.UpdateProvisionedProductProperties
import Amazonka.ServiceCatalog.UpdateProvisioningArtifact
import Amazonka.ServiceCatalog.UpdateServiceAction
import Amazonka.ServiceCatalog.UpdateTagOption
import Amazonka.ServiceCatalog.Waiters

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
