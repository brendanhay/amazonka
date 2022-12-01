{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalog.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Lens
  ( -- * Operations

    -- ** AcceptPortfolioShare
    acceptPortfolioShare_portfolioShareType,
    acceptPortfolioShare_acceptLanguage,
    acceptPortfolioShare_portfolioId,
    acceptPortfolioShareResponse_httpStatus,

    -- ** AssociateBudgetWithResource
    associateBudgetWithResource_budgetName,
    associateBudgetWithResource_resourceId,
    associateBudgetWithResourceResponse_httpStatus,

    -- ** AssociatePrincipalWithPortfolio
    associatePrincipalWithPortfolio_acceptLanguage,
    associatePrincipalWithPortfolio_portfolioId,
    associatePrincipalWithPortfolio_principalARN,
    associatePrincipalWithPortfolio_principalType,
    associatePrincipalWithPortfolioResponse_httpStatus,

    -- ** AssociateProductWithPortfolio
    associateProductWithPortfolio_sourcePortfolioId,
    associateProductWithPortfolio_acceptLanguage,
    associateProductWithPortfolio_productId,
    associateProductWithPortfolio_portfolioId,
    associateProductWithPortfolioResponse_httpStatus,

    -- ** AssociateServiceActionWithProvisioningArtifact
    associateServiceActionWithProvisioningArtifact_acceptLanguage,
    associateServiceActionWithProvisioningArtifact_productId,
    associateServiceActionWithProvisioningArtifact_provisioningArtifactId,
    associateServiceActionWithProvisioningArtifact_serviceActionId,
    associateServiceActionWithProvisioningArtifactResponse_httpStatus,

    -- ** AssociateTagOptionWithResource
    associateTagOptionWithResource_resourceId,
    associateTagOptionWithResource_tagOptionId,
    associateTagOptionWithResourceResponse_httpStatus,

    -- ** BatchAssociateServiceActionWithProvisioningArtifact
    batchAssociateServiceActionWithProvisioningArtifact_acceptLanguage,
    batchAssociateServiceActionWithProvisioningArtifact_serviceActionAssociations,
    batchAssociateServiceActionWithProvisioningArtifactResponse_failedServiceActionAssociations,
    batchAssociateServiceActionWithProvisioningArtifactResponse_httpStatus,

    -- ** BatchDisassociateServiceActionFromProvisioningArtifact
    batchDisassociateServiceActionFromProvisioningArtifact_acceptLanguage,
    batchDisassociateServiceActionFromProvisioningArtifact_serviceActionAssociations,
    batchDisassociateServiceActionFromProvisioningArtifactResponse_failedServiceActionAssociations,
    batchDisassociateServiceActionFromProvisioningArtifactResponse_httpStatus,

    -- ** CopyProduct
    copyProduct_targetProductName,
    copyProduct_targetProductId,
    copyProduct_copyOptions,
    copyProduct_acceptLanguage,
    copyProduct_sourceProvisioningArtifactIdentifiers,
    copyProduct_sourceProductArn,
    copyProduct_idempotencyToken,
    copyProductResponse_copyProductToken,
    copyProductResponse_httpStatus,

    -- ** CreateConstraint
    createConstraint_description,
    createConstraint_acceptLanguage,
    createConstraint_portfolioId,
    createConstraint_productId,
    createConstraint_parameters,
    createConstraint_type,
    createConstraint_idempotencyToken,
    createConstraintResponse_constraintDetail,
    createConstraintResponse_status,
    createConstraintResponse_constraintParameters,
    createConstraintResponse_httpStatus,

    -- ** CreatePortfolio
    createPortfolio_tags,
    createPortfolio_description,
    createPortfolio_acceptLanguage,
    createPortfolio_displayName,
    createPortfolio_providerName,
    createPortfolio_idempotencyToken,
    createPortfolioResponse_tags,
    createPortfolioResponse_portfolioDetail,
    createPortfolioResponse_httpStatus,

    -- ** CreatePortfolioShare
    createPortfolioShare_accountId,
    createPortfolioShare_organizationNode,
    createPortfolioShare_sharePrincipals,
    createPortfolioShare_acceptLanguage,
    createPortfolioShare_shareTagOptions,
    createPortfolioShare_portfolioId,
    createPortfolioShareResponse_portfolioShareToken,
    createPortfolioShareResponse_httpStatus,

    -- ** CreateProduct
    createProduct_tags,
    createProduct_supportDescription,
    createProduct_supportEmail,
    createProduct_supportUrl,
    createProduct_description,
    createProduct_provisioningArtifactParameters,
    createProduct_distributor,
    createProduct_sourceConnection,
    createProduct_acceptLanguage,
    createProduct_name,
    createProduct_owner,
    createProduct_productType,
    createProduct_idempotencyToken,
    createProductResponse_tags,
    createProductResponse_productViewDetail,
    createProductResponse_provisioningArtifactDetail,
    createProductResponse_httpStatus,

    -- ** CreateProvisionedProductPlan
    createProvisionedProductPlan_tags,
    createProvisionedProductPlan_pathId,
    createProvisionedProductPlan_notificationArns,
    createProvisionedProductPlan_provisioningParameters,
    createProvisionedProductPlan_acceptLanguage,
    createProvisionedProductPlan_planName,
    createProvisionedProductPlan_planType,
    createProvisionedProductPlan_productId,
    createProvisionedProductPlan_provisionedProductName,
    createProvisionedProductPlan_provisioningArtifactId,
    createProvisionedProductPlan_idempotencyToken,
    createProvisionedProductPlanResponse_planId,
    createProvisionedProductPlanResponse_provisionProductId,
    createProvisionedProductPlanResponse_planName,
    createProvisionedProductPlanResponse_provisionedProductName,
    createProvisionedProductPlanResponse_provisioningArtifactId,
    createProvisionedProductPlanResponse_httpStatus,

    -- ** CreateProvisioningArtifact
    createProvisioningArtifact_acceptLanguage,
    createProvisioningArtifact_productId,
    createProvisioningArtifact_parameters,
    createProvisioningArtifact_idempotencyToken,
    createProvisioningArtifactResponse_info,
    createProvisioningArtifactResponse_status,
    createProvisioningArtifactResponse_provisioningArtifactDetail,
    createProvisioningArtifactResponse_httpStatus,

    -- ** CreateServiceAction
    createServiceAction_description,
    createServiceAction_acceptLanguage,
    createServiceAction_name,
    createServiceAction_definitionType,
    createServiceAction_definition,
    createServiceAction_idempotencyToken,
    createServiceActionResponse_serviceActionDetail,
    createServiceActionResponse_httpStatus,

    -- ** CreateTagOption
    createTagOption_key,
    createTagOption_value,
    createTagOptionResponse_tagOptionDetail,
    createTagOptionResponse_httpStatus,

    -- ** DeleteConstraint
    deleteConstraint_acceptLanguage,
    deleteConstraint_id,
    deleteConstraintResponse_httpStatus,

    -- ** DeletePortfolio
    deletePortfolio_acceptLanguage,
    deletePortfolio_id,
    deletePortfolioResponse_httpStatus,

    -- ** DeletePortfolioShare
    deletePortfolioShare_accountId,
    deletePortfolioShare_organizationNode,
    deletePortfolioShare_acceptLanguage,
    deletePortfolioShare_portfolioId,
    deletePortfolioShareResponse_portfolioShareToken,
    deletePortfolioShareResponse_httpStatus,

    -- ** DeleteProduct
    deleteProduct_acceptLanguage,
    deleteProduct_id,
    deleteProductResponse_httpStatus,

    -- ** DeleteProvisionedProductPlan
    deleteProvisionedProductPlan_ignoreErrors,
    deleteProvisionedProductPlan_acceptLanguage,
    deleteProvisionedProductPlan_planId,
    deleteProvisionedProductPlanResponse_httpStatus,

    -- ** DeleteProvisioningArtifact
    deleteProvisioningArtifact_acceptLanguage,
    deleteProvisioningArtifact_productId,
    deleteProvisioningArtifact_provisioningArtifactId,
    deleteProvisioningArtifactResponse_httpStatus,

    -- ** DeleteServiceAction
    deleteServiceAction_acceptLanguage,
    deleteServiceAction_id,
    deleteServiceActionResponse_httpStatus,

    -- ** DeleteTagOption
    deleteTagOption_id,
    deleteTagOptionResponse_httpStatus,

    -- ** DescribeConstraint
    describeConstraint_acceptLanguage,
    describeConstraint_id,
    describeConstraintResponse_constraintDetail,
    describeConstraintResponse_status,
    describeConstraintResponse_constraintParameters,
    describeConstraintResponse_httpStatus,

    -- ** DescribeCopyProductStatus
    describeCopyProductStatus_acceptLanguage,
    describeCopyProductStatus_copyProductToken,
    describeCopyProductStatusResponse_targetProductId,
    describeCopyProductStatusResponse_statusDetail,
    describeCopyProductStatusResponse_copyProductStatus,
    describeCopyProductStatusResponse_httpStatus,

    -- ** DescribePortfolio
    describePortfolio_acceptLanguage,
    describePortfolio_id,
    describePortfolioResponse_tags,
    describePortfolioResponse_portfolioDetail,
    describePortfolioResponse_budgets,
    describePortfolioResponse_tagOptions,
    describePortfolioResponse_httpStatus,

    -- ** DescribePortfolioShareStatus
    describePortfolioShareStatus_portfolioShareToken,
    describePortfolioShareStatusResponse_portfolioId,
    describePortfolioShareStatusResponse_shareDetails,
    describePortfolioShareStatusResponse_status,
    describePortfolioShareStatusResponse_portfolioShareToken,
    describePortfolioShareStatusResponse_organizationNodeValue,
    describePortfolioShareStatusResponse_httpStatus,

    -- ** DescribePortfolioShares
    describePortfolioShares_pageToken,
    describePortfolioShares_pageSize,
    describePortfolioShares_portfolioId,
    describePortfolioShares_type,
    describePortfolioSharesResponse_portfolioShareDetails,
    describePortfolioSharesResponse_nextPageToken,
    describePortfolioSharesResponse_httpStatus,

    -- ** DescribeProduct
    describeProduct_name,
    describeProduct_id,
    describeProduct_acceptLanguage,
    describeProductResponse_budgets,
    describeProductResponse_launchPaths,
    describeProductResponse_productViewSummary,
    describeProductResponse_provisioningArtifacts,
    describeProductResponse_httpStatus,

    -- ** DescribeProductAsAdmin
    describeProductAsAdmin_name,
    describeProductAsAdmin_id,
    describeProductAsAdmin_sourcePortfolioId,
    describeProductAsAdmin_acceptLanguage,
    describeProductAsAdminResponse_tags,
    describeProductAsAdminResponse_provisioningArtifactSummaries,
    describeProductAsAdminResponse_budgets,
    describeProductAsAdminResponse_tagOptions,
    describeProductAsAdminResponse_productViewDetail,
    describeProductAsAdminResponse_httpStatus,

    -- ** DescribeProductView
    describeProductView_acceptLanguage,
    describeProductView_id,
    describeProductViewResponse_productViewSummary,
    describeProductViewResponse_provisioningArtifacts,
    describeProductViewResponse_httpStatus,

    -- ** DescribeProvisionedProduct
    describeProvisionedProduct_name,
    describeProvisionedProduct_id,
    describeProvisionedProduct_acceptLanguage,
    describeProvisionedProductResponse_cloudWatchDashboards,
    describeProvisionedProductResponse_provisionedProductDetail,
    describeProvisionedProductResponse_httpStatus,

    -- ** DescribeProvisionedProductPlan
    describeProvisionedProductPlan_pageToken,
    describeProvisionedProductPlan_pageSize,
    describeProvisionedProductPlan_acceptLanguage,
    describeProvisionedProductPlan_planId,
    describeProvisionedProductPlanResponse_nextPageToken,
    describeProvisionedProductPlanResponse_provisionedProductPlanDetails,
    describeProvisionedProductPlanResponse_resourceChanges,
    describeProvisionedProductPlanResponse_httpStatus,

    -- ** DescribeProvisioningArtifact
    describeProvisioningArtifact_productName,
    describeProvisioningArtifact_verbose,
    describeProvisioningArtifact_productId,
    describeProvisioningArtifact_provisioningArtifactName,
    describeProvisioningArtifact_provisioningArtifactId,
    describeProvisioningArtifact_acceptLanguage,
    describeProvisioningArtifactResponse_info,
    describeProvisioningArtifactResponse_status,
    describeProvisioningArtifactResponse_provisioningArtifactDetail,
    describeProvisioningArtifactResponse_httpStatus,

    -- ** DescribeProvisioningParameters
    describeProvisioningParameters_productName,
    describeProvisioningParameters_pathId,
    describeProvisioningParameters_productId,
    describeProvisioningParameters_pathName,
    describeProvisioningParameters_provisioningArtifactName,
    describeProvisioningParameters_provisioningArtifactId,
    describeProvisioningParameters_acceptLanguage,
    describeProvisioningParametersResponse_constraintSummaries,
    describeProvisioningParametersResponse_usageInstructions,
    describeProvisioningParametersResponse_provisioningArtifactOutputs,
    describeProvisioningParametersResponse_tagOptions,
    describeProvisioningParametersResponse_provisioningArtifactParameters,
    describeProvisioningParametersResponse_provisioningArtifactPreferences,
    describeProvisioningParametersResponse_provisioningArtifactOutputKeys,
    describeProvisioningParametersResponse_httpStatus,

    -- ** DescribeRecord
    describeRecord_pageToken,
    describeRecord_pageSize,
    describeRecord_acceptLanguage,
    describeRecord_id,
    describeRecordResponse_nextPageToken,
    describeRecordResponse_recordDetail,
    describeRecordResponse_recordOutputs,
    describeRecordResponse_httpStatus,

    -- ** DescribeServiceAction
    describeServiceAction_acceptLanguage,
    describeServiceAction_id,
    describeServiceActionResponse_serviceActionDetail,
    describeServiceActionResponse_httpStatus,

    -- ** DescribeServiceActionExecutionParameters
    describeServiceActionExecutionParameters_acceptLanguage,
    describeServiceActionExecutionParameters_provisionedProductId,
    describeServiceActionExecutionParameters_serviceActionId,
    describeServiceActionExecutionParametersResponse_serviceActionParameters,
    describeServiceActionExecutionParametersResponse_httpStatus,

    -- ** DescribeTagOption
    describeTagOption_id,
    describeTagOptionResponse_tagOptionDetail,
    describeTagOptionResponse_httpStatus,

    -- ** DisableAWSOrganizationsAccess
    disableAWSOrganizationsAccessResponse_httpStatus,

    -- ** DisassociateBudgetFromResource
    disassociateBudgetFromResource_budgetName,
    disassociateBudgetFromResource_resourceId,
    disassociateBudgetFromResourceResponse_httpStatus,

    -- ** DisassociatePrincipalFromPortfolio
    disassociatePrincipalFromPortfolio_principalType,
    disassociatePrincipalFromPortfolio_acceptLanguage,
    disassociatePrincipalFromPortfolio_portfolioId,
    disassociatePrincipalFromPortfolio_principalARN,
    disassociatePrincipalFromPortfolioResponse_httpStatus,

    -- ** DisassociateProductFromPortfolio
    disassociateProductFromPortfolio_acceptLanguage,
    disassociateProductFromPortfolio_productId,
    disassociateProductFromPortfolio_portfolioId,
    disassociateProductFromPortfolioResponse_httpStatus,

    -- ** DisassociateServiceActionFromProvisioningArtifact
    disassociateServiceActionFromProvisioningArtifact_acceptLanguage,
    disassociateServiceActionFromProvisioningArtifact_productId,
    disassociateServiceActionFromProvisioningArtifact_provisioningArtifactId,
    disassociateServiceActionFromProvisioningArtifact_serviceActionId,
    disassociateServiceActionFromProvisioningArtifactResponse_httpStatus,

    -- ** DisassociateTagOptionFromResource
    disassociateTagOptionFromResource_resourceId,
    disassociateTagOptionFromResource_tagOptionId,
    disassociateTagOptionFromResourceResponse_httpStatus,

    -- ** EnableAWSOrganizationsAccess
    enableAWSOrganizationsAccessResponse_httpStatus,

    -- ** ExecuteProvisionedProductPlan
    executeProvisionedProductPlan_acceptLanguage,
    executeProvisionedProductPlan_planId,
    executeProvisionedProductPlan_idempotencyToken,
    executeProvisionedProductPlanResponse_recordDetail,
    executeProvisionedProductPlanResponse_httpStatus,

    -- ** ExecuteProvisionedProductServiceAction
    executeProvisionedProductServiceAction_acceptLanguage,
    executeProvisionedProductServiceAction_parameters,
    executeProvisionedProductServiceAction_provisionedProductId,
    executeProvisionedProductServiceAction_serviceActionId,
    executeProvisionedProductServiceAction_executeToken,
    executeProvisionedProductServiceActionResponse_recordDetail,
    executeProvisionedProductServiceActionResponse_httpStatus,

    -- ** GetAWSOrganizationsAccessStatus
    getAWSOrganizationsAccessStatusResponse_accessStatus,
    getAWSOrganizationsAccessStatusResponse_httpStatus,

    -- ** GetProvisionedProductOutputs
    getProvisionedProductOutputs_provisionedProductName,
    getProvisionedProductOutputs_pageToken,
    getProvisionedProductOutputs_pageSize,
    getProvisionedProductOutputs_outputKeys,
    getProvisionedProductOutputs_acceptLanguage,
    getProvisionedProductOutputs_provisionedProductId,
    getProvisionedProductOutputsResponse_nextPageToken,
    getProvisionedProductOutputsResponse_outputs,
    getProvisionedProductOutputsResponse_httpStatus,

    -- ** ImportAsProvisionedProduct
    importAsProvisionedProduct_acceptLanguage,
    importAsProvisionedProduct_productId,
    importAsProvisionedProduct_provisioningArtifactId,
    importAsProvisionedProduct_provisionedProductName,
    importAsProvisionedProduct_physicalId,
    importAsProvisionedProduct_idempotencyToken,
    importAsProvisionedProductResponse_recordDetail,
    importAsProvisionedProductResponse_httpStatus,

    -- ** ListAcceptedPortfolioShares
    listAcceptedPortfolioShares_pageToken,
    listAcceptedPortfolioShares_pageSize,
    listAcceptedPortfolioShares_portfolioShareType,
    listAcceptedPortfolioShares_acceptLanguage,
    listAcceptedPortfolioSharesResponse_nextPageToken,
    listAcceptedPortfolioSharesResponse_portfolioDetails,
    listAcceptedPortfolioSharesResponse_httpStatus,

    -- ** ListBudgetsForResource
    listBudgetsForResource_pageToken,
    listBudgetsForResource_pageSize,
    listBudgetsForResource_acceptLanguage,
    listBudgetsForResource_resourceId,
    listBudgetsForResourceResponse_nextPageToken,
    listBudgetsForResourceResponse_budgets,
    listBudgetsForResourceResponse_httpStatus,

    -- ** ListConstraintsForPortfolio
    listConstraintsForPortfolio_productId,
    listConstraintsForPortfolio_pageToken,
    listConstraintsForPortfolio_pageSize,
    listConstraintsForPortfolio_acceptLanguage,
    listConstraintsForPortfolio_portfolioId,
    listConstraintsForPortfolioResponse_nextPageToken,
    listConstraintsForPortfolioResponse_constraintDetails,
    listConstraintsForPortfolioResponse_httpStatus,

    -- ** ListLaunchPaths
    listLaunchPaths_pageToken,
    listLaunchPaths_pageSize,
    listLaunchPaths_acceptLanguage,
    listLaunchPaths_productId,
    listLaunchPathsResponse_nextPageToken,
    listLaunchPathsResponse_launchPathSummaries,
    listLaunchPathsResponse_httpStatus,

    -- ** ListOrganizationPortfolioAccess
    listOrganizationPortfolioAccess_pageToken,
    listOrganizationPortfolioAccess_pageSize,
    listOrganizationPortfolioAccess_acceptLanguage,
    listOrganizationPortfolioAccess_portfolioId,
    listOrganizationPortfolioAccess_organizationNodeType,
    listOrganizationPortfolioAccessResponse_nextPageToken,
    listOrganizationPortfolioAccessResponse_organizationNodes,
    listOrganizationPortfolioAccessResponse_httpStatus,

    -- ** ListPortfolioAccess
    listPortfolioAccess_organizationParentId,
    listPortfolioAccess_pageToken,
    listPortfolioAccess_pageSize,
    listPortfolioAccess_acceptLanguage,
    listPortfolioAccess_portfolioId,
    listPortfolioAccessResponse_accountIds,
    listPortfolioAccessResponse_nextPageToken,
    listPortfolioAccessResponse_httpStatus,

    -- ** ListPortfolios
    listPortfolios_pageToken,
    listPortfolios_pageSize,
    listPortfolios_acceptLanguage,
    listPortfoliosResponse_nextPageToken,
    listPortfoliosResponse_portfolioDetails,
    listPortfoliosResponse_httpStatus,

    -- ** ListPortfoliosForProduct
    listPortfoliosForProduct_pageToken,
    listPortfoliosForProduct_pageSize,
    listPortfoliosForProduct_acceptLanguage,
    listPortfoliosForProduct_productId,
    listPortfoliosForProductResponse_nextPageToken,
    listPortfoliosForProductResponse_portfolioDetails,
    listPortfoliosForProductResponse_httpStatus,

    -- ** ListPrincipalsForPortfolio
    listPrincipalsForPortfolio_pageToken,
    listPrincipalsForPortfolio_pageSize,
    listPrincipalsForPortfolio_acceptLanguage,
    listPrincipalsForPortfolio_portfolioId,
    listPrincipalsForPortfolioResponse_nextPageToken,
    listPrincipalsForPortfolioResponse_principals,
    listPrincipalsForPortfolioResponse_httpStatus,

    -- ** ListProvisionedProductPlans
    listProvisionedProductPlans_accessLevelFilter,
    listProvisionedProductPlans_provisionProductId,
    listProvisionedProductPlans_pageToken,
    listProvisionedProductPlans_pageSize,
    listProvisionedProductPlans_acceptLanguage,
    listProvisionedProductPlansResponse_nextPageToken,
    listProvisionedProductPlansResponse_provisionedProductPlans,
    listProvisionedProductPlansResponse_httpStatus,

    -- ** ListProvisioningArtifacts
    listProvisioningArtifacts_acceptLanguage,
    listProvisioningArtifacts_productId,
    listProvisioningArtifactsResponse_nextPageToken,
    listProvisioningArtifactsResponse_provisioningArtifactDetails,
    listProvisioningArtifactsResponse_httpStatus,

    -- ** ListProvisioningArtifactsForServiceAction
    listProvisioningArtifactsForServiceAction_pageToken,
    listProvisioningArtifactsForServiceAction_pageSize,
    listProvisioningArtifactsForServiceAction_acceptLanguage,
    listProvisioningArtifactsForServiceAction_serviceActionId,
    listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews,
    listProvisioningArtifactsForServiceActionResponse_nextPageToken,
    listProvisioningArtifactsForServiceActionResponse_httpStatus,

    -- ** ListRecordHistory
    listRecordHistory_accessLevelFilter,
    listRecordHistory_searchFilter,
    listRecordHistory_pageToken,
    listRecordHistory_pageSize,
    listRecordHistory_acceptLanguage,
    listRecordHistoryResponse_nextPageToken,
    listRecordHistoryResponse_recordDetails,
    listRecordHistoryResponse_httpStatus,

    -- ** ListResourcesForTagOption
    listResourcesForTagOption_resourceType,
    listResourcesForTagOption_pageToken,
    listResourcesForTagOption_pageSize,
    listResourcesForTagOption_tagOptionId,
    listResourcesForTagOptionResponse_pageToken,
    listResourcesForTagOptionResponse_resourceDetails,
    listResourcesForTagOptionResponse_httpStatus,

    -- ** ListServiceActions
    listServiceActions_pageToken,
    listServiceActions_pageSize,
    listServiceActions_acceptLanguage,
    listServiceActionsResponse_nextPageToken,
    listServiceActionsResponse_serviceActionSummaries,
    listServiceActionsResponse_httpStatus,

    -- ** ListServiceActionsForProvisioningArtifact
    listServiceActionsForProvisioningArtifact_pageToken,
    listServiceActionsForProvisioningArtifact_pageSize,
    listServiceActionsForProvisioningArtifact_acceptLanguage,
    listServiceActionsForProvisioningArtifact_productId,
    listServiceActionsForProvisioningArtifact_provisioningArtifactId,
    listServiceActionsForProvisioningArtifactResponse_nextPageToken,
    listServiceActionsForProvisioningArtifactResponse_serviceActionSummaries,
    listServiceActionsForProvisioningArtifactResponse_httpStatus,

    -- ** ListStackInstancesForProvisionedProduct
    listStackInstancesForProvisionedProduct_pageToken,
    listStackInstancesForProvisionedProduct_pageSize,
    listStackInstancesForProvisionedProduct_acceptLanguage,
    listStackInstancesForProvisionedProduct_provisionedProductId,
    listStackInstancesForProvisionedProductResponse_nextPageToken,
    listStackInstancesForProvisionedProductResponse_stackInstances,
    listStackInstancesForProvisionedProductResponse_httpStatus,

    -- ** ListTagOptions
    listTagOptions_filters,
    listTagOptions_pageToken,
    listTagOptions_pageSize,
    listTagOptionsResponse_tagOptionDetails,
    listTagOptionsResponse_pageToken,
    listTagOptionsResponse_httpStatus,

    -- ** ProvisionProduct
    provisionProduct_tags,
    provisionProduct_productName,
    provisionProduct_pathId,
    provisionProduct_notificationArns,
    provisionProduct_productId,
    provisionProduct_pathName,
    provisionProduct_provisioningParameters,
    provisionProduct_provisioningPreferences,
    provisionProduct_provisioningArtifactName,
    provisionProduct_provisioningArtifactId,
    provisionProduct_acceptLanguage,
    provisionProduct_provisionedProductName,
    provisionProduct_provisionToken,
    provisionProductResponse_recordDetail,
    provisionProductResponse_httpStatus,

    -- ** RejectPortfolioShare
    rejectPortfolioShare_portfolioShareType,
    rejectPortfolioShare_acceptLanguage,
    rejectPortfolioShare_portfolioId,
    rejectPortfolioShareResponse_httpStatus,

    -- ** ScanProvisionedProducts
    scanProvisionedProducts_accessLevelFilter,
    scanProvisionedProducts_pageToken,
    scanProvisionedProducts_pageSize,
    scanProvisionedProducts_acceptLanguage,
    scanProvisionedProductsResponse_nextPageToken,
    scanProvisionedProductsResponse_provisionedProducts,
    scanProvisionedProductsResponse_httpStatus,

    -- ** SearchProducts
    searchProducts_sortOrder,
    searchProducts_filters,
    searchProducts_sortBy,
    searchProducts_pageToken,
    searchProducts_pageSize,
    searchProducts_acceptLanguage,
    searchProductsResponse_nextPageToken,
    searchProductsResponse_productViewSummaries,
    searchProductsResponse_productViewAggregations,
    searchProductsResponse_httpStatus,

    -- ** SearchProductsAsAdmin
    searchProductsAsAdmin_portfolioId,
    searchProductsAsAdmin_sortOrder,
    searchProductsAsAdmin_filters,
    searchProductsAsAdmin_sortBy,
    searchProductsAsAdmin_pageToken,
    searchProductsAsAdmin_productSource,
    searchProductsAsAdmin_pageSize,
    searchProductsAsAdmin_acceptLanguage,
    searchProductsAsAdminResponse_nextPageToken,
    searchProductsAsAdminResponse_productViewDetails,
    searchProductsAsAdminResponse_httpStatus,

    -- ** SearchProvisionedProducts
    searchProvisionedProducts_sortOrder,
    searchProvisionedProducts_accessLevelFilter,
    searchProvisionedProducts_filters,
    searchProvisionedProducts_sortBy,
    searchProvisionedProducts_pageToken,
    searchProvisionedProducts_pageSize,
    searchProvisionedProducts_acceptLanguage,
    searchProvisionedProductsResponse_nextPageToken,
    searchProvisionedProductsResponse_totalResultsCount,
    searchProvisionedProductsResponse_provisionedProducts,
    searchProvisionedProductsResponse_httpStatus,

    -- ** TerminateProvisionedProduct
    terminateProvisionedProduct_ignoreErrors,
    terminateProvisionedProduct_provisionedProductName,
    terminateProvisionedProduct_acceptLanguage,
    terminateProvisionedProduct_retainPhysicalResources,
    terminateProvisionedProduct_provisionedProductId,
    terminateProvisionedProduct_terminateToken,
    terminateProvisionedProductResponse_recordDetail,
    terminateProvisionedProductResponse_httpStatus,

    -- ** UpdateConstraint
    updateConstraint_description,
    updateConstraint_acceptLanguage,
    updateConstraint_parameters,
    updateConstraint_id,
    updateConstraintResponse_constraintDetail,
    updateConstraintResponse_status,
    updateConstraintResponse_constraintParameters,
    updateConstraintResponse_httpStatus,

    -- ** UpdatePortfolio
    updatePortfolio_addTags,
    updatePortfolio_removeTags,
    updatePortfolio_providerName,
    updatePortfolio_displayName,
    updatePortfolio_description,
    updatePortfolio_acceptLanguage,
    updatePortfolio_id,
    updatePortfolioResponse_tags,
    updatePortfolioResponse_portfolioDetail,
    updatePortfolioResponse_httpStatus,

    -- ** UpdatePortfolioShare
    updatePortfolioShare_accountId,
    updatePortfolioShare_organizationNode,
    updatePortfolioShare_sharePrincipals,
    updatePortfolioShare_acceptLanguage,
    updatePortfolioShare_shareTagOptions,
    updatePortfolioShare_portfolioId,
    updatePortfolioShareResponse_status,
    updatePortfolioShareResponse_portfolioShareToken,
    updatePortfolioShareResponse_httpStatus,

    -- ** UpdateProduct
    updateProduct_addTags,
    updateProduct_removeTags,
    updateProduct_supportDescription,
    updateProduct_name,
    updateProduct_supportEmail,
    updateProduct_supportUrl,
    updateProduct_owner,
    updateProduct_description,
    updateProduct_distributor,
    updateProduct_sourceConnection,
    updateProduct_acceptLanguage,
    updateProduct_id,
    updateProductResponse_tags,
    updateProductResponse_productViewDetail,
    updateProductResponse_httpStatus,

    -- ** UpdateProvisionedProduct
    updateProvisionedProduct_tags,
    updateProvisionedProduct_productName,
    updateProvisionedProduct_pathId,
    updateProvisionedProduct_productId,
    updateProvisionedProduct_pathName,
    updateProvisionedProduct_provisionedProductName,
    updateProvisionedProduct_provisioningParameters,
    updateProvisionedProduct_provisioningPreferences,
    updateProvisionedProduct_provisioningArtifactName,
    updateProvisionedProduct_provisioningArtifactId,
    updateProvisionedProduct_acceptLanguage,
    updateProvisionedProduct_provisionedProductId,
    updateProvisionedProduct_updateToken,
    updateProvisionedProductResponse_recordDetail,
    updateProvisionedProductResponse_httpStatus,

    -- ** UpdateProvisionedProductProperties
    updateProvisionedProductProperties_acceptLanguage,
    updateProvisionedProductProperties_provisionedProductId,
    updateProvisionedProductProperties_provisionedProductProperties,
    updateProvisionedProductProperties_idempotencyToken,
    updateProvisionedProductPropertiesResponse_provisionedProductProperties,
    updateProvisionedProductPropertiesResponse_recordId,
    updateProvisionedProductPropertiesResponse_status,
    updateProvisionedProductPropertiesResponse_provisionedProductId,
    updateProvisionedProductPropertiesResponse_httpStatus,

    -- ** UpdateProvisioningArtifact
    updateProvisioningArtifact_name,
    updateProvisioningArtifact_active,
    updateProvisioningArtifact_description,
    updateProvisioningArtifact_guidance,
    updateProvisioningArtifact_acceptLanguage,
    updateProvisioningArtifact_productId,
    updateProvisioningArtifact_provisioningArtifactId,
    updateProvisioningArtifactResponse_info,
    updateProvisioningArtifactResponse_status,
    updateProvisioningArtifactResponse_provisioningArtifactDetail,
    updateProvisioningArtifactResponse_httpStatus,

    -- ** UpdateServiceAction
    updateServiceAction_name,
    updateServiceAction_description,
    updateServiceAction_acceptLanguage,
    updateServiceAction_definition,
    updateServiceAction_id,
    updateServiceActionResponse_serviceActionDetail,
    updateServiceActionResponse_httpStatus,

    -- ** UpdateTagOption
    updateTagOption_active,
    updateTagOption_value,
    updateTagOption_id,
    updateTagOptionResponse_tagOptionDetail,
    updateTagOptionResponse_httpStatus,

    -- * Types

    -- ** AccessLevelFilter
    accessLevelFilter_key,
    accessLevelFilter_value,

    -- ** BudgetDetail
    budgetDetail_budgetName,

    -- ** CloudWatchDashboard
    cloudWatchDashboard_name,

    -- ** CodeStarParameters
    codeStarParameters_connectionArn,
    codeStarParameters_repository,
    codeStarParameters_branch,
    codeStarParameters_artifactPath,

    -- ** ConstraintDetail
    constraintDetail_portfolioId,
    constraintDetail_constraintId,
    constraintDetail_type,
    constraintDetail_productId,
    constraintDetail_owner,
    constraintDetail_description,

    -- ** ConstraintSummary
    constraintSummary_type,
    constraintSummary_description,

    -- ** ExecutionParameter
    executionParameter_name,
    executionParameter_type,
    executionParameter_defaultValues,

    -- ** FailedServiceActionAssociation
    failedServiceActionAssociation_errorMessage,
    failedServiceActionAssociation_productId,
    failedServiceActionAssociation_serviceActionId,
    failedServiceActionAssociation_errorCode,
    failedServiceActionAssociation_provisioningArtifactId,

    -- ** LastSync
    lastSync_lastSyncTime,
    lastSync_lastSuccessfulSyncTime,
    lastSync_lastSuccessfulSyncProvisioningArtifactId,
    lastSync_lastSyncStatus,
    lastSync_lastSyncStatusMessage,

    -- ** LaunchPath
    launchPath_name,
    launchPath_id,

    -- ** LaunchPathSummary
    launchPathSummary_tags,
    launchPathSummary_constraintSummaries,
    launchPathSummary_name,
    launchPathSummary_id,

    -- ** ListRecordHistorySearchFilter
    listRecordHistorySearchFilter_key,
    listRecordHistorySearchFilter_value,

    -- ** ListTagOptionsFilters
    listTagOptionsFilters_key,
    listTagOptionsFilters_active,
    listTagOptionsFilters_value,

    -- ** OrganizationNode
    organizationNode_type,
    organizationNode_value,

    -- ** ParameterConstraints
    parameterConstraints_maxLength,
    parameterConstraints_allowedPattern,
    parameterConstraints_minValue,
    parameterConstraints_minLength,
    parameterConstraints_allowedValues,
    parameterConstraints_maxValue,
    parameterConstraints_constraintDescription,

    -- ** PortfolioDetail
    portfolioDetail_createdTime,
    portfolioDetail_providerName,
    portfolioDetail_arn,
    portfolioDetail_displayName,
    portfolioDetail_id,
    portfolioDetail_description,

    -- ** PortfolioShareDetail
    portfolioShareDetail_principalId,
    portfolioShareDetail_type,
    portfolioShareDetail_accepted,
    portfolioShareDetail_sharePrincipals,
    portfolioShareDetail_shareTagOptions,

    -- ** Principal
    principal_principalARN,
    principal_principalType,

    -- ** ProductViewAggregationValue
    productViewAggregationValue_approximateCount,
    productViewAggregationValue_value,

    -- ** ProductViewDetail
    productViewDetail_createdTime,
    productViewDetail_status,
    productViewDetail_productViewSummary,
    productViewDetail_productARN,
    productViewDetail_sourceConnection,

    -- ** ProductViewSummary
    productViewSummary_supportDescription,
    productViewSummary_name,
    productViewSummary_shortDescription,
    productViewSummary_type,
    productViewSummary_supportEmail,
    productViewSummary_supportUrl,
    productViewSummary_productId,
    productViewSummary_owner,
    productViewSummary_id,
    productViewSummary_distributor,
    productViewSummary_hasDefaultPath,

    -- ** ProvisionedProductAttribute
    provisionedProductAttribute_tags,
    provisionedProductAttribute_productName,
    provisionedProductAttribute_name,
    provisionedProductAttribute_type,
    provisionedProductAttribute_createdTime,
    provisionedProductAttribute_lastSuccessfulProvisioningRecordId,
    provisionedProductAttribute_lastRecordId,
    provisionedProductAttribute_idempotencyToken,
    provisionedProductAttribute_arn,
    provisionedProductAttribute_userArnSession,
    provisionedProductAttribute_productId,
    provisionedProductAttribute_status,
    provisionedProductAttribute_id,
    provisionedProductAttribute_userArn,
    provisionedProductAttribute_provisioningArtifactName,
    provisionedProductAttribute_lastProvisioningRecordId,
    provisionedProductAttribute_provisioningArtifactId,
    provisionedProductAttribute_statusMessage,
    provisionedProductAttribute_physicalId,

    -- ** ProvisionedProductDetail
    provisionedProductDetail_name,
    provisionedProductDetail_type,
    provisionedProductDetail_createdTime,
    provisionedProductDetail_lastSuccessfulProvisioningRecordId,
    provisionedProductDetail_lastRecordId,
    provisionedProductDetail_idempotencyToken,
    provisionedProductDetail_arn,
    provisionedProductDetail_productId,
    provisionedProductDetail_status,
    provisionedProductDetail_id,
    provisionedProductDetail_launchRoleArn,
    provisionedProductDetail_lastProvisioningRecordId,
    provisionedProductDetail_provisioningArtifactId,
    provisionedProductDetail_statusMessage,

    -- ** ProvisionedProductPlanDetails
    provisionedProductPlanDetails_tags,
    provisionedProductPlanDetails_pathId,
    provisionedProductPlanDetails_createdTime,
    provisionedProductPlanDetails_planId,
    provisionedProductPlanDetails_provisionProductId,
    provisionedProductPlanDetails_planType,
    provisionedProductPlanDetails_notificationArns,
    provisionedProductPlanDetails_planName,
    provisionedProductPlanDetails_productId,
    provisionedProductPlanDetails_status,
    provisionedProductPlanDetails_provisioningParameters,
    provisionedProductPlanDetails_provisioningArtifactId,
    provisionedProductPlanDetails_statusMessage,
    provisionedProductPlanDetails_provisionProductName,
    provisionedProductPlanDetails_updatedTime,

    -- ** ProvisionedProductPlanSummary
    provisionedProductPlanSummary_planId,
    provisionedProductPlanSummary_provisionProductId,
    provisionedProductPlanSummary_planType,
    provisionedProductPlanSummary_planName,
    provisionedProductPlanSummary_provisioningArtifactId,
    provisionedProductPlanSummary_provisionProductName,

    -- ** ProvisioningArtifact
    provisioningArtifact_name,
    provisioningArtifact_createdTime,
    provisioningArtifact_id,
    provisioningArtifact_description,
    provisioningArtifact_guidance,

    -- ** ProvisioningArtifactDetail
    provisioningArtifactDetail_name,
    provisioningArtifactDetail_type,
    provisioningArtifactDetail_createdTime,
    provisioningArtifactDetail_active,
    provisioningArtifactDetail_id,
    provisioningArtifactDetail_description,
    provisioningArtifactDetail_guidance,
    provisioningArtifactDetail_sourceRevision,

    -- ** ProvisioningArtifactOutput
    provisioningArtifactOutput_key,
    provisioningArtifactOutput_description,

    -- ** ProvisioningArtifactParameter
    provisioningArtifactParameter_isNoEcho,
    provisioningArtifactParameter_defaultValue,
    provisioningArtifactParameter_description,
    provisioningArtifactParameter_parameterConstraints,
    provisioningArtifactParameter_parameterType,
    provisioningArtifactParameter_parameterKey,

    -- ** ProvisioningArtifactPreferences
    provisioningArtifactPreferences_stackSetRegions,
    provisioningArtifactPreferences_stackSetAccounts,

    -- ** ProvisioningArtifactProperties
    provisioningArtifactProperties_name,
    provisioningArtifactProperties_type,
    provisioningArtifactProperties_disableTemplateValidation,
    provisioningArtifactProperties_info,
    provisioningArtifactProperties_description,

    -- ** ProvisioningArtifactSummary
    provisioningArtifactSummary_name,
    provisioningArtifactSummary_createdTime,
    provisioningArtifactSummary_id,
    provisioningArtifactSummary_description,
    provisioningArtifactSummary_provisioningArtifactMetadata,

    -- ** ProvisioningArtifactView
    provisioningArtifactView_provisioningArtifact,
    provisioningArtifactView_productViewSummary,

    -- ** ProvisioningParameter
    provisioningParameter_key,
    provisioningParameter_value,

    -- ** ProvisioningPreferences
    provisioningPreferences_stackSetRegions,
    provisioningPreferences_stackSetFailureToleranceCount,
    provisioningPreferences_stackSetMaxConcurrencyPercentage,
    provisioningPreferences_stackSetAccounts,
    provisioningPreferences_stackSetMaxConcurrencyCount,
    provisioningPreferences_stackSetFailureTolerancePercentage,

    -- ** RecordDetail
    recordDetail_pathId,
    recordDetail_createdTime,
    recordDetail_recordId,
    recordDetail_provisionedProductType,
    recordDetail_productId,
    recordDetail_provisionedProductName,
    recordDetail_status,
    recordDetail_recordType,
    recordDetail_launchRoleArn,
    recordDetail_recordTags,
    recordDetail_recordErrors,
    recordDetail_provisioningArtifactId,
    recordDetail_updatedTime,
    recordDetail_provisionedProductId,

    -- ** RecordError
    recordError_code,
    recordError_description,

    -- ** RecordOutput
    recordOutput_outputKey,
    recordOutput_description,
    recordOutput_outputValue,

    -- ** RecordTag
    recordTag_key,
    recordTag_value,

    -- ** ResourceChange
    resourceChange_resourceType,
    resourceChange_replacement,
    resourceChange_details,
    resourceChange_logicalResourceId,
    resourceChange_scope,
    resourceChange_action,
    resourceChange_physicalResourceId,

    -- ** ResourceChangeDetail
    resourceChangeDetail_target,
    resourceChangeDetail_evaluation,
    resourceChangeDetail_causingEntity,

    -- ** ResourceDetail
    resourceDetail_name,
    resourceDetail_createdTime,
    resourceDetail_arn,
    resourceDetail_id,
    resourceDetail_description,

    -- ** ResourceTargetDefinition
    resourceTargetDefinition_name,
    resourceTargetDefinition_attribute,
    resourceTargetDefinition_requiresRecreation,

    -- ** ServiceActionAssociation
    serviceActionAssociation_serviceActionId,
    serviceActionAssociation_productId,
    serviceActionAssociation_provisioningArtifactId,

    -- ** ServiceActionDetail
    serviceActionDetail_serviceActionSummary,
    serviceActionDetail_definition,

    -- ** ServiceActionSummary
    serviceActionSummary_name,
    serviceActionSummary_definitionType,
    serviceActionSummary_id,
    serviceActionSummary_description,

    -- ** ShareDetails
    shareDetails_successfulShares,
    shareDetails_shareErrors,

    -- ** ShareError
    shareError_message,
    shareError_accounts,
    shareError_error,

    -- ** SourceConnection
    sourceConnection_type,
    sourceConnection_connectionParameters,

    -- ** SourceConnectionDetail
    sourceConnectionDetail_lastSync,
    sourceConnectionDetail_type,
    sourceConnectionDetail_connectionParameters,

    -- ** SourceConnectionParameters
    sourceConnectionParameters_codeStar,

    -- ** StackInstance
    stackInstance_stackInstanceStatus,
    stackInstance_account,
    stackInstance_region,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagOptionDetail
    tagOptionDetail_key,
    tagOptionDetail_active,
    tagOptionDetail_owner,
    tagOptionDetail_id,
    tagOptionDetail_value,

    -- ** TagOptionSummary
    tagOptionSummary_key,
    tagOptionSummary_values,

    -- ** UpdateProvisioningParameter
    updateProvisioningParameter_key,
    updateProvisioningParameter_usePreviousValue,
    updateProvisioningParameter_value,

    -- ** UpdateProvisioningPreferences
    updateProvisioningPreferences_stackSetOperationType,
    updateProvisioningPreferences_stackSetRegions,
    updateProvisioningPreferences_stackSetFailureToleranceCount,
    updateProvisioningPreferences_stackSetMaxConcurrencyPercentage,
    updateProvisioningPreferences_stackSetAccounts,
    updateProvisioningPreferences_stackSetMaxConcurrencyCount,
    updateProvisioningPreferences_stackSetFailureTolerancePercentage,

    -- ** UsageInstruction
    usageInstruction_type,
    usageInstruction_value,
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
import Amazonka.ServiceCatalog.Types.AccessLevelFilter
import Amazonka.ServiceCatalog.Types.BudgetDetail
import Amazonka.ServiceCatalog.Types.CloudWatchDashboard
import Amazonka.ServiceCatalog.Types.CodeStarParameters
import Amazonka.ServiceCatalog.Types.ConstraintDetail
import Amazonka.ServiceCatalog.Types.ConstraintSummary
import Amazonka.ServiceCatalog.Types.ExecutionParameter
import Amazonka.ServiceCatalog.Types.FailedServiceActionAssociation
import Amazonka.ServiceCatalog.Types.LastSync
import Amazonka.ServiceCatalog.Types.LaunchPath
import Amazonka.ServiceCatalog.Types.LaunchPathSummary
import Amazonka.ServiceCatalog.Types.ListRecordHistorySearchFilter
import Amazonka.ServiceCatalog.Types.ListTagOptionsFilters
import Amazonka.ServiceCatalog.Types.OrganizationNode
import Amazonka.ServiceCatalog.Types.ParameterConstraints
import Amazonka.ServiceCatalog.Types.PortfolioDetail
import Amazonka.ServiceCatalog.Types.PortfolioShareDetail
import Amazonka.ServiceCatalog.Types.Principal
import Amazonka.ServiceCatalog.Types.ProductViewAggregationValue
import Amazonka.ServiceCatalog.Types.ProductViewDetail
import Amazonka.ServiceCatalog.Types.ProductViewSummary
import Amazonka.ServiceCatalog.Types.ProvisionedProductAttribute
import Amazonka.ServiceCatalog.Types.ProvisionedProductDetail
import Amazonka.ServiceCatalog.Types.ProvisionedProductPlanDetails
import Amazonka.ServiceCatalog.Types.ProvisionedProductPlanSummary
import Amazonka.ServiceCatalog.Types.ProvisioningArtifact
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactDetail
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactOutput
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactParameter
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactPreferences
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactProperties
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactSummary
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactView
import Amazonka.ServiceCatalog.Types.ProvisioningParameter
import Amazonka.ServiceCatalog.Types.ProvisioningPreferences
import Amazonka.ServiceCatalog.Types.RecordDetail
import Amazonka.ServiceCatalog.Types.RecordError
import Amazonka.ServiceCatalog.Types.RecordOutput
import Amazonka.ServiceCatalog.Types.RecordTag
import Amazonka.ServiceCatalog.Types.ResourceChange
import Amazonka.ServiceCatalog.Types.ResourceChangeDetail
import Amazonka.ServiceCatalog.Types.ResourceDetail
import Amazonka.ServiceCatalog.Types.ResourceTargetDefinition
import Amazonka.ServiceCatalog.Types.ServiceActionAssociation
import Amazonka.ServiceCatalog.Types.ServiceActionDetail
import Amazonka.ServiceCatalog.Types.ServiceActionSummary
import Amazonka.ServiceCatalog.Types.ShareDetails
import Amazonka.ServiceCatalog.Types.ShareError
import Amazonka.ServiceCatalog.Types.SourceConnection
import Amazonka.ServiceCatalog.Types.SourceConnectionDetail
import Amazonka.ServiceCatalog.Types.SourceConnectionParameters
import Amazonka.ServiceCatalog.Types.StackInstance
import Amazonka.ServiceCatalog.Types.Tag
import Amazonka.ServiceCatalog.Types.TagOptionDetail
import Amazonka.ServiceCatalog.Types.TagOptionSummary
import Amazonka.ServiceCatalog.Types.UpdateProvisioningParameter
import Amazonka.ServiceCatalog.Types.UpdateProvisioningPreferences
import Amazonka.ServiceCatalog.Types.UsageInstruction
import Amazonka.ServiceCatalog.UpdateConstraint
import Amazonka.ServiceCatalog.UpdatePortfolio
import Amazonka.ServiceCatalog.UpdatePortfolioShare
import Amazonka.ServiceCatalog.UpdateProduct
import Amazonka.ServiceCatalog.UpdateProvisionedProduct
import Amazonka.ServiceCatalog.UpdateProvisionedProductProperties
import Amazonka.ServiceCatalog.UpdateProvisioningArtifact
import Amazonka.ServiceCatalog.UpdateServiceAction
import Amazonka.ServiceCatalog.UpdateTagOption
