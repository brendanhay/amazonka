{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalog.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Lens
  ( -- * Operations

    -- ** AcceptPortfolioShare
    acceptPortfolioShare_acceptLanguage,
    acceptPortfolioShare_portfolioShareType,
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
    associateProductWithPortfolio_acceptLanguage,
    associateProductWithPortfolio_sourcePortfolioId,
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
    copyProduct_acceptLanguage,
    copyProduct_copyOptions,
    copyProduct_sourceProvisioningArtifactIdentifiers,
    copyProduct_targetProductId,
    copyProduct_targetProductName,
    copyProduct_sourceProductArn,
    copyProduct_idempotencyToken,
    copyProductResponse_copyProductToken,
    copyProductResponse_httpStatus,

    -- ** CreateConstraint
    createConstraint_acceptLanguage,
    createConstraint_description,
    createConstraint_portfolioId,
    createConstraint_productId,
    createConstraint_parameters,
    createConstraint_type,
    createConstraint_idempotencyToken,
    createConstraintResponse_constraintDetail,
    createConstraintResponse_constraintParameters,
    createConstraintResponse_status,
    createConstraintResponse_httpStatus,

    -- ** CreatePortfolio
    createPortfolio_acceptLanguage,
    createPortfolio_description,
    createPortfolio_tags,
    createPortfolio_displayName,
    createPortfolio_providerName,
    createPortfolio_idempotencyToken,
    createPortfolioResponse_portfolioDetail,
    createPortfolioResponse_tags,
    createPortfolioResponse_httpStatus,

    -- ** CreatePortfolioShare
    createPortfolioShare_acceptLanguage,
    createPortfolioShare_accountId,
    createPortfolioShare_organizationNode,
    createPortfolioShare_sharePrincipals,
    createPortfolioShare_shareTagOptions,
    createPortfolioShare_portfolioId,
    createPortfolioShareResponse_portfolioShareToken,
    createPortfolioShareResponse_httpStatus,

    -- ** CreateProduct
    createProduct_acceptLanguage,
    createProduct_description,
    createProduct_distributor,
    createProduct_provisioningArtifactParameters,
    createProduct_sourceConnection,
    createProduct_supportDescription,
    createProduct_supportEmail,
    createProduct_supportUrl,
    createProduct_tags,
    createProduct_name,
    createProduct_owner,
    createProduct_productType,
    createProduct_idempotencyToken,
    createProductResponse_productViewDetail,
    createProductResponse_provisioningArtifactDetail,
    createProductResponse_tags,
    createProductResponse_httpStatus,

    -- ** CreateProvisionedProductPlan
    createProvisionedProductPlan_acceptLanguage,
    createProvisionedProductPlan_notificationArns,
    createProvisionedProductPlan_pathId,
    createProvisionedProductPlan_provisioningParameters,
    createProvisionedProductPlan_tags,
    createProvisionedProductPlan_planName,
    createProvisionedProductPlan_planType,
    createProvisionedProductPlan_productId,
    createProvisionedProductPlan_provisionedProductName,
    createProvisionedProductPlan_provisioningArtifactId,
    createProvisionedProductPlan_idempotencyToken,
    createProvisionedProductPlanResponse_planId,
    createProvisionedProductPlanResponse_planName,
    createProvisionedProductPlanResponse_provisionProductId,
    createProvisionedProductPlanResponse_provisionedProductName,
    createProvisionedProductPlanResponse_provisioningArtifactId,
    createProvisionedProductPlanResponse_httpStatus,

    -- ** CreateProvisioningArtifact
    createProvisioningArtifact_acceptLanguage,
    createProvisioningArtifact_productId,
    createProvisioningArtifact_parameters,
    createProvisioningArtifact_idempotencyToken,
    createProvisioningArtifactResponse_info,
    createProvisioningArtifactResponse_provisioningArtifactDetail,
    createProvisioningArtifactResponse_status,
    createProvisioningArtifactResponse_httpStatus,

    -- ** CreateServiceAction
    createServiceAction_acceptLanguage,
    createServiceAction_description,
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
    deletePortfolioShare_acceptLanguage,
    deletePortfolioShare_accountId,
    deletePortfolioShare_organizationNode,
    deletePortfolioShare_portfolioId,
    deletePortfolioShareResponse_portfolioShareToken,
    deletePortfolioShareResponse_httpStatus,

    -- ** DeleteProduct
    deleteProduct_acceptLanguage,
    deleteProduct_id,
    deleteProductResponse_httpStatus,

    -- ** DeleteProvisionedProductPlan
    deleteProvisionedProductPlan_acceptLanguage,
    deleteProvisionedProductPlan_ignoreErrors,
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
    describeConstraintResponse_constraintParameters,
    describeConstraintResponse_status,
    describeConstraintResponse_httpStatus,

    -- ** DescribeCopyProductStatus
    describeCopyProductStatus_acceptLanguage,
    describeCopyProductStatus_copyProductToken,
    describeCopyProductStatusResponse_copyProductStatus,
    describeCopyProductStatusResponse_statusDetail,
    describeCopyProductStatusResponse_targetProductId,
    describeCopyProductStatusResponse_httpStatus,

    -- ** DescribePortfolio
    describePortfolio_acceptLanguage,
    describePortfolio_id,
    describePortfolioResponse_budgets,
    describePortfolioResponse_portfolioDetail,
    describePortfolioResponse_tagOptions,
    describePortfolioResponse_tags,
    describePortfolioResponse_httpStatus,

    -- ** DescribePortfolioShareStatus
    describePortfolioShareStatus_portfolioShareToken,
    describePortfolioShareStatusResponse_organizationNodeValue,
    describePortfolioShareStatusResponse_portfolioId,
    describePortfolioShareStatusResponse_portfolioShareToken,
    describePortfolioShareStatusResponse_shareDetails,
    describePortfolioShareStatusResponse_status,
    describePortfolioShareStatusResponse_httpStatus,

    -- ** DescribePortfolioShares
    describePortfolioShares_pageSize,
    describePortfolioShares_pageToken,
    describePortfolioShares_portfolioId,
    describePortfolioShares_type,
    describePortfolioSharesResponse_nextPageToken,
    describePortfolioSharesResponse_portfolioShareDetails,
    describePortfolioSharesResponse_httpStatus,

    -- ** DescribeProduct
    describeProduct_acceptLanguage,
    describeProduct_id,
    describeProduct_name,
    describeProductResponse_budgets,
    describeProductResponse_launchPaths,
    describeProductResponse_productViewSummary,
    describeProductResponse_provisioningArtifacts,
    describeProductResponse_httpStatus,

    -- ** DescribeProductAsAdmin
    describeProductAsAdmin_acceptLanguage,
    describeProductAsAdmin_id,
    describeProductAsAdmin_name,
    describeProductAsAdmin_sourcePortfolioId,
    describeProductAsAdminResponse_budgets,
    describeProductAsAdminResponse_productViewDetail,
    describeProductAsAdminResponse_provisioningArtifactSummaries,
    describeProductAsAdminResponse_tagOptions,
    describeProductAsAdminResponse_tags,
    describeProductAsAdminResponse_httpStatus,

    -- ** DescribeProductView
    describeProductView_acceptLanguage,
    describeProductView_id,
    describeProductViewResponse_productViewSummary,
    describeProductViewResponse_provisioningArtifacts,
    describeProductViewResponse_httpStatus,

    -- ** DescribeProvisionedProduct
    describeProvisionedProduct_acceptLanguage,
    describeProvisionedProduct_id,
    describeProvisionedProduct_name,
    describeProvisionedProductResponse_cloudWatchDashboards,
    describeProvisionedProductResponse_provisionedProductDetail,
    describeProvisionedProductResponse_httpStatus,

    -- ** DescribeProvisionedProductPlan
    describeProvisionedProductPlan_acceptLanguage,
    describeProvisionedProductPlan_pageSize,
    describeProvisionedProductPlan_pageToken,
    describeProvisionedProductPlan_planId,
    describeProvisionedProductPlanResponse_nextPageToken,
    describeProvisionedProductPlanResponse_provisionedProductPlanDetails,
    describeProvisionedProductPlanResponse_resourceChanges,
    describeProvisionedProductPlanResponse_httpStatus,

    -- ** DescribeProvisioningArtifact
    describeProvisioningArtifact_acceptLanguage,
    describeProvisioningArtifact_includeProvisioningArtifactParameters,
    describeProvisioningArtifact_productId,
    describeProvisioningArtifact_productName,
    describeProvisioningArtifact_provisioningArtifactId,
    describeProvisioningArtifact_provisioningArtifactName,
    describeProvisioningArtifact_verbose,
    describeProvisioningArtifactResponse_info,
    describeProvisioningArtifactResponse_provisioningArtifactDetail,
    describeProvisioningArtifactResponse_provisioningArtifactParameters,
    describeProvisioningArtifactResponse_status,
    describeProvisioningArtifactResponse_httpStatus,

    -- ** DescribeProvisioningParameters
    describeProvisioningParameters_acceptLanguage,
    describeProvisioningParameters_pathId,
    describeProvisioningParameters_pathName,
    describeProvisioningParameters_productId,
    describeProvisioningParameters_productName,
    describeProvisioningParameters_provisioningArtifactId,
    describeProvisioningParameters_provisioningArtifactName,
    describeProvisioningParametersResponse_constraintSummaries,
    describeProvisioningParametersResponse_provisioningArtifactOutputKeys,
    describeProvisioningParametersResponse_provisioningArtifactOutputs,
    describeProvisioningParametersResponse_provisioningArtifactParameters,
    describeProvisioningParametersResponse_provisioningArtifactPreferences,
    describeProvisioningParametersResponse_tagOptions,
    describeProvisioningParametersResponse_usageInstructions,
    describeProvisioningParametersResponse_httpStatus,

    -- ** DescribeRecord
    describeRecord_acceptLanguage,
    describeRecord_pageSize,
    describeRecord_pageToken,
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
    disassociatePrincipalFromPortfolio_acceptLanguage,
    disassociatePrincipalFromPortfolio_principalType,
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
    getProvisionedProductOutputs_acceptLanguage,
    getProvisionedProductOutputs_outputKeys,
    getProvisionedProductOutputs_pageSize,
    getProvisionedProductOutputs_pageToken,
    getProvisionedProductOutputs_provisionedProductId,
    getProvisionedProductOutputs_provisionedProductName,
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
    listAcceptedPortfolioShares_acceptLanguage,
    listAcceptedPortfolioShares_pageSize,
    listAcceptedPortfolioShares_pageToken,
    listAcceptedPortfolioShares_portfolioShareType,
    listAcceptedPortfolioSharesResponse_nextPageToken,
    listAcceptedPortfolioSharesResponse_portfolioDetails,
    listAcceptedPortfolioSharesResponse_httpStatus,

    -- ** ListBudgetsForResource
    listBudgetsForResource_acceptLanguage,
    listBudgetsForResource_pageSize,
    listBudgetsForResource_pageToken,
    listBudgetsForResource_resourceId,
    listBudgetsForResourceResponse_budgets,
    listBudgetsForResourceResponse_nextPageToken,
    listBudgetsForResourceResponse_httpStatus,

    -- ** ListConstraintsForPortfolio
    listConstraintsForPortfolio_acceptLanguage,
    listConstraintsForPortfolio_pageSize,
    listConstraintsForPortfolio_pageToken,
    listConstraintsForPortfolio_productId,
    listConstraintsForPortfolio_portfolioId,
    listConstraintsForPortfolioResponse_constraintDetails,
    listConstraintsForPortfolioResponse_nextPageToken,
    listConstraintsForPortfolioResponse_httpStatus,

    -- ** ListLaunchPaths
    listLaunchPaths_acceptLanguage,
    listLaunchPaths_pageSize,
    listLaunchPaths_pageToken,
    listLaunchPaths_productId,
    listLaunchPathsResponse_launchPathSummaries,
    listLaunchPathsResponse_nextPageToken,
    listLaunchPathsResponse_httpStatus,

    -- ** ListOrganizationPortfolioAccess
    listOrganizationPortfolioAccess_acceptLanguage,
    listOrganizationPortfolioAccess_pageSize,
    listOrganizationPortfolioAccess_pageToken,
    listOrganizationPortfolioAccess_portfolioId,
    listOrganizationPortfolioAccess_organizationNodeType,
    listOrganizationPortfolioAccessResponse_nextPageToken,
    listOrganizationPortfolioAccessResponse_organizationNodes,
    listOrganizationPortfolioAccessResponse_httpStatus,

    -- ** ListPortfolioAccess
    listPortfolioAccess_acceptLanguage,
    listPortfolioAccess_organizationParentId,
    listPortfolioAccess_pageSize,
    listPortfolioAccess_pageToken,
    listPortfolioAccess_portfolioId,
    listPortfolioAccessResponse_accountIds,
    listPortfolioAccessResponse_nextPageToken,
    listPortfolioAccessResponse_httpStatus,

    -- ** ListPortfolios
    listPortfolios_acceptLanguage,
    listPortfolios_pageSize,
    listPortfolios_pageToken,
    listPortfoliosResponse_nextPageToken,
    listPortfoliosResponse_portfolioDetails,
    listPortfoliosResponse_httpStatus,

    -- ** ListPortfoliosForProduct
    listPortfoliosForProduct_acceptLanguage,
    listPortfoliosForProduct_pageSize,
    listPortfoliosForProduct_pageToken,
    listPortfoliosForProduct_productId,
    listPortfoliosForProductResponse_nextPageToken,
    listPortfoliosForProductResponse_portfolioDetails,
    listPortfoliosForProductResponse_httpStatus,

    -- ** ListPrincipalsForPortfolio
    listPrincipalsForPortfolio_acceptLanguage,
    listPrincipalsForPortfolio_pageSize,
    listPrincipalsForPortfolio_pageToken,
    listPrincipalsForPortfolio_portfolioId,
    listPrincipalsForPortfolioResponse_nextPageToken,
    listPrincipalsForPortfolioResponse_principals,
    listPrincipalsForPortfolioResponse_httpStatus,

    -- ** ListProvisionedProductPlans
    listProvisionedProductPlans_acceptLanguage,
    listProvisionedProductPlans_accessLevelFilter,
    listProvisionedProductPlans_pageSize,
    listProvisionedProductPlans_pageToken,
    listProvisionedProductPlans_provisionProductId,
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
    listProvisioningArtifactsForServiceAction_acceptLanguage,
    listProvisioningArtifactsForServiceAction_pageSize,
    listProvisioningArtifactsForServiceAction_pageToken,
    listProvisioningArtifactsForServiceAction_serviceActionId,
    listProvisioningArtifactsForServiceActionResponse_nextPageToken,
    listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews,
    listProvisioningArtifactsForServiceActionResponse_httpStatus,

    -- ** ListRecordHistory
    listRecordHistory_acceptLanguage,
    listRecordHistory_accessLevelFilter,
    listRecordHistory_pageSize,
    listRecordHistory_pageToken,
    listRecordHistory_searchFilter,
    listRecordHistoryResponse_nextPageToken,
    listRecordHistoryResponse_recordDetails,
    listRecordHistoryResponse_httpStatus,

    -- ** ListResourcesForTagOption
    listResourcesForTagOption_pageSize,
    listResourcesForTagOption_pageToken,
    listResourcesForTagOption_resourceType,
    listResourcesForTagOption_tagOptionId,
    listResourcesForTagOptionResponse_pageToken,
    listResourcesForTagOptionResponse_resourceDetails,
    listResourcesForTagOptionResponse_httpStatus,

    -- ** ListServiceActions
    listServiceActions_acceptLanguage,
    listServiceActions_pageSize,
    listServiceActions_pageToken,
    listServiceActionsResponse_nextPageToken,
    listServiceActionsResponse_serviceActionSummaries,
    listServiceActionsResponse_httpStatus,

    -- ** ListServiceActionsForProvisioningArtifact
    listServiceActionsForProvisioningArtifact_acceptLanguage,
    listServiceActionsForProvisioningArtifact_pageSize,
    listServiceActionsForProvisioningArtifact_pageToken,
    listServiceActionsForProvisioningArtifact_productId,
    listServiceActionsForProvisioningArtifact_provisioningArtifactId,
    listServiceActionsForProvisioningArtifactResponse_nextPageToken,
    listServiceActionsForProvisioningArtifactResponse_serviceActionSummaries,
    listServiceActionsForProvisioningArtifactResponse_httpStatus,

    -- ** ListStackInstancesForProvisionedProduct
    listStackInstancesForProvisionedProduct_acceptLanguage,
    listStackInstancesForProvisionedProduct_pageSize,
    listStackInstancesForProvisionedProduct_pageToken,
    listStackInstancesForProvisionedProduct_provisionedProductId,
    listStackInstancesForProvisionedProductResponse_nextPageToken,
    listStackInstancesForProvisionedProductResponse_stackInstances,
    listStackInstancesForProvisionedProductResponse_httpStatus,

    -- ** ListTagOptions
    listTagOptions_filters,
    listTagOptions_pageSize,
    listTagOptions_pageToken,
    listTagOptionsResponse_pageToken,
    listTagOptionsResponse_tagOptionDetails,
    listTagOptionsResponse_httpStatus,

    -- ** NotifyProvisionProductEngineWorkflowResult
    notifyProvisionProductEngineWorkflowResult_failureReason,
    notifyProvisionProductEngineWorkflowResult_outputs,
    notifyProvisionProductEngineWorkflowResult_resourceIdentifier,
    notifyProvisionProductEngineWorkflowResult_workflowToken,
    notifyProvisionProductEngineWorkflowResult_recordId,
    notifyProvisionProductEngineWorkflowResult_status,
    notifyProvisionProductEngineWorkflowResult_idempotencyToken,
    notifyProvisionProductEngineWorkflowResultResponse_httpStatus,

    -- ** NotifyTerminateProvisionedProductEngineWorkflowResult
    notifyTerminateProvisionedProductEngineWorkflowResult_failureReason,
    notifyTerminateProvisionedProductEngineWorkflowResult_workflowToken,
    notifyTerminateProvisionedProductEngineWorkflowResult_recordId,
    notifyTerminateProvisionedProductEngineWorkflowResult_status,
    notifyTerminateProvisionedProductEngineWorkflowResult_idempotencyToken,
    notifyTerminateProvisionedProductEngineWorkflowResultResponse_httpStatus,

    -- ** NotifyUpdateProvisionedProductEngineWorkflowResult
    notifyUpdateProvisionedProductEngineWorkflowResult_failureReason,
    notifyUpdateProvisionedProductEngineWorkflowResult_outputs,
    notifyUpdateProvisionedProductEngineWorkflowResult_workflowToken,
    notifyUpdateProvisionedProductEngineWorkflowResult_recordId,
    notifyUpdateProvisionedProductEngineWorkflowResult_status,
    notifyUpdateProvisionedProductEngineWorkflowResult_idempotencyToken,
    notifyUpdateProvisionedProductEngineWorkflowResultResponse_httpStatus,

    -- ** ProvisionProduct
    provisionProduct_acceptLanguage,
    provisionProduct_notificationArns,
    provisionProduct_pathId,
    provisionProduct_pathName,
    provisionProduct_productId,
    provisionProduct_productName,
    provisionProduct_provisioningArtifactId,
    provisionProduct_provisioningArtifactName,
    provisionProduct_provisioningParameters,
    provisionProduct_provisioningPreferences,
    provisionProduct_tags,
    provisionProduct_provisionedProductName,
    provisionProduct_provisionToken,
    provisionProductResponse_recordDetail,
    provisionProductResponse_httpStatus,

    -- ** RejectPortfolioShare
    rejectPortfolioShare_acceptLanguage,
    rejectPortfolioShare_portfolioShareType,
    rejectPortfolioShare_portfolioId,
    rejectPortfolioShareResponse_httpStatus,

    -- ** ScanProvisionedProducts
    scanProvisionedProducts_acceptLanguage,
    scanProvisionedProducts_accessLevelFilter,
    scanProvisionedProducts_pageSize,
    scanProvisionedProducts_pageToken,
    scanProvisionedProductsResponse_nextPageToken,
    scanProvisionedProductsResponse_provisionedProducts,
    scanProvisionedProductsResponse_httpStatus,

    -- ** SearchProducts
    searchProducts_acceptLanguage,
    searchProducts_filters,
    searchProducts_pageSize,
    searchProducts_pageToken,
    searchProducts_sortBy,
    searchProducts_sortOrder,
    searchProductsResponse_nextPageToken,
    searchProductsResponse_productViewAggregations,
    searchProductsResponse_productViewSummaries,
    searchProductsResponse_httpStatus,

    -- ** SearchProductsAsAdmin
    searchProductsAsAdmin_acceptLanguage,
    searchProductsAsAdmin_filters,
    searchProductsAsAdmin_pageSize,
    searchProductsAsAdmin_pageToken,
    searchProductsAsAdmin_portfolioId,
    searchProductsAsAdmin_productSource,
    searchProductsAsAdmin_sortBy,
    searchProductsAsAdmin_sortOrder,
    searchProductsAsAdminResponse_nextPageToken,
    searchProductsAsAdminResponse_productViewDetails,
    searchProductsAsAdminResponse_httpStatus,

    -- ** SearchProvisionedProducts
    searchProvisionedProducts_acceptLanguage,
    searchProvisionedProducts_accessLevelFilter,
    searchProvisionedProducts_filters,
    searchProvisionedProducts_pageSize,
    searchProvisionedProducts_pageToken,
    searchProvisionedProducts_sortBy,
    searchProvisionedProducts_sortOrder,
    searchProvisionedProductsResponse_nextPageToken,
    searchProvisionedProductsResponse_provisionedProducts,
    searchProvisionedProductsResponse_totalResultsCount,
    searchProvisionedProductsResponse_httpStatus,

    -- ** TerminateProvisionedProduct
    terminateProvisionedProduct_acceptLanguage,
    terminateProvisionedProduct_ignoreErrors,
    terminateProvisionedProduct_provisionedProductId,
    terminateProvisionedProduct_provisionedProductName,
    terminateProvisionedProduct_retainPhysicalResources,
    terminateProvisionedProduct_terminateToken,
    terminateProvisionedProductResponse_recordDetail,
    terminateProvisionedProductResponse_httpStatus,

    -- ** UpdateConstraint
    updateConstraint_acceptLanguage,
    updateConstraint_description,
    updateConstraint_parameters,
    updateConstraint_id,
    updateConstraintResponse_constraintDetail,
    updateConstraintResponse_constraintParameters,
    updateConstraintResponse_status,
    updateConstraintResponse_httpStatus,

    -- ** UpdatePortfolio
    updatePortfolio_acceptLanguage,
    updatePortfolio_addTags,
    updatePortfolio_description,
    updatePortfolio_displayName,
    updatePortfolio_providerName,
    updatePortfolio_removeTags,
    updatePortfolio_id,
    updatePortfolioResponse_portfolioDetail,
    updatePortfolioResponse_tags,
    updatePortfolioResponse_httpStatus,

    -- ** UpdatePortfolioShare
    updatePortfolioShare_acceptLanguage,
    updatePortfolioShare_accountId,
    updatePortfolioShare_organizationNode,
    updatePortfolioShare_sharePrincipals,
    updatePortfolioShare_shareTagOptions,
    updatePortfolioShare_portfolioId,
    updatePortfolioShareResponse_portfolioShareToken,
    updatePortfolioShareResponse_status,
    updatePortfolioShareResponse_httpStatus,

    -- ** UpdateProduct
    updateProduct_acceptLanguage,
    updateProduct_addTags,
    updateProduct_description,
    updateProduct_distributor,
    updateProduct_name,
    updateProduct_owner,
    updateProduct_removeTags,
    updateProduct_sourceConnection,
    updateProduct_supportDescription,
    updateProduct_supportEmail,
    updateProduct_supportUrl,
    updateProduct_id,
    updateProductResponse_productViewDetail,
    updateProductResponse_tags,
    updateProductResponse_httpStatus,

    -- ** UpdateProvisionedProduct
    updateProvisionedProduct_acceptLanguage,
    updateProvisionedProduct_pathId,
    updateProvisionedProduct_pathName,
    updateProvisionedProduct_productId,
    updateProvisionedProduct_productName,
    updateProvisionedProduct_provisionedProductId,
    updateProvisionedProduct_provisionedProductName,
    updateProvisionedProduct_provisioningArtifactId,
    updateProvisionedProduct_provisioningArtifactName,
    updateProvisionedProduct_provisioningParameters,
    updateProvisionedProduct_provisioningPreferences,
    updateProvisionedProduct_tags,
    updateProvisionedProduct_updateToken,
    updateProvisionedProductResponse_recordDetail,
    updateProvisionedProductResponse_httpStatus,

    -- ** UpdateProvisionedProductProperties
    updateProvisionedProductProperties_acceptLanguage,
    updateProvisionedProductProperties_provisionedProductId,
    updateProvisionedProductProperties_provisionedProductProperties,
    updateProvisionedProductProperties_idempotencyToken,
    updateProvisionedProductPropertiesResponse_provisionedProductId,
    updateProvisionedProductPropertiesResponse_provisionedProductProperties,
    updateProvisionedProductPropertiesResponse_recordId,
    updateProvisionedProductPropertiesResponse_status,
    updateProvisionedProductPropertiesResponse_httpStatus,

    -- ** UpdateProvisioningArtifact
    updateProvisioningArtifact_acceptLanguage,
    updateProvisioningArtifact_active,
    updateProvisioningArtifact_description,
    updateProvisioningArtifact_guidance,
    updateProvisioningArtifact_name,
    updateProvisioningArtifact_productId,
    updateProvisioningArtifact_provisioningArtifactId,
    updateProvisioningArtifactResponse_info,
    updateProvisioningArtifactResponse_provisioningArtifactDetail,
    updateProvisioningArtifactResponse_status,
    updateProvisioningArtifactResponse_httpStatus,

    -- ** UpdateServiceAction
    updateServiceAction_acceptLanguage,
    updateServiceAction_definition,
    updateServiceAction_description,
    updateServiceAction_name,
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
    constraintDetail_constraintId,
    constraintDetail_description,
    constraintDetail_owner,
    constraintDetail_portfolioId,
    constraintDetail_productId,
    constraintDetail_type,

    -- ** ConstraintSummary
    constraintSummary_description,
    constraintSummary_type,

    -- ** EngineWorkflowResourceIdentifier
    engineWorkflowResourceIdentifier_uniqueTag,

    -- ** ExecutionParameter
    executionParameter_defaultValues,
    executionParameter_name,
    executionParameter_type,

    -- ** FailedServiceActionAssociation
    failedServiceActionAssociation_errorCode,
    failedServiceActionAssociation_errorMessage,
    failedServiceActionAssociation_productId,
    failedServiceActionAssociation_provisioningArtifactId,
    failedServiceActionAssociation_serviceActionId,

    -- ** LastSync
    lastSync_lastSuccessfulSyncProvisioningArtifactId,
    lastSync_lastSuccessfulSyncTime,
    lastSync_lastSyncStatus,
    lastSync_lastSyncStatusMessage,
    lastSync_lastSyncTime,

    -- ** LaunchPath
    launchPath_id,
    launchPath_name,

    -- ** LaunchPathSummary
    launchPathSummary_constraintSummaries,
    launchPathSummary_id,
    launchPathSummary_name,
    launchPathSummary_tags,

    -- ** ListRecordHistorySearchFilter
    listRecordHistorySearchFilter_key,
    listRecordHistorySearchFilter_value,

    -- ** ListTagOptionsFilters
    listTagOptionsFilters_active,
    listTagOptionsFilters_key,
    listTagOptionsFilters_value,

    -- ** OrganizationNode
    organizationNode_type,
    organizationNode_value,

    -- ** ParameterConstraints
    parameterConstraints_allowedPattern,
    parameterConstraints_allowedValues,
    parameterConstraints_constraintDescription,
    parameterConstraints_maxLength,
    parameterConstraints_maxValue,
    parameterConstraints_minLength,
    parameterConstraints_minValue,

    -- ** PortfolioDetail
    portfolioDetail_arn,
    portfolioDetail_createdTime,
    portfolioDetail_description,
    portfolioDetail_displayName,
    portfolioDetail_id,
    portfolioDetail_providerName,

    -- ** PortfolioShareDetail
    portfolioShareDetail_accepted,
    portfolioShareDetail_principalId,
    portfolioShareDetail_sharePrincipals,
    portfolioShareDetail_shareTagOptions,
    portfolioShareDetail_type,

    -- ** Principal
    principal_principalARN,
    principal_principalType,

    -- ** ProductViewAggregationValue
    productViewAggregationValue_approximateCount,
    productViewAggregationValue_value,

    -- ** ProductViewDetail
    productViewDetail_createdTime,
    productViewDetail_productARN,
    productViewDetail_productViewSummary,
    productViewDetail_sourceConnection,
    productViewDetail_status,

    -- ** ProductViewSummary
    productViewSummary_distributor,
    productViewSummary_hasDefaultPath,
    productViewSummary_id,
    productViewSummary_name,
    productViewSummary_owner,
    productViewSummary_productId,
    productViewSummary_shortDescription,
    productViewSummary_supportDescription,
    productViewSummary_supportEmail,
    productViewSummary_supportUrl,
    productViewSummary_type,

    -- ** ProvisionedProductAttribute
    provisionedProductAttribute_arn,
    provisionedProductAttribute_createdTime,
    provisionedProductAttribute_id,
    provisionedProductAttribute_idempotencyToken,
    provisionedProductAttribute_lastProvisioningRecordId,
    provisionedProductAttribute_lastRecordId,
    provisionedProductAttribute_lastSuccessfulProvisioningRecordId,
    provisionedProductAttribute_name,
    provisionedProductAttribute_physicalId,
    provisionedProductAttribute_productId,
    provisionedProductAttribute_productName,
    provisionedProductAttribute_provisioningArtifactId,
    provisionedProductAttribute_provisioningArtifactName,
    provisionedProductAttribute_status,
    provisionedProductAttribute_statusMessage,
    provisionedProductAttribute_tags,
    provisionedProductAttribute_type,
    provisionedProductAttribute_userArn,
    provisionedProductAttribute_userArnSession,

    -- ** ProvisionedProductDetail
    provisionedProductDetail_arn,
    provisionedProductDetail_createdTime,
    provisionedProductDetail_id,
    provisionedProductDetail_idempotencyToken,
    provisionedProductDetail_lastProvisioningRecordId,
    provisionedProductDetail_lastRecordId,
    provisionedProductDetail_lastSuccessfulProvisioningRecordId,
    provisionedProductDetail_launchRoleArn,
    provisionedProductDetail_name,
    provisionedProductDetail_productId,
    provisionedProductDetail_provisioningArtifactId,
    provisionedProductDetail_status,
    provisionedProductDetail_statusMessage,
    provisionedProductDetail_type,

    -- ** ProvisionedProductPlanDetails
    provisionedProductPlanDetails_createdTime,
    provisionedProductPlanDetails_notificationArns,
    provisionedProductPlanDetails_pathId,
    provisionedProductPlanDetails_planId,
    provisionedProductPlanDetails_planName,
    provisionedProductPlanDetails_planType,
    provisionedProductPlanDetails_productId,
    provisionedProductPlanDetails_provisionProductId,
    provisionedProductPlanDetails_provisionProductName,
    provisionedProductPlanDetails_provisioningArtifactId,
    provisionedProductPlanDetails_provisioningParameters,
    provisionedProductPlanDetails_status,
    provisionedProductPlanDetails_statusMessage,
    provisionedProductPlanDetails_tags,
    provisionedProductPlanDetails_updatedTime,

    -- ** ProvisionedProductPlanSummary
    provisionedProductPlanSummary_planId,
    provisionedProductPlanSummary_planName,
    provisionedProductPlanSummary_planType,
    provisionedProductPlanSummary_provisionProductId,
    provisionedProductPlanSummary_provisionProductName,
    provisionedProductPlanSummary_provisioningArtifactId,

    -- ** ProvisioningArtifact
    provisioningArtifact_createdTime,
    provisioningArtifact_description,
    provisioningArtifact_guidance,
    provisioningArtifact_id,
    provisioningArtifact_name,

    -- ** ProvisioningArtifactDetail
    provisioningArtifactDetail_active,
    provisioningArtifactDetail_createdTime,
    provisioningArtifactDetail_description,
    provisioningArtifactDetail_guidance,
    provisioningArtifactDetail_id,
    provisioningArtifactDetail_name,
    provisioningArtifactDetail_sourceRevision,
    provisioningArtifactDetail_type,

    -- ** ProvisioningArtifactOutput
    provisioningArtifactOutput_description,
    provisioningArtifactOutput_key,

    -- ** ProvisioningArtifactParameter
    provisioningArtifactParameter_defaultValue,
    provisioningArtifactParameter_description,
    provisioningArtifactParameter_isNoEcho,
    provisioningArtifactParameter_parameterConstraints,
    provisioningArtifactParameter_parameterKey,
    provisioningArtifactParameter_parameterType,

    -- ** ProvisioningArtifactPreferences
    provisioningArtifactPreferences_stackSetAccounts,
    provisioningArtifactPreferences_stackSetRegions,

    -- ** ProvisioningArtifactProperties
    provisioningArtifactProperties_description,
    provisioningArtifactProperties_disableTemplateValidation,
    provisioningArtifactProperties_info,
    provisioningArtifactProperties_name,
    provisioningArtifactProperties_type,

    -- ** ProvisioningArtifactSummary
    provisioningArtifactSummary_createdTime,
    provisioningArtifactSummary_description,
    provisioningArtifactSummary_id,
    provisioningArtifactSummary_name,
    provisioningArtifactSummary_provisioningArtifactMetadata,

    -- ** ProvisioningArtifactView
    provisioningArtifactView_productViewSummary,
    provisioningArtifactView_provisioningArtifact,

    -- ** ProvisioningParameter
    provisioningParameter_key,
    provisioningParameter_value,

    -- ** ProvisioningPreferences
    provisioningPreferences_stackSetAccounts,
    provisioningPreferences_stackSetFailureToleranceCount,
    provisioningPreferences_stackSetFailureTolerancePercentage,
    provisioningPreferences_stackSetMaxConcurrencyCount,
    provisioningPreferences_stackSetMaxConcurrencyPercentage,
    provisioningPreferences_stackSetRegions,

    -- ** RecordDetail
    recordDetail_createdTime,
    recordDetail_launchRoleArn,
    recordDetail_pathId,
    recordDetail_productId,
    recordDetail_provisionedProductId,
    recordDetail_provisionedProductName,
    recordDetail_provisionedProductType,
    recordDetail_provisioningArtifactId,
    recordDetail_recordErrors,
    recordDetail_recordId,
    recordDetail_recordTags,
    recordDetail_recordType,
    recordDetail_status,
    recordDetail_updatedTime,

    -- ** RecordError
    recordError_code,
    recordError_description,

    -- ** RecordOutput
    recordOutput_description,
    recordOutput_outputKey,
    recordOutput_outputValue,

    -- ** RecordTag
    recordTag_key,
    recordTag_value,

    -- ** ResourceChange
    resourceChange_action,
    resourceChange_details,
    resourceChange_logicalResourceId,
    resourceChange_physicalResourceId,
    resourceChange_replacement,
    resourceChange_resourceType,
    resourceChange_scope,

    -- ** ResourceChangeDetail
    resourceChangeDetail_causingEntity,
    resourceChangeDetail_evaluation,
    resourceChangeDetail_target,

    -- ** ResourceDetail
    resourceDetail_arn,
    resourceDetail_createdTime,
    resourceDetail_description,
    resourceDetail_id,
    resourceDetail_name,

    -- ** ResourceTargetDefinition
    resourceTargetDefinition_attribute,
    resourceTargetDefinition_name,
    resourceTargetDefinition_requiresRecreation,

    -- ** ServiceActionAssociation
    serviceActionAssociation_serviceActionId,
    serviceActionAssociation_productId,
    serviceActionAssociation_provisioningArtifactId,

    -- ** ServiceActionDetail
    serviceActionDetail_definition,
    serviceActionDetail_serviceActionSummary,

    -- ** ServiceActionSummary
    serviceActionSummary_definitionType,
    serviceActionSummary_description,
    serviceActionSummary_id,
    serviceActionSummary_name,

    -- ** ShareDetails
    shareDetails_shareErrors,
    shareDetails_successfulShares,

    -- ** ShareError
    shareError_accounts,
    shareError_error,
    shareError_message,

    -- ** SourceConnection
    sourceConnection_type,
    sourceConnection_connectionParameters,

    -- ** SourceConnectionDetail
    sourceConnectionDetail_connectionParameters,
    sourceConnectionDetail_lastSync,
    sourceConnectionDetail_type,

    -- ** SourceConnectionParameters
    sourceConnectionParameters_codeStar,

    -- ** StackInstance
    stackInstance_account,
    stackInstance_region,
    stackInstance_stackInstanceStatus,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagOptionDetail
    tagOptionDetail_active,
    tagOptionDetail_id,
    tagOptionDetail_key,
    tagOptionDetail_owner,
    tagOptionDetail_value,

    -- ** TagOptionSummary
    tagOptionSummary_key,
    tagOptionSummary_values,

    -- ** UniqueTagResourceIdentifier
    uniqueTagResourceIdentifier_key,
    uniqueTagResourceIdentifier_value,

    -- ** UpdateProvisioningParameter
    updateProvisioningParameter_key,
    updateProvisioningParameter_usePreviousValue,
    updateProvisioningParameter_value,

    -- ** UpdateProvisioningPreferences
    updateProvisioningPreferences_stackSetAccounts,
    updateProvisioningPreferences_stackSetFailureToleranceCount,
    updateProvisioningPreferences_stackSetFailureTolerancePercentage,
    updateProvisioningPreferences_stackSetMaxConcurrencyCount,
    updateProvisioningPreferences_stackSetMaxConcurrencyPercentage,
    updateProvisioningPreferences_stackSetOperationType,
    updateProvisioningPreferences_stackSetRegions,

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
import Amazonka.ServiceCatalog.NotifyProvisionProductEngineWorkflowResult
import Amazonka.ServiceCatalog.NotifyTerminateProvisionedProductEngineWorkflowResult
import Amazonka.ServiceCatalog.NotifyUpdateProvisionedProductEngineWorkflowResult
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
import Amazonka.ServiceCatalog.Types.EngineWorkflowResourceIdentifier
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
import Amazonka.ServiceCatalog.Types.UniqueTagResourceIdentifier
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
