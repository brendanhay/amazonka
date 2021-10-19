{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Lens
  ( -- * Operations

    -- ** ImportAsProvisionedProduct
    importAsProvisionedProduct_acceptLanguage,
    importAsProvisionedProduct_productId,
    importAsProvisionedProduct_provisioningArtifactId,
    importAsProvisionedProduct_provisionedProductName,
    importAsProvisionedProduct_physicalId,
    importAsProvisionedProduct_idempotencyToken,
    importAsProvisionedProductResponse_recordDetail,
    importAsProvisionedProductResponse_httpStatus,

    -- ** DeleteConstraint
    deleteConstraint_acceptLanguage,
    deleteConstraint_id,
    deleteConstraintResponse_httpStatus,

    -- ** UpdateConstraint
    updateConstraint_acceptLanguage,
    updateConstraint_parameters,
    updateConstraint_description,
    updateConstraint_id,
    updateConstraintResponse_status,
    updateConstraintResponse_constraintDetail,
    updateConstraintResponse_constraintParameters,
    updateConstraintResponse_httpStatus,

    -- ** CreateProvisionedProductPlan
    createProvisionedProductPlan_notificationArns,
    createProvisionedProductPlan_acceptLanguage,
    createProvisionedProductPlan_pathId,
    createProvisionedProductPlan_provisioningParameters,
    createProvisionedProductPlan_tags,
    createProvisionedProductPlan_planName,
    createProvisionedProductPlan_planType,
    createProvisionedProductPlan_productId,
    createProvisionedProductPlan_provisionedProductName,
    createProvisionedProductPlan_provisioningArtifactId,
    createProvisionedProductPlan_idempotencyToken,
    createProvisionedProductPlanResponse_provisionedProductName,
    createProvisionedProductPlanResponse_provisionProductId,
    createProvisionedProductPlanResponse_provisioningArtifactId,
    createProvisionedProductPlanResponse_planId,
    createProvisionedProductPlanResponse_planName,
    createProvisionedProductPlanResponse_httpStatus,

    -- ** ExecuteProvisionedProductServiceAction
    executeProvisionedProductServiceAction_acceptLanguage,
    executeProvisionedProductServiceAction_parameters,
    executeProvisionedProductServiceAction_provisionedProductId,
    executeProvisionedProductServiceAction_serviceActionId,
    executeProvisionedProductServiceAction_executeToken,
    executeProvisionedProductServiceActionResponse_recordDetail,
    executeProvisionedProductServiceActionResponse_httpStatus,

    -- ** CreateProduct
    createProduct_supportUrl,
    createProduct_distributor,
    createProduct_acceptLanguage,
    createProduct_supportEmail,
    createProduct_description,
    createProduct_tags,
    createProduct_supportDescription,
    createProduct_name,
    createProduct_owner,
    createProduct_productType,
    createProduct_provisioningArtifactParameters,
    createProduct_idempotencyToken,
    createProductResponse_productViewDetail,
    createProductResponse_provisioningArtifactDetail,
    createProductResponse_tags,
    createProductResponse_httpStatus,

    -- ** DescribeCopyProductStatus
    describeCopyProductStatus_acceptLanguage,
    describeCopyProductStatus_copyProductToken,
    describeCopyProductStatusResponse_targetProductId,
    describeCopyProductStatusResponse_copyProductStatus,
    describeCopyProductStatusResponse_statusDetail,
    describeCopyProductStatusResponse_httpStatus,

    -- ** CreateServiceAction
    createServiceAction_acceptLanguage,
    createServiceAction_description,
    createServiceAction_name,
    createServiceAction_definitionType,
    createServiceAction_definition,
    createServiceAction_idempotencyToken,
    createServiceActionResponse_serviceActionDetail,
    createServiceActionResponse_httpStatus,

    -- ** TerminateProvisionedProduct
    terminateProvisionedProduct_provisionedProductName,
    terminateProvisionedProduct_retainPhysicalResources,
    terminateProvisionedProduct_acceptLanguage,
    terminateProvisionedProduct_ignoreErrors,
    terminateProvisionedProduct_provisionedProductId,
    terminateProvisionedProduct_terminateToken,
    terminateProvisionedProductResponse_recordDetail,
    terminateProvisionedProductResponse_httpStatus,

    -- ** UpdateProvisionedProduct
    updateProvisionedProduct_productName,
    updateProvisionedProduct_provisionedProductName,
    updateProvisionedProduct_provisioningArtifactId,
    updateProvisionedProduct_provisioningArtifactName,
    updateProvisionedProduct_pathName,
    updateProvisionedProduct_acceptLanguage,
    updateProvisionedProduct_pathId,
    updateProvisionedProduct_provisioningParameters,
    updateProvisionedProduct_provisionedProductId,
    updateProvisionedProduct_productId,
    updateProvisionedProduct_tags,
    updateProvisionedProduct_provisioningPreferences,
    updateProvisionedProduct_updateToken,
    updateProvisionedProductResponse_recordDetail,
    updateProvisionedProductResponse_httpStatus,

    -- ** DescribeProvisioningArtifact
    describeProvisioningArtifact_productName,
    describeProvisioningArtifact_provisioningArtifactId,
    describeProvisioningArtifact_verbose,
    describeProvisioningArtifact_provisioningArtifactName,
    describeProvisioningArtifact_acceptLanguage,
    describeProvisioningArtifact_productId,
    describeProvisioningArtifactResponse_status,
    describeProvisioningArtifactResponse_info,
    describeProvisioningArtifactResponse_provisioningArtifactDetail,
    describeProvisioningArtifactResponse_httpStatus,

    -- ** AssociateServiceActionWithProvisioningArtifact
    associateServiceActionWithProvisioningArtifact_acceptLanguage,
    associateServiceActionWithProvisioningArtifact_productId,
    associateServiceActionWithProvisioningArtifact_provisioningArtifactId,
    associateServiceActionWithProvisioningArtifact_serviceActionId,
    associateServiceActionWithProvisioningArtifactResponse_httpStatus,

    -- ** ListRecordHistory
    listRecordHistory_searchFilter,
    listRecordHistory_acceptLanguage,
    listRecordHistory_accessLevelFilter,
    listRecordHistory_pageToken,
    listRecordHistory_pageSize,
    listRecordHistoryResponse_nextPageToken,
    listRecordHistoryResponse_recordDetails,
    listRecordHistoryResponse_httpStatus,

    -- ** DescribeProvisionedProductPlan
    describeProvisionedProductPlan_acceptLanguage,
    describeProvisionedProductPlan_pageToken,
    describeProvisionedProductPlan_pageSize,
    describeProvisionedProductPlan_planId,
    describeProvisionedProductPlanResponse_nextPageToken,
    describeProvisionedProductPlanResponse_provisionedProductPlanDetails,
    describeProvisionedProductPlanResponse_resourceChanges,
    describeProvisionedProductPlanResponse_httpStatus,

    -- ** AssociateTagOptionWithResource
    associateTagOptionWithResource_resourceId,
    associateTagOptionWithResource_tagOptionId,
    associateTagOptionWithResourceResponse_httpStatus,

    -- ** CreateTagOption
    createTagOption_key,
    createTagOption_value,
    createTagOptionResponse_tagOptionDetail,
    createTagOptionResponse_httpStatus,

    -- ** ListBudgetsForResource
    listBudgetsForResource_acceptLanguage,
    listBudgetsForResource_pageToken,
    listBudgetsForResource_pageSize,
    listBudgetsForResource_resourceId,
    listBudgetsForResourceResponse_nextPageToken,
    listBudgetsForResourceResponse_budgets,
    listBudgetsForResourceResponse_httpStatus,

    -- ** DisassociateProductFromPortfolio
    disassociateProductFromPortfolio_acceptLanguage,
    disassociateProductFromPortfolio_productId,
    disassociateProductFromPortfolio_portfolioId,
    disassociateProductFromPortfolioResponse_httpStatus,

    -- ** ListConstraintsForPortfolio
    listConstraintsForPortfolio_acceptLanguage,
    listConstraintsForPortfolio_pageToken,
    listConstraintsForPortfolio_pageSize,
    listConstraintsForPortfolio_productId,
    listConstraintsForPortfolio_portfolioId,
    listConstraintsForPortfolioResponse_nextPageToken,
    listConstraintsForPortfolioResponse_constraintDetails,
    listConstraintsForPortfolioResponse_httpStatus,

    -- ** DescribeRecord
    describeRecord_acceptLanguage,
    describeRecord_pageToken,
    describeRecord_pageSize,
    describeRecord_id,
    describeRecordResponse_recordDetail,
    describeRecordResponse_nextPageToken,
    describeRecordResponse_recordOutputs,
    describeRecordResponse_httpStatus,

    -- ** EnableAWSOrganizationsAccess
    enableAWSOrganizationsAccessResponse_httpStatus,

    -- ** DescribeConstraint
    describeConstraint_acceptLanguage,
    describeConstraint_id,
    describeConstraintResponse_status,
    describeConstraintResponse_constraintDetail,
    describeConstraintResponse_constraintParameters,
    describeConstraintResponse_httpStatus,

    -- ** CreateProvisioningArtifact
    createProvisioningArtifact_acceptLanguage,
    createProvisioningArtifact_productId,
    createProvisioningArtifact_parameters,
    createProvisioningArtifact_idempotencyToken,
    createProvisioningArtifactResponse_status,
    createProvisioningArtifactResponse_info,
    createProvisioningArtifactResponse_provisioningArtifactDetail,
    createProvisioningArtifactResponse_httpStatus,

    -- ** ListPortfolios
    listPortfolios_acceptLanguage,
    listPortfolios_pageToken,
    listPortfolios_pageSize,
    listPortfoliosResponse_nextPageToken,
    listPortfoliosResponse_portfolioDetails,
    listPortfoliosResponse_httpStatus,

    -- ** DisassociateBudgetFromResource
    disassociateBudgetFromResource_budgetName,
    disassociateBudgetFromResource_resourceId,
    disassociateBudgetFromResourceResponse_httpStatus,

    -- ** DescribeProductView
    describeProductView_acceptLanguage,
    describeProductView_id,
    describeProductViewResponse_productViewSummary,
    describeProductViewResponse_provisioningArtifacts,
    describeProductViewResponse_httpStatus,

    -- ** CreatePortfolioShare
    createPortfolioShare_accountId,
    createPortfolioShare_shareTagOptions,
    createPortfolioShare_acceptLanguage,
    createPortfolioShare_organizationNode,
    createPortfolioShare_portfolioId,
    createPortfolioShareResponse_portfolioShareToken,
    createPortfolioShareResponse_httpStatus,

    -- ** ListProvisioningArtifacts
    listProvisioningArtifacts_acceptLanguage,
    listProvisioningArtifacts_productId,
    listProvisioningArtifactsResponse_nextPageToken,
    listProvisioningArtifactsResponse_provisioningArtifactDetails,
    listProvisioningArtifactsResponse_httpStatus,

    -- ** ListServiceActionsForProvisioningArtifact
    listServiceActionsForProvisioningArtifact_acceptLanguage,
    listServiceActionsForProvisioningArtifact_pageToken,
    listServiceActionsForProvisioningArtifact_pageSize,
    listServiceActionsForProvisioningArtifact_productId,
    listServiceActionsForProvisioningArtifact_provisioningArtifactId,
    listServiceActionsForProvisioningArtifactResponse_nextPageToken,
    listServiceActionsForProvisioningArtifactResponse_serviceActionSummaries,
    listServiceActionsForProvisioningArtifactResponse_httpStatus,

    -- ** SearchProducts
    searchProducts_filters,
    searchProducts_sortOrder,
    searchProducts_acceptLanguage,
    searchProducts_pageToken,
    searchProducts_pageSize,
    searchProducts_sortBy,
    searchProductsResponse_nextPageToken,
    searchProductsResponse_productViewAggregations,
    searchProductsResponse_productViewSummaries,
    searchProductsResponse_httpStatus,

    -- ** DescribeServiceActionExecutionParameters
    describeServiceActionExecutionParameters_acceptLanguage,
    describeServiceActionExecutionParameters_provisionedProductId,
    describeServiceActionExecutionParameters_serviceActionId,
    describeServiceActionExecutionParametersResponse_serviceActionParameters,
    describeServiceActionExecutionParametersResponse_httpStatus,

    -- ** SearchProvisionedProducts
    searchProvisionedProducts_filters,
    searchProvisionedProducts_sortOrder,
    searchProvisionedProducts_acceptLanguage,
    searchProvisionedProducts_accessLevelFilter,
    searchProvisionedProducts_pageToken,
    searchProvisionedProducts_pageSize,
    searchProvisionedProducts_sortBy,
    searchProvisionedProductsResponse_nextPageToken,
    searchProvisionedProductsResponse_provisionedProducts,
    searchProvisionedProductsResponse_totalResultsCount,
    searchProvisionedProductsResponse_httpStatus,

    -- ** ListStackInstancesForProvisionedProduct
    listStackInstancesForProvisionedProduct_acceptLanguage,
    listStackInstancesForProvisionedProduct_pageToken,
    listStackInstancesForProvisionedProduct_pageSize,
    listStackInstancesForProvisionedProduct_provisionedProductId,
    listStackInstancesForProvisionedProductResponse_nextPageToken,
    listStackInstancesForProvisionedProductResponse_stackInstances,
    listStackInstancesForProvisionedProductResponse_httpStatus,

    -- ** DescribeServiceAction
    describeServiceAction_acceptLanguage,
    describeServiceAction_id,
    describeServiceActionResponse_serviceActionDetail,
    describeServiceActionResponse_httpStatus,

    -- ** DescribeProduct
    describeProduct_name,
    describeProduct_acceptLanguage,
    describeProduct_id,
    describeProductResponse_productViewSummary,
    describeProductResponse_provisioningArtifacts,
    describeProductResponse_launchPaths,
    describeProductResponse_budgets,
    describeProductResponse_httpStatus,

    -- ** DeleteProvisionedProductPlan
    deleteProvisionedProductPlan_acceptLanguage,
    deleteProvisionedProductPlan_ignoreErrors,
    deleteProvisionedProductPlan_planId,
    deleteProvisionedProductPlanResponse_httpStatus,

    -- ** GetProvisionedProductOutputs
    getProvisionedProductOutputs_provisionedProductName,
    getProvisionedProductOutputs_outputKeys,
    getProvisionedProductOutputs_acceptLanguage,
    getProvisionedProductOutputs_pageToken,
    getProvisionedProductOutputs_pageSize,
    getProvisionedProductOutputs_provisionedProductId,
    getProvisionedProductOutputsResponse_nextPageToken,
    getProvisionedProductOutputsResponse_outputs,
    getProvisionedProductOutputsResponse_httpStatus,

    -- ** CreateConstraint
    createConstraint_acceptLanguage,
    createConstraint_description,
    createConstraint_portfolioId,
    createConstraint_productId,
    createConstraint_parameters,
    createConstraint_type,
    createConstraint_idempotencyToken,
    createConstraintResponse_status,
    createConstraintResponse_constraintDetail,
    createConstraintResponse_constraintParameters,
    createConstraintResponse_httpStatus,

    -- ** ListProvisionedProductPlans
    listProvisionedProductPlans_provisionProductId,
    listProvisionedProductPlans_acceptLanguage,
    listProvisionedProductPlans_accessLevelFilter,
    listProvisionedProductPlans_pageToken,
    listProvisionedProductPlans_pageSize,
    listProvisionedProductPlansResponse_nextPageToken,
    listProvisionedProductPlansResponse_provisionedProductPlans,
    listProvisionedProductPlansResponse_httpStatus,

    -- ** ListPortfolioAccess
    listPortfolioAccess_organizationParentId,
    listPortfolioAccess_acceptLanguage,
    listPortfolioAccess_pageToken,
    listPortfolioAccess_pageSize,
    listPortfolioAccess_portfolioId,
    listPortfolioAccessResponse_nextPageToken,
    listPortfolioAccessResponse_accountIds,
    listPortfolioAccessResponse_httpStatus,

    -- ** BatchDisassociateServiceActionFromProvisioningArtifact
    batchDisassociateServiceActionFromProvisioningArtifact_acceptLanguage,
    batchDisassociateServiceActionFromProvisioningArtifact_serviceActionAssociations,
    batchDisassociateServiceActionFromProvisioningArtifactResponse_failedServiceActionAssociations,
    batchDisassociateServiceActionFromProvisioningArtifactResponse_httpStatus,

    -- ** DisassociatePrincipalFromPortfolio
    disassociatePrincipalFromPortfolio_acceptLanguage,
    disassociatePrincipalFromPortfolio_portfolioId,
    disassociatePrincipalFromPortfolio_principalARN,
    disassociatePrincipalFromPortfolioResponse_httpStatus,

    -- ** DescribeTagOption
    describeTagOption_id,
    describeTagOptionResponse_tagOptionDetail,
    describeTagOptionResponse_httpStatus,

    -- ** DisassociateTagOptionFromResource
    disassociateTagOptionFromResource_resourceId,
    disassociateTagOptionFromResource_tagOptionId,
    disassociateTagOptionFromResourceResponse_httpStatus,

    -- ** DescribePortfolio
    describePortfolio_acceptLanguage,
    describePortfolio_id,
    describePortfolioResponse_portfolioDetail,
    describePortfolioResponse_tagOptions,
    describePortfolioResponse_budgets,
    describePortfolioResponse_tags,
    describePortfolioResponse_httpStatus,

    -- ** AssociateProductWithPortfolio
    associateProductWithPortfolio_sourcePortfolioId,
    associateProductWithPortfolio_acceptLanguage,
    associateProductWithPortfolio_productId,
    associateProductWithPortfolio_portfolioId,
    associateProductWithPortfolioResponse_httpStatus,

    -- ** ListAcceptedPortfolioShares
    listAcceptedPortfolioShares_portfolioShareType,
    listAcceptedPortfolioShares_acceptLanguage,
    listAcceptedPortfolioShares_pageToken,
    listAcceptedPortfolioShares_pageSize,
    listAcceptedPortfolioSharesResponse_nextPageToken,
    listAcceptedPortfolioSharesResponse_portfolioDetails,
    listAcceptedPortfolioSharesResponse_httpStatus,

    -- ** ExecuteProvisionedProductPlan
    executeProvisionedProductPlan_acceptLanguage,
    executeProvisionedProductPlan_planId,
    executeProvisionedProductPlan_idempotencyToken,
    executeProvisionedProductPlanResponse_recordDetail,
    executeProvisionedProductPlanResponse_httpStatus,

    -- ** AcceptPortfolioShare
    acceptPortfolioShare_portfolioShareType,
    acceptPortfolioShare_acceptLanguage,
    acceptPortfolioShare_portfolioId,
    acceptPortfolioShareResponse_httpStatus,

    -- ** ScanProvisionedProducts
    scanProvisionedProducts_acceptLanguage,
    scanProvisionedProducts_accessLevelFilter,
    scanProvisionedProducts_pageToken,
    scanProvisionedProducts_pageSize,
    scanProvisionedProductsResponse_nextPageToken,
    scanProvisionedProductsResponse_provisionedProducts,
    scanProvisionedProductsResponse_httpStatus,

    -- ** ListOrganizationPortfolioAccess
    listOrganizationPortfolioAccess_acceptLanguage,
    listOrganizationPortfolioAccess_pageToken,
    listOrganizationPortfolioAccess_pageSize,
    listOrganizationPortfolioAccess_portfolioId,
    listOrganizationPortfolioAccess_organizationNodeType,
    listOrganizationPortfolioAccessResponse_nextPageToken,
    listOrganizationPortfolioAccessResponse_organizationNodes,
    listOrganizationPortfolioAccessResponse_httpStatus,

    -- ** ListPrincipalsForPortfolio
    listPrincipalsForPortfolio_acceptLanguage,
    listPrincipalsForPortfolio_pageToken,
    listPrincipalsForPortfolio_pageSize,
    listPrincipalsForPortfolio_portfolioId,
    listPrincipalsForPortfolioResponse_nextPageToken,
    listPrincipalsForPortfolioResponse_principals,
    listPrincipalsForPortfolioResponse_httpStatus,

    -- ** DeleteProduct
    deleteProduct_acceptLanguage,
    deleteProduct_id,
    deleteProductResponse_httpStatus,

    -- ** UpdateProduct
    updateProduct_removeTags,
    updateProduct_owner,
    updateProduct_supportUrl,
    updateProduct_distributor,
    updateProduct_name,
    updateProduct_acceptLanguage,
    updateProduct_addTags,
    updateProduct_supportEmail,
    updateProduct_description,
    updateProduct_supportDescription,
    updateProduct_id,
    updateProductResponse_productViewDetail,
    updateProductResponse_tags,
    updateProductResponse_httpStatus,

    -- ** ListServiceActions
    listServiceActions_acceptLanguage,
    listServiceActions_pageToken,
    listServiceActions_pageSize,
    listServiceActionsResponse_nextPageToken,
    listServiceActionsResponse_serviceActionSummaries,
    listServiceActionsResponse_httpStatus,

    -- ** ProvisionProduct
    provisionProduct_productName,
    provisionProduct_provisioningArtifactId,
    provisionProduct_provisioningArtifactName,
    provisionProduct_notificationArns,
    provisionProduct_pathName,
    provisionProduct_acceptLanguage,
    provisionProduct_pathId,
    provisionProduct_provisioningParameters,
    provisionProduct_productId,
    provisionProduct_tags,
    provisionProduct_provisioningPreferences,
    provisionProduct_provisionedProductName,
    provisionProduct_provisionToken,
    provisionProductResponse_recordDetail,
    provisionProductResponse_httpStatus,

    -- ** DeleteServiceAction
    deleteServiceAction_acceptLanguage,
    deleteServiceAction_id,
    deleteServiceActionResponse_httpStatus,

    -- ** UpdateServiceAction
    updateServiceAction_definition,
    updateServiceAction_name,
    updateServiceAction_acceptLanguage,
    updateServiceAction_description,
    updateServiceAction_id,
    updateServiceActionResponse_serviceActionDetail,
    updateServiceActionResponse_httpStatus,

    -- ** DisableAWSOrganizationsAccess
    disableAWSOrganizationsAccessResponse_httpStatus,

    -- ** RejectPortfolioShare
    rejectPortfolioShare_portfolioShareType,
    rejectPortfolioShare_acceptLanguage,
    rejectPortfolioShare_portfolioId,
    rejectPortfolioShareResponse_httpStatus,

    -- ** DisassociateServiceActionFromProvisioningArtifact
    disassociateServiceActionFromProvisioningArtifact_acceptLanguage,
    disassociateServiceActionFromProvisioningArtifact_productId,
    disassociateServiceActionFromProvisioningArtifact_provisioningArtifactId,
    disassociateServiceActionFromProvisioningArtifact_serviceActionId,
    disassociateServiceActionFromProvisioningArtifactResponse_httpStatus,

    -- ** DeleteTagOption
    deleteTagOption_id,
    deleteTagOptionResponse_httpStatus,

    -- ** UpdateTagOption
    updateTagOption_value,
    updateTagOption_active,
    updateTagOption_id,
    updateTagOptionResponse_tagOptionDetail,
    updateTagOptionResponse_httpStatus,

    -- ** ListTagOptions
    listTagOptions_filters,
    listTagOptions_pageToken,
    listTagOptions_pageSize,
    listTagOptionsResponse_pageToken,
    listTagOptionsResponse_tagOptionDetails,
    listTagOptionsResponse_httpStatus,

    -- ** UpdateProvisionedProductProperties
    updateProvisionedProductProperties_acceptLanguage,
    updateProvisionedProductProperties_provisionedProductId,
    updateProvisionedProductProperties_provisionedProductProperties,
    updateProvisionedProductProperties_idempotencyToken,
    updateProvisionedProductPropertiesResponse_status,
    updateProvisionedProductPropertiesResponse_provisionedProductProperties,
    updateProvisionedProductPropertiesResponse_recordId,
    updateProvisionedProductPropertiesResponse_provisionedProductId,
    updateProvisionedProductPropertiesResponse_httpStatus,

    -- ** SearchProductsAsAdmin
    searchProductsAsAdmin_portfolioId,
    searchProductsAsAdmin_filters,
    searchProductsAsAdmin_sortOrder,
    searchProductsAsAdmin_acceptLanguage,
    searchProductsAsAdmin_pageToken,
    searchProductsAsAdmin_pageSize,
    searchProductsAsAdmin_productSource,
    searchProductsAsAdmin_sortBy,
    searchProductsAsAdminResponse_nextPageToken,
    searchProductsAsAdminResponse_productViewDetails,
    searchProductsAsAdminResponse_httpStatus,

    -- ** DeletePortfolio
    deletePortfolio_acceptLanguage,
    deletePortfolio_id,
    deletePortfolioResponse_httpStatus,

    -- ** UpdatePortfolio
    updatePortfolio_removeTags,
    updatePortfolio_acceptLanguage,
    updatePortfolio_displayName,
    updatePortfolio_addTags,
    updatePortfolio_description,
    updatePortfolio_providerName,
    updatePortfolio_id,
    updatePortfolioResponse_portfolioDetail,
    updatePortfolioResponse_tags,
    updatePortfolioResponse_httpStatus,

    -- ** ListPortfoliosForProduct
    listPortfoliosForProduct_acceptLanguage,
    listPortfoliosForProduct_pageToken,
    listPortfoliosForProduct_pageSize,
    listPortfoliosForProduct_productId,
    listPortfoliosForProductResponse_nextPageToken,
    listPortfoliosForProductResponse_portfolioDetails,
    listPortfoliosForProductResponse_httpStatus,

    -- ** GetAWSOrganizationsAccessStatus
    getAWSOrganizationsAccessStatusResponse_accessStatus,
    getAWSOrganizationsAccessStatusResponse_httpStatus,

    -- ** DescribeProductAsAdmin
    describeProductAsAdmin_sourcePortfolioId,
    describeProductAsAdmin_name,
    describeProductAsAdmin_acceptLanguage,
    describeProductAsAdmin_id,
    describeProductAsAdminResponse_productViewDetail,
    describeProductAsAdminResponse_tagOptions,
    describeProductAsAdminResponse_provisioningArtifactSummaries,
    describeProductAsAdminResponse_budgets,
    describeProductAsAdminResponse_tags,
    describeProductAsAdminResponse_httpStatus,

    -- ** BatchAssociateServiceActionWithProvisioningArtifact
    batchAssociateServiceActionWithProvisioningArtifact_acceptLanguage,
    batchAssociateServiceActionWithProvisioningArtifact_serviceActionAssociations,
    batchAssociateServiceActionWithProvisioningArtifactResponse_failedServiceActionAssociations,
    batchAssociateServiceActionWithProvisioningArtifactResponse_httpStatus,

    -- ** DescribeProvisioningParameters
    describeProvisioningParameters_productName,
    describeProvisioningParameters_provisioningArtifactId,
    describeProvisioningParameters_provisioningArtifactName,
    describeProvisioningParameters_pathName,
    describeProvisioningParameters_acceptLanguage,
    describeProvisioningParameters_pathId,
    describeProvisioningParameters_productId,
    describeProvisioningParametersResponse_provisioningArtifactPreferences,
    describeProvisioningParametersResponse_provisioningArtifactParameters,
    describeProvisioningParametersResponse_usageInstructions,
    describeProvisioningParametersResponse_constraintSummaries,
    describeProvisioningParametersResponse_tagOptions,
    describeProvisioningParametersResponse_provisioningArtifactOutputs,
    describeProvisioningParametersResponse_httpStatus,

    -- ** AssociatePrincipalWithPortfolio
    associatePrincipalWithPortfolio_acceptLanguage,
    associatePrincipalWithPortfolio_portfolioId,
    associatePrincipalWithPortfolio_principalARN,
    associatePrincipalWithPortfolio_principalType,
    associatePrincipalWithPortfolioResponse_httpStatus,

    -- ** DescribeProvisionedProduct
    describeProvisionedProduct_name,
    describeProvisionedProduct_acceptLanguage,
    describeProvisionedProduct_id,
    describeProvisionedProductResponse_provisionedProductDetail,
    describeProvisionedProductResponse_cloudWatchDashboards,
    describeProvisionedProductResponse_httpStatus,

    -- ** CopyProduct
    copyProduct_targetProductId,
    copyProduct_sourceProvisioningArtifactIdentifiers,
    copyProduct_targetProductName,
    copyProduct_copyOptions,
    copyProduct_acceptLanguage,
    copyProduct_sourceProductArn,
    copyProduct_idempotencyToken,
    copyProductResponse_copyProductToken,
    copyProductResponse_httpStatus,

    -- ** DescribePortfolioShareStatus
    describePortfolioShareStatus_portfolioShareToken,
    describePortfolioShareStatusResponse_status,
    describePortfolioShareStatusResponse_portfolioShareToken,
    describePortfolioShareStatusResponse_shareDetails,
    describePortfolioShareStatusResponse_portfolioId,
    describePortfolioShareStatusResponse_organizationNodeValue,
    describePortfolioShareStatusResponse_httpStatus,

    -- ** UpdateProvisioningArtifact
    updateProvisioningArtifact_active,
    updateProvisioningArtifact_name,
    updateProvisioningArtifact_acceptLanguage,
    updateProvisioningArtifact_guidance,
    updateProvisioningArtifact_description,
    updateProvisioningArtifact_productId,
    updateProvisioningArtifact_provisioningArtifactId,
    updateProvisioningArtifactResponse_status,
    updateProvisioningArtifactResponse_info,
    updateProvisioningArtifactResponse_provisioningArtifactDetail,
    updateProvisioningArtifactResponse_httpStatus,

    -- ** DeletePortfolioShare
    deletePortfolioShare_accountId,
    deletePortfolioShare_acceptLanguage,
    deletePortfolioShare_organizationNode,
    deletePortfolioShare_portfolioId,
    deletePortfolioShareResponse_portfolioShareToken,
    deletePortfolioShareResponse_httpStatus,

    -- ** DeleteProvisioningArtifact
    deleteProvisioningArtifact_acceptLanguage,
    deleteProvisioningArtifact_productId,
    deleteProvisioningArtifact_provisioningArtifactId,
    deleteProvisioningArtifactResponse_httpStatus,

    -- ** UpdatePortfolioShare
    updatePortfolioShare_accountId,
    updatePortfolioShare_shareTagOptions,
    updatePortfolioShare_acceptLanguage,
    updatePortfolioShare_organizationNode,
    updatePortfolioShare_portfolioId,
    updatePortfolioShareResponse_status,
    updatePortfolioShareResponse_portfolioShareToken,
    updatePortfolioShareResponse_httpStatus,

    -- ** ListProvisioningArtifactsForServiceAction
    listProvisioningArtifactsForServiceAction_acceptLanguage,
    listProvisioningArtifactsForServiceAction_pageToken,
    listProvisioningArtifactsForServiceAction_pageSize,
    listProvisioningArtifactsForServiceAction_serviceActionId,
    listProvisioningArtifactsForServiceActionResponse_nextPageToken,
    listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews,
    listProvisioningArtifactsForServiceActionResponse_httpStatus,

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

    -- ** ListLaunchPaths
    listLaunchPaths_acceptLanguage,
    listLaunchPaths_pageToken,
    listLaunchPaths_pageSize,
    listLaunchPaths_productId,
    listLaunchPathsResponse_nextPageToken,
    listLaunchPathsResponse_launchPathSummaries,
    listLaunchPathsResponse_httpStatus,

    -- ** DescribePortfolioShares
    describePortfolioShares_pageToken,
    describePortfolioShares_pageSize,
    describePortfolioShares_portfolioId,
    describePortfolioShares_type,
    describePortfolioSharesResponse_nextPageToken,
    describePortfolioSharesResponse_portfolioShareDetails,
    describePortfolioSharesResponse_httpStatus,

    -- ** ListResourcesForTagOption
    listResourcesForTagOption_resourceType,
    listResourcesForTagOption_pageToken,
    listResourcesForTagOption_pageSize,
    listResourcesForTagOption_tagOptionId,
    listResourcesForTagOptionResponse_resourceDetails,
    listResourcesForTagOptionResponse_pageToken,
    listResourcesForTagOptionResponse_httpStatus,

    -- ** AssociateBudgetWithResource
    associateBudgetWithResource_budgetName,
    associateBudgetWithResource_resourceId,
    associateBudgetWithResourceResponse_httpStatus,

    -- * Types

    -- ** AccessLevelFilter
    accessLevelFilter_value,
    accessLevelFilter_key,

    -- ** BudgetDetail
    budgetDetail_budgetName,

    -- ** CloudWatchDashboard
    cloudWatchDashboard_name,

    -- ** ConstraintDetail
    constraintDetail_portfolioId,
    constraintDetail_constraintId,
    constraintDetail_owner,
    constraintDetail_type,
    constraintDetail_description,
    constraintDetail_productId,

    -- ** ConstraintSummary
    constraintSummary_type,
    constraintSummary_description,

    -- ** ExecutionParameter
    executionParameter_defaultValues,
    executionParameter_name,
    executionParameter_type,

    -- ** FailedServiceActionAssociation
    failedServiceActionAssociation_provisioningArtifactId,
    failedServiceActionAssociation_errorCode,
    failedServiceActionAssociation_errorMessage,
    failedServiceActionAssociation_serviceActionId,
    failedServiceActionAssociation_productId,

    -- ** LaunchPath
    launchPath_name,
    launchPath_id,

    -- ** LaunchPathSummary
    launchPathSummary_constraintSummaries,
    launchPathSummary_name,
    launchPathSummary_id,
    launchPathSummary_tags,

    -- ** ListRecordHistorySearchFilter
    listRecordHistorySearchFilter_value,
    listRecordHistorySearchFilter_key,

    -- ** ListTagOptionsFilters
    listTagOptionsFilters_value,
    listTagOptionsFilters_active,
    listTagOptionsFilters_key,

    -- ** OrganizationNode
    organizationNode_value,
    organizationNode_type,

    -- ** ParameterConstraints
    parameterConstraints_maxValue,
    parameterConstraints_maxLength,
    parameterConstraints_constraintDescription,
    parameterConstraints_minLength,
    parameterConstraints_allowedPattern,
    parameterConstraints_allowedValues,
    parameterConstraints_minValue,

    -- ** PortfolioDetail
    portfolioDetail_arn,
    portfolioDetail_createdTime,
    portfolioDetail_id,
    portfolioDetail_displayName,
    portfolioDetail_description,
    portfolioDetail_providerName,

    -- ** PortfolioShareDetail
    portfolioShareDetail_principalId,
    portfolioShareDetail_shareTagOptions,
    portfolioShareDetail_type,
    portfolioShareDetail_accepted,

    -- ** Principal
    principal_principalType,
    principal_principalARN,

    -- ** ProductViewAggregationValue
    productViewAggregationValue_value,
    productViewAggregationValue_approximateCount,

    -- ** ProductViewDetail
    productViewDetail_status,
    productViewDetail_productViewSummary,
    productViewDetail_createdTime,
    productViewDetail_productARN,

    -- ** ProductViewSummary
    productViewSummary_owner,
    productViewSummary_supportUrl,
    productViewSummary_shortDescription,
    productViewSummary_hasDefaultPath,
    productViewSummary_distributor,
    productViewSummary_name,
    productViewSummary_id,
    productViewSummary_type,
    productViewSummary_supportEmail,
    productViewSummary_productId,
    productViewSummary_supportDescription,

    -- ** ProvisionedProductAttribute
    provisionedProductAttribute_idempotencyToken,
    provisionedProductAttribute_status,
    provisionedProductAttribute_productName,
    provisionedProductAttribute_lastSuccessfulProvisioningRecordId,
    provisionedProductAttribute_provisioningArtifactId,
    provisionedProductAttribute_arn,
    provisionedProductAttribute_createdTime,
    provisionedProductAttribute_provisioningArtifactName,
    provisionedProductAttribute_userArn,
    provisionedProductAttribute_statusMessage,
    provisionedProductAttribute_name,
    provisionedProductAttribute_lastRecordId,
    provisionedProductAttribute_userArnSession,
    provisionedProductAttribute_id,
    provisionedProductAttribute_type,
    provisionedProductAttribute_physicalId,
    provisionedProductAttribute_lastProvisioningRecordId,
    provisionedProductAttribute_productId,
    provisionedProductAttribute_tags,

    -- ** ProvisionedProductDetail
    provisionedProductDetail_launchRoleArn,
    provisionedProductDetail_idempotencyToken,
    provisionedProductDetail_status,
    provisionedProductDetail_lastSuccessfulProvisioningRecordId,
    provisionedProductDetail_provisioningArtifactId,
    provisionedProductDetail_arn,
    provisionedProductDetail_createdTime,
    provisionedProductDetail_statusMessage,
    provisionedProductDetail_name,
    provisionedProductDetail_lastRecordId,
    provisionedProductDetail_id,
    provisionedProductDetail_type,
    provisionedProductDetail_lastProvisioningRecordId,
    provisionedProductDetail_productId,

    -- ** ProvisionedProductPlanDetails
    provisionedProductPlanDetails_status,
    provisionedProductPlanDetails_provisionProductId,
    provisionedProductPlanDetails_provisioningArtifactId,
    provisionedProductPlanDetails_provisionProductName,
    provisionedProductPlanDetails_createdTime,
    provisionedProductPlanDetails_notificationArns,
    provisionedProductPlanDetails_planId,
    provisionedProductPlanDetails_planName,
    provisionedProductPlanDetails_statusMessage,
    provisionedProductPlanDetails_updatedTime,
    provisionedProductPlanDetails_pathId,
    provisionedProductPlanDetails_provisioningParameters,
    provisionedProductPlanDetails_planType,
    provisionedProductPlanDetails_productId,
    provisionedProductPlanDetails_tags,

    -- ** ProvisionedProductPlanSummary
    provisionedProductPlanSummary_provisionProductId,
    provisionedProductPlanSummary_provisioningArtifactId,
    provisionedProductPlanSummary_provisionProductName,
    provisionedProductPlanSummary_planId,
    provisionedProductPlanSummary_planName,
    provisionedProductPlanSummary_planType,

    -- ** ProvisioningArtifact
    provisioningArtifact_createdTime,
    provisioningArtifact_name,
    provisioningArtifact_id,
    provisioningArtifact_guidance,
    provisioningArtifact_description,

    -- ** ProvisioningArtifactDetail
    provisioningArtifactDetail_createdTime,
    provisioningArtifactDetail_active,
    provisioningArtifactDetail_name,
    provisioningArtifactDetail_id,
    provisioningArtifactDetail_type,
    provisioningArtifactDetail_guidance,
    provisioningArtifactDetail_description,

    -- ** ProvisioningArtifactOutput
    provisioningArtifactOutput_key,
    provisioningArtifactOutput_description,

    -- ** ProvisioningArtifactParameter
    provisioningArtifactParameter_isNoEcho,
    provisioningArtifactParameter_parameterKey,
    provisioningArtifactParameter_parameterType,
    provisioningArtifactParameter_parameterConstraints,
    provisioningArtifactParameter_defaultValue,
    provisioningArtifactParameter_description,

    -- ** ProvisioningArtifactPreferences
    provisioningArtifactPreferences_stackSetRegions,
    provisioningArtifactPreferences_stackSetAccounts,

    -- ** ProvisioningArtifactProperties
    provisioningArtifactProperties_disableTemplateValidation,
    provisioningArtifactProperties_name,
    provisioningArtifactProperties_type,
    provisioningArtifactProperties_description,
    provisioningArtifactProperties_info,

    -- ** ProvisioningArtifactSummary
    provisioningArtifactSummary_provisioningArtifactMetadata,
    provisioningArtifactSummary_createdTime,
    provisioningArtifactSummary_name,
    provisioningArtifactSummary_id,
    provisioningArtifactSummary_description,

    -- ** ProvisioningArtifactView
    provisioningArtifactView_productViewSummary,
    provisioningArtifactView_provisioningArtifact,

    -- ** ProvisioningParameter
    provisioningParameter_value,
    provisioningParameter_key,

    -- ** ProvisioningPreferences
    provisioningPreferences_stackSetRegions,
    provisioningPreferences_stackSetMaxConcurrencyPercentage,
    provisioningPreferences_stackSetFailureToleranceCount,
    provisioningPreferences_stackSetFailureTolerancePercentage,
    provisioningPreferences_stackSetAccounts,
    provisioningPreferences_stackSetMaxConcurrencyCount,

    -- ** RecordDetail
    recordDetail_launchRoleArn,
    recordDetail_status,
    recordDetail_recordTags,
    recordDetail_provisionedProductName,
    recordDetail_provisioningArtifactId,
    recordDetail_createdTime,
    recordDetail_recordType,
    recordDetail_recordId,
    recordDetail_provisionedProductType,
    recordDetail_updatedTime,
    recordDetail_pathId,
    recordDetail_provisionedProductId,
    recordDetail_recordErrors,
    recordDetail_productId,

    -- ** RecordError
    recordError_code,
    recordError_description,

    -- ** RecordOutput
    recordOutput_outputValue,
    recordOutput_outputKey,
    recordOutput_description,

    -- ** RecordTag
    recordTag_value,
    recordTag_key,

    -- ** ResourceChange
    resourceChange_logicalResourceId,
    resourceChange_physicalResourceId,
    resourceChange_resourceType,
    resourceChange_action,
    resourceChange_scope,
    resourceChange_details,
    resourceChange_replacement,

    -- ** ResourceChangeDetail
    resourceChangeDetail_causingEntity,
    resourceChangeDetail_evaluation,
    resourceChangeDetail_target,

    -- ** ResourceDetail
    resourceDetail_arn,
    resourceDetail_createdTime,
    resourceDetail_name,
    resourceDetail_id,
    resourceDetail_description,

    -- ** ResourceTargetDefinition
    resourceTargetDefinition_attribute,
    resourceTargetDefinition_requiresRecreation,
    resourceTargetDefinition_name,

    -- ** ServiceActionAssociation
    serviceActionAssociation_serviceActionId,
    serviceActionAssociation_productId,
    serviceActionAssociation_provisioningArtifactId,

    -- ** ServiceActionDetail
    serviceActionDetail_serviceActionSummary,
    serviceActionDetail_definition,

    -- ** ServiceActionSummary
    serviceActionSummary_name,
    serviceActionSummary_id,
    serviceActionSummary_definitionType,
    serviceActionSummary_description,

    -- ** ShareDetails
    shareDetails_shareErrors,
    shareDetails_successfulShares,

    -- ** ShareError
    shareError_accounts,
    shareError_error,
    shareError_message,

    -- ** StackInstance
    stackInstance_account,
    stackInstance_region,
    stackInstance_stackInstanceStatus,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagOptionDetail
    tagOptionDetail_value,
    tagOptionDetail_owner,
    tagOptionDetail_active,
    tagOptionDetail_key,
    tagOptionDetail_id,

    -- ** TagOptionSummary
    tagOptionSummary_values,
    tagOptionSummary_key,

    -- ** UpdateProvisioningParameter
    updateProvisioningParameter_value,
    updateProvisioningParameter_key,
    updateProvisioningParameter_usePreviousValue,

    -- ** UpdateProvisioningPreferences
    updateProvisioningPreferences_stackSetRegions,
    updateProvisioningPreferences_stackSetMaxConcurrencyPercentage,
    updateProvisioningPreferences_stackSetFailureToleranceCount,
    updateProvisioningPreferences_stackSetFailureTolerancePercentage,
    updateProvisioningPreferences_stackSetAccounts,
    updateProvisioningPreferences_stackSetMaxConcurrencyCount,
    updateProvisioningPreferences_stackSetOperationType,

    -- ** UsageInstruction
    usageInstruction_value,
    usageInstruction_type,
  )
where

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
import Network.AWS.ServiceCatalog.DescribePortfolioShares
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
import Network.AWS.ServiceCatalog.Types.AccessLevelFilter
import Network.AWS.ServiceCatalog.Types.BudgetDetail
import Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
import Network.AWS.ServiceCatalog.Types.ConstraintDetail
import Network.AWS.ServiceCatalog.Types.ConstraintSummary
import Network.AWS.ServiceCatalog.Types.ExecutionParameter
import Network.AWS.ServiceCatalog.Types.FailedServiceActionAssociation
import Network.AWS.ServiceCatalog.Types.LaunchPath
import Network.AWS.ServiceCatalog.Types.LaunchPathSummary
import Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter
import Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
import Network.AWS.ServiceCatalog.Types.OrganizationNode
import Network.AWS.ServiceCatalog.Types.ParameterConstraints
import Network.AWS.ServiceCatalog.Types.PortfolioDetail
import Network.AWS.ServiceCatalog.Types.PortfolioShareDetail
import Network.AWS.ServiceCatalog.Types.Principal
import Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
import Network.AWS.ServiceCatalog.Types.ProductViewDetail
import Network.AWS.ServiceCatalog.Types.ProductViewSummary
import Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute
import Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanSummary
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView
import Network.AWS.ServiceCatalog.Types.ProvisioningParameter
import Network.AWS.ServiceCatalog.Types.ProvisioningPreferences
import Network.AWS.ServiceCatalog.Types.RecordDetail
import Network.AWS.ServiceCatalog.Types.RecordError
import Network.AWS.ServiceCatalog.Types.RecordOutput
import Network.AWS.ServiceCatalog.Types.RecordTag
import Network.AWS.ServiceCatalog.Types.ResourceChange
import Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
import Network.AWS.ServiceCatalog.Types.ResourceDetail
import Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
import Network.AWS.ServiceCatalog.Types.ServiceActionAssociation
import Network.AWS.ServiceCatalog.Types.ServiceActionDetail
import Network.AWS.ServiceCatalog.Types.ServiceActionSummary
import Network.AWS.ServiceCatalog.Types.ShareDetails
import Network.AWS.ServiceCatalog.Types.ShareError
import Network.AWS.ServiceCatalog.Types.StackInstance
import Network.AWS.ServiceCatalog.Types.Tag
import Network.AWS.ServiceCatalog.Types.TagOptionDetail
import Network.AWS.ServiceCatalog.Types.TagOptionSummary
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningPreferences
import Network.AWS.ServiceCatalog.Types.UsageInstruction
import Network.AWS.ServiceCatalog.UpdateConstraint
import Network.AWS.ServiceCatalog.UpdatePortfolio
import Network.AWS.ServiceCatalog.UpdatePortfolioShare
import Network.AWS.ServiceCatalog.UpdateProduct
import Network.AWS.ServiceCatalog.UpdateProvisionedProduct
import Network.AWS.ServiceCatalog.UpdateProvisionedProductProperties
import Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
import Network.AWS.ServiceCatalog.UpdateServiceAction
import Network.AWS.ServiceCatalog.UpdateTagOption
