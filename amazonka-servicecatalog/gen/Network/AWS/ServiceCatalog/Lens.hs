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

    -- ** DescribePortfolio
    describePortfolio_acceptLanguage,
    describePortfolio_id,
    describePortfolioResponse_tags,
    describePortfolioResponse_budgets,
    describePortfolioResponse_portfolioDetail,
    describePortfolioResponse_tagOptions,
    describePortfolioResponse_httpStatus,

    -- ** ListAcceptedPortfolioShares
    listAcceptedPortfolioShares_portfolioShareType,
    listAcceptedPortfolioShares_pageSize,
    listAcceptedPortfolioShares_pageToken,
    listAcceptedPortfolioShares_acceptLanguage,
    listAcceptedPortfolioSharesResponse_portfolioDetails,
    listAcceptedPortfolioSharesResponse_nextPageToken,
    listAcceptedPortfolioSharesResponse_httpStatus,

    -- ** DisassociateTagOptionFromResource
    disassociateTagOptionFromResource_resourceId,
    disassociateTagOptionFromResource_tagOptionId,
    disassociateTagOptionFromResourceResponse_httpStatus,

    -- ** ScanProvisionedProducts
    scanProvisionedProducts_pageSize,
    scanProvisionedProducts_pageToken,
    scanProvisionedProducts_accessLevelFilter,
    scanProvisionedProducts_acceptLanguage,
    scanProvisionedProductsResponse_provisionedProducts,
    scanProvisionedProductsResponse_nextPageToken,
    scanProvisionedProductsResponse_httpStatus,

    -- ** AssociateProductWithPortfolio
    associateProductWithPortfolio_sourcePortfolioId,
    associateProductWithPortfolio_acceptLanguage,
    associateProductWithPortfolio_productId,
    associateProductWithPortfolio_portfolioId,
    associateProductWithPortfolioResponse_httpStatus,

    -- ** ListOrganizationPortfolioAccess
    listOrganizationPortfolioAccess_pageSize,
    listOrganizationPortfolioAccess_pageToken,
    listOrganizationPortfolioAccess_acceptLanguage,
    listOrganizationPortfolioAccess_portfolioId,
    listOrganizationPortfolioAccess_organizationNodeType,
    listOrganizationPortfolioAccessResponse_organizationNodes,
    listOrganizationPortfolioAccessResponse_nextPageToken,
    listOrganizationPortfolioAccessResponse_httpStatus,

    -- ** ExecuteProvisionedProductPlan
    executeProvisionedProductPlan_acceptLanguage,
    executeProvisionedProductPlan_planId,
    executeProvisionedProductPlan_idempotencyToken,
    executeProvisionedProductPlanResponse_recordDetail,
    executeProvisionedProductPlanResponse_httpStatus,

    -- ** ExecuteProvisionedProductServiceAction
    executeProvisionedProductServiceAction_parameters,
    executeProvisionedProductServiceAction_acceptLanguage,
    executeProvisionedProductServiceAction_provisionedProductId,
    executeProvisionedProductServiceAction_serviceActionId,
    executeProvisionedProductServiceAction_executeToken,
    executeProvisionedProductServiceActionResponse_recordDetail,
    executeProvisionedProductServiceActionResponse_httpStatus,

    -- ** ImportAsProvisionedProduct
    importAsProvisionedProduct_acceptLanguage,
    importAsProvisionedProduct_productId,
    importAsProvisionedProduct_provisioningArtifactId,
    importAsProvisionedProduct_provisionedProductName,
    importAsProvisionedProduct_physicalId,
    importAsProvisionedProduct_idempotencyToken,
    importAsProvisionedProductResponse_recordDetail,
    importAsProvisionedProductResponse_httpStatus,

    -- ** ListPortfolioAccess
    listPortfolioAccess_pageSize,
    listPortfolioAccess_pageToken,
    listPortfolioAccess_organizationParentId,
    listPortfolioAccess_acceptLanguage,
    listPortfolioAccess_portfolioId,
    listPortfolioAccessResponse_accountIds,
    listPortfolioAccessResponse_nextPageToken,
    listPortfolioAccessResponse_httpStatus,

    -- ** CreateProvisionedProductPlan
    createProvisionedProductPlan_notificationArns,
    createProvisionedProductPlan_tags,
    createProvisionedProductPlan_provisioningParameters,
    createProvisionedProductPlan_pathId,
    createProvisionedProductPlan_acceptLanguage,
    createProvisionedProductPlan_planName,
    createProvisionedProductPlan_planType,
    createProvisionedProductPlan_productId,
    createProvisionedProductPlan_provisionedProductName,
    createProvisionedProductPlan_provisioningArtifactId,
    createProvisionedProductPlan_idempotencyToken,
    createProvisionedProductPlanResponse_provisionProductId,
    createProvisionedProductPlanResponse_provisionedProductName,
    createProvisionedProductPlanResponse_provisioningArtifactId,
    createProvisionedProductPlanResponse_planName,
    createProvisionedProductPlanResponse_planId,
    createProvisionedProductPlanResponse_httpStatus,

    -- ** DescribeTagOption
    describeTagOption_id,
    describeTagOptionResponse_tagOptionDetail,
    describeTagOptionResponse_httpStatus,

    -- ** DeleteConstraint
    deleteConstraint_acceptLanguage,
    deleteConstraint_id,
    deleteConstraintResponse_httpStatus,

    -- ** UpdateConstraint
    updateConstraint_description,
    updateConstraint_parameters,
    updateConstraint_acceptLanguage,
    updateConstraint_id,
    updateConstraintResponse_constraintParameters,
    updateConstraintResponse_status,
    updateConstraintResponse_constraintDetail,
    updateConstraintResponse_httpStatus,

    -- ** ListResourcesForTagOption
    listResourcesForTagOption_pageSize,
    listResourcesForTagOption_pageToken,
    listResourcesForTagOption_resourceType,
    listResourcesForTagOption_tagOptionId,
    listResourcesForTagOptionResponse_pageToken,
    listResourcesForTagOptionResponse_resourceDetails,
    listResourcesForTagOptionResponse_httpStatus,

    -- ** DescribePortfolioShares
    describePortfolioShares_pageSize,
    describePortfolioShares_pageToken,
    describePortfolioShares_portfolioId,
    describePortfolioShares_type,
    describePortfolioSharesResponse_nextPageToken,
    describePortfolioSharesResponse_portfolioShareDetails,
    describePortfolioSharesResponse_httpStatus,

    -- ** GetProvisionedProductOutputs
    getProvisionedProductOutputs_provisionedProductName,
    getProvisionedProductOutputs_provisionedProductId,
    getProvisionedProductOutputs_pageSize,
    getProvisionedProductOutputs_outputKeys,
    getProvisionedProductOutputs_pageToken,
    getProvisionedProductOutputs_acceptLanguage,
    getProvisionedProductOutputsResponse_outputs,
    getProvisionedProductOutputsResponse_nextPageToken,
    getProvisionedProductOutputsResponse_httpStatus,

    -- ** AssociateBudgetWithResource
    associateBudgetWithResource_budgetName,
    associateBudgetWithResource_resourceId,
    associateBudgetWithResourceResponse_httpStatus,

    -- ** DeleteProvisionedProductPlan
    deleteProvisionedProductPlan_ignoreErrors,
    deleteProvisionedProductPlan_acceptLanguage,
    deleteProvisionedProductPlan_planId,
    deleteProvisionedProductPlanResponse_httpStatus,

    -- ** ListLaunchPaths
    listLaunchPaths_pageSize,
    listLaunchPaths_pageToken,
    listLaunchPaths_acceptLanguage,
    listLaunchPaths_productId,
    listLaunchPathsResponse_launchPathSummaries,
    listLaunchPathsResponse_nextPageToken,
    listLaunchPathsResponse_httpStatus,

    -- ** CreateConstraint
    createConstraint_description,
    createConstraint_acceptLanguage,
    createConstraint_portfolioId,
    createConstraint_productId,
    createConstraint_parameters,
    createConstraint_type,
    createConstraint_idempotencyToken,
    createConstraintResponse_constraintParameters,
    createConstraintResponse_status,
    createConstraintResponse_constraintDetail,
    createConstraintResponse_httpStatus,

    -- ** DescribePortfolioShareStatus
    describePortfolioShareStatus_portfolioShareToken,
    describePortfolioShareStatusResponse_shareDetails,
    describePortfolioShareStatusResponse_status,
    describePortfolioShareStatusResponse_portfolioId,
    describePortfolioShareStatusResponse_portfolioShareToken,
    describePortfolioShareStatusResponse_organizationNodeValue,
    describePortfolioShareStatusResponse_httpStatus,

    -- ** DeletePortfolioShare
    deletePortfolioShare_accountId,
    deletePortfolioShare_organizationNode,
    deletePortfolioShare_acceptLanguage,
    deletePortfolioShare_portfolioId,
    deletePortfolioShareResponse_portfolioShareToken,
    deletePortfolioShareResponse_httpStatus,

    -- ** DescribeServiceAction
    describeServiceAction_acceptLanguage,
    describeServiceAction_id,
    describeServiceActionResponse_serviceActionDetail,
    describeServiceActionResponse_httpStatus,

    -- ** UpdateProvisioningArtifact
    updateProvisioningArtifact_guidance,
    updateProvisioningArtifact_name,
    updateProvisioningArtifact_active,
    updateProvisioningArtifact_description,
    updateProvisioningArtifact_acceptLanguage,
    updateProvisioningArtifact_productId,
    updateProvisioningArtifact_provisioningArtifactId,
    updateProvisioningArtifactResponse_status,
    updateProvisioningArtifactResponse_info,
    updateProvisioningArtifactResponse_provisioningArtifactDetail,
    updateProvisioningArtifactResponse_httpStatus,

    -- ** ListStackInstancesForProvisionedProduct
    listStackInstancesForProvisionedProduct_pageSize,
    listStackInstancesForProvisionedProduct_pageToken,
    listStackInstancesForProvisionedProduct_acceptLanguage,
    listStackInstancesForProvisionedProduct_provisionedProductId,
    listStackInstancesForProvisionedProductResponse_nextPageToken,
    listStackInstancesForProvisionedProductResponse_stackInstances,
    listStackInstancesForProvisionedProductResponse_httpStatus,

    -- ** DeleteProvisioningArtifact
    deleteProvisioningArtifact_acceptLanguage,
    deleteProvisioningArtifact_productId,
    deleteProvisioningArtifact_provisioningArtifactId,
    deleteProvisioningArtifactResponse_httpStatus,

    -- ** DescribeProvisioningParameters
    describeProvisioningParameters_provisioningArtifactName,
    describeProvisioningParameters_provisioningArtifactId,
    describeProvisioningParameters_productName,
    describeProvisioningParameters_productId,
    describeProvisioningParameters_pathId,
    describeProvisioningParameters_acceptLanguage,
    describeProvisioningParameters_pathName,
    describeProvisioningParametersResponse_constraintSummaries,
    describeProvisioningParametersResponse_usageInstructions,
    describeProvisioningParametersResponse_provisioningArtifactOutputs,
    describeProvisioningParametersResponse_provisioningArtifactPreferences,
    describeProvisioningParametersResponse_provisioningArtifactParameters,
    describeProvisioningParametersResponse_tagOptions,
    describeProvisioningParametersResponse_httpStatus,

    -- ** ListProvisioningArtifacts
    listProvisioningArtifacts_acceptLanguage,
    listProvisioningArtifacts_productId,
    listProvisioningArtifactsResponse_nextPageToken,
    listProvisioningArtifactsResponse_provisioningArtifactDetails,
    listProvisioningArtifactsResponse_httpStatus,

    -- ** DescribeProvisionedProduct
    describeProvisionedProduct_id,
    describeProvisionedProduct_name,
    describeProvisionedProduct_acceptLanguage,
    describeProvisionedProductResponse_provisionedProductDetail,
    describeProvisionedProductResponse_cloudWatchDashboards,
    describeProvisionedProductResponse_httpStatus,

    -- ** DescribeProduct
    describeProduct_id,
    describeProduct_name,
    describeProduct_acceptLanguage,
    describeProductResponse_provisioningArtifacts,
    describeProductResponse_launchPaths,
    describeProductResponse_productViewSummary,
    describeProductResponse_budgets,
    describeProductResponse_httpStatus,

    -- ** UpdatePortfolioShare
    updatePortfolioShare_shareTagOptions,
    updatePortfolioShare_accountId,
    updatePortfolioShare_organizationNode,
    updatePortfolioShare_acceptLanguage,
    updatePortfolioShare_portfolioId,
    updatePortfolioShareResponse_status,
    updatePortfolioShareResponse_portfolioShareToken,
    updatePortfolioShareResponse_httpStatus,

    -- ** SearchProvisionedProducts
    searchProvisionedProducts_sortOrder,
    searchProvisionedProducts_pageSize,
    searchProvisionedProducts_pageToken,
    searchProvisionedProducts_accessLevelFilter,
    searchProvisionedProducts_sortBy,
    searchProvisionedProducts_filters,
    searchProvisionedProducts_acceptLanguage,
    searchProvisionedProductsResponse_totalResultsCount,
    searchProvisionedProductsResponse_provisionedProducts,
    searchProvisionedProductsResponse_nextPageToken,
    searchProvisionedProductsResponse_httpStatus,

    -- ** ListServiceActionsForProvisioningArtifact
    listServiceActionsForProvisioningArtifact_pageSize,
    listServiceActionsForProvisioningArtifact_pageToken,
    listServiceActionsForProvisioningArtifact_acceptLanguage,
    listServiceActionsForProvisioningArtifact_productId,
    listServiceActionsForProvisioningArtifact_provisioningArtifactId,
    listServiceActionsForProvisioningArtifactResponse_nextPageToken,
    listServiceActionsForProvisioningArtifactResponse_serviceActionSummaries,
    listServiceActionsForProvisioningArtifactResponse_httpStatus,

    -- ** CreateProvisioningArtifact
    createProvisioningArtifact_acceptLanguage,
    createProvisioningArtifact_productId,
    createProvisioningArtifact_parameters,
    createProvisioningArtifact_idempotencyToken,
    createProvisioningArtifactResponse_status,
    createProvisioningArtifactResponse_info,
    createProvisioningArtifactResponse_provisioningArtifactDetail,
    createProvisioningArtifactResponse_httpStatus,

    -- ** DeletePortfolio
    deletePortfolio_acceptLanguage,
    deletePortfolio_id,
    deletePortfolioResponse_httpStatus,

    -- ** CreatePortfolioShare
    createPortfolioShare_shareTagOptions,
    createPortfolioShare_accountId,
    createPortfolioShare_organizationNode,
    createPortfolioShare_acceptLanguage,
    createPortfolioShare_portfolioId,
    createPortfolioShareResponse_portfolioShareToken,
    createPortfolioShareResponse_httpStatus,

    -- ** DisassociateBudgetFromResource
    disassociateBudgetFromResource_budgetName,
    disassociateBudgetFromResource_resourceId,
    disassociateBudgetFromResourceResponse_httpStatus,

    -- ** UpdatePortfolio
    updatePortfolio_removeTags,
    updatePortfolio_providerName,
    updatePortfolio_addTags,
    updatePortfolio_description,
    updatePortfolio_displayName,
    updatePortfolio_acceptLanguage,
    updatePortfolio_id,
    updatePortfolioResponse_tags,
    updatePortfolioResponse_portfolioDetail,
    updatePortfolioResponse_httpStatus,

    -- ** ListPortfolios
    listPortfolios_pageSize,
    listPortfolios_pageToken,
    listPortfolios_acceptLanguage,
    listPortfoliosResponse_portfolioDetails,
    listPortfoliosResponse_nextPageToken,
    listPortfoliosResponse_httpStatus,

    -- ** GetAWSOrganizationsAccessStatus
    getAWSOrganizationsAccessStatusResponse_accessStatus,
    getAWSOrganizationsAccessStatusResponse_httpStatus,

    -- ** SearchProductsAsAdmin
    searchProductsAsAdmin_sortOrder,
    searchProductsAsAdmin_pageSize,
    searchProductsAsAdmin_pageToken,
    searchProductsAsAdmin_portfolioId,
    searchProductsAsAdmin_sortBy,
    searchProductsAsAdmin_productSource,
    searchProductsAsAdmin_filters,
    searchProductsAsAdmin_acceptLanguage,
    searchProductsAsAdminResponse_nextPageToken,
    searchProductsAsAdminResponse_productViewDetails,
    searchProductsAsAdminResponse_httpStatus,

    -- ** DescribeRecord
    describeRecord_pageSize,
    describeRecord_pageToken,
    describeRecord_acceptLanguage,
    describeRecord_id,
    describeRecordResponse_recordDetail,
    describeRecordResponse_recordOutputs,
    describeRecordResponse_nextPageToken,
    describeRecordResponse_httpStatus,

    -- ** DescribeConstraint
    describeConstraint_acceptLanguage,
    describeConstraint_id,
    describeConstraintResponse_constraintParameters,
    describeConstraintResponse_status,
    describeConstraintResponse_constraintDetail,
    describeConstraintResponse_httpStatus,

    -- ** EnableAWSOrganizationsAccess
    enableAWSOrganizationsAccessResponse_httpStatus,

    -- ** DeleteTagOption
    deleteTagOption_id,
    deleteTagOptionResponse_httpStatus,

    -- ** DisassociateServiceActionFromProvisioningArtifact
    disassociateServiceActionFromProvisioningArtifact_acceptLanguage,
    disassociateServiceActionFromProvisioningArtifact_productId,
    disassociateServiceActionFromProvisioningArtifact_provisioningArtifactId,
    disassociateServiceActionFromProvisioningArtifact_serviceActionId,
    disassociateServiceActionFromProvisioningArtifactResponse_httpStatus,

    -- ** UpdateTagOption
    updateTagOption_active,
    updateTagOption_value,
    updateTagOption_id,
    updateTagOptionResponse_tagOptionDetail,
    updateTagOptionResponse_httpStatus,

    -- ** ListConstraintsForPortfolio
    listConstraintsForPortfolio_pageSize,
    listConstraintsForPortfolio_pageToken,
    listConstraintsForPortfolio_productId,
    listConstraintsForPortfolio_acceptLanguage,
    listConstraintsForPortfolio_portfolioId,
    listConstraintsForPortfolioResponse_constraintDetails,
    listConstraintsForPortfolioResponse_nextPageToken,
    listConstraintsForPortfolioResponse_httpStatus,

    -- ** ListRecordHistory
    listRecordHistory_pageSize,
    listRecordHistory_pageToken,
    listRecordHistory_accessLevelFilter,
    listRecordHistory_searchFilter,
    listRecordHistory_acceptLanguage,
    listRecordHistoryResponse_recordDetails,
    listRecordHistoryResponse_nextPageToken,
    listRecordHistoryResponse_httpStatus,

    -- ** CreateTagOption
    createTagOption_key,
    createTagOption_value,
    createTagOptionResponse_tagOptionDetail,
    createTagOptionResponse_httpStatus,

    -- ** UpdateProduct
    updateProduct_distributor,
    updateProduct_removeTags,
    updateProduct_addTags,
    updateProduct_name,
    updateProduct_supportUrl,
    updateProduct_supportDescription,
    updateProduct_owner,
    updateProduct_description,
    updateProduct_supportEmail,
    updateProduct_acceptLanguage,
    updateProduct_id,
    updateProductResponse_productViewDetail,
    updateProductResponse_tags,
    updateProductResponse_httpStatus,

    -- ** UpdateServiceAction
    updateServiceAction_name,
    updateServiceAction_description,
    updateServiceAction_definition,
    updateServiceAction_acceptLanguage,
    updateServiceAction_id,
    updateServiceActionResponse_serviceActionDetail,
    updateServiceActionResponse_httpStatus,

    -- ** DescribeProvisioningArtifact
    describeProvisioningArtifact_provisioningArtifactName,
    describeProvisioningArtifact_provisioningArtifactId,
    describeProvisioningArtifact_productName,
    describeProvisioningArtifact_productId,
    describeProvisioningArtifact_verbose,
    describeProvisioningArtifact_acceptLanguage,
    describeProvisioningArtifactResponse_status,
    describeProvisioningArtifactResponse_info,
    describeProvisioningArtifactResponse_provisioningArtifactDetail,
    describeProvisioningArtifactResponse_httpStatus,

    -- ** DeleteServiceAction
    deleteServiceAction_acceptLanguage,
    deleteServiceAction_id,
    deleteServiceActionResponse_httpStatus,

    -- ** AssociateServiceActionWithProvisioningArtifact
    associateServiceActionWithProvisioningArtifact_acceptLanguage,
    associateServiceActionWithProvisioningArtifact_productId,
    associateServiceActionWithProvisioningArtifact_provisioningArtifactId,
    associateServiceActionWithProvisioningArtifact_serviceActionId,
    associateServiceActionWithProvisioningArtifactResponse_httpStatus,

    -- ** UpdateProvisionedProduct
    updateProvisionedProduct_provisionedProductName,
    updateProvisionedProduct_provisioningPreferences,
    updateProvisionedProduct_provisionedProductId,
    updateProvisionedProduct_provisioningArtifactName,
    updateProvisionedProduct_provisioningArtifactId,
    updateProvisionedProduct_productName,
    updateProvisionedProduct_tags,
    updateProvisionedProduct_productId,
    updateProvisionedProduct_provisioningParameters,
    updateProvisionedProduct_pathId,
    updateProvisionedProduct_acceptLanguage,
    updateProvisionedProduct_pathName,
    updateProvisionedProduct_updateToken,
    updateProvisionedProductResponse_recordDetail,
    updateProvisionedProductResponse_httpStatus,

    -- ** DeleteProduct
    deleteProduct_acceptLanguage,
    deleteProduct_id,
    deleteProductResponse_httpStatus,

    -- ** DescribeCopyProductStatus
    describeCopyProductStatus_acceptLanguage,
    describeCopyProductStatus_copyProductToken,
    describeCopyProductStatusResponse_statusDetail,
    describeCopyProductStatusResponse_targetProductId,
    describeCopyProductStatusResponse_copyProductStatus,
    describeCopyProductStatusResponse_httpStatus,

    -- ** CreateServiceAction
    createServiceAction_description,
    createServiceAction_acceptLanguage,
    createServiceAction_name,
    createServiceAction_definitionType,
    createServiceAction_definition,
    createServiceAction_idempotencyToken,
    createServiceActionResponse_serviceActionDetail,
    createServiceActionResponse_httpStatus,

    -- ** CreateProduct
    createProduct_distributor,
    createProduct_supportUrl,
    createProduct_tags,
    createProduct_supportDescription,
    createProduct_description,
    createProduct_supportEmail,
    createProduct_acceptLanguage,
    createProduct_name,
    createProduct_owner,
    createProduct_productType,
    createProduct_provisioningArtifactParameters,
    createProduct_idempotencyToken,
    createProductResponse_productViewDetail,
    createProductResponse_provisioningArtifactDetail,
    createProductResponse_tags,
    createProductResponse_httpStatus,

    -- ** AcceptPortfolioShare
    acceptPortfolioShare_portfolioShareType,
    acceptPortfolioShare_acceptLanguage,
    acceptPortfolioShare_portfolioId,
    acceptPortfolioShareResponse_httpStatus,

    -- ** DisassociatePrincipalFromPortfolio
    disassociatePrincipalFromPortfolio_acceptLanguage,
    disassociatePrincipalFromPortfolio_portfolioId,
    disassociatePrincipalFromPortfolio_principalARN,
    disassociatePrincipalFromPortfolioResponse_httpStatus,

    -- ** BatchDisassociateServiceActionFromProvisioningArtifact
    batchDisassociateServiceActionFromProvisioningArtifact_acceptLanguage,
    batchDisassociateServiceActionFromProvisioningArtifact_serviceActionAssociations,
    batchDisassociateServiceActionFromProvisioningArtifactResponse_failedServiceActionAssociations,
    batchDisassociateServiceActionFromProvisioningArtifactResponse_httpStatus,

    -- ** ListProvisionedProductPlans
    listProvisionedProductPlans_provisionProductId,
    listProvisionedProductPlans_pageSize,
    listProvisionedProductPlans_pageToken,
    listProvisionedProductPlans_accessLevelFilter,
    listProvisionedProductPlans_acceptLanguage,
    listProvisionedProductPlansResponse_nextPageToken,
    listProvisionedProductPlansResponse_provisionedProductPlans,
    listProvisionedProductPlansResponse_httpStatus,

    -- ** BatchAssociateServiceActionWithProvisioningArtifact
    batchAssociateServiceActionWithProvisioningArtifact_acceptLanguage,
    batchAssociateServiceActionWithProvisioningArtifact_serviceActionAssociations,
    batchAssociateServiceActionWithProvisioningArtifactResponse_failedServiceActionAssociations,
    batchAssociateServiceActionWithProvisioningArtifactResponse_httpStatus,

    -- ** SearchProducts
    searchProducts_sortOrder,
    searchProducts_pageSize,
    searchProducts_pageToken,
    searchProducts_sortBy,
    searchProducts_filters,
    searchProducts_acceptLanguage,
    searchProductsResponse_nextPageToken,
    searchProductsResponse_productViewSummaries,
    searchProductsResponse_productViewAggregations,
    searchProductsResponse_httpStatus,

    -- ** ListProvisioningArtifactsForServiceAction
    listProvisioningArtifactsForServiceAction_pageSize,
    listProvisioningArtifactsForServiceAction_pageToken,
    listProvisioningArtifactsForServiceAction_acceptLanguage,
    listProvisioningArtifactsForServiceAction_serviceActionId,
    listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews,
    listProvisioningArtifactsForServiceActionResponse_nextPageToken,
    listProvisioningArtifactsForServiceActionResponse_httpStatus,

    -- ** AssociatePrincipalWithPortfolio
    associatePrincipalWithPortfolio_acceptLanguage,
    associatePrincipalWithPortfolio_portfolioId,
    associatePrincipalWithPortfolio_principalARN,
    associatePrincipalWithPortfolio_principalType,
    associatePrincipalWithPortfolioResponse_httpStatus,

    -- ** DescribeServiceActionExecutionParameters
    describeServiceActionExecutionParameters_acceptLanguage,
    describeServiceActionExecutionParameters_provisionedProductId,
    describeServiceActionExecutionParameters_serviceActionId,
    describeServiceActionExecutionParametersResponse_serviceActionParameters,
    describeServiceActionExecutionParametersResponse_httpStatus,

    -- ** CopyProduct
    copyProduct_targetProductName,
    copyProduct_copyOptions,
    copyProduct_targetProductId,
    copyProduct_sourceProvisioningArtifactIdentifiers,
    copyProduct_acceptLanguage,
    copyProduct_sourceProductArn,
    copyProduct_idempotencyToken,
    copyProductResponse_copyProductToken,
    copyProductResponse_httpStatus,

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

    -- ** UpdateProvisionedProductProperties
    updateProvisionedProductProperties_acceptLanguage,
    updateProvisionedProductProperties_provisionedProductId,
    updateProvisionedProductProperties_provisionedProductProperties,
    updateProvisionedProductProperties_idempotencyToken,
    updateProvisionedProductPropertiesResponse_status,
    updateProvisionedProductPropertiesResponse_provisionedProductId,
    updateProvisionedProductPropertiesResponse_recordId,
    updateProvisionedProductPropertiesResponse_provisionedProductProperties,
    updateProvisionedProductPropertiesResponse_httpStatus,

    -- ** DescribeProductView
    describeProductView_acceptLanguage,
    describeProductView_id,
    describeProductViewResponse_provisioningArtifacts,
    describeProductViewResponse_productViewSummary,
    describeProductViewResponse_httpStatus,

    -- ** DescribeProductAsAdmin
    describeProductAsAdmin_id,
    describeProductAsAdmin_name,
    describeProductAsAdmin_sourcePortfolioId,
    describeProductAsAdmin_acceptLanguage,
    describeProductAsAdminResponse_productViewDetail,
    describeProductAsAdminResponse_tags,
    describeProductAsAdminResponse_budgets,
    describeProductAsAdminResponse_provisioningArtifactSummaries,
    describeProductAsAdminResponse_tagOptions,
    describeProductAsAdminResponse_httpStatus,

    -- ** ListPortfoliosForProduct
    listPortfoliosForProduct_pageSize,
    listPortfoliosForProduct_pageToken,
    listPortfoliosForProduct_acceptLanguage,
    listPortfoliosForProduct_productId,
    listPortfoliosForProductResponse_portfolioDetails,
    listPortfoliosForProductResponse_nextPageToken,
    listPortfoliosForProductResponse_httpStatus,

    -- ** RejectPortfolioShare
    rejectPortfolioShare_portfolioShareType,
    rejectPortfolioShare_acceptLanguage,
    rejectPortfolioShare_portfolioId,
    rejectPortfolioShareResponse_httpStatus,

    -- ** ListTagOptions
    listTagOptions_pageSize,
    listTagOptions_pageToken,
    listTagOptions_filters,
    listTagOptionsResponse_pageToken,
    listTagOptionsResponse_tagOptionDetails,
    listTagOptionsResponse_httpStatus,

    -- ** AssociateTagOptionWithResource
    associateTagOptionWithResource_resourceId,
    associateTagOptionWithResource_tagOptionId,
    associateTagOptionWithResourceResponse_httpStatus,

    -- ** DisableAWSOrganizationsAccess
    disableAWSOrganizationsAccessResponse_httpStatus,

    -- ** DescribeProvisionedProductPlan
    describeProvisionedProductPlan_pageSize,
    describeProvisionedProductPlan_pageToken,
    describeProvisionedProductPlan_acceptLanguage,
    describeProvisionedProductPlan_planId,
    describeProvisionedProductPlanResponse_resourceChanges,
    describeProvisionedProductPlanResponse_nextPageToken,
    describeProvisionedProductPlanResponse_provisionedProductPlanDetails,
    describeProvisionedProductPlanResponse_httpStatus,

    -- ** ListBudgetsForResource
    listBudgetsForResource_pageSize,
    listBudgetsForResource_pageToken,
    listBudgetsForResource_acceptLanguage,
    listBudgetsForResource_resourceId,
    listBudgetsForResourceResponse_nextPageToken,
    listBudgetsForResourceResponse_budgets,
    listBudgetsForResourceResponse_httpStatus,

    -- ** DisassociateProductFromPortfolio
    disassociateProductFromPortfolio_acceptLanguage,
    disassociateProductFromPortfolio_productId,
    disassociateProductFromPortfolio_portfolioId,
    disassociateProductFromPortfolioResponse_httpStatus,

    -- ** ListPrincipalsForPortfolio
    listPrincipalsForPortfolio_pageSize,
    listPrincipalsForPortfolio_pageToken,
    listPrincipalsForPortfolio_acceptLanguage,
    listPrincipalsForPortfolio_portfolioId,
    listPrincipalsForPortfolioResponse_principals,
    listPrincipalsForPortfolioResponse_nextPageToken,
    listPrincipalsForPortfolioResponse_httpStatus,

    -- ** ProvisionProduct
    provisionProduct_provisioningPreferences,
    provisionProduct_notificationArns,
    provisionProduct_provisioningArtifactName,
    provisionProduct_provisioningArtifactId,
    provisionProduct_productName,
    provisionProduct_tags,
    provisionProduct_productId,
    provisionProduct_provisioningParameters,
    provisionProduct_pathId,
    provisionProduct_acceptLanguage,
    provisionProduct_pathName,
    provisionProduct_provisionedProductName,
    provisionProduct_provisionToken,
    provisionProductResponse_recordDetail,
    provisionProductResponse_httpStatus,

    -- ** TerminateProvisionedProduct
    terminateProvisionedProduct_provisionedProductName,
    terminateProvisionedProduct_provisionedProductId,
    terminateProvisionedProduct_retainPhysicalResources,
    terminateProvisionedProduct_ignoreErrors,
    terminateProvisionedProduct_acceptLanguage,
    terminateProvisionedProduct_terminateToken,
    terminateProvisionedProductResponse_recordDetail,
    terminateProvisionedProductResponse_httpStatus,

    -- ** ListServiceActions
    listServiceActions_pageSize,
    listServiceActions_pageToken,
    listServiceActions_acceptLanguage,
    listServiceActionsResponse_nextPageToken,
    listServiceActionsResponse_serviceActionSummaries,
    listServiceActionsResponse_httpStatus,

    -- * Types

    -- ** AccessLevelFilter
    accessLevelFilter_key,
    accessLevelFilter_value,

    -- ** BudgetDetail
    budgetDetail_budgetName,

    -- ** CloudWatchDashboard
    cloudWatchDashboard_name,

    -- ** ConstraintDetail
    constraintDetail_constraintId,
    constraintDetail_portfolioId,
    constraintDetail_owner,
    constraintDetail_productId,
    constraintDetail_description,
    constraintDetail_type,

    -- ** ConstraintSummary
    constraintSummary_description,
    constraintSummary_type,

    -- ** ExecutionParameter
    executionParameter_name,
    executionParameter_defaultValues,
    executionParameter_type,

    -- ** FailedServiceActionAssociation
    failedServiceActionAssociation_provisioningArtifactId,
    failedServiceActionAssociation_serviceActionId,
    failedServiceActionAssociation_productId,
    failedServiceActionAssociation_errorMessage,
    failedServiceActionAssociation_errorCode,

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
    listTagOptionsFilters_key,
    listTagOptionsFilters_active,
    listTagOptionsFilters_value,

    -- ** OrganizationNode
    organizationNode_value,
    organizationNode_type,

    -- ** ParameterConstraints
    parameterConstraints_maxValue,
    parameterConstraints_minLength,
    parameterConstraints_allowedValues,
    parameterConstraints_minValue,
    parameterConstraints_constraintDescription,
    parameterConstraints_maxLength,
    parameterConstraints_allowedPattern,

    -- ** PortfolioDetail
    portfolioDetail_providerName,
    portfolioDetail_id,
    portfolioDetail_arn,
    portfolioDetail_createdTime,
    portfolioDetail_description,
    portfolioDetail_displayName,

    -- ** PortfolioShareDetail
    portfolioShareDetail_shareTagOptions,
    portfolioShareDetail_principalId,
    portfolioShareDetail_accepted,
    portfolioShareDetail_type,

    -- ** Principal
    principal_principalARN,
    principal_principalType,

    -- ** ProductViewAggregationValue
    productViewAggregationValue_value,
    productViewAggregationValue_approximateCount,

    -- ** ProductViewDetail
    productViewDetail_status,
    productViewDetail_productARN,
    productViewDetail_createdTime,
    productViewDetail_productViewSummary,

    -- ** ProductViewSummary
    productViewSummary_distributor,
    productViewSummary_id,
    productViewSummary_name,
    productViewSummary_hasDefaultPath,
    productViewSummary_shortDescription,
    productViewSummary_supportUrl,
    productViewSummary_supportDescription,
    productViewSummary_owner,
    productViewSummary_productId,
    productViewSummary_supportEmail,
    productViewSummary_type,

    -- ** ProvisionedProductAttribute
    provisionedProductAttribute_statusMessage,
    provisionedProductAttribute_lastSuccessfulProvisioningRecordId,
    provisionedProductAttribute_idempotencyToken,
    provisionedProductAttribute_status,
    provisionedProductAttribute_userArn,
    provisionedProductAttribute_provisioningArtifactName,
    provisionedProductAttribute_arn,
    provisionedProductAttribute_id,
    provisionedProductAttribute_createdTime,
    provisionedProductAttribute_provisioningArtifactId,
    provisionedProductAttribute_name,
    provisionedProductAttribute_productName,
    provisionedProductAttribute_tags,
    provisionedProductAttribute_productId,
    provisionedProductAttribute_lastProvisioningRecordId,
    provisionedProductAttribute_type,
    provisionedProductAttribute_physicalId,
    provisionedProductAttribute_userArnSession,
    provisionedProductAttribute_lastRecordId,

    -- ** ProvisionedProductDetail
    provisionedProductDetail_statusMessage,
    provisionedProductDetail_lastSuccessfulProvisioningRecordId,
    provisionedProductDetail_idempotencyToken,
    provisionedProductDetail_status,
    provisionedProductDetail_arn,
    provisionedProductDetail_id,
    provisionedProductDetail_createdTime,
    provisionedProductDetail_provisioningArtifactId,
    provisionedProductDetail_name,
    provisionedProductDetail_launchRoleArn,
    provisionedProductDetail_productId,
    provisionedProductDetail_lastProvisioningRecordId,
    provisionedProductDetail_type,
    provisionedProductDetail_lastRecordId,

    -- ** ProvisionedProductPlanDetails
    provisionedProductPlanDetails_provisionProductId,
    provisionedProductPlanDetails_statusMessage,
    provisionedProductPlanDetails_status,
    provisionedProductPlanDetails_notificationArns,
    provisionedProductPlanDetails_updatedTime,
    provisionedProductPlanDetails_createdTime,
    provisionedProductPlanDetails_provisioningArtifactId,
    provisionedProductPlanDetails_planName,
    provisionedProductPlanDetails_tags,
    provisionedProductPlanDetails_planId,
    provisionedProductPlanDetails_productId,
    provisionedProductPlanDetails_provisioningParameters,
    provisionedProductPlanDetails_planType,
    provisionedProductPlanDetails_pathId,
    provisionedProductPlanDetails_provisionProductName,

    -- ** ProvisionedProductPlanSummary
    provisionedProductPlanSummary_provisionProductId,
    provisionedProductPlanSummary_provisioningArtifactId,
    provisionedProductPlanSummary_planName,
    provisionedProductPlanSummary_planId,
    provisionedProductPlanSummary_planType,
    provisionedProductPlanSummary_provisionProductName,

    -- ** ProvisioningArtifact
    provisioningArtifact_guidance,
    provisioningArtifact_id,
    provisioningArtifact_createdTime,
    provisioningArtifact_name,
    provisioningArtifact_description,

    -- ** ProvisioningArtifactDetail
    provisioningArtifactDetail_guidance,
    provisioningArtifactDetail_id,
    provisioningArtifactDetail_createdTime,
    provisioningArtifactDetail_name,
    provisioningArtifactDetail_active,
    provisioningArtifactDetail_description,
    provisioningArtifactDetail_type,

    -- ** ProvisioningArtifactOutput
    provisioningArtifactOutput_key,
    provisioningArtifactOutput_description,

    -- ** ProvisioningArtifactParameter
    provisioningArtifactParameter_isNoEcho,
    provisioningArtifactParameter_parameterConstraints,
    provisioningArtifactParameter_parameterType,
    provisioningArtifactParameter_parameterKey,
    provisioningArtifactParameter_description,
    provisioningArtifactParameter_defaultValue,

    -- ** ProvisioningArtifactPreferences
    provisioningArtifactPreferences_stackSetAccounts,
    provisioningArtifactPreferences_stackSetRegions,

    -- ** ProvisioningArtifactProperties
    provisioningArtifactProperties_disableTemplateValidation,
    provisioningArtifactProperties_name,
    provisioningArtifactProperties_description,
    provisioningArtifactProperties_type,
    provisioningArtifactProperties_info,

    -- ** ProvisioningArtifactSummary
    provisioningArtifactSummary_id,
    provisioningArtifactSummary_createdTime,
    provisioningArtifactSummary_provisioningArtifactMetadata,
    provisioningArtifactSummary_name,
    provisioningArtifactSummary_description,

    -- ** ProvisioningArtifactView
    provisioningArtifactView_productViewSummary,
    provisioningArtifactView_provisioningArtifact,

    -- ** ProvisioningParameter
    provisioningParameter_key,
    provisioningParameter_value,

    -- ** ProvisioningPreferences
    provisioningPreferences_stackSetFailureToleranceCount,
    provisioningPreferences_stackSetAccounts,
    provisioningPreferences_stackSetFailureTolerancePercentage,
    provisioningPreferences_stackSetRegions,
    provisioningPreferences_stackSetMaxConcurrencyCount,
    provisioningPreferences_stackSetMaxConcurrencyPercentage,

    -- ** RecordDetail
    recordDetail_recordTags,
    recordDetail_status,
    recordDetail_provisionedProductName,
    recordDetail_recordErrors,
    recordDetail_provisionedProductId,
    recordDetail_provisionedProductType,
    recordDetail_recordId,
    recordDetail_updatedTime,
    recordDetail_createdTime,
    recordDetail_provisioningArtifactId,
    recordDetail_launchRoleArn,
    recordDetail_productId,
    recordDetail_pathId,
    recordDetail_recordType,

    -- ** RecordError
    recordError_code,
    recordError_description,

    -- ** RecordOutput
    recordOutput_outputKey,
    recordOutput_outputValue,
    recordOutput_description,

    -- ** RecordTag
    recordTag_key,
    recordTag_value,

    -- ** ResourceChange
    resourceChange_physicalResourceId,
    resourceChange_resourceType,
    resourceChange_scope,
    resourceChange_details,
    resourceChange_logicalResourceId,
    resourceChange_action,
    resourceChange_replacement,

    -- ** ResourceChangeDetail
    resourceChangeDetail_evaluation,
    resourceChangeDetail_causingEntity,
    resourceChangeDetail_target,

    -- ** ResourceDetail
    resourceDetail_id,
    resourceDetail_arn,
    resourceDetail_createdTime,
    resourceDetail_name,
    resourceDetail_description,

    -- ** ResourceTargetDefinition
    resourceTargetDefinition_requiresRecreation,
    resourceTargetDefinition_name,
    resourceTargetDefinition_attribute,

    -- ** ServiceActionAssociation
    serviceActionAssociation_serviceActionId,
    serviceActionAssociation_productId,
    serviceActionAssociation_provisioningArtifactId,

    -- ** ServiceActionDetail
    serviceActionDetail_serviceActionSummary,
    serviceActionDetail_definition,

    -- ** ServiceActionSummary
    serviceActionSummary_id,
    serviceActionSummary_definitionType,
    serviceActionSummary_name,
    serviceActionSummary_description,

    -- ** ShareDetails
    shareDetails_shareErrors,
    shareDetails_successfulShares,

    -- ** ShareError
    shareError_message,
    shareError_accounts,
    shareError_error,

    -- ** StackInstance
    stackInstance_stackInstanceStatus,
    stackInstance_account,
    stackInstance_region,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagOptionDetail
    tagOptionDetail_key,
    tagOptionDetail_id,
    tagOptionDetail_active,
    tagOptionDetail_owner,
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
    updateProvisioningPreferences_stackSetFailureToleranceCount,
    updateProvisioningPreferences_stackSetAccounts,
    updateProvisioningPreferences_stackSetFailureTolerancePercentage,
    updateProvisioningPreferences_stackSetRegions,
    updateProvisioningPreferences_stackSetMaxConcurrencyCount,
    updateProvisioningPreferences_stackSetMaxConcurrencyPercentage,

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
