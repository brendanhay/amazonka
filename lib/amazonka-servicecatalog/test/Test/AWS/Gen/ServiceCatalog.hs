{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ServiceCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ServiceCatalog where

import Data.Proxy
import Network.AWS.ServiceCatalog
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.ServiceCatalog.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestImportAsProvisionedProduct $
--             importAsProvisionedProduct
--
--         , requestDeleteConstraint $
--             deleteConstraint
--
--         , requestUpdateConstraint $
--             updateConstraint
--
--         , requestCreateProvisionedProductPlan $
--             createProvisionedProductPlan
--
--         , requestExecuteProvisionedProductServiceAction $
--             executeProvisionedProductServiceAction
--
--         , requestCreateProduct $
--             createProduct
--
--         , requestDescribeCopyProductStatus $
--             describeCopyProductStatus
--
--         , requestCreateServiceAction $
--             createServiceAction
--
--         , requestTerminateProvisionedProduct $
--             terminateProvisionedProduct
--
--         , requestUpdateProvisionedProduct $
--             updateProvisionedProduct
--
--         , requestDescribeProvisioningArtifact $
--             describeProvisioningArtifact
--
--         , requestAssociateServiceActionWithProvisioningArtifact $
--             associateServiceActionWithProvisioningArtifact
--
--         , requestListRecordHistory $
--             listRecordHistory
--
--         , requestDescribeProvisionedProductPlan $
--             describeProvisionedProductPlan
--
--         , requestAssociateTagOptionWithResource $
--             associateTagOptionWithResource
--
--         , requestCreateTagOption $
--             createTagOption
--
--         , requestListBudgetsForResource $
--             listBudgetsForResource
--
--         , requestDisassociateProductFromPortfolio $
--             disassociateProductFromPortfolio
--
--         , requestListConstraintsForPortfolio $
--             listConstraintsForPortfolio
--
--         , requestDescribeRecord $
--             describeRecord
--
--         , requestEnableAWSOrganizationsAccess $
--             enableAWSOrganizationsAccess
--
--         , requestDescribeConstraint $
--             describeConstraint
--
--         , requestCreateProvisioningArtifact $
--             createProvisioningArtifact
--
--         , requestListPortfolios $
--             listPortfolios
--
--         , requestDisassociateBudgetFromResource $
--             disassociateBudgetFromResource
--
--         , requestDescribeProductView $
--             describeProductView
--
--         , requestCreatePortfolioShare $
--             createPortfolioShare
--
--         , requestListProvisioningArtifacts $
--             listProvisioningArtifacts
--
--         , requestListServiceActionsForProvisioningArtifact $
--             listServiceActionsForProvisioningArtifact
--
--         , requestSearchProducts $
--             searchProducts
--
--         , requestDescribeServiceActionExecutionParameters $
--             describeServiceActionExecutionParameters
--
--         , requestSearchProvisionedProducts $
--             searchProvisionedProducts
--
--         , requestListStackInstancesForProvisionedProduct $
--             listStackInstancesForProvisionedProduct
--
--         , requestDescribeServiceAction $
--             describeServiceAction
--
--         , requestDescribeProduct $
--             describeProduct
--
--         , requestDeleteProvisionedProductPlan $
--             deleteProvisionedProductPlan
--
--         , requestGetProvisionedProductOutputs $
--             getProvisionedProductOutputs
--
--         , requestCreateConstraint $
--             createConstraint
--
--         , requestListProvisionedProductPlans $
--             listProvisionedProductPlans
--
--         , requestListPortfolioAccess $
--             listPortfolioAccess
--
--         , requestBatchDisassociateServiceActionFromProvisioningArtifact $
--             batchDisassociateServiceActionFromProvisioningArtifact
--
--         , requestDisassociatePrincipalFromPortfolio $
--             disassociatePrincipalFromPortfolio
--
--         , requestDescribeTagOption $
--             describeTagOption
--
--         , requestDisassociateTagOptionFromResource $
--             disassociateTagOptionFromResource
--
--         , requestDescribePortfolio $
--             describePortfolio
--
--         , requestAssociateProductWithPortfolio $
--             associateProductWithPortfolio
--
--         , requestListAcceptedPortfolioShares $
--             listAcceptedPortfolioShares
--
--         , requestExecuteProvisionedProductPlan $
--             executeProvisionedProductPlan
--
--         , requestAcceptPortfolioShare $
--             acceptPortfolioShare
--
--         , requestScanProvisionedProducts $
--             scanProvisionedProducts
--
--         , requestListOrganizationPortfolioAccess $
--             listOrganizationPortfolioAccess
--
--         , requestListPrincipalsForPortfolio $
--             listPrincipalsForPortfolio
--
--         , requestDeleteProduct $
--             deleteProduct
--
--         , requestUpdateProduct $
--             updateProduct
--
--         , requestListServiceActions $
--             listServiceActions
--
--         , requestProvisionProduct $
--             provisionProduct
--
--         , requestDeleteServiceAction $
--             deleteServiceAction
--
--         , requestUpdateServiceAction $
--             updateServiceAction
--
--         , requestDisableAWSOrganizationsAccess $
--             disableAWSOrganizationsAccess
--
--         , requestRejectPortfolioShare $
--             rejectPortfolioShare
--
--         , requestDisassociateServiceActionFromProvisioningArtifact $
--             disassociateServiceActionFromProvisioningArtifact
--
--         , requestDeleteTagOption $
--             deleteTagOption
--
--         , requestUpdateTagOption $
--             updateTagOption
--
--         , requestListTagOptions $
--             listTagOptions
--
--         , requestUpdateProvisionedProductProperties $
--             updateProvisionedProductProperties
--
--         , requestSearchProductsAsAdmin $
--             searchProductsAsAdmin
--
--         , requestDeletePortfolio $
--             deletePortfolio
--
--         , requestUpdatePortfolio $
--             updatePortfolio
--
--         , requestListPortfoliosForProduct $
--             listPortfoliosForProduct
--
--         , requestGetAWSOrganizationsAccessStatus $
--             getAWSOrganizationsAccessStatus
--
--         , requestDescribeProductAsAdmin $
--             describeProductAsAdmin
--
--         , requestBatchAssociateServiceActionWithProvisioningArtifact $
--             batchAssociateServiceActionWithProvisioningArtifact
--
--         , requestDescribeProvisioningParameters $
--             describeProvisioningParameters
--
--         , requestAssociatePrincipalWithPortfolio $
--             associatePrincipalWithPortfolio
--
--         , requestDescribeProvisionedProduct $
--             describeProvisionedProduct
--
--         , requestCopyProduct $
--             copyProduct
--
--         , requestDescribePortfolioShareStatus $
--             describePortfolioShareStatus
--
--         , requestUpdateProvisioningArtifact $
--             updateProvisioningArtifact
--
--         , requestDeletePortfolioShare $
--             deletePortfolioShare
--
--         , requestDeleteProvisioningArtifact $
--             deleteProvisioningArtifact
--
--         , requestListProvisioningArtifactsForServiceAction $
--             listProvisioningArtifactsForServiceAction
--
--         , requestCreatePortfolio $
--             createPortfolio
--
--         , requestListLaunchPaths $
--             listLaunchPaths
--
--         , requestListResourcesForTagOption $
--             listResourcesForTagOption
--
--         , requestAssociateBudgetWithResource $
--             associateBudgetWithResource
--
--           ]

--     , testGroup "response"
--         [ responseImportAsProvisionedProduct $
--             importAsProvisionedProductResponse
--
--         , responseDeleteConstraint $
--             deleteConstraintResponse
--
--         , responseUpdateConstraint $
--             updateConstraintResponse
--
--         , responseCreateProvisionedProductPlan $
--             createProvisionedProductPlanResponse
--
--         , responseExecuteProvisionedProductServiceAction $
--             executeProvisionedProductServiceActionResponse
--
--         , responseCreateProduct $
--             createProductResponse
--
--         , responseDescribeCopyProductStatus $
--             describeCopyProductStatusResponse
--
--         , responseCreateServiceAction $
--             createServiceActionResponse
--
--         , responseTerminateProvisionedProduct $
--             terminateProvisionedProductResponse
--
--         , responseUpdateProvisionedProduct $
--             updateProvisionedProductResponse
--
--         , responseDescribeProvisioningArtifact $
--             describeProvisioningArtifactResponse
--
--         , responseAssociateServiceActionWithProvisioningArtifact $
--             associateServiceActionWithProvisioningArtifactResponse
--
--         , responseListRecordHistory $
--             listRecordHistoryResponse
--
--         , responseDescribeProvisionedProductPlan $
--             describeProvisionedProductPlanResponse
--
--         , responseAssociateTagOptionWithResource $
--             associateTagOptionWithResourceResponse
--
--         , responseCreateTagOption $
--             createTagOptionResponse
--
--         , responseListBudgetsForResource $
--             listBudgetsForResourceResponse
--
--         , responseDisassociateProductFromPortfolio $
--             disassociateProductFromPortfolioResponse
--
--         , responseListConstraintsForPortfolio $
--             listConstraintsForPortfolioResponse
--
--         , responseDescribeRecord $
--             describeRecordResponse
--
--         , responseEnableAWSOrganizationsAccess $
--             enableAWSOrganizationsAccessResponse
--
--         , responseDescribeConstraint $
--             describeConstraintResponse
--
--         , responseCreateProvisioningArtifact $
--             createProvisioningArtifactResponse
--
--         , responseListPortfolios $
--             listPortfoliosResponse
--
--         , responseDisassociateBudgetFromResource $
--             disassociateBudgetFromResourceResponse
--
--         , responseDescribeProductView $
--             describeProductViewResponse
--
--         , responseCreatePortfolioShare $
--             createPortfolioShareResponse
--
--         , responseListProvisioningArtifacts $
--             listProvisioningArtifactsResponse
--
--         , responseListServiceActionsForProvisioningArtifact $
--             listServiceActionsForProvisioningArtifactResponse
--
--         , responseSearchProducts $
--             searchProductsResponse
--
--         , responseDescribeServiceActionExecutionParameters $
--             describeServiceActionExecutionParametersResponse
--
--         , responseSearchProvisionedProducts $
--             searchProvisionedProductsResponse
--
--         , responseListStackInstancesForProvisionedProduct $
--             listStackInstancesForProvisionedProductResponse
--
--         , responseDescribeServiceAction $
--             describeServiceActionResponse
--
--         , responseDescribeProduct $
--             describeProductResponse
--
--         , responseDeleteProvisionedProductPlan $
--             deleteProvisionedProductPlanResponse
--
--         , responseGetProvisionedProductOutputs $
--             getProvisionedProductOutputsResponse
--
--         , responseCreateConstraint $
--             createConstraintResponse
--
--         , responseListProvisionedProductPlans $
--             listProvisionedProductPlansResponse
--
--         , responseListPortfolioAccess $
--             listPortfolioAccessResponse
--
--         , responseBatchDisassociateServiceActionFromProvisioningArtifact $
--             batchDisassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseDisassociatePrincipalFromPortfolio $
--             disassociatePrincipalFromPortfolioResponse
--
--         , responseDescribeTagOption $
--             describeTagOptionResponse
--
--         , responseDisassociateTagOptionFromResource $
--             disassociateTagOptionFromResourceResponse
--
--         , responseDescribePortfolio $
--             describePortfolioResponse
--
--         , responseAssociateProductWithPortfolio $
--             associateProductWithPortfolioResponse
--
--         , responseListAcceptedPortfolioShares $
--             listAcceptedPortfolioSharesResponse
--
--         , responseExecuteProvisionedProductPlan $
--             executeProvisionedProductPlanResponse
--
--         , responseAcceptPortfolioShare $
--             acceptPortfolioShareResponse
--
--         , responseScanProvisionedProducts $
--             scanProvisionedProductsResponse
--
--         , responseListOrganizationPortfolioAccess $
--             listOrganizationPortfolioAccessResponse
--
--         , responseListPrincipalsForPortfolio $
--             listPrincipalsForPortfolioResponse
--
--         , responseDeleteProduct $
--             deleteProductResponse
--
--         , responseUpdateProduct $
--             updateProductResponse
--
--         , responseListServiceActions $
--             listServiceActionsResponse
--
--         , responseProvisionProduct $
--             provisionProductResponse
--
--         , responseDeleteServiceAction $
--             deleteServiceActionResponse
--
--         , responseUpdateServiceAction $
--             updateServiceActionResponse
--
--         , responseDisableAWSOrganizationsAccess $
--             disableAWSOrganizationsAccessResponse
--
--         , responseRejectPortfolioShare $
--             rejectPortfolioShareResponse
--
--         , responseDisassociateServiceActionFromProvisioningArtifact $
--             disassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseDeleteTagOption $
--             deleteTagOptionResponse
--
--         , responseUpdateTagOption $
--             updateTagOptionResponse
--
--         , responseListTagOptions $
--             listTagOptionsResponse
--
--         , responseUpdateProvisionedProductProperties $
--             updateProvisionedProductPropertiesResponse
--
--         , responseSearchProductsAsAdmin $
--             searchProductsAsAdminResponse
--
--         , responseDeletePortfolio $
--             deletePortfolioResponse
--
--         , responseUpdatePortfolio $
--             updatePortfolioResponse
--
--         , responseListPortfoliosForProduct $
--             listPortfoliosForProductResponse
--
--         , responseGetAWSOrganizationsAccessStatus $
--             getAWSOrganizationsAccessStatusResponse
--
--         , responseDescribeProductAsAdmin $
--             describeProductAsAdminResponse
--
--         , responseBatchAssociateServiceActionWithProvisioningArtifact $
--             batchAssociateServiceActionWithProvisioningArtifactResponse
--
--         , responseDescribeProvisioningParameters $
--             describeProvisioningParametersResponse
--
--         , responseAssociatePrincipalWithPortfolio $
--             associatePrincipalWithPortfolioResponse
--
--         , responseDescribeProvisionedProduct $
--             describeProvisionedProductResponse
--
--         , responseCopyProduct $
--             copyProductResponse
--
--         , responseDescribePortfolioShareStatus $
--             describePortfolioShareStatusResponse
--
--         , responseUpdateProvisioningArtifact $
--             updateProvisioningArtifactResponse
--
--         , responseDeletePortfolioShare $
--             deletePortfolioShareResponse
--
--         , responseDeleteProvisioningArtifact $
--             deleteProvisioningArtifactResponse
--
--         , responseListProvisioningArtifactsForServiceAction $
--             listProvisioningArtifactsForServiceActionResponse
--
--         , responseCreatePortfolio $
--             createPortfolioResponse
--
--         , responseListLaunchPaths $
--             listLaunchPathsResponse
--
--         , responseListResourcesForTagOption $
--             listResourcesForTagOptionResponse
--
--         , responseAssociateBudgetWithResource $
--             associateBudgetWithResourceResponse
--
--           ]
--     ]

-- Requests

requestImportAsProvisionedProduct :: ImportAsProvisionedProduct -> TestTree
requestImportAsProvisionedProduct =
  req
    "ImportAsProvisionedProduct"
    "fixture/ImportAsProvisionedProduct.yaml"

requestDeleteConstraint :: DeleteConstraint -> TestTree
requestDeleteConstraint =
  req
    "DeleteConstraint"
    "fixture/DeleteConstraint.yaml"

requestUpdateConstraint :: UpdateConstraint -> TestTree
requestUpdateConstraint =
  req
    "UpdateConstraint"
    "fixture/UpdateConstraint.yaml"

requestCreateProvisionedProductPlan :: CreateProvisionedProductPlan -> TestTree
requestCreateProvisionedProductPlan =
  req
    "CreateProvisionedProductPlan"
    "fixture/CreateProvisionedProductPlan.yaml"

requestExecuteProvisionedProductServiceAction :: ExecuteProvisionedProductServiceAction -> TestTree
requestExecuteProvisionedProductServiceAction =
  req
    "ExecuteProvisionedProductServiceAction"
    "fixture/ExecuteProvisionedProductServiceAction.yaml"

requestCreateProduct :: CreateProduct -> TestTree
requestCreateProduct =
  req
    "CreateProduct"
    "fixture/CreateProduct.yaml"

requestDescribeCopyProductStatus :: DescribeCopyProductStatus -> TestTree
requestDescribeCopyProductStatus =
  req
    "DescribeCopyProductStatus"
    "fixture/DescribeCopyProductStatus.yaml"

requestCreateServiceAction :: CreateServiceAction -> TestTree
requestCreateServiceAction =
  req
    "CreateServiceAction"
    "fixture/CreateServiceAction.yaml"

requestTerminateProvisionedProduct :: TerminateProvisionedProduct -> TestTree
requestTerminateProvisionedProduct =
  req
    "TerminateProvisionedProduct"
    "fixture/TerminateProvisionedProduct.yaml"

requestUpdateProvisionedProduct :: UpdateProvisionedProduct -> TestTree
requestUpdateProvisionedProduct =
  req
    "UpdateProvisionedProduct"
    "fixture/UpdateProvisionedProduct.yaml"

requestDescribeProvisioningArtifact :: DescribeProvisioningArtifact -> TestTree
requestDescribeProvisioningArtifact =
  req
    "DescribeProvisioningArtifact"
    "fixture/DescribeProvisioningArtifact.yaml"

requestAssociateServiceActionWithProvisioningArtifact :: AssociateServiceActionWithProvisioningArtifact -> TestTree
requestAssociateServiceActionWithProvisioningArtifact =
  req
    "AssociateServiceActionWithProvisioningArtifact"
    "fixture/AssociateServiceActionWithProvisioningArtifact.yaml"

requestListRecordHistory :: ListRecordHistory -> TestTree
requestListRecordHistory =
  req
    "ListRecordHistory"
    "fixture/ListRecordHistory.yaml"

requestDescribeProvisionedProductPlan :: DescribeProvisionedProductPlan -> TestTree
requestDescribeProvisionedProductPlan =
  req
    "DescribeProvisionedProductPlan"
    "fixture/DescribeProvisionedProductPlan.yaml"

requestAssociateTagOptionWithResource :: AssociateTagOptionWithResource -> TestTree
requestAssociateTagOptionWithResource =
  req
    "AssociateTagOptionWithResource"
    "fixture/AssociateTagOptionWithResource.yaml"

requestCreateTagOption :: CreateTagOption -> TestTree
requestCreateTagOption =
  req
    "CreateTagOption"
    "fixture/CreateTagOption.yaml"

requestListBudgetsForResource :: ListBudgetsForResource -> TestTree
requestListBudgetsForResource =
  req
    "ListBudgetsForResource"
    "fixture/ListBudgetsForResource.yaml"

requestDisassociateProductFromPortfolio :: DisassociateProductFromPortfolio -> TestTree
requestDisassociateProductFromPortfolio =
  req
    "DisassociateProductFromPortfolio"
    "fixture/DisassociateProductFromPortfolio.yaml"

requestListConstraintsForPortfolio :: ListConstraintsForPortfolio -> TestTree
requestListConstraintsForPortfolio =
  req
    "ListConstraintsForPortfolio"
    "fixture/ListConstraintsForPortfolio.yaml"

requestDescribeRecord :: DescribeRecord -> TestTree
requestDescribeRecord =
  req
    "DescribeRecord"
    "fixture/DescribeRecord.yaml"

requestEnableAWSOrganizationsAccess :: EnableAWSOrganizationsAccess -> TestTree
requestEnableAWSOrganizationsAccess =
  req
    "EnableAWSOrganizationsAccess"
    "fixture/EnableAWSOrganizationsAccess.yaml"

requestDescribeConstraint :: DescribeConstraint -> TestTree
requestDescribeConstraint =
  req
    "DescribeConstraint"
    "fixture/DescribeConstraint.yaml"

requestCreateProvisioningArtifact :: CreateProvisioningArtifact -> TestTree
requestCreateProvisioningArtifact =
  req
    "CreateProvisioningArtifact"
    "fixture/CreateProvisioningArtifact.yaml"

requestListPortfolios :: ListPortfolios -> TestTree
requestListPortfolios =
  req
    "ListPortfolios"
    "fixture/ListPortfolios.yaml"

requestDisassociateBudgetFromResource :: DisassociateBudgetFromResource -> TestTree
requestDisassociateBudgetFromResource =
  req
    "DisassociateBudgetFromResource"
    "fixture/DisassociateBudgetFromResource.yaml"

requestDescribeProductView :: DescribeProductView -> TestTree
requestDescribeProductView =
  req
    "DescribeProductView"
    "fixture/DescribeProductView.yaml"

requestCreatePortfolioShare :: CreatePortfolioShare -> TestTree
requestCreatePortfolioShare =
  req
    "CreatePortfolioShare"
    "fixture/CreatePortfolioShare.yaml"

requestListProvisioningArtifacts :: ListProvisioningArtifacts -> TestTree
requestListProvisioningArtifacts =
  req
    "ListProvisioningArtifacts"
    "fixture/ListProvisioningArtifacts.yaml"

requestListServiceActionsForProvisioningArtifact :: ListServiceActionsForProvisioningArtifact -> TestTree
requestListServiceActionsForProvisioningArtifact =
  req
    "ListServiceActionsForProvisioningArtifact"
    "fixture/ListServiceActionsForProvisioningArtifact.yaml"

requestSearchProducts :: SearchProducts -> TestTree
requestSearchProducts =
  req
    "SearchProducts"
    "fixture/SearchProducts.yaml"

requestDescribeServiceActionExecutionParameters :: DescribeServiceActionExecutionParameters -> TestTree
requestDescribeServiceActionExecutionParameters =
  req
    "DescribeServiceActionExecutionParameters"
    "fixture/DescribeServiceActionExecutionParameters.yaml"

requestSearchProvisionedProducts :: SearchProvisionedProducts -> TestTree
requestSearchProvisionedProducts =
  req
    "SearchProvisionedProducts"
    "fixture/SearchProvisionedProducts.yaml"

requestListStackInstancesForProvisionedProduct :: ListStackInstancesForProvisionedProduct -> TestTree
requestListStackInstancesForProvisionedProduct =
  req
    "ListStackInstancesForProvisionedProduct"
    "fixture/ListStackInstancesForProvisionedProduct.yaml"

requestDescribeServiceAction :: DescribeServiceAction -> TestTree
requestDescribeServiceAction =
  req
    "DescribeServiceAction"
    "fixture/DescribeServiceAction.yaml"

requestDescribeProduct :: DescribeProduct -> TestTree
requestDescribeProduct =
  req
    "DescribeProduct"
    "fixture/DescribeProduct.yaml"

requestDeleteProvisionedProductPlan :: DeleteProvisionedProductPlan -> TestTree
requestDeleteProvisionedProductPlan =
  req
    "DeleteProvisionedProductPlan"
    "fixture/DeleteProvisionedProductPlan.yaml"

requestGetProvisionedProductOutputs :: GetProvisionedProductOutputs -> TestTree
requestGetProvisionedProductOutputs =
  req
    "GetProvisionedProductOutputs"
    "fixture/GetProvisionedProductOutputs.yaml"

requestCreateConstraint :: CreateConstraint -> TestTree
requestCreateConstraint =
  req
    "CreateConstraint"
    "fixture/CreateConstraint.yaml"

requestListProvisionedProductPlans :: ListProvisionedProductPlans -> TestTree
requestListProvisionedProductPlans =
  req
    "ListProvisionedProductPlans"
    "fixture/ListProvisionedProductPlans.yaml"

requestListPortfolioAccess :: ListPortfolioAccess -> TestTree
requestListPortfolioAccess =
  req
    "ListPortfolioAccess"
    "fixture/ListPortfolioAccess.yaml"

requestBatchDisassociateServiceActionFromProvisioningArtifact :: BatchDisassociateServiceActionFromProvisioningArtifact -> TestTree
requestBatchDisassociateServiceActionFromProvisioningArtifact =
  req
    "BatchDisassociateServiceActionFromProvisioningArtifact"
    "fixture/BatchDisassociateServiceActionFromProvisioningArtifact.yaml"

requestDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolio -> TestTree
requestDisassociatePrincipalFromPortfolio =
  req
    "DisassociatePrincipalFromPortfolio"
    "fixture/DisassociatePrincipalFromPortfolio.yaml"

requestDescribeTagOption :: DescribeTagOption -> TestTree
requestDescribeTagOption =
  req
    "DescribeTagOption"
    "fixture/DescribeTagOption.yaml"

requestDisassociateTagOptionFromResource :: DisassociateTagOptionFromResource -> TestTree
requestDisassociateTagOptionFromResource =
  req
    "DisassociateTagOptionFromResource"
    "fixture/DisassociateTagOptionFromResource.yaml"

requestDescribePortfolio :: DescribePortfolio -> TestTree
requestDescribePortfolio =
  req
    "DescribePortfolio"
    "fixture/DescribePortfolio.yaml"

requestAssociateProductWithPortfolio :: AssociateProductWithPortfolio -> TestTree
requestAssociateProductWithPortfolio =
  req
    "AssociateProductWithPortfolio"
    "fixture/AssociateProductWithPortfolio.yaml"

requestListAcceptedPortfolioShares :: ListAcceptedPortfolioShares -> TestTree
requestListAcceptedPortfolioShares =
  req
    "ListAcceptedPortfolioShares"
    "fixture/ListAcceptedPortfolioShares.yaml"

requestExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlan -> TestTree
requestExecuteProvisionedProductPlan =
  req
    "ExecuteProvisionedProductPlan"
    "fixture/ExecuteProvisionedProductPlan.yaml"

requestAcceptPortfolioShare :: AcceptPortfolioShare -> TestTree
requestAcceptPortfolioShare =
  req
    "AcceptPortfolioShare"
    "fixture/AcceptPortfolioShare.yaml"

requestScanProvisionedProducts :: ScanProvisionedProducts -> TestTree
requestScanProvisionedProducts =
  req
    "ScanProvisionedProducts"
    "fixture/ScanProvisionedProducts.yaml"

requestListOrganizationPortfolioAccess :: ListOrganizationPortfolioAccess -> TestTree
requestListOrganizationPortfolioAccess =
  req
    "ListOrganizationPortfolioAccess"
    "fixture/ListOrganizationPortfolioAccess.yaml"

requestListPrincipalsForPortfolio :: ListPrincipalsForPortfolio -> TestTree
requestListPrincipalsForPortfolio =
  req
    "ListPrincipalsForPortfolio"
    "fixture/ListPrincipalsForPortfolio.yaml"

requestDeleteProduct :: DeleteProduct -> TestTree
requestDeleteProduct =
  req
    "DeleteProduct"
    "fixture/DeleteProduct.yaml"

requestUpdateProduct :: UpdateProduct -> TestTree
requestUpdateProduct =
  req
    "UpdateProduct"
    "fixture/UpdateProduct.yaml"

requestListServiceActions :: ListServiceActions -> TestTree
requestListServiceActions =
  req
    "ListServiceActions"
    "fixture/ListServiceActions.yaml"

requestProvisionProduct :: ProvisionProduct -> TestTree
requestProvisionProduct =
  req
    "ProvisionProduct"
    "fixture/ProvisionProduct.yaml"

requestDeleteServiceAction :: DeleteServiceAction -> TestTree
requestDeleteServiceAction =
  req
    "DeleteServiceAction"
    "fixture/DeleteServiceAction.yaml"

requestUpdateServiceAction :: UpdateServiceAction -> TestTree
requestUpdateServiceAction =
  req
    "UpdateServiceAction"
    "fixture/UpdateServiceAction.yaml"

requestDisableAWSOrganizationsAccess :: DisableAWSOrganizationsAccess -> TestTree
requestDisableAWSOrganizationsAccess =
  req
    "DisableAWSOrganizationsAccess"
    "fixture/DisableAWSOrganizationsAccess.yaml"

requestRejectPortfolioShare :: RejectPortfolioShare -> TestTree
requestRejectPortfolioShare =
  req
    "RejectPortfolioShare"
    "fixture/RejectPortfolioShare.yaml"

requestDisassociateServiceActionFromProvisioningArtifact :: DisassociateServiceActionFromProvisioningArtifact -> TestTree
requestDisassociateServiceActionFromProvisioningArtifact =
  req
    "DisassociateServiceActionFromProvisioningArtifact"
    "fixture/DisassociateServiceActionFromProvisioningArtifact.yaml"

requestDeleteTagOption :: DeleteTagOption -> TestTree
requestDeleteTagOption =
  req
    "DeleteTagOption"
    "fixture/DeleteTagOption.yaml"

requestUpdateTagOption :: UpdateTagOption -> TestTree
requestUpdateTagOption =
  req
    "UpdateTagOption"
    "fixture/UpdateTagOption.yaml"

requestListTagOptions :: ListTagOptions -> TestTree
requestListTagOptions =
  req
    "ListTagOptions"
    "fixture/ListTagOptions.yaml"

requestUpdateProvisionedProductProperties :: UpdateProvisionedProductProperties -> TestTree
requestUpdateProvisionedProductProperties =
  req
    "UpdateProvisionedProductProperties"
    "fixture/UpdateProvisionedProductProperties.yaml"

requestSearchProductsAsAdmin :: SearchProductsAsAdmin -> TestTree
requestSearchProductsAsAdmin =
  req
    "SearchProductsAsAdmin"
    "fixture/SearchProductsAsAdmin.yaml"

requestDeletePortfolio :: DeletePortfolio -> TestTree
requestDeletePortfolio =
  req
    "DeletePortfolio"
    "fixture/DeletePortfolio.yaml"

requestUpdatePortfolio :: UpdatePortfolio -> TestTree
requestUpdatePortfolio =
  req
    "UpdatePortfolio"
    "fixture/UpdatePortfolio.yaml"

requestListPortfoliosForProduct :: ListPortfoliosForProduct -> TestTree
requestListPortfoliosForProduct =
  req
    "ListPortfoliosForProduct"
    "fixture/ListPortfoliosForProduct.yaml"

requestGetAWSOrganizationsAccessStatus :: GetAWSOrganizationsAccessStatus -> TestTree
requestGetAWSOrganizationsAccessStatus =
  req
    "GetAWSOrganizationsAccessStatus"
    "fixture/GetAWSOrganizationsAccessStatus.yaml"

requestDescribeProductAsAdmin :: DescribeProductAsAdmin -> TestTree
requestDescribeProductAsAdmin =
  req
    "DescribeProductAsAdmin"
    "fixture/DescribeProductAsAdmin.yaml"

requestBatchAssociateServiceActionWithProvisioningArtifact :: BatchAssociateServiceActionWithProvisioningArtifact -> TestTree
requestBatchAssociateServiceActionWithProvisioningArtifact =
  req
    "BatchAssociateServiceActionWithProvisioningArtifact"
    "fixture/BatchAssociateServiceActionWithProvisioningArtifact.yaml"

requestDescribeProvisioningParameters :: DescribeProvisioningParameters -> TestTree
requestDescribeProvisioningParameters =
  req
    "DescribeProvisioningParameters"
    "fixture/DescribeProvisioningParameters.yaml"

requestAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolio -> TestTree
requestAssociatePrincipalWithPortfolio =
  req
    "AssociatePrincipalWithPortfolio"
    "fixture/AssociatePrincipalWithPortfolio.yaml"

requestDescribeProvisionedProduct :: DescribeProvisionedProduct -> TestTree
requestDescribeProvisionedProduct =
  req
    "DescribeProvisionedProduct"
    "fixture/DescribeProvisionedProduct.yaml"

requestCopyProduct :: CopyProduct -> TestTree
requestCopyProduct =
  req
    "CopyProduct"
    "fixture/CopyProduct.yaml"

requestDescribePortfolioShareStatus :: DescribePortfolioShareStatus -> TestTree
requestDescribePortfolioShareStatus =
  req
    "DescribePortfolioShareStatus"
    "fixture/DescribePortfolioShareStatus.yaml"

requestUpdateProvisioningArtifact :: UpdateProvisioningArtifact -> TestTree
requestUpdateProvisioningArtifact =
  req
    "UpdateProvisioningArtifact"
    "fixture/UpdateProvisioningArtifact.yaml"

requestDeletePortfolioShare :: DeletePortfolioShare -> TestTree
requestDeletePortfolioShare =
  req
    "DeletePortfolioShare"
    "fixture/DeletePortfolioShare.yaml"

requestDeleteProvisioningArtifact :: DeleteProvisioningArtifact -> TestTree
requestDeleteProvisioningArtifact =
  req
    "DeleteProvisioningArtifact"
    "fixture/DeleteProvisioningArtifact.yaml"

requestListProvisioningArtifactsForServiceAction :: ListProvisioningArtifactsForServiceAction -> TestTree
requestListProvisioningArtifactsForServiceAction =
  req
    "ListProvisioningArtifactsForServiceAction"
    "fixture/ListProvisioningArtifactsForServiceAction.yaml"

requestCreatePortfolio :: CreatePortfolio -> TestTree
requestCreatePortfolio =
  req
    "CreatePortfolio"
    "fixture/CreatePortfolio.yaml"

requestListLaunchPaths :: ListLaunchPaths -> TestTree
requestListLaunchPaths =
  req
    "ListLaunchPaths"
    "fixture/ListLaunchPaths.yaml"

requestListResourcesForTagOption :: ListResourcesForTagOption -> TestTree
requestListResourcesForTagOption =
  req
    "ListResourcesForTagOption"
    "fixture/ListResourcesForTagOption.yaml"

requestAssociateBudgetWithResource :: AssociateBudgetWithResource -> TestTree
requestAssociateBudgetWithResource =
  req
    "AssociateBudgetWithResource"
    "fixture/AssociateBudgetWithResource.yaml"

-- Responses

responseImportAsProvisionedProduct :: ImportAsProvisionedProductResponse -> TestTree
responseImportAsProvisionedProduct =
  res
    "ImportAsProvisionedProductResponse"
    "fixture/ImportAsProvisionedProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ImportAsProvisionedProduct)

responseDeleteConstraint :: DeleteConstraintResponse -> TestTree
responseDeleteConstraint =
  res
    "DeleteConstraintResponse"
    "fixture/DeleteConstraintResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteConstraint)

responseUpdateConstraint :: UpdateConstraintResponse -> TestTree
responseUpdateConstraint =
  res
    "UpdateConstraintResponse"
    "fixture/UpdateConstraintResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateConstraint)

responseCreateProvisionedProductPlan :: CreateProvisionedProductPlanResponse -> TestTree
responseCreateProvisionedProductPlan =
  res
    "CreateProvisionedProductPlanResponse"
    "fixture/CreateProvisionedProductPlanResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateProvisionedProductPlan)

responseExecuteProvisionedProductServiceAction :: ExecuteProvisionedProductServiceActionResponse -> TestTree
responseExecuteProvisionedProductServiceAction =
  res
    "ExecuteProvisionedProductServiceActionResponse"
    "fixture/ExecuteProvisionedProductServiceActionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ExecuteProvisionedProductServiceAction)

responseCreateProduct :: CreateProductResponse -> TestTree
responseCreateProduct =
  res
    "CreateProductResponse"
    "fixture/CreateProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateProduct)

responseDescribeCopyProductStatus :: DescribeCopyProductStatusResponse -> TestTree
responseDescribeCopyProductStatus =
  res
    "DescribeCopyProductStatusResponse"
    "fixture/DescribeCopyProductStatusResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeCopyProductStatus)

responseCreateServiceAction :: CreateServiceActionResponse -> TestTree
responseCreateServiceAction =
  res
    "CreateServiceActionResponse"
    "fixture/CreateServiceActionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateServiceAction)

responseTerminateProvisionedProduct :: TerminateProvisionedProductResponse -> TestTree
responseTerminateProvisionedProduct =
  res
    "TerminateProvisionedProductResponse"
    "fixture/TerminateProvisionedProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy TerminateProvisionedProduct)

responseUpdateProvisionedProduct :: UpdateProvisionedProductResponse -> TestTree
responseUpdateProvisionedProduct =
  res
    "UpdateProvisionedProductResponse"
    "fixture/UpdateProvisionedProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateProvisionedProduct)

responseDescribeProvisioningArtifact :: DescribeProvisioningArtifactResponse -> TestTree
responseDescribeProvisioningArtifact =
  res
    "DescribeProvisioningArtifactResponse"
    "fixture/DescribeProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProvisioningArtifact)

responseAssociateServiceActionWithProvisioningArtifact :: AssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseAssociateServiceActionWithProvisioningArtifact =
  res
    "AssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/AssociateServiceActionWithProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AssociateServiceActionWithProvisioningArtifact)

responseListRecordHistory :: ListRecordHistoryResponse -> TestTree
responseListRecordHistory =
  res
    "ListRecordHistoryResponse"
    "fixture/ListRecordHistoryResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListRecordHistory)

responseDescribeProvisionedProductPlan :: DescribeProvisionedProductPlanResponse -> TestTree
responseDescribeProvisionedProductPlan =
  res
    "DescribeProvisionedProductPlanResponse"
    "fixture/DescribeProvisionedProductPlanResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProvisionedProductPlan)

responseAssociateTagOptionWithResource :: AssociateTagOptionWithResourceResponse -> TestTree
responseAssociateTagOptionWithResource =
  res
    "AssociateTagOptionWithResourceResponse"
    "fixture/AssociateTagOptionWithResourceResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AssociateTagOptionWithResource)

responseCreateTagOption :: CreateTagOptionResponse -> TestTree
responseCreateTagOption =
  res
    "CreateTagOptionResponse"
    "fixture/CreateTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateTagOption)

responseListBudgetsForResource :: ListBudgetsForResourceResponse -> TestTree
responseListBudgetsForResource =
  res
    "ListBudgetsForResourceResponse"
    "fixture/ListBudgetsForResourceResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListBudgetsForResource)

responseDisassociateProductFromPortfolio :: DisassociateProductFromPortfolioResponse -> TestTree
responseDisassociateProductFromPortfolio =
  res
    "DisassociateProductFromPortfolioResponse"
    "fixture/DisassociateProductFromPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DisassociateProductFromPortfolio)

responseListConstraintsForPortfolio :: ListConstraintsForPortfolioResponse -> TestTree
responseListConstraintsForPortfolio =
  res
    "ListConstraintsForPortfolioResponse"
    "fixture/ListConstraintsForPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListConstraintsForPortfolio)

responseDescribeRecord :: DescribeRecordResponse -> TestTree
responseDescribeRecord =
  res
    "DescribeRecordResponse"
    "fixture/DescribeRecordResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeRecord)

responseEnableAWSOrganizationsAccess :: EnableAWSOrganizationsAccessResponse -> TestTree
responseEnableAWSOrganizationsAccess =
  res
    "EnableAWSOrganizationsAccessResponse"
    "fixture/EnableAWSOrganizationsAccessResponse.proto"
    serviceCatalog
    (Proxy :: Proxy EnableAWSOrganizationsAccess)

responseDescribeConstraint :: DescribeConstraintResponse -> TestTree
responseDescribeConstraint =
  res
    "DescribeConstraintResponse"
    "fixture/DescribeConstraintResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeConstraint)

responseCreateProvisioningArtifact :: CreateProvisioningArtifactResponse -> TestTree
responseCreateProvisioningArtifact =
  res
    "CreateProvisioningArtifactResponse"
    "fixture/CreateProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateProvisioningArtifact)

responseListPortfolios :: ListPortfoliosResponse -> TestTree
responseListPortfolios =
  res
    "ListPortfoliosResponse"
    "fixture/ListPortfoliosResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListPortfolios)

responseDisassociateBudgetFromResource :: DisassociateBudgetFromResourceResponse -> TestTree
responseDisassociateBudgetFromResource =
  res
    "DisassociateBudgetFromResourceResponse"
    "fixture/DisassociateBudgetFromResourceResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DisassociateBudgetFromResource)

responseDescribeProductView :: DescribeProductViewResponse -> TestTree
responseDescribeProductView =
  res
    "DescribeProductViewResponse"
    "fixture/DescribeProductViewResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProductView)

responseCreatePortfolioShare :: CreatePortfolioShareResponse -> TestTree
responseCreatePortfolioShare =
  res
    "CreatePortfolioShareResponse"
    "fixture/CreatePortfolioShareResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreatePortfolioShare)

responseListProvisioningArtifacts :: ListProvisioningArtifactsResponse -> TestTree
responseListProvisioningArtifacts =
  res
    "ListProvisioningArtifactsResponse"
    "fixture/ListProvisioningArtifactsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListProvisioningArtifacts)

responseListServiceActionsForProvisioningArtifact :: ListServiceActionsForProvisioningArtifactResponse -> TestTree
responseListServiceActionsForProvisioningArtifact =
  res
    "ListServiceActionsForProvisioningArtifactResponse"
    "fixture/ListServiceActionsForProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListServiceActionsForProvisioningArtifact)

responseSearchProducts :: SearchProductsResponse -> TestTree
responseSearchProducts =
  res
    "SearchProductsResponse"
    "fixture/SearchProductsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy SearchProducts)

responseDescribeServiceActionExecutionParameters :: DescribeServiceActionExecutionParametersResponse -> TestTree
responseDescribeServiceActionExecutionParameters =
  res
    "DescribeServiceActionExecutionParametersResponse"
    "fixture/DescribeServiceActionExecutionParametersResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeServiceActionExecutionParameters)

responseSearchProvisionedProducts :: SearchProvisionedProductsResponse -> TestTree
responseSearchProvisionedProducts =
  res
    "SearchProvisionedProductsResponse"
    "fixture/SearchProvisionedProductsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy SearchProvisionedProducts)

responseListStackInstancesForProvisionedProduct :: ListStackInstancesForProvisionedProductResponse -> TestTree
responseListStackInstancesForProvisionedProduct =
  res
    "ListStackInstancesForProvisionedProductResponse"
    "fixture/ListStackInstancesForProvisionedProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListStackInstancesForProvisionedProduct)

responseDescribeServiceAction :: DescribeServiceActionResponse -> TestTree
responseDescribeServiceAction =
  res
    "DescribeServiceActionResponse"
    "fixture/DescribeServiceActionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeServiceAction)

responseDescribeProduct :: DescribeProductResponse -> TestTree
responseDescribeProduct =
  res
    "DescribeProductResponse"
    "fixture/DescribeProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProduct)

responseDeleteProvisionedProductPlan :: DeleteProvisionedProductPlanResponse -> TestTree
responseDeleteProvisionedProductPlan =
  res
    "DeleteProvisionedProductPlanResponse"
    "fixture/DeleteProvisionedProductPlanResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteProvisionedProductPlan)

responseGetProvisionedProductOutputs :: GetProvisionedProductOutputsResponse -> TestTree
responseGetProvisionedProductOutputs =
  res
    "GetProvisionedProductOutputsResponse"
    "fixture/GetProvisionedProductOutputsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy GetProvisionedProductOutputs)

responseCreateConstraint :: CreateConstraintResponse -> TestTree
responseCreateConstraint =
  res
    "CreateConstraintResponse"
    "fixture/CreateConstraintResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateConstraint)

responseListProvisionedProductPlans :: ListProvisionedProductPlansResponse -> TestTree
responseListProvisionedProductPlans =
  res
    "ListProvisionedProductPlansResponse"
    "fixture/ListProvisionedProductPlansResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListProvisionedProductPlans)

responseListPortfolioAccess :: ListPortfolioAccessResponse -> TestTree
responseListPortfolioAccess =
  res
    "ListPortfolioAccessResponse"
    "fixture/ListPortfolioAccessResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListPortfolioAccess)

responseBatchDisassociateServiceActionFromProvisioningArtifact :: BatchDisassociateServiceActionFromProvisioningArtifactResponse -> TestTree
responseBatchDisassociateServiceActionFromProvisioningArtifact =
  res
    "BatchDisassociateServiceActionFromProvisioningArtifactResponse"
    "fixture/BatchDisassociateServiceActionFromProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy BatchDisassociateServiceActionFromProvisioningArtifact)

responseDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolioResponse -> TestTree
responseDisassociatePrincipalFromPortfolio =
  res
    "DisassociatePrincipalFromPortfolioResponse"
    "fixture/DisassociatePrincipalFromPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DisassociatePrincipalFromPortfolio)

responseDescribeTagOption :: DescribeTagOptionResponse -> TestTree
responseDescribeTagOption =
  res
    "DescribeTagOptionResponse"
    "fixture/DescribeTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeTagOption)

responseDisassociateTagOptionFromResource :: DisassociateTagOptionFromResourceResponse -> TestTree
responseDisassociateTagOptionFromResource =
  res
    "DisassociateTagOptionFromResourceResponse"
    "fixture/DisassociateTagOptionFromResourceResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DisassociateTagOptionFromResource)

responseDescribePortfolio :: DescribePortfolioResponse -> TestTree
responseDescribePortfolio =
  res
    "DescribePortfolioResponse"
    "fixture/DescribePortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribePortfolio)

responseAssociateProductWithPortfolio :: AssociateProductWithPortfolioResponse -> TestTree
responseAssociateProductWithPortfolio =
  res
    "AssociateProductWithPortfolioResponse"
    "fixture/AssociateProductWithPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AssociateProductWithPortfolio)

responseListAcceptedPortfolioShares :: ListAcceptedPortfolioSharesResponse -> TestTree
responseListAcceptedPortfolioShares =
  res
    "ListAcceptedPortfolioSharesResponse"
    "fixture/ListAcceptedPortfolioSharesResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListAcceptedPortfolioShares)

responseExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlanResponse -> TestTree
responseExecuteProvisionedProductPlan =
  res
    "ExecuteProvisionedProductPlanResponse"
    "fixture/ExecuteProvisionedProductPlanResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ExecuteProvisionedProductPlan)

responseAcceptPortfolioShare :: AcceptPortfolioShareResponse -> TestTree
responseAcceptPortfolioShare =
  res
    "AcceptPortfolioShareResponse"
    "fixture/AcceptPortfolioShareResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AcceptPortfolioShare)

responseScanProvisionedProducts :: ScanProvisionedProductsResponse -> TestTree
responseScanProvisionedProducts =
  res
    "ScanProvisionedProductsResponse"
    "fixture/ScanProvisionedProductsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ScanProvisionedProducts)

responseListOrganizationPortfolioAccess :: ListOrganizationPortfolioAccessResponse -> TestTree
responseListOrganizationPortfolioAccess =
  res
    "ListOrganizationPortfolioAccessResponse"
    "fixture/ListOrganizationPortfolioAccessResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListOrganizationPortfolioAccess)

responseListPrincipalsForPortfolio :: ListPrincipalsForPortfolioResponse -> TestTree
responseListPrincipalsForPortfolio =
  res
    "ListPrincipalsForPortfolioResponse"
    "fixture/ListPrincipalsForPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListPrincipalsForPortfolio)

responseDeleteProduct :: DeleteProductResponse -> TestTree
responseDeleteProduct =
  res
    "DeleteProductResponse"
    "fixture/DeleteProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteProduct)

responseUpdateProduct :: UpdateProductResponse -> TestTree
responseUpdateProduct =
  res
    "UpdateProductResponse"
    "fixture/UpdateProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateProduct)

responseListServiceActions :: ListServiceActionsResponse -> TestTree
responseListServiceActions =
  res
    "ListServiceActionsResponse"
    "fixture/ListServiceActionsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListServiceActions)

responseProvisionProduct :: ProvisionProductResponse -> TestTree
responseProvisionProduct =
  res
    "ProvisionProductResponse"
    "fixture/ProvisionProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ProvisionProduct)

responseDeleteServiceAction :: DeleteServiceActionResponse -> TestTree
responseDeleteServiceAction =
  res
    "DeleteServiceActionResponse"
    "fixture/DeleteServiceActionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteServiceAction)

responseUpdateServiceAction :: UpdateServiceActionResponse -> TestTree
responseUpdateServiceAction =
  res
    "UpdateServiceActionResponse"
    "fixture/UpdateServiceActionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateServiceAction)

responseDisableAWSOrganizationsAccess :: DisableAWSOrganizationsAccessResponse -> TestTree
responseDisableAWSOrganizationsAccess =
  res
    "DisableAWSOrganizationsAccessResponse"
    "fixture/DisableAWSOrganizationsAccessResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DisableAWSOrganizationsAccess)

responseRejectPortfolioShare :: RejectPortfolioShareResponse -> TestTree
responseRejectPortfolioShare =
  res
    "RejectPortfolioShareResponse"
    "fixture/RejectPortfolioShareResponse.proto"
    serviceCatalog
    (Proxy :: Proxy RejectPortfolioShare)

responseDisassociateServiceActionFromProvisioningArtifact :: DisassociateServiceActionFromProvisioningArtifactResponse -> TestTree
responseDisassociateServiceActionFromProvisioningArtifact =
  res
    "DisassociateServiceActionFromProvisioningArtifactResponse"
    "fixture/DisassociateServiceActionFromProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DisassociateServiceActionFromProvisioningArtifact)

responseDeleteTagOption :: DeleteTagOptionResponse -> TestTree
responseDeleteTagOption =
  res
    "DeleteTagOptionResponse"
    "fixture/DeleteTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteTagOption)

responseUpdateTagOption :: UpdateTagOptionResponse -> TestTree
responseUpdateTagOption =
  res
    "UpdateTagOptionResponse"
    "fixture/UpdateTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateTagOption)

responseListTagOptions :: ListTagOptionsResponse -> TestTree
responseListTagOptions =
  res
    "ListTagOptionsResponse"
    "fixture/ListTagOptionsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListTagOptions)

responseUpdateProvisionedProductProperties :: UpdateProvisionedProductPropertiesResponse -> TestTree
responseUpdateProvisionedProductProperties =
  res
    "UpdateProvisionedProductPropertiesResponse"
    "fixture/UpdateProvisionedProductPropertiesResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateProvisionedProductProperties)

responseSearchProductsAsAdmin :: SearchProductsAsAdminResponse -> TestTree
responseSearchProductsAsAdmin =
  res
    "SearchProductsAsAdminResponse"
    "fixture/SearchProductsAsAdminResponse.proto"
    serviceCatalog
    (Proxy :: Proxy SearchProductsAsAdmin)

responseDeletePortfolio :: DeletePortfolioResponse -> TestTree
responseDeletePortfolio =
  res
    "DeletePortfolioResponse"
    "fixture/DeletePortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeletePortfolio)

responseUpdatePortfolio :: UpdatePortfolioResponse -> TestTree
responseUpdatePortfolio =
  res
    "UpdatePortfolioResponse"
    "fixture/UpdatePortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdatePortfolio)

responseListPortfoliosForProduct :: ListPortfoliosForProductResponse -> TestTree
responseListPortfoliosForProduct =
  res
    "ListPortfoliosForProductResponse"
    "fixture/ListPortfoliosForProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListPortfoliosForProduct)

responseGetAWSOrganizationsAccessStatus :: GetAWSOrganizationsAccessStatusResponse -> TestTree
responseGetAWSOrganizationsAccessStatus =
  res
    "GetAWSOrganizationsAccessStatusResponse"
    "fixture/GetAWSOrganizationsAccessStatusResponse.proto"
    serviceCatalog
    (Proxy :: Proxy GetAWSOrganizationsAccessStatus)

responseDescribeProductAsAdmin :: DescribeProductAsAdminResponse -> TestTree
responseDescribeProductAsAdmin =
  res
    "DescribeProductAsAdminResponse"
    "fixture/DescribeProductAsAdminResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProductAsAdmin)

responseBatchAssociateServiceActionWithProvisioningArtifact :: BatchAssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseBatchAssociateServiceActionWithProvisioningArtifact =
  res
    "BatchAssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/BatchAssociateServiceActionWithProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy BatchAssociateServiceActionWithProvisioningArtifact)

responseDescribeProvisioningParameters :: DescribeProvisioningParametersResponse -> TestTree
responseDescribeProvisioningParameters =
  res
    "DescribeProvisioningParametersResponse"
    "fixture/DescribeProvisioningParametersResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProvisioningParameters)

responseAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolioResponse -> TestTree
responseAssociatePrincipalWithPortfolio =
  res
    "AssociatePrincipalWithPortfolioResponse"
    "fixture/AssociatePrincipalWithPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AssociatePrincipalWithPortfolio)

responseDescribeProvisionedProduct :: DescribeProvisionedProductResponse -> TestTree
responseDescribeProvisionedProduct =
  res
    "DescribeProvisionedProductResponse"
    "fixture/DescribeProvisionedProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProvisionedProduct)

responseCopyProduct :: CopyProductResponse -> TestTree
responseCopyProduct =
  res
    "CopyProductResponse"
    "fixture/CopyProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CopyProduct)

responseDescribePortfolioShareStatus :: DescribePortfolioShareStatusResponse -> TestTree
responseDescribePortfolioShareStatus =
  res
    "DescribePortfolioShareStatusResponse"
    "fixture/DescribePortfolioShareStatusResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribePortfolioShareStatus)

responseUpdateProvisioningArtifact :: UpdateProvisioningArtifactResponse -> TestTree
responseUpdateProvisioningArtifact =
  res
    "UpdateProvisioningArtifactResponse"
    "fixture/UpdateProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateProvisioningArtifact)

responseDeletePortfolioShare :: DeletePortfolioShareResponse -> TestTree
responseDeletePortfolioShare =
  res
    "DeletePortfolioShareResponse"
    "fixture/DeletePortfolioShareResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeletePortfolioShare)

responseDeleteProvisioningArtifact :: DeleteProvisioningArtifactResponse -> TestTree
responseDeleteProvisioningArtifact =
  res
    "DeleteProvisioningArtifactResponse"
    "fixture/DeleteProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteProvisioningArtifact)

responseListProvisioningArtifactsForServiceAction :: ListProvisioningArtifactsForServiceActionResponse -> TestTree
responseListProvisioningArtifactsForServiceAction =
  res
    "ListProvisioningArtifactsForServiceActionResponse"
    "fixture/ListProvisioningArtifactsForServiceActionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListProvisioningArtifactsForServiceAction)

responseCreatePortfolio :: CreatePortfolioResponse -> TestTree
responseCreatePortfolio =
  res
    "CreatePortfolioResponse"
    "fixture/CreatePortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreatePortfolio)

responseListLaunchPaths :: ListLaunchPathsResponse -> TestTree
responseListLaunchPaths =
  res
    "ListLaunchPathsResponse"
    "fixture/ListLaunchPathsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListLaunchPaths)

responseListResourcesForTagOption :: ListResourcesForTagOptionResponse -> TestTree
responseListResourcesForTagOption =
  res
    "ListResourcesForTagOptionResponse"
    "fixture/ListResourcesForTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListResourcesForTagOption)

responseAssociateBudgetWithResource :: AssociateBudgetWithResourceResponse -> TestTree
responseAssociateBudgetWithResource =
  res
    "AssociateBudgetWithResourceResponse"
    "fixture/AssociateBudgetWithResourceResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AssociateBudgetWithResource)
