{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ServiceCatalog
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestListOrganizationPortfolioAccess $
--             newListOrganizationPortfolioAccess
--
--         , requestListAcceptedPortfolioShares $
--             newListAcceptedPortfolioShares
--
--         , requestDisassociateTagOptionFromResource $
--             newDisassociateTagOptionFromResource
--
--         , requestDescribePortfolio $
--             newDescribePortfolio
--
--         , requestExecuteProvisionedProductServiceAction $
--             newExecuteProvisionedProductServiceAction
--
--         , requestExecuteProvisionedProductPlan $
--             newExecuteProvisionedProductPlan
--
--         , requestScanProvisionedProducts $
--             newScanProvisionedProducts
--
--         , requestAssociateProductWithPortfolio $
--             newAssociateProductWithPortfolio
--
--         , requestUpdateConstraint $
--             newUpdateConstraint
--
--         , requestImportAsProvisionedProduct $
--             newImportAsProvisionedProduct
--
--         , requestCreateProvisionedProductPlan $
--             newCreateProvisionedProductPlan
--
--         , requestDescribeTagOption $
--             newDescribeTagOption
--
--         , requestDeleteConstraint $
--             newDeleteConstraint
--
--         , requestListPortfolioAccess $
--             newListPortfolioAccess
--
--         , requestDeleteProvisionedProductPlan $
--             newDeleteProvisionedProductPlan
--
--         , requestAssociateBudgetWithResource $
--             newAssociateBudgetWithResource
--
--         , requestGetProvisionedProductOutputs $
--             newGetProvisionedProductOutputs
--
--         , requestListResourcesForTagOption $
--             newListResourcesForTagOption
--
--         , requestListLaunchPaths $
--             newListLaunchPaths
--
--         , requestCreateConstraint $
--             newCreateConstraint
--
--         , requestDescribePortfolioShares $
--             newDescribePortfolioShares
--
--         , requestDescribeProvisioningParameters $
--             newDescribeProvisioningParameters
--
--         , requestDeleteProvisioningArtifact $
--             newDeleteProvisioningArtifact
--
--         , requestUpdateProvisioningArtifact $
--             newUpdateProvisioningArtifact
--
--         , requestDescribeServiceAction $
--             newDescribeServiceAction
--
--         , requestListServiceActionsForProvisioningArtifact $
--             newListServiceActionsForProvisioningArtifact
--
--         , requestDeletePortfolioShare $
--             newDeletePortfolioShare
--
--         , requestListProvisioningArtifacts $
--             newListProvisioningArtifacts
--
--         , requestDescribePortfolioShareStatus $
--             newDescribePortfolioShareStatus
--
--         , requestDescribeProvisionedProduct $
--             newDescribeProvisionedProduct
--
--         , requestUpdatePortfolioShare $
--             newUpdatePortfolioShare
--
--         , requestSearchProvisionedProducts $
--             newSearchProvisionedProducts
--
--         , requestListStackInstancesForProvisionedProduct $
--             newListStackInstancesForProvisionedProduct
--
--         , requestDescribeProduct $
--             newDescribeProduct
--
--         , requestCreatePortfolioShare $
--             newCreatePortfolioShare
--
--         , requestCreateProvisioningArtifact $
--             newCreateProvisioningArtifact
--
--         , requestSearchProductsAsAdmin $
--             newSearchProductsAsAdmin
--
--         , requestGetAWSOrganizationsAccessStatus $
--             newGetAWSOrganizationsAccessStatus
--
--         , requestListPortfolios $
--             newListPortfolios
--
--         , requestDisassociateBudgetFromResource $
--             newDisassociateBudgetFromResource
--
--         , requestDeletePortfolio $
--             newDeletePortfolio
--
--         , requestUpdatePortfolio $
--             newUpdatePortfolio
--
--         , requestDescribeConstraint $
--             newDescribeConstraint
--
--         , requestDeleteTagOption $
--             newDeleteTagOption
--
--         , requestDescribeRecord $
--             newDescribeRecord
--
--         , requestDisassociateServiceActionFromProvisioningArtifact $
--             newDisassociateServiceActionFromProvisioningArtifact
--
--         , requestUpdateTagOption $
--             newUpdateTagOption
--
--         , requestEnableAWSOrganizationsAccess $
--             newEnableAWSOrganizationsAccess
--
--         , requestCreateTagOption $
--             newCreateTagOption
--
--         , requestListRecordHistory $
--             newListRecordHistory
--
--         , requestListConstraintsForPortfolio $
--             newListConstraintsForPortfolio
--
--         , requestAssociateServiceActionWithProvisioningArtifact $
--             newAssociateServiceActionWithProvisioningArtifact
--
--         , requestDeleteServiceAction $
--             newDeleteServiceAction
--
--         , requestUpdateProvisionedProduct $
--             newUpdateProvisionedProduct
--
--         , requestDeleteProduct $
--             newDeleteProduct
--
--         , requestUpdateServiceAction $
--             newUpdateServiceAction
--
--         , requestDescribeProvisioningArtifact $
--             newDescribeProvisioningArtifact
--
--         , requestUpdateProduct $
--             newUpdateProduct
--
--         , requestCreateServiceAction $
--             newCreateServiceAction
--
--         , requestAcceptPortfolioShare $
--             newAcceptPortfolioShare
--
--         , requestDescribeCopyProductStatus $
--             newDescribeCopyProductStatus
--
--         , requestCreateProduct $
--             newCreateProduct
--
--         , requestBatchDisassociateServiceActionFromProvisioningArtifact $
--             newBatchDisassociateServiceActionFromProvisioningArtifact
--
--         , requestDisassociatePrincipalFromPortfolio $
--             newDisassociatePrincipalFromPortfolio
--
--         , requestListProvisionedProductPlans $
--             newListProvisionedProductPlans
--
--         , requestSearchProducts $
--             newSearchProducts
--
--         , requestListProvisioningArtifactsForServiceAction $
--             newListProvisioningArtifactsForServiceAction
--
--         , requestDescribeServiceActionExecutionParameters $
--             newDescribeServiceActionExecutionParameters
--
--         , requestBatchAssociateServiceActionWithProvisioningArtifact $
--             newBatchAssociateServiceActionWithProvisioningArtifact
--
--         , requestCopyProduct $
--             newCopyProduct
--
--         , requestAssociatePrincipalWithPortfolio $
--             newAssociatePrincipalWithPortfolio
--
--         , requestCreatePortfolio $
--             newCreatePortfolio
--
--         , requestDescribeProductView $
--             newDescribeProductView
--
--         , requestDescribeProductAsAdmin $
--             newDescribeProductAsAdmin
--
--         , requestUpdateProvisionedProductProperties $
--             newUpdateProvisionedProductProperties
--
--         , requestListPortfoliosForProduct $
--             newListPortfoliosForProduct
--
--         , requestListTagOptions $
--             newListTagOptions
--
--         , requestRejectPortfolioShare $
--             newRejectPortfolioShare
--
--         , requestAssociateTagOptionWithResource $
--             newAssociateTagOptionWithResource
--
--         , requestListBudgetsForResource $
--             newListBudgetsForResource
--
--         , requestDescribeProvisionedProductPlan $
--             newDescribeProvisionedProductPlan
--
--         , requestDisableAWSOrganizationsAccess $
--             newDisableAWSOrganizationsAccess
--
--         , requestDisassociateProductFromPortfolio $
--             newDisassociateProductFromPortfolio
--
--         , requestTerminateProvisionedProduct $
--             newTerminateProvisionedProduct
--
--         , requestListPrincipalsForPortfolio $
--             newListPrincipalsForPortfolio
--
--         , requestProvisionProduct $
--             newProvisionProduct
--
--         , requestListServiceActions $
--             newListServiceActions
--
--           ]

--     , testGroup "response"
--         [ responseListOrganizationPortfolioAccess $
--             newListOrganizationPortfolioAccessResponse
--
--         , responseListAcceptedPortfolioShares $
--             newListAcceptedPortfolioSharesResponse
--
--         , responseDisassociateTagOptionFromResource $
--             newDisassociateTagOptionFromResourceResponse
--
--         , responseDescribePortfolio $
--             newDescribePortfolioResponse
--
--         , responseExecuteProvisionedProductServiceAction $
--             newExecuteProvisionedProductServiceActionResponse
--
--         , responseExecuteProvisionedProductPlan $
--             newExecuteProvisionedProductPlanResponse
--
--         , responseScanProvisionedProducts $
--             newScanProvisionedProductsResponse
--
--         , responseAssociateProductWithPortfolio $
--             newAssociateProductWithPortfolioResponse
--
--         , responseUpdateConstraint $
--             newUpdateConstraintResponse
--
--         , responseImportAsProvisionedProduct $
--             newImportAsProvisionedProductResponse
--
--         , responseCreateProvisionedProductPlan $
--             newCreateProvisionedProductPlanResponse
--
--         , responseDescribeTagOption $
--             newDescribeTagOptionResponse
--
--         , responseDeleteConstraint $
--             newDeleteConstraintResponse
--
--         , responseListPortfolioAccess $
--             newListPortfolioAccessResponse
--
--         , responseDeleteProvisionedProductPlan $
--             newDeleteProvisionedProductPlanResponse
--
--         , responseAssociateBudgetWithResource $
--             newAssociateBudgetWithResourceResponse
--
--         , responseGetProvisionedProductOutputs $
--             newGetProvisionedProductOutputsResponse
--
--         , responseListResourcesForTagOption $
--             newListResourcesForTagOptionResponse
--
--         , responseListLaunchPaths $
--             newListLaunchPathsResponse
--
--         , responseCreateConstraint $
--             newCreateConstraintResponse
--
--         , responseDescribePortfolioShares $
--             newDescribePortfolioSharesResponse
--
--         , responseDescribeProvisioningParameters $
--             newDescribeProvisioningParametersResponse
--
--         , responseDeleteProvisioningArtifact $
--             newDeleteProvisioningArtifactResponse
--
--         , responseUpdateProvisioningArtifact $
--             newUpdateProvisioningArtifactResponse
--
--         , responseDescribeServiceAction $
--             newDescribeServiceActionResponse
--
--         , responseListServiceActionsForProvisioningArtifact $
--             newListServiceActionsForProvisioningArtifactResponse
--
--         , responseDeletePortfolioShare $
--             newDeletePortfolioShareResponse
--
--         , responseListProvisioningArtifacts $
--             newListProvisioningArtifactsResponse
--
--         , responseDescribePortfolioShareStatus $
--             newDescribePortfolioShareStatusResponse
--
--         , responseDescribeProvisionedProduct $
--             newDescribeProvisionedProductResponse
--
--         , responseUpdatePortfolioShare $
--             newUpdatePortfolioShareResponse
--
--         , responseSearchProvisionedProducts $
--             newSearchProvisionedProductsResponse
--
--         , responseListStackInstancesForProvisionedProduct $
--             newListStackInstancesForProvisionedProductResponse
--
--         , responseDescribeProduct $
--             newDescribeProductResponse
--
--         , responseCreatePortfolioShare $
--             newCreatePortfolioShareResponse
--
--         , responseCreateProvisioningArtifact $
--             newCreateProvisioningArtifactResponse
--
--         , responseSearchProductsAsAdmin $
--             newSearchProductsAsAdminResponse
--
--         , responseGetAWSOrganizationsAccessStatus $
--             newGetAWSOrganizationsAccessStatusResponse
--
--         , responseListPortfolios $
--             newListPortfoliosResponse
--
--         , responseDisassociateBudgetFromResource $
--             newDisassociateBudgetFromResourceResponse
--
--         , responseDeletePortfolio $
--             newDeletePortfolioResponse
--
--         , responseUpdatePortfolio $
--             newUpdatePortfolioResponse
--
--         , responseDescribeConstraint $
--             newDescribeConstraintResponse
--
--         , responseDeleteTagOption $
--             newDeleteTagOptionResponse
--
--         , responseDescribeRecord $
--             newDescribeRecordResponse
--
--         , responseDisassociateServiceActionFromProvisioningArtifact $
--             newDisassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseUpdateTagOption $
--             newUpdateTagOptionResponse
--
--         , responseEnableAWSOrganizationsAccess $
--             newEnableAWSOrganizationsAccessResponse
--
--         , responseCreateTagOption $
--             newCreateTagOptionResponse
--
--         , responseListRecordHistory $
--             newListRecordHistoryResponse
--
--         , responseListConstraintsForPortfolio $
--             newListConstraintsForPortfolioResponse
--
--         , responseAssociateServiceActionWithProvisioningArtifact $
--             newAssociateServiceActionWithProvisioningArtifactResponse
--
--         , responseDeleteServiceAction $
--             newDeleteServiceActionResponse
--
--         , responseUpdateProvisionedProduct $
--             newUpdateProvisionedProductResponse
--
--         , responseDeleteProduct $
--             newDeleteProductResponse
--
--         , responseUpdateServiceAction $
--             newUpdateServiceActionResponse
--
--         , responseDescribeProvisioningArtifact $
--             newDescribeProvisioningArtifactResponse
--
--         , responseUpdateProduct $
--             newUpdateProductResponse
--
--         , responseCreateServiceAction $
--             newCreateServiceActionResponse
--
--         , responseAcceptPortfolioShare $
--             newAcceptPortfolioShareResponse
--
--         , responseDescribeCopyProductStatus $
--             newDescribeCopyProductStatusResponse
--
--         , responseCreateProduct $
--             newCreateProductResponse
--
--         , responseBatchDisassociateServiceActionFromProvisioningArtifact $
--             newBatchDisassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseDisassociatePrincipalFromPortfolio $
--             newDisassociatePrincipalFromPortfolioResponse
--
--         , responseListProvisionedProductPlans $
--             newListProvisionedProductPlansResponse
--
--         , responseSearchProducts $
--             newSearchProductsResponse
--
--         , responseListProvisioningArtifactsForServiceAction $
--             newListProvisioningArtifactsForServiceActionResponse
--
--         , responseDescribeServiceActionExecutionParameters $
--             newDescribeServiceActionExecutionParametersResponse
--
--         , responseBatchAssociateServiceActionWithProvisioningArtifact $
--             newBatchAssociateServiceActionWithProvisioningArtifactResponse
--
--         , responseCopyProduct $
--             newCopyProductResponse
--
--         , responseAssociatePrincipalWithPortfolio $
--             newAssociatePrincipalWithPortfolioResponse
--
--         , responseCreatePortfolio $
--             newCreatePortfolioResponse
--
--         , responseDescribeProductView $
--             newDescribeProductViewResponse
--
--         , responseDescribeProductAsAdmin $
--             newDescribeProductAsAdminResponse
--
--         , responseUpdateProvisionedProductProperties $
--             newUpdateProvisionedProductPropertiesResponse
--
--         , responseListPortfoliosForProduct $
--             newListPortfoliosForProductResponse
--
--         , responseListTagOptions $
--             newListTagOptionsResponse
--
--         , responseRejectPortfolioShare $
--             newRejectPortfolioShareResponse
--
--         , responseAssociateTagOptionWithResource $
--             newAssociateTagOptionWithResourceResponse
--
--         , responseListBudgetsForResource $
--             newListBudgetsForResourceResponse
--
--         , responseDescribeProvisionedProductPlan $
--             newDescribeProvisionedProductPlanResponse
--
--         , responseDisableAWSOrganizationsAccess $
--             newDisableAWSOrganizationsAccessResponse
--
--         , responseDisassociateProductFromPortfolio $
--             newDisassociateProductFromPortfolioResponse
--
--         , responseTerminateProvisionedProduct $
--             newTerminateProvisionedProductResponse
--
--         , responseListPrincipalsForPortfolio $
--             newListPrincipalsForPortfolioResponse
--
--         , responseProvisionProduct $
--             newProvisionProductResponse
--
--         , responseListServiceActions $
--             newListServiceActionsResponse
--
--           ]
--     ]

-- Requests

requestListOrganizationPortfolioAccess :: ListOrganizationPortfolioAccess -> TestTree
requestListOrganizationPortfolioAccess =
  req
    "ListOrganizationPortfolioAccess"
    "fixture/ListOrganizationPortfolioAccess.yaml"

requestListAcceptedPortfolioShares :: ListAcceptedPortfolioShares -> TestTree
requestListAcceptedPortfolioShares =
  req
    "ListAcceptedPortfolioShares"
    "fixture/ListAcceptedPortfolioShares.yaml"

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

requestExecuteProvisionedProductServiceAction :: ExecuteProvisionedProductServiceAction -> TestTree
requestExecuteProvisionedProductServiceAction =
  req
    "ExecuteProvisionedProductServiceAction"
    "fixture/ExecuteProvisionedProductServiceAction.yaml"

requestExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlan -> TestTree
requestExecuteProvisionedProductPlan =
  req
    "ExecuteProvisionedProductPlan"
    "fixture/ExecuteProvisionedProductPlan.yaml"

requestScanProvisionedProducts :: ScanProvisionedProducts -> TestTree
requestScanProvisionedProducts =
  req
    "ScanProvisionedProducts"
    "fixture/ScanProvisionedProducts.yaml"

requestAssociateProductWithPortfolio :: AssociateProductWithPortfolio -> TestTree
requestAssociateProductWithPortfolio =
  req
    "AssociateProductWithPortfolio"
    "fixture/AssociateProductWithPortfolio.yaml"

requestUpdateConstraint :: UpdateConstraint -> TestTree
requestUpdateConstraint =
  req
    "UpdateConstraint"
    "fixture/UpdateConstraint.yaml"

requestImportAsProvisionedProduct :: ImportAsProvisionedProduct -> TestTree
requestImportAsProvisionedProduct =
  req
    "ImportAsProvisionedProduct"
    "fixture/ImportAsProvisionedProduct.yaml"

requestCreateProvisionedProductPlan :: CreateProvisionedProductPlan -> TestTree
requestCreateProvisionedProductPlan =
  req
    "CreateProvisionedProductPlan"
    "fixture/CreateProvisionedProductPlan.yaml"

requestDescribeTagOption :: DescribeTagOption -> TestTree
requestDescribeTagOption =
  req
    "DescribeTagOption"
    "fixture/DescribeTagOption.yaml"

requestDeleteConstraint :: DeleteConstraint -> TestTree
requestDeleteConstraint =
  req
    "DeleteConstraint"
    "fixture/DeleteConstraint.yaml"

requestListPortfolioAccess :: ListPortfolioAccess -> TestTree
requestListPortfolioAccess =
  req
    "ListPortfolioAccess"
    "fixture/ListPortfolioAccess.yaml"

requestDeleteProvisionedProductPlan :: DeleteProvisionedProductPlan -> TestTree
requestDeleteProvisionedProductPlan =
  req
    "DeleteProvisionedProductPlan"
    "fixture/DeleteProvisionedProductPlan.yaml"

requestAssociateBudgetWithResource :: AssociateBudgetWithResource -> TestTree
requestAssociateBudgetWithResource =
  req
    "AssociateBudgetWithResource"
    "fixture/AssociateBudgetWithResource.yaml"

requestGetProvisionedProductOutputs :: GetProvisionedProductOutputs -> TestTree
requestGetProvisionedProductOutputs =
  req
    "GetProvisionedProductOutputs"
    "fixture/GetProvisionedProductOutputs.yaml"

requestListResourcesForTagOption :: ListResourcesForTagOption -> TestTree
requestListResourcesForTagOption =
  req
    "ListResourcesForTagOption"
    "fixture/ListResourcesForTagOption.yaml"

requestListLaunchPaths :: ListLaunchPaths -> TestTree
requestListLaunchPaths =
  req
    "ListLaunchPaths"
    "fixture/ListLaunchPaths.yaml"

requestCreateConstraint :: CreateConstraint -> TestTree
requestCreateConstraint =
  req
    "CreateConstraint"
    "fixture/CreateConstraint.yaml"

requestDescribePortfolioShares :: DescribePortfolioShares -> TestTree
requestDescribePortfolioShares =
  req
    "DescribePortfolioShares"
    "fixture/DescribePortfolioShares.yaml"

requestDescribeProvisioningParameters :: DescribeProvisioningParameters -> TestTree
requestDescribeProvisioningParameters =
  req
    "DescribeProvisioningParameters"
    "fixture/DescribeProvisioningParameters.yaml"

requestDeleteProvisioningArtifact :: DeleteProvisioningArtifact -> TestTree
requestDeleteProvisioningArtifact =
  req
    "DeleteProvisioningArtifact"
    "fixture/DeleteProvisioningArtifact.yaml"

requestUpdateProvisioningArtifact :: UpdateProvisioningArtifact -> TestTree
requestUpdateProvisioningArtifact =
  req
    "UpdateProvisioningArtifact"
    "fixture/UpdateProvisioningArtifact.yaml"

requestDescribeServiceAction :: DescribeServiceAction -> TestTree
requestDescribeServiceAction =
  req
    "DescribeServiceAction"
    "fixture/DescribeServiceAction.yaml"

requestListServiceActionsForProvisioningArtifact :: ListServiceActionsForProvisioningArtifact -> TestTree
requestListServiceActionsForProvisioningArtifact =
  req
    "ListServiceActionsForProvisioningArtifact"
    "fixture/ListServiceActionsForProvisioningArtifact.yaml"

requestDeletePortfolioShare :: DeletePortfolioShare -> TestTree
requestDeletePortfolioShare =
  req
    "DeletePortfolioShare"
    "fixture/DeletePortfolioShare.yaml"

requestListProvisioningArtifacts :: ListProvisioningArtifacts -> TestTree
requestListProvisioningArtifacts =
  req
    "ListProvisioningArtifacts"
    "fixture/ListProvisioningArtifacts.yaml"

requestDescribePortfolioShareStatus :: DescribePortfolioShareStatus -> TestTree
requestDescribePortfolioShareStatus =
  req
    "DescribePortfolioShareStatus"
    "fixture/DescribePortfolioShareStatus.yaml"

requestDescribeProvisionedProduct :: DescribeProvisionedProduct -> TestTree
requestDescribeProvisionedProduct =
  req
    "DescribeProvisionedProduct"
    "fixture/DescribeProvisionedProduct.yaml"

requestUpdatePortfolioShare :: UpdatePortfolioShare -> TestTree
requestUpdatePortfolioShare =
  req
    "UpdatePortfolioShare"
    "fixture/UpdatePortfolioShare.yaml"

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

requestDescribeProduct :: DescribeProduct -> TestTree
requestDescribeProduct =
  req
    "DescribeProduct"
    "fixture/DescribeProduct.yaml"

requestCreatePortfolioShare :: CreatePortfolioShare -> TestTree
requestCreatePortfolioShare =
  req
    "CreatePortfolioShare"
    "fixture/CreatePortfolioShare.yaml"

requestCreateProvisioningArtifact :: CreateProvisioningArtifact -> TestTree
requestCreateProvisioningArtifact =
  req
    "CreateProvisioningArtifact"
    "fixture/CreateProvisioningArtifact.yaml"

requestSearchProductsAsAdmin :: SearchProductsAsAdmin -> TestTree
requestSearchProductsAsAdmin =
  req
    "SearchProductsAsAdmin"
    "fixture/SearchProductsAsAdmin.yaml"

requestGetAWSOrganizationsAccessStatus :: GetAWSOrganizationsAccessStatus -> TestTree
requestGetAWSOrganizationsAccessStatus =
  req
    "GetAWSOrganizationsAccessStatus"
    "fixture/GetAWSOrganizationsAccessStatus.yaml"

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

requestDescribeConstraint :: DescribeConstraint -> TestTree
requestDescribeConstraint =
  req
    "DescribeConstraint"
    "fixture/DescribeConstraint.yaml"

requestDeleteTagOption :: DeleteTagOption -> TestTree
requestDeleteTagOption =
  req
    "DeleteTagOption"
    "fixture/DeleteTagOption.yaml"

requestDescribeRecord :: DescribeRecord -> TestTree
requestDescribeRecord =
  req
    "DescribeRecord"
    "fixture/DescribeRecord.yaml"

requestDisassociateServiceActionFromProvisioningArtifact :: DisassociateServiceActionFromProvisioningArtifact -> TestTree
requestDisassociateServiceActionFromProvisioningArtifact =
  req
    "DisassociateServiceActionFromProvisioningArtifact"
    "fixture/DisassociateServiceActionFromProvisioningArtifact.yaml"

requestUpdateTagOption :: UpdateTagOption -> TestTree
requestUpdateTagOption =
  req
    "UpdateTagOption"
    "fixture/UpdateTagOption.yaml"

requestEnableAWSOrganizationsAccess :: EnableAWSOrganizationsAccess -> TestTree
requestEnableAWSOrganizationsAccess =
  req
    "EnableAWSOrganizationsAccess"
    "fixture/EnableAWSOrganizationsAccess.yaml"

requestCreateTagOption :: CreateTagOption -> TestTree
requestCreateTagOption =
  req
    "CreateTagOption"
    "fixture/CreateTagOption.yaml"

requestListRecordHistory :: ListRecordHistory -> TestTree
requestListRecordHistory =
  req
    "ListRecordHistory"
    "fixture/ListRecordHistory.yaml"

requestListConstraintsForPortfolio :: ListConstraintsForPortfolio -> TestTree
requestListConstraintsForPortfolio =
  req
    "ListConstraintsForPortfolio"
    "fixture/ListConstraintsForPortfolio.yaml"

requestAssociateServiceActionWithProvisioningArtifact :: AssociateServiceActionWithProvisioningArtifact -> TestTree
requestAssociateServiceActionWithProvisioningArtifact =
  req
    "AssociateServiceActionWithProvisioningArtifact"
    "fixture/AssociateServiceActionWithProvisioningArtifact.yaml"

requestDeleteServiceAction :: DeleteServiceAction -> TestTree
requestDeleteServiceAction =
  req
    "DeleteServiceAction"
    "fixture/DeleteServiceAction.yaml"

requestUpdateProvisionedProduct :: UpdateProvisionedProduct -> TestTree
requestUpdateProvisionedProduct =
  req
    "UpdateProvisionedProduct"
    "fixture/UpdateProvisionedProduct.yaml"

requestDeleteProduct :: DeleteProduct -> TestTree
requestDeleteProduct =
  req
    "DeleteProduct"
    "fixture/DeleteProduct.yaml"

requestUpdateServiceAction :: UpdateServiceAction -> TestTree
requestUpdateServiceAction =
  req
    "UpdateServiceAction"
    "fixture/UpdateServiceAction.yaml"

requestDescribeProvisioningArtifact :: DescribeProvisioningArtifact -> TestTree
requestDescribeProvisioningArtifact =
  req
    "DescribeProvisioningArtifact"
    "fixture/DescribeProvisioningArtifact.yaml"

requestUpdateProduct :: UpdateProduct -> TestTree
requestUpdateProduct =
  req
    "UpdateProduct"
    "fixture/UpdateProduct.yaml"

requestCreateServiceAction :: CreateServiceAction -> TestTree
requestCreateServiceAction =
  req
    "CreateServiceAction"
    "fixture/CreateServiceAction.yaml"

requestAcceptPortfolioShare :: AcceptPortfolioShare -> TestTree
requestAcceptPortfolioShare =
  req
    "AcceptPortfolioShare"
    "fixture/AcceptPortfolioShare.yaml"

requestDescribeCopyProductStatus :: DescribeCopyProductStatus -> TestTree
requestDescribeCopyProductStatus =
  req
    "DescribeCopyProductStatus"
    "fixture/DescribeCopyProductStatus.yaml"

requestCreateProduct :: CreateProduct -> TestTree
requestCreateProduct =
  req
    "CreateProduct"
    "fixture/CreateProduct.yaml"

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

requestListProvisionedProductPlans :: ListProvisionedProductPlans -> TestTree
requestListProvisionedProductPlans =
  req
    "ListProvisionedProductPlans"
    "fixture/ListProvisionedProductPlans.yaml"

requestSearchProducts :: SearchProducts -> TestTree
requestSearchProducts =
  req
    "SearchProducts"
    "fixture/SearchProducts.yaml"

requestListProvisioningArtifactsForServiceAction :: ListProvisioningArtifactsForServiceAction -> TestTree
requestListProvisioningArtifactsForServiceAction =
  req
    "ListProvisioningArtifactsForServiceAction"
    "fixture/ListProvisioningArtifactsForServiceAction.yaml"

requestDescribeServiceActionExecutionParameters :: DescribeServiceActionExecutionParameters -> TestTree
requestDescribeServiceActionExecutionParameters =
  req
    "DescribeServiceActionExecutionParameters"
    "fixture/DescribeServiceActionExecutionParameters.yaml"

requestBatchAssociateServiceActionWithProvisioningArtifact :: BatchAssociateServiceActionWithProvisioningArtifact -> TestTree
requestBatchAssociateServiceActionWithProvisioningArtifact =
  req
    "BatchAssociateServiceActionWithProvisioningArtifact"
    "fixture/BatchAssociateServiceActionWithProvisioningArtifact.yaml"

requestCopyProduct :: CopyProduct -> TestTree
requestCopyProduct =
  req
    "CopyProduct"
    "fixture/CopyProduct.yaml"

requestAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolio -> TestTree
requestAssociatePrincipalWithPortfolio =
  req
    "AssociatePrincipalWithPortfolio"
    "fixture/AssociatePrincipalWithPortfolio.yaml"

requestCreatePortfolio :: CreatePortfolio -> TestTree
requestCreatePortfolio =
  req
    "CreatePortfolio"
    "fixture/CreatePortfolio.yaml"

requestDescribeProductView :: DescribeProductView -> TestTree
requestDescribeProductView =
  req
    "DescribeProductView"
    "fixture/DescribeProductView.yaml"

requestDescribeProductAsAdmin :: DescribeProductAsAdmin -> TestTree
requestDescribeProductAsAdmin =
  req
    "DescribeProductAsAdmin"
    "fixture/DescribeProductAsAdmin.yaml"

requestUpdateProvisionedProductProperties :: UpdateProvisionedProductProperties -> TestTree
requestUpdateProvisionedProductProperties =
  req
    "UpdateProvisionedProductProperties"
    "fixture/UpdateProvisionedProductProperties.yaml"

requestListPortfoliosForProduct :: ListPortfoliosForProduct -> TestTree
requestListPortfoliosForProduct =
  req
    "ListPortfoliosForProduct"
    "fixture/ListPortfoliosForProduct.yaml"

requestListTagOptions :: ListTagOptions -> TestTree
requestListTagOptions =
  req
    "ListTagOptions"
    "fixture/ListTagOptions.yaml"

requestRejectPortfolioShare :: RejectPortfolioShare -> TestTree
requestRejectPortfolioShare =
  req
    "RejectPortfolioShare"
    "fixture/RejectPortfolioShare.yaml"

requestAssociateTagOptionWithResource :: AssociateTagOptionWithResource -> TestTree
requestAssociateTagOptionWithResource =
  req
    "AssociateTagOptionWithResource"
    "fixture/AssociateTagOptionWithResource.yaml"

requestListBudgetsForResource :: ListBudgetsForResource -> TestTree
requestListBudgetsForResource =
  req
    "ListBudgetsForResource"
    "fixture/ListBudgetsForResource.yaml"

requestDescribeProvisionedProductPlan :: DescribeProvisionedProductPlan -> TestTree
requestDescribeProvisionedProductPlan =
  req
    "DescribeProvisionedProductPlan"
    "fixture/DescribeProvisionedProductPlan.yaml"

requestDisableAWSOrganizationsAccess :: DisableAWSOrganizationsAccess -> TestTree
requestDisableAWSOrganizationsAccess =
  req
    "DisableAWSOrganizationsAccess"
    "fixture/DisableAWSOrganizationsAccess.yaml"

requestDisassociateProductFromPortfolio :: DisassociateProductFromPortfolio -> TestTree
requestDisassociateProductFromPortfolio =
  req
    "DisassociateProductFromPortfolio"
    "fixture/DisassociateProductFromPortfolio.yaml"

requestTerminateProvisionedProduct :: TerminateProvisionedProduct -> TestTree
requestTerminateProvisionedProduct =
  req
    "TerminateProvisionedProduct"
    "fixture/TerminateProvisionedProduct.yaml"

requestListPrincipalsForPortfolio :: ListPrincipalsForPortfolio -> TestTree
requestListPrincipalsForPortfolio =
  req
    "ListPrincipalsForPortfolio"
    "fixture/ListPrincipalsForPortfolio.yaml"

requestProvisionProduct :: ProvisionProduct -> TestTree
requestProvisionProduct =
  req
    "ProvisionProduct"
    "fixture/ProvisionProduct.yaml"

requestListServiceActions :: ListServiceActions -> TestTree
requestListServiceActions =
  req
    "ListServiceActions"
    "fixture/ListServiceActions.yaml"

-- Responses

responseListOrganizationPortfolioAccess :: ListOrganizationPortfolioAccessResponse -> TestTree
responseListOrganizationPortfolioAccess =
  res
    "ListOrganizationPortfolioAccessResponse"
    "fixture/ListOrganizationPortfolioAccessResponse.proto"
    defaultService
    (Proxy :: Proxy ListOrganizationPortfolioAccess)

responseListAcceptedPortfolioShares :: ListAcceptedPortfolioSharesResponse -> TestTree
responseListAcceptedPortfolioShares =
  res
    "ListAcceptedPortfolioSharesResponse"
    "fixture/ListAcceptedPortfolioSharesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAcceptedPortfolioShares)

responseDisassociateTagOptionFromResource :: DisassociateTagOptionFromResourceResponse -> TestTree
responseDisassociateTagOptionFromResource =
  res
    "DisassociateTagOptionFromResourceResponse"
    "fixture/DisassociateTagOptionFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateTagOptionFromResource)

responseDescribePortfolio :: DescribePortfolioResponse -> TestTree
responseDescribePortfolio =
  res
    "DescribePortfolioResponse"
    "fixture/DescribePortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePortfolio)

responseExecuteProvisionedProductServiceAction :: ExecuteProvisionedProductServiceActionResponse -> TestTree
responseExecuteProvisionedProductServiceAction =
  res
    "ExecuteProvisionedProductServiceActionResponse"
    "fixture/ExecuteProvisionedProductServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteProvisionedProductServiceAction)

responseExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlanResponse -> TestTree
responseExecuteProvisionedProductPlan =
  res
    "ExecuteProvisionedProductPlanResponse"
    "fixture/ExecuteProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteProvisionedProductPlan)

responseScanProvisionedProducts :: ScanProvisionedProductsResponse -> TestTree
responseScanProvisionedProducts =
  res
    "ScanProvisionedProductsResponse"
    "fixture/ScanProvisionedProductsResponse.proto"
    defaultService
    (Proxy :: Proxy ScanProvisionedProducts)

responseAssociateProductWithPortfolio :: AssociateProductWithPortfolioResponse -> TestTree
responseAssociateProductWithPortfolio =
  res
    "AssociateProductWithPortfolioResponse"
    "fixture/AssociateProductWithPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateProductWithPortfolio)

responseUpdateConstraint :: UpdateConstraintResponse -> TestTree
responseUpdateConstraint =
  res
    "UpdateConstraintResponse"
    "fixture/UpdateConstraintResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConstraint)

responseImportAsProvisionedProduct :: ImportAsProvisionedProductResponse -> TestTree
responseImportAsProvisionedProduct =
  res
    "ImportAsProvisionedProductResponse"
    "fixture/ImportAsProvisionedProductResponse.proto"
    defaultService
    (Proxy :: Proxy ImportAsProvisionedProduct)

responseCreateProvisionedProductPlan :: CreateProvisionedProductPlanResponse -> TestTree
responseCreateProvisionedProductPlan =
  res
    "CreateProvisionedProductPlanResponse"
    "fixture/CreateProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProvisionedProductPlan)

responseDescribeTagOption :: DescribeTagOptionResponse -> TestTree
responseDescribeTagOption =
  res
    "DescribeTagOptionResponse"
    "fixture/DescribeTagOptionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTagOption)

responseDeleteConstraint :: DeleteConstraintResponse -> TestTree
responseDeleteConstraint =
  res
    "DeleteConstraintResponse"
    "fixture/DeleteConstraintResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConstraint)

responseListPortfolioAccess :: ListPortfolioAccessResponse -> TestTree
responseListPortfolioAccess =
  res
    "ListPortfolioAccessResponse"
    "fixture/ListPortfolioAccessResponse.proto"
    defaultService
    (Proxy :: Proxy ListPortfolioAccess)

responseDeleteProvisionedProductPlan :: DeleteProvisionedProductPlanResponse -> TestTree
responseDeleteProvisionedProductPlan =
  res
    "DeleteProvisionedProductPlanResponse"
    "fixture/DeleteProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProvisionedProductPlan)

responseAssociateBudgetWithResource :: AssociateBudgetWithResourceResponse -> TestTree
responseAssociateBudgetWithResource =
  res
    "AssociateBudgetWithResourceResponse"
    "fixture/AssociateBudgetWithResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateBudgetWithResource)

responseGetProvisionedProductOutputs :: GetProvisionedProductOutputsResponse -> TestTree
responseGetProvisionedProductOutputs =
  res
    "GetProvisionedProductOutputsResponse"
    "fixture/GetProvisionedProductOutputsResponse.proto"
    defaultService
    (Proxy :: Proxy GetProvisionedProductOutputs)

responseListResourcesForTagOption :: ListResourcesForTagOptionResponse -> TestTree
responseListResourcesForTagOption =
  res
    "ListResourcesForTagOptionResponse"
    "fixture/ListResourcesForTagOptionResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourcesForTagOption)

responseListLaunchPaths :: ListLaunchPathsResponse -> TestTree
responseListLaunchPaths =
  res
    "ListLaunchPathsResponse"
    "fixture/ListLaunchPathsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLaunchPaths)

responseCreateConstraint :: CreateConstraintResponse -> TestTree
responseCreateConstraint =
  res
    "CreateConstraintResponse"
    "fixture/CreateConstraintResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConstraint)

responseDescribePortfolioShares :: DescribePortfolioSharesResponse -> TestTree
responseDescribePortfolioShares =
  res
    "DescribePortfolioSharesResponse"
    "fixture/DescribePortfolioSharesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePortfolioShares)

responseDescribeProvisioningParameters :: DescribeProvisioningParametersResponse -> TestTree
responseDescribeProvisioningParameters =
  res
    "DescribeProvisioningParametersResponse"
    "fixture/DescribeProvisioningParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisioningParameters)

responseDeleteProvisioningArtifact :: DeleteProvisioningArtifactResponse -> TestTree
responseDeleteProvisioningArtifact =
  res
    "DeleteProvisioningArtifactResponse"
    "fixture/DeleteProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProvisioningArtifact)

responseUpdateProvisioningArtifact :: UpdateProvisioningArtifactResponse -> TestTree
responseUpdateProvisioningArtifact =
  res
    "UpdateProvisioningArtifactResponse"
    "fixture/UpdateProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProvisioningArtifact)

responseDescribeServiceAction :: DescribeServiceActionResponse -> TestTree
responseDescribeServiceAction =
  res
    "DescribeServiceActionResponse"
    "fixture/DescribeServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServiceAction)

responseListServiceActionsForProvisioningArtifact :: ListServiceActionsForProvisioningArtifactResponse -> TestTree
responseListServiceActionsForProvisioningArtifact =
  res
    "ListServiceActionsForProvisioningArtifactResponse"
    "fixture/ListServiceActionsForProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy ListServiceActionsForProvisioningArtifact)

responseDeletePortfolioShare :: DeletePortfolioShareResponse -> TestTree
responseDeletePortfolioShare =
  res
    "DeletePortfolioShareResponse"
    "fixture/DeletePortfolioShareResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePortfolioShare)

responseListProvisioningArtifacts :: ListProvisioningArtifactsResponse -> TestTree
responseListProvisioningArtifacts =
  res
    "ListProvisioningArtifactsResponse"
    "fixture/ListProvisioningArtifactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisioningArtifacts)

responseDescribePortfolioShareStatus :: DescribePortfolioShareStatusResponse -> TestTree
responseDescribePortfolioShareStatus =
  res
    "DescribePortfolioShareStatusResponse"
    "fixture/DescribePortfolioShareStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePortfolioShareStatus)

responseDescribeProvisionedProduct :: DescribeProvisionedProductResponse -> TestTree
responseDescribeProvisionedProduct =
  res
    "DescribeProvisionedProductResponse"
    "fixture/DescribeProvisionedProductResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisionedProduct)

responseUpdatePortfolioShare :: UpdatePortfolioShareResponse -> TestTree
responseUpdatePortfolioShare =
  res
    "UpdatePortfolioShareResponse"
    "fixture/UpdatePortfolioShareResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePortfolioShare)

responseSearchProvisionedProducts :: SearchProvisionedProductsResponse -> TestTree
responseSearchProvisionedProducts =
  res
    "SearchProvisionedProductsResponse"
    "fixture/SearchProvisionedProductsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchProvisionedProducts)

responseListStackInstancesForProvisionedProduct :: ListStackInstancesForProvisionedProductResponse -> TestTree
responseListStackInstancesForProvisionedProduct =
  res
    "ListStackInstancesForProvisionedProductResponse"
    "fixture/ListStackInstancesForProvisionedProductResponse.proto"
    defaultService
    (Proxy :: Proxy ListStackInstancesForProvisionedProduct)

responseDescribeProduct :: DescribeProductResponse -> TestTree
responseDescribeProduct =
  res
    "DescribeProductResponse"
    "fixture/DescribeProductResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProduct)

responseCreatePortfolioShare :: CreatePortfolioShareResponse -> TestTree
responseCreatePortfolioShare =
  res
    "CreatePortfolioShareResponse"
    "fixture/CreatePortfolioShareResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePortfolioShare)

responseCreateProvisioningArtifact :: CreateProvisioningArtifactResponse -> TestTree
responseCreateProvisioningArtifact =
  res
    "CreateProvisioningArtifactResponse"
    "fixture/CreateProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProvisioningArtifact)

responseSearchProductsAsAdmin :: SearchProductsAsAdminResponse -> TestTree
responseSearchProductsAsAdmin =
  res
    "SearchProductsAsAdminResponse"
    "fixture/SearchProductsAsAdminResponse.proto"
    defaultService
    (Proxy :: Proxy SearchProductsAsAdmin)

responseGetAWSOrganizationsAccessStatus :: GetAWSOrganizationsAccessStatusResponse -> TestTree
responseGetAWSOrganizationsAccessStatus =
  res
    "GetAWSOrganizationsAccessStatusResponse"
    "fixture/GetAWSOrganizationsAccessStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetAWSOrganizationsAccessStatus)

responseListPortfolios :: ListPortfoliosResponse -> TestTree
responseListPortfolios =
  res
    "ListPortfoliosResponse"
    "fixture/ListPortfoliosResponse.proto"
    defaultService
    (Proxy :: Proxy ListPortfolios)

responseDisassociateBudgetFromResource :: DisassociateBudgetFromResourceResponse -> TestTree
responseDisassociateBudgetFromResource =
  res
    "DisassociateBudgetFromResourceResponse"
    "fixture/DisassociateBudgetFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateBudgetFromResource)

responseDeletePortfolio :: DeletePortfolioResponse -> TestTree
responseDeletePortfolio =
  res
    "DeletePortfolioResponse"
    "fixture/DeletePortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePortfolio)

responseUpdatePortfolio :: UpdatePortfolioResponse -> TestTree
responseUpdatePortfolio =
  res
    "UpdatePortfolioResponse"
    "fixture/UpdatePortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePortfolio)

responseDescribeConstraint :: DescribeConstraintResponse -> TestTree
responseDescribeConstraint =
  res
    "DescribeConstraintResponse"
    "fixture/DescribeConstraintResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConstraint)

responseDeleteTagOption :: DeleteTagOptionResponse -> TestTree
responseDeleteTagOption =
  res
    "DeleteTagOptionResponse"
    "fixture/DeleteTagOptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTagOption)

responseDescribeRecord :: DescribeRecordResponse -> TestTree
responseDescribeRecord =
  res
    "DescribeRecordResponse"
    "fixture/DescribeRecordResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRecord)

responseDisassociateServiceActionFromProvisioningArtifact :: DisassociateServiceActionFromProvisioningArtifactResponse -> TestTree
responseDisassociateServiceActionFromProvisioningArtifact =
  res
    "DisassociateServiceActionFromProvisioningArtifactResponse"
    "fixture/DisassociateServiceActionFromProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateServiceActionFromProvisioningArtifact)

responseUpdateTagOption :: UpdateTagOptionResponse -> TestTree
responseUpdateTagOption =
  res
    "UpdateTagOptionResponse"
    "fixture/UpdateTagOptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTagOption)

responseEnableAWSOrganizationsAccess :: EnableAWSOrganizationsAccessResponse -> TestTree
responseEnableAWSOrganizationsAccess =
  res
    "EnableAWSOrganizationsAccessResponse"
    "fixture/EnableAWSOrganizationsAccessResponse.proto"
    defaultService
    (Proxy :: Proxy EnableAWSOrganizationsAccess)

responseCreateTagOption :: CreateTagOptionResponse -> TestTree
responseCreateTagOption =
  res
    "CreateTagOptionResponse"
    "fixture/CreateTagOptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTagOption)

responseListRecordHistory :: ListRecordHistoryResponse -> TestTree
responseListRecordHistory =
  res
    "ListRecordHistoryResponse"
    "fixture/ListRecordHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListRecordHistory)

responseListConstraintsForPortfolio :: ListConstraintsForPortfolioResponse -> TestTree
responseListConstraintsForPortfolio =
  res
    "ListConstraintsForPortfolioResponse"
    "fixture/ListConstraintsForPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy ListConstraintsForPortfolio)

responseAssociateServiceActionWithProvisioningArtifact :: AssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseAssociateServiceActionWithProvisioningArtifact =
  res
    "AssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/AssociateServiceActionWithProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateServiceActionWithProvisioningArtifact)

responseDeleteServiceAction :: DeleteServiceActionResponse -> TestTree
responseDeleteServiceAction =
  res
    "DeleteServiceActionResponse"
    "fixture/DeleteServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServiceAction)

responseUpdateProvisionedProduct :: UpdateProvisionedProductResponse -> TestTree
responseUpdateProvisionedProduct =
  res
    "UpdateProvisionedProductResponse"
    "fixture/UpdateProvisionedProductResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProvisionedProduct)

responseDeleteProduct :: DeleteProductResponse -> TestTree
responseDeleteProduct =
  res
    "DeleteProductResponse"
    "fixture/DeleteProductResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProduct)

responseUpdateServiceAction :: UpdateServiceActionResponse -> TestTree
responseUpdateServiceAction =
  res
    "UpdateServiceActionResponse"
    "fixture/UpdateServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServiceAction)

responseDescribeProvisioningArtifact :: DescribeProvisioningArtifactResponse -> TestTree
responseDescribeProvisioningArtifact =
  res
    "DescribeProvisioningArtifactResponse"
    "fixture/DescribeProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisioningArtifact)

responseUpdateProduct :: UpdateProductResponse -> TestTree
responseUpdateProduct =
  res
    "UpdateProductResponse"
    "fixture/UpdateProductResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProduct)

responseCreateServiceAction :: CreateServiceActionResponse -> TestTree
responseCreateServiceAction =
  res
    "CreateServiceActionResponse"
    "fixture/CreateServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateServiceAction)

responseAcceptPortfolioShare :: AcceptPortfolioShareResponse -> TestTree
responseAcceptPortfolioShare =
  res
    "AcceptPortfolioShareResponse"
    "fixture/AcceptPortfolioShareResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptPortfolioShare)

responseDescribeCopyProductStatus :: DescribeCopyProductStatusResponse -> TestTree
responseDescribeCopyProductStatus =
  res
    "DescribeCopyProductStatusResponse"
    "fixture/DescribeCopyProductStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCopyProductStatus)

responseCreateProduct :: CreateProductResponse -> TestTree
responseCreateProduct =
  res
    "CreateProductResponse"
    "fixture/CreateProductResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProduct)

responseBatchDisassociateServiceActionFromProvisioningArtifact :: BatchDisassociateServiceActionFromProvisioningArtifactResponse -> TestTree
responseBatchDisassociateServiceActionFromProvisioningArtifact =
  res
    "BatchDisassociateServiceActionFromProvisioningArtifactResponse"
    "fixture/BatchDisassociateServiceActionFromProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDisassociateServiceActionFromProvisioningArtifact)

responseDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolioResponse -> TestTree
responseDisassociatePrincipalFromPortfolio =
  res
    "DisassociatePrincipalFromPortfolioResponse"
    "fixture/DisassociatePrincipalFromPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociatePrincipalFromPortfolio)

responseListProvisionedProductPlans :: ListProvisionedProductPlansResponse -> TestTree
responseListProvisionedProductPlans =
  res
    "ListProvisionedProductPlansResponse"
    "fixture/ListProvisionedProductPlansResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisionedProductPlans)

responseSearchProducts :: SearchProductsResponse -> TestTree
responseSearchProducts =
  res
    "SearchProductsResponse"
    "fixture/SearchProductsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchProducts)

responseListProvisioningArtifactsForServiceAction :: ListProvisioningArtifactsForServiceActionResponse -> TestTree
responseListProvisioningArtifactsForServiceAction =
  res
    "ListProvisioningArtifactsForServiceActionResponse"
    "fixture/ListProvisioningArtifactsForServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisioningArtifactsForServiceAction)

responseDescribeServiceActionExecutionParameters :: DescribeServiceActionExecutionParametersResponse -> TestTree
responseDescribeServiceActionExecutionParameters =
  res
    "DescribeServiceActionExecutionParametersResponse"
    "fixture/DescribeServiceActionExecutionParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServiceActionExecutionParameters)

responseBatchAssociateServiceActionWithProvisioningArtifact :: BatchAssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseBatchAssociateServiceActionWithProvisioningArtifact =
  res
    "BatchAssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/BatchAssociateServiceActionWithProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy BatchAssociateServiceActionWithProvisioningArtifact)

responseCopyProduct :: CopyProductResponse -> TestTree
responseCopyProduct =
  res
    "CopyProductResponse"
    "fixture/CopyProductResponse.proto"
    defaultService
    (Proxy :: Proxy CopyProduct)

responseAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolioResponse -> TestTree
responseAssociatePrincipalWithPortfolio =
  res
    "AssociatePrincipalWithPortfolioResponse"
    "fixture/AssociatePrincipalWithPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy AssociatePrincipalWithPortfolio)

responseCreatePortfolio :: CreatePortfolioResponse -> TestTree
responseCreatePortfolio =
  res
    "CreatePortfolioResponse"
    "fixture/CreatePortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePortfolio)

responseDescribeProductView :: DescribeProductViewResponse -> TestTree
responseDescribeProductView =
  res
    "DescribeProductViewResponse"
    "fixture/DescribeProductViewResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProductView)

responseDescribeProductAsAdmin :: DescribeProductAsAdminResponse -> TestTree
responseDescribeProductAsAdmin =
  res
    "DescribeProductAsAdminResponse"
    "fixture/DescribeProductAsAdminResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProductAsAdmin)

responseUpdateProvisionedProductProperties :: UpdateProvisionedProductPropertiesResponse -> TestTree
responseUpdateProvisionedProductProperties =
  res
    "UpdateProvisionedProductPropertiesResponse"
    "fixture/UpdateProvisionedProductPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProvisionedProductProperties)

responseListPortfoliosForProduct :: ListPortfoliosForProductResponse -> TestTree
responseListPortfoliosForProduct =
  res
    "ListPortfoliosForProductResponse"
    "fixture/ListPortfoliosForProductResponse.proto"
    defaultService
    (Proxy :: Proxy ListPortfoliosForProduct)

responseListTagOptions :: ListTagOptionsResponse -> TestTree
responseListTagOptions =
  res
    "ListTagOptionsResponse"
    "fixture/ListTagOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagOptions)

responseRejectPortfolioShare :: RejectPortfolioShareResponse -> TestTree
responseRejectPortfolioShare =
  res
    "RejectPortfolioShareResponse"
    "fixture/RejectPortfolioShareResponse.proto"
    defaultService
    (Proxy :: Proxy RejectPortfolioShare)

responseAssociateTagOptionWithResource :: AssociateTagOptionWithResourceResponse -> TestTree
responseAssociateTagOptionWithResource =
  res
    "AssociateTagOptionWithResourceResponse"
    "fixture/AssociateTagOptionWithResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTagOptionWithResource)

responseListBudgetsForResource :: ListBudgetsForResourceResponse -> TestTree
responseListBudgetsForResource =
  res
    "ListBudgetsForResourceResponse"
    "fixture/ListBudgetsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListBudgetsForResource)

responseDescribeProvisionedProductPlan :: DescribeProvisionedProductPlanResponse -> TestTree
responseDescribeProvisionedProductPlan =
  res
    "DescribeProvisionedProductPlanResponse"
    "fixture/DescribeProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisionedProductPlan)

responseDisableAWSOrganizationsAccess :: DisableAWSOrganizationsAccessResponse -> TestTree
responseDisableAWSOrganizationsAccess =
  res
    "DisableAWSOrganizationsAccessResponse"
    "fixture/DisableAWSOrganizationsAccessResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAWSOrganizationsAccess)

responseDisassociateProductFromPortfolio :: DisassociateProductFromPortfolioResponse -> TestTree
responseDisassociateProductFromPortfolio =
  res
    "DisassociateProductFromPortfolioResponse"
    "fixture/DisassociateProductFromPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateProductFromPortfolio)

responseTerminateProvisionedProduct :: TerminateProvisionedProductResponse -> TestTree
responseTerminateProvisionedProduct =
  res
    "TerminateProvisionedProductResponse"
    "fixture/TerminateProvisionedProductResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateProvisionedProduct)

responseListPrincipalsForPortfolio :: ListPrincipalsForPortfolioResponse -> TestTree
responseListPrincipalsForPortfolio =
  res
    "ListPrincipalsForPortfolioResponse"
    "fixture/ListPrincipalsForPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy ListPrincipalsForPortfolio)

responseProvisionProduct :: ProvisionProductResponse -> TestTree
responseProvisionProduct =
  res
    "ProvisionProductResponse"
    "fixture/ProvisionProductResponse.proto"
    defaultService
    (Proxy :: Proxy ProvisionProduct)

responseListServiceActions :: ListServiceActionsResponse -> TestTree
responseListServiceActions =
  res
    "ListServiceActionsResponse"
    "fixture/ListServiceActionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListServiceActions)
