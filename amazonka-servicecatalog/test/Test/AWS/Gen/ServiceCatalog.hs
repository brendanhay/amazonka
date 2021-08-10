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
--         [ requestDescribePortfolio $
--             newDescribePortfolio
--
--         , requestListAcceptedPortfolioShares $
--             newListAcceptedPortfolioShares
--
--         , requestDisassociateTagOptionFromResource $
--             newDisassociateTagOptionFromResource
--
--         , requestScanProvisionedProducts $
--             newScanProvisionedProducts
--
--         , requestAssociateProductWithPortfolio $
--             newAssociateProductWithPortfolio
--
--         , requestListOrganizationPortfolioAccess $
--             newListOrganizationPortfolioAccess
--
--         , requestExecuteProvisionedProductPlan $
--             newExecuteProvisionedProductPlan
--
--         , requestExecuteProvisionedProductServiceAction $
--             newExecuteProvisionedProductServiceAction
--
--         , requestImportAsProvisionedProduct $
--             newImportAsProvisionedProduct
--
--         , requestListPortfolioAccess $
--             newListPortfolioAccess
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
--         , requestUpdateConstraint $
--             newUpdateConstraint
--
--         , requestListResourcesForTagOption $
--             newListResourcesForTagOption
--
--         , requestDescribePortfolioShares $
--             newDescribePortfolioShares
--
--         , requestGetProvisionedProductOutputs $
--             newGetProvisionedProductOutputs
--
--         , requestAssociateBudgetWithResource $
--             newAssociateBudgetWithResource
--
--         , requestDeleteProvisionedProductPlan $
--             newDeleteProvisionedProductPlan
--
--         , requestListLaunchPaths $
--             newListLaunchPaths
--
--         , requestCreateConstraint $
--             newCreateConstraint
--
--         , requestDescribePortfolioShareStatus $
--             newDescribePortfolioShareStatus
--
--         , requestDeletePortfolioShare $
--             newDeletePortfolioShare
--
--         , requestDescribeServiceAction $
--             newDescribeServiceAction
--
--         , requestUpdateProvisioningArtifact $
--             newUpdateProvisioningArtifact
--
--         , requestListStackInstancesForProvisionedProduct $
--             newListStackInstancesForProvisionedProduct
--
--         , requestDeleteProvisioningArtifact $
--             newDeleteProvisioningArtifact
--
--         , requestDescribeProvisioningParameters $
--             newDescribeProvisioningParameters
--
--         , requestListProvisioningArtifacts $
--             newListProvisioningArtifacts
--
--         , requestDescribeProvisionedProduct $
--             newDescribeProvisionedProduct
--
--         , requestDescribeProduct $
--             newDescribeProduct
--
--         , requestUpdatePortfolioShare $
--             newUpdatePortfolioShare
--
--         , requestSearchProvisionedProducts $
--             newSearchProvisionedProducts
--
--         , requestListServiceActionsForProvisioningArtifact $
--             newListServiceActionsForProvisioningArtifact
--
--         , requestCreateProvisioningArtifact $
--             newCreateProvisioningArtifact
--
--         , requestDeletePortfolio $
--             newDeletePortfolio
--
--         , requestCreatePortfolioShare $
--             newCreatePortfolioShare
--
--         , requestDisassociateBudgetFromResource $
--             newDisassociateBudgetFromResource
--
--         , requestUpdatePortfolio $
--             newUpdatePortfolio
--
--         , requestListPortfolios $
--             newListPortfolios
--
--         , requestGetAWSOrganizationsAccessStatus $
--             newGetAWSOrganizationsAccessStatus
--
--         , requestSearchProductsAsAdmin $
--             newSearchProductsAsAdmin
--
--         , requestDescribeRecord $
--             newDescribeRecord
--
--         , requestDescribeConstraint $
--             newDescribeConstraint
--
--         , requestEnableAWSOrganizationsAccess $
--             newEnableAWSOrganizationsAccess
--
--         , requestDeleteTagOption $
--             newDeleteTagOption
--
--         , requestDisassociateServiceActionFromProvisioningArtifact $
--             newDisassociateServiceActionFromProvisioningArtifact
--
--         , requestUpdateTagOption $
--             newUpdateTagOption
--
--         , requestListConstraintsForPortfolio $
--             newListConstraintsForPortfolio
--
--         , requestListRecordHistory $
--             newListRecordHistory
--
--         , requestCreateTagOption $
--             newCreateTagOption
--
--         , requestUpdateProduct $
--             newUpdateProduct
--
--         , requestUpdateServiceAction $
--             newUpdateServiceAction
--
--         , requestDescribeProvisioningArtifact $
--             newDescribeProvisioningArtifact
--
--         , requestDeleteServiceAction $
--             newDeleteServiceAction
--
--         , requestAssociateServiceActionWithProvisioningArtifact $
--             newAssociateServiceActionWithProvisioningArtifact
--
--         , requestUpdateProvisionedProduct $
--             newUpdateProvisionedProduct
--
--         , requestDeleteProduct $
--             newDeleteProduct
--
--         , requestDescribeCopyProductStatus $
--             newDescribeCopyProductStatus
--
--         , requestCreateServiceAction $
--             newCreateServiceAction
--
--         , requestCreateProduct $
--             newCreateProduct
--
--         , requestAcceptPortfolioShare $
--             newAcceptPortfolioShare
--
--         , requestDisassociatePrincipalFromPortfolio $
--             newDisassociatePrincipalFromPortfolio
--
--         , requestBatchDisassociateServiceActionFromProvisioningArtifact $
--             newBatchDisassociateServiceActionFromProvisioningArtifact
--
--         , requestListProvisionedProductPlans $
--             newListProvisionedProductPlans
--
--         , requestBatchAssociateServiceActionWithProvisioningArtifact $
--             newBatchAssociateServiceActionWithProvisioningArtifact
--
--         , requestSearchProducts $
--             newSearchProducts
--
--         , requestListProvisioningArtifactsForServiceAction $
--             newListProvisioningArtifactsForServiceAction
--
--         , requestAssociatePrincipalWithPortfolio $
--             newAssociatePrincipalWithPortfolio
--
--         , requestDescribeServiceActionExecutionParameters $
--             newDescribeServiceActionExecutionParameters
--
--         , requestCopyProduct $
--             newCopyProduct
--
--         , requestCreatePortfolio $
--             newCreatePortfolio
--
--         , requestUpdateProvisionedProductProperties $
--             newUpdateProvisionedProductProperties
--
--         , requestDescribeProductView $
--             newDescribeProductView
--
--         , requestDescribeProductAsAdmin $
--             newDescribeProductAsAdmin
--
--         , requestListPortfoliosForProduct $
--             newListPortfoliosForProduct
--
--         , requestRejectPortfolioShare $
--             newRejectPortfolioShare
--
--         , requestListTagOptions $
--             newListTagOptions
--
--         , requestAssociateTagOptionWithResource $
--             newAssociateTagOptionWithResource
--
--         , requestDisableAWSOrganizationsAccess $
--             newDisableAWSOrganizationsAccess
--
--         , requestDescribeProvisionedProductPlan $
--             newDescribeProvisionedProductPlan
--
--         , requestListBudgetsForResource $
--             newListBudgetsForResource
--
--         , requestDisassociateProductFromPortfolio $
--             newDisassociateProductFromPortfolio
--
--         , requestListPrincipalsForPortfolio $
--             newListPrincipalsForPortfolio
--
--         , requestProvisionProduct $
--             newProvisionProduct
--
--         , requestTerminateProvisionedProduct $
--             newTerminateProvisionedProduct
--
--         , requestListServiceActions $
--             newListServiceActions
--
--           ]

--     , testGroup "response"
--         [ responseDescribePortfolio $
--             newDescribePortfolioResponse
--
--         , responseListAcceptedPortfolioShares $
--             newListAcceptedPortfolioSharesResponse
--
--         , responseDisassociateTagOptionFromResource $
--             newDisassociateTagOptionFromResourceResponse
--
--         , responseScanProvisionedProducts $
--             newScanProvisionedProductsResponse
--
--         , responseAssociateProductWithPortfolio $
--             newAssociateProductWithPortfolioResponse
--
--         , responseListOrganizationPortfolioAccess $
--             newListOrganizationPortfolioAccessResponse
--
--         , responseExecuteProvisionedProductPlan $
--             newExecuteProvisionedProductPlanResponse
--
--         , responseExecuteProvisionedProductServiceAction $
--             newExecuteProvisionedProductServiceActionResponse
--
--         , responseImportAsProvisionedProduct $
--             newImportAsProvisionedProductResponse
--
--         , responseListPortfolioAccess $
--             newListPortfolioAccessResponse
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
--         , responseUpdateConstraint $
--             newUpdateConstraintResponse
--
--         , responseListResourcesForTagOption $
--             newListResourcesForTagOptionResponse
--
--         , responseDescribePortfolioShares $
--             newDescribePortfolioSharesResponse
--
--         , responseGetProvisionedProductOutputs $
--             newGetProvisionedProductOutputsResponse
--
--         , responseAssociateBudgetWithResource $
--             newAssociateBudgetWithResourceResponse
--
--         , responseDeleteProvisionedProductPlan $
--             newDeleteProvisionedProductPlanResponse
--
--         , responseListLaunchPaths $
--             newListLaunchPathsResponse
--
--         , responseCreateConstraint $
--             newCreateConstraintResponse
--
--         , responseDescribePortfolioShareStatus $
--             newDescribePortfolioShareStatusResponse
--
--         , responseDeletePortfolioShare $
--             newDeletePortfolioShareResponse
--
--         , responseDescribeServiceAction $
--             newDescribeServiceActionResponse
--
--         , responseUpdateProvisioningArtifact $
--             newUpdateProvisioningArtifactResponse
--
--         , responseListStackInstancesForProvisionedProduct $
--             newListStackInstancesForProvisionedProductResponse
--
--         , responseDeleteProvisioningArtifact $
--             newDeleteProvisioningArtifactResponse
--
--         , responseDescribeProvisioningParameters $
--             newDescribeProvisioningParametersResponse
--
--         , responseListProvisioningArtifacts $
--             newListProvisioningArtifactsResponse
--
--         , responseDescribeProvisionedProduct $
--             newDescribeProvisionedProductResponse
--
--         , responseDescribeProduct $
--             newDescribeProductResponse
--
--         , responseUpdatePortfolioShare $
--             newUpdatePortfolioShareResponse
--
--         , responseSearchProvisionedProducts $
--             newSearchProvisionedProductsResponse
--
--         , responseListServiceActionsForProvisioningArtifact $
--             newListServiceActionsForProvisioningArtifactResponse
--
--         , responseCreateProvisioningArtifact $
--             newCreateProvisioningArtifactResponse
--
--         , responseDeletePortfolio $
--             newDeletePortfolioResponse
--
--         , responseCreatePortfolioShare $
--             newCreatePortfolioShareResponse
--
--         , responseDisassociateBudgetFromResource $
--             newDisassociateBudgetFromResourceResponse
--
--         , responseUpdatePortfolio $
--             newUpdatePortfolioResponse
--
--         , responseListPortfolios $
--             newListPortfoliosResponse
--
--         , responseGetAWSOrganizationsAccessStatus $
--             newGetAWSOrganizationsAccessStatusResponse
--
--         , responseSearchProductsAsAdmin $
--             newSearchProductsAsAdminResponse
--
--         , responseDescribeRecord $
--             newDescribeRecordResponse
--
--         , responseDescribeConstraint $
--             newDescribeConstraintResponse
--
--         , responseEnableAWSOrganizationsAccess $
--             newEnableAWSOrganizationsAccessResponse
--
--         , responseDeleteTagOption $
--             newDeleteTagOptionResponse
--
--         , responseDisassociateServiceActionFromProvisioningArtifact $
--             newDisassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseUpdateTagOption $
--             newUpdateTagOptionResponse
--
--         , responseListConstraintsForPortfolio $
--             newListConstraintsForPortfolioResponse
--
--         , responseListRecordHistory $
--             newListRecordHistoryResponse
--
--         , responseCreateTagOption $
--             newCreateTagOptionResponse
--
--         , responseUpdateProduct $
--             newUpdateProductResponse
--
--         , responseUpdateServiceAction $
--             newUpdateServiceActionResponse
--
--         , responseDescribeProvisioningArtifact $
--             newDescribeProvisioningArtifactResponse
--
--         , responseDeleteServiceAction $
--             newDeleteServiceActionResponse
--
--         , responseAssociateServiceActionWithProvisioningArtifact $
--             newAssociateServiceActionWithProvisioningArtifactResponse
--
--         , responseUpdateProvisionedProduct $
--             newUpdateProvisionedProductResponse
--
--         , responseDeleteProduct $
--             newDeleteProductResponse
--
--         , responseDescribeCopyProductStatus $
--             newDescribeCopyProductStatusResponse
--
--         , responseCreateServiceAction $
--             newCreateServiceActionResponse
--
--         , responseCreateProduct $
--             newCreateProductResponse
--
--         , responseAcceptPortfolioShare $
--             newAcceptPortfolioShareResponse
--
--         , responseDisassociatePrincipalFromPortfolio $
--             newDisassociatePrincipalFromPortfolioResponse
--
--         , responseBatchDisassociateServiceActionFromProvisioningArtifact $
--             newBatchDisassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseListProvisionedProductPlans $
--             newListProvisionedProductPlansResponse
--
--         , responseBatchAssociateServiceActionWithProvisioningArtifact $
--             newBatchAssociateServiceActionWithProvisioningArtifactResponse
--
--         , responseSearchProducts $
--             newSearchProductsResponse
--
--         , responseListProvisioningArtifactsForServiceAction $
--             newListProvisioningArtifactsForServiceActionResponse
--
--         , responseAssociatePrincipalWithPortfolio $
--             newAssociatePrincipalWithPortfolioResponse
--
--         , responseDescribeServiceActionExecutionParameters $
--             newDescribeServiceActionExecutionParametersResponse
--
--         , responseCopyProduct $
--             newCopyProductResponse
--
--         , responseCreatePortfolio $
--             newCreatePortfolioResponse
--
--         , responseUpdateProvisionedProductProperties $
--             newUpdateProvisionedProductPropertiesResponse
--
--         , responseDescribeProductView $
--             newDescribeProductViewResponse
--
--         , responseDescribeProductAsAdmin $
--             newDescribeProductAsAdminResponse
--
--         , responseListPortfoliosForProduct $
--             newListPortfoliosForProductResponse
--
--         , responseRejectPortfolioShare $
--             newRejectPortfolioShareResponse
--
--         , responseListTagOptions $
--             newListTagOptionsResponse
--
--         , responseAssociateTagOptionWithResource $
--             newAssociateTagOptionWithResourceResponse
--
--         , responseDisableAWSOrganizationsAccess $
--             newDisableAWSOrganizationsAccessResponse
--
--         , responseDescribeProvisionedProductPlan $
--             newDescribeProvisionedProductPlanResponse
--
--         , responseListBudgetsForResource $
--             newListBudgetsForResourceResponse
--
--         , responseDisassociateProductFromPortfolio $
--             newDisassociateProductFromPortfolioResponse
--
--         , responseListPrincipalsForPortfolio $
--             newListPrincipalsForPortfolioResponse
--
--         , responseProvisionProduct $
--             newProvisionProductResponse
--
--         , responseTerminateProvisionedProduct $
--             newTerminateProvisionedProductResponse
--
--         , responseListServiceActions $
--             newListServiceActionsResponse
--
--           ]
--     ]

-- Requests

requestDescribePortfolio :: DescribePortfolio -> TestTree
requestDescribePortfolio =
  req
    "DescribePortfolio"
    "fixture/DescribePortfolio.yaml"

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

requestListOrganizationPortfolioAccess :: ListOrganizationPortfolioAccess -> TestTree
requestListOrganizationPortfolioAccess =
  req
    "ListOrganizationPortfolioAccess"
    "fixture/ListOrganizationPortfolioAccess.yaml"

requestExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlan -> TestTree
requestExecuteProvisionedProductPlan =
  req
    "ExecuteProvisionedProductPlan"
    "fixture/ExecuteProvisionedProductPlan.yaml"

requestExecuteProvisionedProductServiceAction :: ExecuteProvisionedProductServiceAction -> TestTree
requestExecuteProvisionedProductServiceAction =
  req
    "ExecuteProvisionedProductServiceAction"
    "fixture/ExecuteProvisionedProductServiceAction.yaml"

requestImportAsProvisionedProduct :: ImportAsProvisionedProduct -> TestTree
requestImportAsProvisionedProduct =
  req
    "ImportAsProvisionedProduct"
    "fixture/ImportAsProvisionedProduct.yaml"

requestListPortfolioAccess :: ListPortfolioAccess -> TestTree
requestListPortfolioAccess =
  req
    "ListPortfolioAccess"
    "fixture/ListPortfolioAccess.yaml"

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

requestUpdateConstraint :: UpdateConstraint -> TestTree
requestUpdateConstraint =
  req
    "UpdateConstraint"
    "fixture/UpdateConstraint.yaml"

requestListResourcesForTagOption :: ListResourcesForTagOption -> TestTree
requestListResourcesForTagOption =
  req
    "ListResourcesForTagOption"
    "fixture/ListResourcesForTagOption.yaml"

requestDescribePortfolioShares :: DescribePortfolioShares -> TestTree
requestDescribePortfolioShares =
  req
    "DescribePortfolioShares"
    "fixture/DescribePortfolioShares.yaml"

requestGetProvisionedProductOutputs :: GetProvisionedProductOutputs -> TestTree
requestGetProvisionedProductOutputs =
  req
    "GetProvisionedProductOutputs"
    "fixture/GetProvisionedProductOutputs.yaml"

requestAssociateBudgetWithResource :: AssociateBudgetWithResource -> TestTree
requestAssociateBudgetWithResource =
  req
    "AssociateBudgetWithResource"
    "fixture/AssociateBudgetWithResource.yaml"

requestDeleteProvisionedProductPlan :: DeleteProvisionedProductPlan -> TestTree
requestDeleteProvisionedProductPlan =
  req
    "DeleteProvisionedProductPlan"
    "fixture/DeleteProvisionedProductPlan.yaml"

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

requestDescribePortfolioShareStatus :: DescribePortfolioShareStatus -> TestTree
requestDescribePortfolioShareStatus =
  req
    "DescribePortfolioShareStatus"
    "fixture/DescribePortfolioShareStatus.yaml"

requestDeletePortfolioShare :: DeletePortfolioShare -> TestTree
requestDeletePortfolioShare =
  req
    "DeletePortfolioShare"
    "fixture/DeletePortfolioShare.yaml"

requestDescribeServiceAction :: DescribeServiceAction -> TestTree
requestDescribeServiceAction =
  req
    "DescribeServiceAction"
    "fixture/DescribeServiceAction.yaml"

requestUpdateProvisioningArtifact :: UpdateProvisioningArtifact -> TestTree
requestUpdateProvisioningArtifact =
  req
    "UpdateProvisioningArtifact"
    "fixture/UpdateProvisioningArtifact.yaml"

requestListStackInstancesForProvisionedProduct :: ListStackInstancesForProvisionedProduct -> TestTree
requestListStackInstancesForProvisionedProduct =
  req
    "ListStackInstancesForProvisionedProduct"
    "fixture/ListStackInstancesForProvisionedProduct.yaml"

requestDeleteProvisioningArtifact :: DeleteProvisioningArtifact -> TestTree
requestDeleteProvisioningArtifact =
  req
    "DeleteProvisioningArtifact"
    "fixture/DeleteProvisioningArtifact.yaml"

requestDescribeProvisioningParameters :: DescribeProvisioningParameters -> TestTree
requestDescribeProvisioningParameters =
  req
    "DescribeProvisioningParameters"
    "fixture/DescribeProvisioningParameters.yaml"

requestListProvisioningArtifacts :: ListProvisioningArtifacts -> TestTree
requestListProvisioningArtifacts =
  req
    "ListProvisioningArtifacts"
    "fixture/ListProvisioningArtifacts.yaml"

requestDescribeProvisionedProduct :: DescribeProvisionedProduct -> TestTree
requestDescribeProvisionedProduct =
  req
    "DescribeProvisionedProduct"
    "fixture/DescribeProvisionedProduct.yaml"

requestDescribeProduct :: DescribeProduct -> TestTree
requestDescribeProduct =
  req
    "DescribeProduct"
    "fixture/DescribeProduct.yaml"

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

requestListServiceActionsForProvisioningArtifact :: ListServiceActionsForProvisioningArtifact -> TestTree
requestListServiceActionsForProvisioningArtifact =
  req
    "ListServiceActionsForProvisioningArtifact"
    "fixture/ListServiceActionsForProvisioningArtifact.yaml"

requestCreateProvisioningArtifact :: CreateProvisioningArtifact -> TestTree
requestCreateProvisioningArtifact =
  req
    "CreateProvisioningArtifact"
    "fixture/CreateProvisioningArtifact.yaml"

requestDeletePortfolio :: DeletePortfolio -> TestTree
requestDeletePortfolio =
  req
    "DeletePortfolio"
    "fixture/DeletePortfolio.yaml"

requestCreatePortfolioShare :: CreatePortfolioShare -> TestTree
requestCreatePortfolioShare =
  req
    "CreatePortfolioShare"
    "fixture/CreatePortfolioShare.yaml"

requestDisassociateBudgetFromResource :: DisassociateBudgetFromResource -> TestTree
requestDisassociateBudgetFromResource =
  req
    "DisassociateBudgetFromResource"
    "fixture/DisassociateBudgetFromResource.yaml"

requestUpdatePortfolio :: UpdatePortfolio -> TestTree
requestUpdatePortfolio =
  req
    "UpdatePortfolio"
    "fixture/UpdatePortfolio.yaml"

requestListPortfolios :: ListPortfolios -> TestTree
requestListPortfolios =
  req
    "ListPortfolios"
    "fixture/ListPortfolios.yaml"

requestGetAWSOrganizationsAccessStatus :: GetAWSOrganizationsAccessStatus -> TestTree
requestGetAWSOrganizationsAccessStatus =
  req
    "GetAWSOrganizationsAccessStatus"
    "fixture/GetAWSOrganizationsAccessStatus.yaml"

requestSearchProductsAsAdmin :: SearchProductsAsAdmin -> TestTree
requestSearchProductsAsAdmin =
  req
    "SearchProductsAsAdmin"
    "fixture/SearchProductsAsAdmin.yaml"

requestDescribeRecord :: DescribeRecord -> TestTree
requestDescribeRecord =
  req
    "DescribeRecord"
    "fixture/DescribeRecord.yaml"

requestDescribeConstraint :: DescribeConstraint -> TestTree
requestDescribeConstraint =
  req
    "DescribeConstraint"
    "fixture/DescribeConstraint.yaml"

requestEnableAWSOrganizationsAccess :: EnableAWSOrganizationsAccess -> TestTree
requestEnableAWSOrganizationsAccess =
  req
    "EnableAWSOrganizationsAccess"
    "fixture/EnableAWSOrganizationsAccess.yaml"

requestDeleteTagOption :: DeleteTagOption -> TestTree
requestDeleteTagOption =
  req
    "DeleteTagOption"
    "fixture/DeleteTagOption.yaml"

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

requestListConstraintsForPortfolio :: ListConstraintsForPortfolio -> TestTree
requestListConstraintsForPortfolio =
  req
    "ListConstraintsForPortfolio"
    "fixture/ListConstraintsForPortfolio.yaml"

requestListRecordHistory :: ListRecordHistory -> TestTree
requestListRecordHistory =
  req
    "ListRecordHistory"
    "fixture/ListRecordHistory.yaml"

requestCreateTagOption :: CreateTagOption -> TestTree
requestCreateTagOption =
  req
    "CreateTagOption"
    "fixture/CreateTagOption.yaml"

requestUpdateProduct :: UpdateProduct -> TestTree
requestUpdateProduct =
  req
    "UpdateProduct"
    "fixture/UpdateProduct.yaml"

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

requestDeleteServiceAction :: DeleteServiceAction -> TestTree
requestDeleteServiceAction =
  req
    "DeleteServiceAction"
    "fixture/DeleteServiceAction.yaml"

requestAssociateServiceActionWithProvisioningArtifact :: AssociateServiceActionWithProvisioningArtifact -> TestTree
requestAssociateServiceActionWithProvisioningArtifact =
  req
    "AssociateServiceActionWithProvisioningArtifact"
    "fixture/AssociateServiceActionWithProvisioningArtifact.yaml"

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

requestCreateProduct :: CreateProduct -> TestTree
requestCreateProduct =
  req
    "CreateProduct"
    "fixture/CreateProduct.yaml"

requestAcceptPortfolioShare :: AcceptPortfolioShare -> TestTree
requestAcceptPortfolioShare =
  req
    "AcceptPortfolioShare"
    "fixture/AcceptPortfolioShare.yaml"

requestDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolio -> TestTree
requestDisassociatePrincipalFromPortfolio =
  req
    "DisassociatePrincipalFromPortfolio"
    "fixture/DisassociatePrincipalFromPortfolio.yaml"

requestBatchDisassociateServiceActionFromProvisioningArtifact :: BatchDisassociateServiceActionFromProvisioningArtifact -> TestTree
requestBatchDisassociateServiceActionFromProvisioningArtifact =
  req
    "BatchDisassociateServiceActionFromProvisioningArtifact"
    "fixture/BatchDisassociateServiceActionFromProvisioningArtifact.yaml"

requestListProvisionedProductPlans :: ListProvisionedProductPlans -> TestTree
requestListProvisionedProductPlans =
  req
    "ListProvisionedProductPlans"
    "fixture/ListProvisionedProductPlans.yaml"

requestBatchAssociateServiceActionWithProvisioningArtifact :: BatchAssociateServiceActionWithProvisioningArtifact -> TestTree
requestBatchAssociateServiceActionWithProvisioningArtifact =
  req
    "BatchAssociateServiceActionWithProvisioningArtifact"
    "fixture/BatchAssociateServiceActionWithProvisioningArtifact.yaml"

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

requestAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolio -> TestTree
requestAssociatePrincipalWithPortfolio =
  req
    "AssociatePrincipalWithPortfolio"
    "fixture/AssociatePrincipalWithPortfolio.yaml"

requestDescribeServiceActionExecutionParameters :: DescribeServiceActionExecutionParameters -> TestTree
requestDescribeServiceActionExecutionParameters =
  req
    "DescribeServiceActionExecutionParameters"
    "fixture/DescribeServiceActionExecutionParameters.yaml"

requestCopyProduct :: CopyProduct -> TestTree
requestCopyProduct =
  req
    "CopyProduct"
    "fixture/CopyProduct.yaml"

requestCreatePortfolio :: CreatePortfolio -> TestTree
requestCreatePortfolio =
  req
    "CreatePortfolio"
    "fixture/CreatePortfolio.yaml"

requestUpdateProvisionedProductProperties :: UpdateProvisionedProductProperties -> TestTree
requestUpdateProvisionedProductProperties =
  req
    "UpdateProvisionedProductProperties"
    "fixture/UpdateProvisionedProductProperties.yaml"

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

requestListPortfoliosForProduct :: ListPortfoliosForProduct -> TestTree
requestListPortfoliosForProduct =
  req
    "ListPortfoliosForProduct"
    "fixture/ListPortfoliosForProduct.yaml"

requestRejectPortfolioShare :: RejectPortfolioShare -> TestTree
requestRejectPortfolioShare =
  req
    "RejectPortfolioShare"
    "fixture/RejectPortfolioShare.yaml"

requestListTagOptions :: ListTagOptions -> TestTree
requestListTagOptions =
  req
    "ListTagOptions"
    "fixture/ListTagOptions.yaml"

requestAssociateTagOptionWithResource :: AssociateTagOptionWithResource -> TestTree
requestAssociateTagOptionWithResource =
  req
    "AssociateTagOptionWithResource"
    "fixture/AssociateTagOptionWithResource.yaml"

requestDisableAWSOrganizationsAccess :: DisableAWSOrganizationsAccess -> TestTree
requestDisableAWSOrganizationsAccess =
  req
    "DisableAWSOrganizationsAccess"
    "fixture/DisableAWSOrganizationsAccess.yaml"

requestDescribeProvisionedProductPlan :: DescribeProvisionedProductPlan -> TestTree
requestDescribeProvisionedProductPlan =
  req
    "DescribeProvisionedProductPlan"
    "fixture/DescribeProvisionedProductPlan.yaml"

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

requestTerminateProvisionedProduct :: TerminateProvisionedProduct -> TestTree
requestTerminateProvisionedProduct =
  req
    "TerminateProvisionedProduct"
    "fixture/TerminateProvisionedProduct.yaml"

requestListServiceActions :: ListServiceActions -> TestTree
requestListServiceActions =
  req
    "ListServiceActions"
    "fixture/ListServiceActions.yaml"

-- Responses

responseDescribePortfolio :: DescribePortfolioResponse -> TestTree
responseDescribePortfolio =
  res
    "DescribePortfolioResponse"
    "fixture/DescribePortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePortfolio)

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

responseListOrganizationPortfolioAccess :: ListOrganizationPortfolioAccessResponse -> TestTree
responseListOrganizationPortfolioAccess =
  res
    "ListOrganizationPortfolioAccessResponse"
    "fixture/ListOrganizationPortfolioAccessResponse.proto"
    defaultService
    (Proxy :: Proxy ListOrganizationPortfolioAccess)

responseExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlanResponse -> TestTree
responseExecuteProvisionedProductPlan =
  res
    "ExecuteProvisionedProductPlanResponse"
    "fixture/ExecuteProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteProvisionedProductPlan)

responseExecuteProvisionedProductServiceAction :: ExecuteProvisionedProductServiceActionResponse -> TestTree
responseExecuteProvisionedProductServiceAction =
  res
    "ExecuteProvisionedProductServiceActionResponse"
    "fixture/ExecuteProvisionedProductServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteProvisionedProductServiceAction)

responseImportAsProvisionedProduct :: ImportAsProvisionedProductResponse -> TestTree
responseImportAsProvisionedProduct =
  res
    "ImportAsProvisionedProductResponse"
    "fixture/ImportAsProvisionedProductResponse.proto"
    defaultService
    (Proxy :: Proxy ImportAsProvisionedProduct)

responseListPortfolioAccess :: ListPortfolioAccessResponse -> TestTree
responseListPortfolioAccess =
  res
    "ListPortfolioAccessResponse"
    "fixture/ListPortfolioAccessResponse.proto"
    defaultService
    (Proxy :: Proxy ListPortfolioAccess)

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

responseUpdateConstraint :: UpdateConstraintResponse -> TestTree
responseUpdateConstraint =
  res
    "UpdateConstraintResponse"
    "fixture/UpdateConstraintResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConstraint)

responseListResourcesForTagOption :: ListResourcesForTagOptionResponse -> TestTree
responseListResourcesForTagOption =
  res
    "ListResourcesForTagOptionResponse"
    "fixture/ListResourcesForTagOptionResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourcesForTagOption)

responseDescribePortfolioShares :: DescribePortfolioSharesResponse -> TestTree
responseDescribePortfolioShares =
  res
    "DescribePortfolioSharesResponse"
    "fixture/DescribePortfolioSharesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePortfolioShares)

responseGetProvisionedProductOutputs :: GetProvisionedProductOutputsResponse -> TestTree
responseGetProvisionedProductOutputs =
  res
    "GetProvisionedProductOutputsResponse"
    "fixture/GetProvisionedProductOutputsResponse.proto"
    defaultService
    (Proxy :: Proxy GetProvisionedProductOutputs)

responseAssociateBudgetWithResource :: AssociateBudgetWithResourceResponse -> TestTree
responseAssociateBudgetWithResource =
  res
    "AssociateBudgetWithResourceResponse"
    "fixture/AssociateBudgetWithResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateBudgetWithResource)

responseDeleteProvisionedProductPlan :: DeleteProvisionedProductPlanResponse -> TestTree
responseDeleteProvisionedProductPlan =
  res
    "DeleteProvisionedProductPlanResponse"
    "fixture/DeleteProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProvisionedProductPlan)

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

responseDescribePortfolioShareStatus :: DescribePortfolioShareStatusResponse -> TestTree
responseDescribePortfolioShareStatus =
  res
    "DescribePortfolioShareStatusResponse"
    "fixture/DescribePortfolioShareStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePortfolioShareStatus)

responseDeletePortfolioShare :: DeletePortfolioShareResponse -> TestTree
responseDeletePortfolioShare =
  res
    "DeletePortfolioShareResponse"
    "fixture/DeletePortfolioShareResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePortfolioShare)

responseDescribeServiceAction :: DescribeServiceActionResponse -> TestTree
responseDescribeServiceAction =
  res
    "DescribeServiceActionResponse"
    "fixture/DescribeServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServiceAction)

responseUpdateProvisioningArtifact :: UpdateProvisioningArtifactResponse -> TestTree
responseUpdateProvisioningArtifact =
  res
    "UpdateProvisioningArtifactResponse"
    "fixture/UpdateProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProvisioningArtifact)

responseListStackInstancesForProvisionedProduct :: ListStackInstancesForProvisionedProductResponse -> TestTree
responseListStackInstancesForProvisionedProduct =
  res
    "ListStackInstancesForProvisionedProductResponse"
    "fixture/ListStackInstancesForProvisionedProductResponse.proto"
    defaultService
    (Proxy :: Proxy ListStackInstancesForProvisionedProduct)

responseDeleteProvisioningArtifact :: DeleteProvisioningArtifactResponse -> TestTree
responseDeleteProvisioningArtifact =
  res
    "DeleteProvisioningArtifactResponse"
    "fixture/DeleteProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProvisioningArtifact)

responseDescribeProvisioningParameters :: DescribeProvisioningParametersResponse -> TestTree
responseDescribeProvisioningParameters =
  res
    "DescribeProvisioningParametersResponse"
    "fixture/DescribeProvisioningParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisioningParameters)

responseListProvisioningArtifacts :: ListProvisioningArtifactsResponse -> TestTree
responseListProvisioningArtifacts =
  res
    "ListProvisioningArtifactsResponse"
    "fixture/ListProvisioningArtifactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisioningArtifacts)

responseDescribeProvisionedProduct :: DescribeProvisionedProductResponse -> TestTree
responseDescribeProvisionedProduct =
  res
    "DescribeProvisionedProductResponse"
    "fixture/DescribeProvisionedProductResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisionedProduct)

responseDescribeProduct :: DescribeProductResponse -> TestTree
responseDescribeProduct =
  res
    "DescribeProductResponse"
    "fixture/DescribeProductResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProduct)

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

responseListServiceActionsForProvisioningArtifact :: ListServiceActionsForProvisioningArtifactResponse -> TestTree
responseListServiceActionsForProvisioningArtifact =
  res
    "ListServiceActionsForProvisioningArtifactResponse"
    "fixture/ListServiceActionsForProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy ListServiceActionsForProvisioningArtifact)

responseCreateProvisioningArtifact :: CreateProvisioningArtifactResponse -> TestTree
responseCreateProvisioningArtifact =
  res
    "CreateProvisioningArtifactResponse"
    "fixture/CreateProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProvisioningArtifact)

responseDeletePortfolio :: DeletePortfolioResponse -> TestTree
responseDeletePortfolio =
  res
    "DeletePortfolioResponse"
    "fixture/DeletePortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePortfolio)

responseCreatePortfolioShare :: CreatePortfolioShareResponse -> TestTree
responseCreatePortfolioShare =
  res
    "CreatePortfolioShareResponse"
    "fixture/CreatePortfolioShareResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePortfolioShare)

responseDisassociateBudgetFromResource :: DisassociateBudgetFromResourceResponse -> TestTree
responseDisassociateBudgetFromResource =
  res
    "DisassociateBudgetFromResourceResponse"
    "fixture/DisassociateBudgetFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateBudgetFromResource)

responseUpdatePortfolio :: UpdatePortfolioResponse -> TestTree
responseUpdatePortfolio =
  res
    "UpdatePortfolioResponse"
    "fixture/UpdatePortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePortfolio)

responseListPortfolios :: ListPortfoliosResponse -> TestTree
responseListPortfolios =
  res
    "ListPortfoliosResponse"
    "fixture/ListPortfoliosResponse.proto"
    defaultService
    (Proxy :: Proxy ListPortfolios)

responseGetAWSOrganizationsAccessStatus :: GetAWSOrganizationsAccessStatusResponse -> TestTree
responseGetAWSOrganizationsAccessStatus =
  res
    "GetAWSOrganizationsAccessStatusResponse"
    "fixture/GetAWSOrganizationsAccessStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetAWSOrganizationsAccessStatus)

responseSearchProductsAsAdmin :: SearchProductsAsAdminResponse -> TestTree
responseSearchProductsAsAdmin =
  res
    "SearchProductsAsAdminResponse"
    "fixture/SearchProductsAsAdminResponse.proto"
    defaultService
    (Proxy :: Proxy SearchProductsAsAdmin)

responseDescribeRecord :: DescribeRecordResponse -> TestTree
responseDescribeRecord =
  res
    "DescribeRecordResponse"
    "fixture/DescribeRecordResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRecord)

responseDescribeConstraint :: DescribeConstraintResponse -> TestTree
responseDescribeConstraint =
  res
    "DescribeConstraintResponse"
    "fixture/DescribeConstraintResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConstraint)

responseEnableAWSOrganizationsAccess :: EnableAWSOrganizationsAccessResponse -> TestTree
responseEnableAWSOrganizationsAccess =
  res
    "EnableAWSOrganizationsAccessResponse"
    "fixture/EnableAWSOrganizationsAccessResponse.proto"
    defaultService
    (Proxy :: Proxy EnableAWSOrganizationsAccess)

responseDeleteTagOption :: DeleteTagOptionResponse -> TestTree
responseDeleteTagOption =
  res
    "DeleteTagOptionResponse"
    "fixture/DeleteTagOptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTagOption)

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

responseListConstraintsForPortfolio :: ListConstraintsForPortfolioResponse -> TestTree
responseListConstraintsForPortfolio =
  res
    "ListConstraintsForPortfolioResponse"
    "fixture/ListConstraintsForPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy ListConstraintsForPortfolio)

responseListRecordHistory :: ListRecordHistoryResponse -> TestTree
responseListRecordHistory =
  res
    "ListRecordHistoryResponse"
    "fixture/ListRecordHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListRecordHistory)

responseCreateTagOption :: CreateTagOptionResponse -> TestTree
responseCreateTagOption =
  res
    "CreateTagOptionResponse"
    "fixture/CreateTagOptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTagOption)

responseUpdateProduct :: UpdateProductResponse -> TestTree
responseUpdateProduct =
  res
    "UpdateProductResponse"
    "fixture/UpdateProductResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProduct)

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

responseDeleteServiceAction :: DeleteServiceActionResponse -> TestTree
responseDeleteServiceAction =
  res
    "DeleteServiceActionResponse"
    "fixture/DeleteServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServiceAction)

responseAssociateServiceActionWithProvisioningArtifact :: AssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseAssociateServiceActionWithProvisioningArtifact =
  res
    "AssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/AssociateServiceActionWithProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateServiceActionWithProvisioningArtifact)

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

responseDescribeCopyProductStatus :: DescribeCopyProductStatusResponse -> TestTree
responseDescribeCopyProductStatus =
  res
    "DescribeCopyProductStatusResponse"
    "fixture/DescribeCopyProductStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCopyProductStatus)

responseCreateServiceAction :: CreateServiceActionResponse -> TestTree
responseCreateServiceAction =
  res
    "CreateServiceActionResponse"
    "fixture/CreateServiceActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateServiceAction)

responseCreateProduct :: CreateProductResponse -> TestTree
responseCreateProduct =
  res
    "CreateProductResponse"
    "fixture/CreateProductResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProduct)

responseAcceptPortfolioShare :: AcceptPortfolioShareResponse -> TestTree
responseAcceptPortfolioShare =
  res
    "AcceptPortfolioShareResponse"
    "fixture/AcceptPortfolioShareResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptPortfolioShare)

responseDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolioResponse -> TestTree
responseDisassociatePrincipalFromPortfolio =
  res
    "DisassociatePrincipalFromPortfolioResponse"
    "fixture/DisassociatePrincipalFromPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociatePrincipalFromPortfolio)

responseBatchDisassociateServiceActionFromProvisioningArtifact :: BatchDisassociateServiceActionFromProvisioningArtifactResponse -> TestTree
responseBatchDisassociateServiceActionFromProvisioningArtifact =
  res
    "BatchDisassociateServiceActionFromProvisioningArtifactResponse"
    "fixture/BatchDisassociateServiceActionFromProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDisassociateServiceActionFromProvisioningArtifact)

responseListProvisionedProductPlans :: ListProvisionedProductPlansResponse -> TestTree
responseListProvisionedProductPlans =
  res
    "ListProvisionedProductPlansResponse"
    "fixture/ListProvisionedProductPlansResponse.proto"
    defaultService
    (Proxy :: Proxy ListProvisionedProductPlans)

responseBatchAssociateServiceActionWithProvisioningArtifact :: BatchAssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseBatchAssociateServiceActionWithProvisioningArtifact =
  res
    "BatchAssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/BatchAssociateServiceActionWithProvisioningArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy BatchAssociateServiceActionWithProvisioningArtifact)

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

responseAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolioResponse -> TestTree
responseAssociatePrincipalWithPortfolio =
  res
    "AssociatePrincipalWithPortfolioResponse"
    "fixture/AssociatePrincipalWithPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy AssociatePrincipalWithPortfolio)

responseDescribeServiceActionExecutionParameters :: DescribeServiceActionExecutionParametersResponse -> TestTree
responseDescribeServiceActionExecutionParameters =
  res
    "DescribeServiceActionExecutionParametersResponse"
    "fixture/DescribeServiceActionExecutionParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServiceActionExecutionParameters)

responseCopyProduct :: CopyProductResponse -> TestTree
responseCopyProduct =
  res
    "CopyProductResponse"
    "fixture/CopyProductResponse.proto"
    defaultService
    (Proxy :: Proxy CopyProduct)

responseCreatePortfolio :: CreatePortfolioResponse -> TestTree
responseCreatePortfolio =
  res
    "CreatePortfolioResponse"
    "fixture/CreatePortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePortfolio)

responseUpdateProvisionedProductProperties :: UpdateProvisionedProductPropertiesResponse -> TestTree
responseUpdateProvisionedProductProperties =
  res
    "UpdateProvisionedProductPropertiesResponse"
    "fixture/UpdateProvisionedProductPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProvisionedProductProperties)

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

responseListPortfoliosForProduct :: ListPortfoliosForProductResponse -> TestTree
responseListPortfoliosForProduct =
  res
    "ListPortfoliosForProductResponse"
    "fixture/ListPortfoliosForProductResponse.proto"
    defaultService
    (Proxy :: Proxy ListPortfoliosForProduct)

responseRejectPortfolioShare :: RejectPortfolioShareResponse -> TestTree
responseRejectPortfolioShare =
  res
    "RejectPortfolioShareResponse"
    "fixture/RejectPortfolioShareResponse.proto"
    defaultService
    (Proxy :: Proxy RejectPortfolioShare)

responseListTagOptions :: ListTagOptionsResponse -> TestTree
responseListTagOptions =
  res
    "ListTagOptionsResponse"
    "fixture/ListTagOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagOptions)

responseAssociateTagOptionWithResource :: AssociateTagOptionWithResourceResponse -> TestTree
responseAssociateTagOptionWithResource =
  res
    "AssociateTagOptionWithResourceResponse"
    "fixture/AssociateTagOptionWithResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTagOptionWithResource)

responseDisableAWSOrganizationsAccess :: DisableAWSOrganizationsAccessResponse -> TestTree
responseDisableAWSOrganizationsAccess =
  res
    "DisableAWSOrganizationsAccessResponse"
    "fixture/DisableAWSOrganizationsAccessResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAWSOrganizationsAccess)

responseDescribeProvisionedProductPlan :: DescribeProvisionedProductPlanResponse -> TestTree
responseDescribeProvisionedProductPlan =
  res
    "DescribeProvisionedProductPlanResponse"
    "fixture/DescribeProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProvisionedProductPlan)

responseListBudgetsForResource :: ListBudgetsForResourceResponse -> TestTree
responseListBudgetsForResource =
  res
    "ListBudgetsForResourceResponse"
    "fixture/ListBudgetsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListBudgetsForResource)

responseDisassociateProductFromPortfolio :: DisassociateProductFromPortfolioResponse -> TestTree
responseDisassociateProductFromPortfolio =
  res
    "DisassociateProductFromPortfolioResponse"
    "fixture/DisassociateProductFromPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateProductFromPortfolio)

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

responseTerminateProvisionedProduct :: TerminateProvisionedProductResponse -> TestTree
responseTerminateProvisionedProduct =
  res
    "TerminateProvisionedProductResponse"
    "fixture/TerminateProvisionedProductResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateProvisionedProduct)

responseListServiceActions :: ListServiceActionsResponse -> TestTree
responseListServiceActions =
  res
    "ListServiceActionsResponse"
    "fixture/ListServiceActionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListServiceActions)
