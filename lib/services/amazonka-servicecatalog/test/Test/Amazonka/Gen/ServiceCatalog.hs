{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ServiceCatalog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ServiceCatalog where

import Amazonka.ServiceCatalog
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.ServiceCatalog.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestImportAsProvisionedProduct $
--             newImportAsProvisionedProduct
--
--         , requestDeleteConstraint $
--             newDeleteConstraint
--
--         , requestUpdateConstraint $
--             newUpdateConstraint
--
--         , requestCreateProvisionedProductPlan $
--             newCreateProvisionedProductPlan
--
--         , requestExecuteProvisionedProductServiceAction $
--             newExecuteProvisionedProductServiceAction
--
--         , requestCreateProduct $
--             newCreateProduct
--
--         , requestDescribeCopyProductStatus $
--             newDescribeCopyProductStatus
--
--         , requestCreateServiceAction $
--             newCreateServiceAction
--
--         , requestTerminateProvisionedProduct $
--             newTerminateProvisionedProduct
--
--         , requestUpdateProvisionedProduct $
--             newUpdateProvisionedProduct
--
--         , requestDescribeProvisioningArtifact $
--             newDescribeProvisioningArtifact
--
--         , requestAssociateServiceActionWithProvisioningArtifact $
--             newAssociateServiceActionWithProvisioningArtifact
--
--         , requestListRecordHistory $
--             newListRecordHistory
--
--         , requestDescribeProvisionedProductPlan $
--             newDescribeProvisionedProductPlan
--
--         , requestAssociateTagOptionWithResource $
--             newAssociateTagOptionWithResource
--
--         , requestCreateTagOption $
--             newCreateTagOption
--
--         , requestListBudgetsForResource $
--             newListBudgetsForResource
--
--         , requestDisassociateProductFromPortfolio $
--             newDisassociateProductFromPortfolio
--
--         , requestListConstraintsForPortfolio $
--             newListConstraintsForPortfolio
--
--         , requestDescribeRecord $
--             newDescribeRecord
--
--         , requestEnableAWSOrganizationsAccess $
--             newEnableAWSOrganizationsAccess
--
--         , requestDescribeConstraint $
--             newDescribeConstraint
--
--         , requestCreateProvisioningArtifact $
--             newCreateProvisioningArtifact
--
--         , requestListPortfolios $
--             newListPortfolios
--
--         , requestDisassociateBudgetFromResource $
--             newDisassociateBudgetFromResource
--
--         , requestDescribeProductView $
--             newDescribeProductView
--
--         , requestCreatePortfolioShare $
--             newCreatePortfolioShare
--
--         , requestListProvisioningArtifacts $
--             newListProvisioningArtifacts
--
--         , requestListServiceActionsForProvisioningArtifact $
--             newListServiceActionsForProvisioningArtifact
--
--         , requestSearchProducts $
--             newSearchProducts
--
--         , requestDescribeServiceActionExecutionParameters $
--             newDescribeServiceActionExecutionParameters
--
--         , requestSearchProvisionedProducts $
--             newSearchProvisionedProducts
--
--         , requestListStackInstancesForProvisionedProduct $
--             newListStackInstancesForProvisionedProduct
--
--         , requestDescribeServiceAction $
--             newDescribeServiceAction
--
--         , requestDescribeProduct $
--             newDescribeProduct
--
--         , requestDeleteProvisionedProductPlan $
--             newDeleteProvisionedProductPlan
--
--         , requestGetProvisionedProductOutputs $
--             newGetProvisionedProductOutputs
--
--         , requestCreateConstraint $
--             newCreateConstraint
--
--         , requestListProvisionedProductPlans $
--             newListProvisionedProductPlans
--
--         , requestListPortfolioAccess $
--             newListPortfolioAccess
--
--         , requestBatchDisassociateServiceActionFromProvisioningArtifact $
--             newBatchDisassociateServiceActionFromProvisioningArtifact
--
--         , requestDisassociatePrincipalFromPortfolio $
--             newDisassociatePrincipalFromPortfolio
--
--         , requestDescribeTagOption $
--             newDescribeTagOption
--
--         , requestDisassociateTagOptionFromResource $
--             newDisassociateTagOptionFromResource
--
--         , requestDescribePortfolio $
--             newDescribePortfolio
--
--         , requestAssociateProductWithPortfolio $
--             newAssociateProductWithPortfolio
--
--         , requestListAcceptedPortfolioShares $
--             newListAcceptedPortfolioShares
--
--         , requestExecuteProvisionedProductPlan $
--             newExecuteProvisionedProductPlan
--
--         , requestAcceptPortfolioShare $
--             newAcceptPortfolioShare
--
--         , requestScanProvisionedProducts $
--             newScanProvisionedProducts
--
--         , requestListOrganizationPortfolioAccess $
--             newListOrganizationPortfolioAccess
--
--         , requestListPrincipalsForPortfolio $
--             newListPrincipalsForPortfolio
--
--         , requestDeleteProduct $
--             newDeleteProduct
--
--         , requestUpdateProduct $
--             newUpdateProduct
--
--         , requestListServiceActions $
--             newListServiceActions
--
--         , requestProvisionProduct $
--             newProvisionProduct
--
--         , requestDeleteServiceAction $
--             newDeleteServiceAction
--
--         , requestUpdateServiceAction $
--             newUpdateServiceAction
--
--         , requestDisableAWSOrganizationsAccess $
--             newDisableAWSOrganizationsAccess
--
--         , requestRejectPortfolioShare $
--             newRejectPortfolioShare
--
--         , requestDisassociateServiceActionFromProvisioningArtifact $
--             newDisassociateServiceActionFromProvisioningArtifact
--
--         , requestDeleteTagOption $
--             newDeleteTagOption
--
--         , requestUpdateTagOption $
--             newUpdateTagOption
--
--         , requestListTagOptions $
--             newListTagOptions
--
--         , requestUpdateProvisionedProductProperties $
--             newUpdateProvisionedProductProperties
--
--         , requestSearchProductsAsAdmin $
--             newSearchProductsAsAdmin
--
--         , requestDeletePortfolio $
--             newDeletePortfolio
--
--         , requestUpdatePortfolio $
--             newUpdatePortfolio
--
--         , requestListPortfoliosForProduct $
--             newListPortfoliosForProduct
--
--         , requestGetAWSOrganizationsAccessStatus $
--             newGetAWSOrganizationsAccessStatus
--
--         , requestDescribeProductAsAdmin $
--             newDescribeProductAsAdmin
--
--         , requestBatchAssociateServiceActionWithProvisioningArtifact $
--             newBatchAssociateServiceActionWithProvisioningArtifact
--
--         , requestDescribeProvisioningParameters $
--             newDescribeProvisioningParameters
--
--         , requestAssociatePrincipalWithPortfolio $
--             newAssociatePrincipalWithPortfolio
--
--         , requestDescribeProvisionedProduct $
--             newDescribeProvisionedProduct
--
--         , requestCopyProduct $
--             newCopyProduct
--
--         , requestDescribePortfolioShareStatus $
--             newDescribePortfolioShareStatus
--
--         , requestUpdateProvisioningArtifact $
--             newUpdateProvisioningArtifact
--
--         , requestDeletePortfolioShare $
--             newDeletePortfolioShare
--
--         , requestDeleteProvisioningArtifact $
--             newDeleteProvisioningArtifact
--
--         , requestUpdatePortfolioShare $
--             newUpdatePortfolioShare
--
--         , requestListProvisioningArtifactsForServiceAction $
--             newListProvisioningArtifactsForServiceAction
--
--         , requestCreatePortfolio $
--             newCreatePortfolio
--
--         , requestListLaunchPaths $
--             newListLaunchPaths
--
--         , requestDescribePortfolioShares $
--             newDescribePortfolioShares
--
--         , requestListResourcesForTagOption $
--             newListResourcesForTagOption
--
--         , requestAssociateBudgetWithResource $
--             newAssociateBudgetWithResource
--
--           ]

--     , testGroup "response"
--         [ responseImportAsProvisionedProduct $
--             newImportAsProvisionedProductResponse
--
--         , responseDeleteConstraint $
--             newDeleteConstraintResponse
--
--         , responseUpdateConstraint $
--             newUpdateConstraintResponse
--
--         , responseCreateProvisionedProductPlan $
--             newCreateProvisionedProductPlanResponse
--
--         , responseExecuteProvisionedProductServiceAction $
--             newExecuteProvisionedProductServiceActionResponse
--
--         , responseCreateProduct $
--             newCreateProductResponse
--
--         , responseDescribeCopyProductStatus $
--             newDescribeCopyProductStatusResponse
--
--         , responseCreateServiceAction $
--             newCreateServiceActionResponse
--
--         , responseTerminateProvisionedProduct $
--             newTerminateProvisionedProductResponse
--
--         , responseUpdateProvisionedProduct $
--             newUpdateProvisionedProductResponse
--
--         , responseDescribeProvisioningArtifact $
--             newDescribeProvisioningArtifactResponse
--
--         , responseAssociateServiceActionWithProvisioningArtifact $
--             newAssociateServiceActionWithProvisioningArtifactResponse
--
--         , responseListRecordHistory $
--             newListRecordHistoryResponse
--
--         , responseDescribeProvisionedProductPlan $
--             newDescribeProvisionedProductPlanResponse
--
--         , responseAssociateTagOptionWithResource $
--             newAssociateTagOptionWithResourceResponse
--
--         , responseCreateTagOption $
--             newCreateTagOptionResponse
--
--         , responseListBudgetsForResource $
--             newListBudgetsForResourceResponse
--
--         , responseDisassociateProductFromPortfolio $
--             newDisassociateProductFromPortfolioResponse
--
--         , responseListConstraintsForPortfolio $
--             newListConstraintsForPortfolioResponse
--
--         , responseDescribeRecord $
--             newDescribeRecordResponse
--
--         , responseEnableAWSOrganizationsAccess $
--             newEnableAWSOrganizationsAccessResponse
--
--         , responseDescribeConstraint $
--             newDescribeConstraintResponse
--
--         , responseCreateProvisioningArtifact $
--             newCreateProvisioningArtifactResponse
--
--         , responseListPortfolios $
--             newListPortfoliosResponse
--
--         , responseDisassociateBudgetFromResource $
--             newDisassociateBudgetFromResourceResponse
--
--         , responseDescribeProductView $
--             newDescribeProductViewResponse
--
--         , responseCreatePortfolioShare $
--             newCreatePortfolioShareResponse
--
--         , responseListProvisioningArtifacts $
--             newListProvisioningArtifactsResponse
--
--         , responseListServiceActionsForProvisioningArtifact $
--             newListServiceActionsForProvisioningArtifactResponse
--
--         , responseSearchProducts $
--             newSearchProductsResponse
--
--         , responseDescribeServiceActionExecutionParameters $
--             newDescribeServiceActionExecutionParametersResponse
--
--         , responseSearchProvisionedProducts $
--             newSearchProvisionedProductsResponse
--
--         , responseListStackInstancesForProvisionedProduct $
--             newListStackInstancesForProvisionedProductResponse
--
--         , responseDescribeServiceAction $
--             newDescribeServiceActionResponse
--
--         , responseDescribeProduct $
--             newDescribeProductResponse
--
--         , responseDeleteProvisionedProductPlan $
--             newDeleteProvisionedProductPlanResponse
--
--         , responseGetProvisionedProductOutputs $
--             newGetProvisionedProductOutputsResponse
--
--         , responseCreateConstraint $
--             newCreateConstraintResponse
--
--         , responseListProvisionedProductPlans $
--             newListProvisionedProductPlansResponse
--
--         , responseListPortfolioAccess $
--             newListPortfolioAccessResponse
--
--         , responseBatchDisassociateServiceActionFromProvisioningArtifact $
--             newBatchDisassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseDisassociatePrincipalFromPortfolio $
--             newDisassociatePrincipalFromPortfolioResponse
--
--         , responseDescribeTagOption $
--             newDescribeTagOptionResponse
--
--         , responseDisassociateTagOptionFromResource $
--             newDisassociateTagOptionFromResourceResponse
--
--         , responseDescribePortfolio $
--             newDescribePortfolioResponse
--
--         , responseAssociateProductWithPortfolio $
--             newAssociateProductWithPortfolioResponse
--
--         , responseListAcceptedPortfolioShares $
--             newListAcceptedPortfolioSharesResponse
--
--         , responseExecuteProvisionedProductPlan $
--             newExecuteProvisionedProductPlanResponse
--
--         , responseAcceptPortfolioShare $
--             newAcceptPortfolioShareResponse
--
--         , responseScanProvisionedProducts $
--             newScanProvisionedProductsResponse
--
--         , responseListOrganizationPortfolioAccess $
--             newListOrganizationPortfolioAccessResponse
--
--         , responseListPrincipalsForPortfolio $
--             newListPrincipalsForPortfolioResponse
--
--         , responseDeleteProduct $
--             newDeleteProductResponse
--
--         , responseUpdateProduct $
--             newUpdateProductResponse
--
--         , responseListServiceActions $
--             newListServiceActionsResponse
--
--         , responseProvisionProduct $
--             newProvisionProductResponse
--
--         , responseDeleteServiceAction $
--             newDeleteServiceActionResponse
--
--         , responseUpdateServiceAction $
--             newUpdateServiceActionResponse
--
--         , responseDisableAWSOrganizationsAccess $
--             newDisableAWSOrganizationsAccessResponse
--
--         , responseRejectPortfolioShare $
--             newRejectPortfolioShareResponse
--
--         , responseDisassociateServiceActionFromProvisioningArtifact $
--             newDisassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseDeleteTagOption $
--             newDeleteTagOptionResponse
--
--         , responseUpdateTagOption $
--             newUpdateTagOptionResponse
--
--         , responseListTagOptions $
--             newListTagOptionsResponse
--
--         , responseUpdateProvisionedProductProperties $
--             newUpdateProvisionedProductPropertiesResponse
--
--         , responseSearchProductsAsAdmin $
--             newSearchProductsAsAdminResponse
--
--         , responseDeletePortfolio $
--             newDeletePortfolioResponse
--
--         , responseUpdatePortfolio $
--             newUpdatePortfolioResponse
--
--         , responseListPortfoliosForProduct $
--             newListPortfoliosForProductResponse
--
--         , responseGetAWSOrganizationsAccessStatus $
--             newGetAWSOrganizationsAccessStatusResponse
--
--         , responseDescribeProductAsAdmin $
--             newDescribeProductAsAdminResponse
--
--         , responseBatchAssociateServiceActionWithProvisioningArtifact $
--             newBatchAssociateServiceActionWithProvisioningArtifactResponse
--
--         , responseDescribeProvisioningParameters $
--             newDescribeProvisioningParametersResponse
--
--         , responseAssociatePrincipalWithPortfolio $
--             newAssociatePrincipalWithPortfolioResponse
--
--         , responseDescribeProvisionedProduct $
--             newDescribeProvisionedProductResponse
--
--         , responseCopyProduct $
--             newCopyProductResponse
--
--         , responseDescribePortfolioShareStatus $
--             newDescribePortfolioShareStatusResponse
--
--         , responseUpdateProvisioningArtifact $
--             newUpdateProvisioningArtifactResponse
--
--         , responseDeletePortfolioShare $
--             newDeletePortfolioShareResponse
--
--         , responseDeleteProvisioningArtifact $
--             newDeleteProvisioningArtifactResponse
--
--         , responseUpdatePortfolioShare $
--             newUpdatePortfolioShareResponse
--
--         , responseListProvisioningArtifactsForServiceAction $
--             newListProvisioningArtifactsForServiceActionResponse
--
--         , responseCreatePortfolio $
--             newCreatePortfolioResponse
--
--         , responseListLaunchPaths $
--             newListLaunchPathsResponse
--
--         , responseDescribePortfolioShares $
--             newDescribePortfolioSharesResponse
--
--         , responseListResourcesForTagOption $
--             newListResourcesForTagOptionResponse
--
--         , responseAssociateBudgetWithResource $
--             newAssociateBudgetWithResourceResponse
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

requestUpdatePortfolioShare :: UpdatePortfolioShare -> TestTree
requestUpdatePortfolioShare =
  req
    "UpdatePortfolioShare"
    "fixture/UpdatePortfolioShare.yaml"

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

requestDescribePortfolioShares :: DescribePortfolioShares -> TestTree
requestDescribePortfolioShares =
  req
    "DescribePortfolioShares"
    "fixture/DescribePortfolioShares.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportAsProvisionedProduct)

responseDeleteConstraint :: DeleteConstraintResponse -> TestTree
responseDeleteConstraint =
  res
    "DeleteConstraintResponse"
    "fixture/DeleteConstraintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConstraint)

responseUpdateConstraint :: UpdateConstraintResponse -> TestTree
responseUpdateConstraint =
  res
    "UpdateConstraintResponse"
    "fixture/UpdateConstraintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConstraint)

responseCreateProvisionedProductPlan :: CreateProvisionedProductPlanResponse -> TestTree
responseCreateProvisionedProductPlan =
  res
    "CreateProvisionedProductPlanResponse"
    "fixture/CreateProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisionedProductPlan)

responseExecuteProvisionedProductServiceAction :: ExecuteProvisionedProductServiceActionResponse -> TestTree
responseExecuteProvisionedProductServiceAction =
  res
    "ExecuteProvisionedProductServiceActionResponse"
    "fixture/ExecuteProvisionedProductServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteProvisionedProductServiceAction)

responseCreateProduct :: CreateProductResponse -> TestTree
responseCreateProduct =
  res
    "CreateProductResponse"
    "fixture/CreateProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProduct)

responseDescribeCopyProductStatus :: DescribeCopyProductStatusResponse -> TestTree
responseDescribeCopyProductStatus =
  res
    "DescribeCopyProductStatusResponse"
    "fixture/DescribeCopyProductStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCopyProductStatus)

responseCreateServiceAction :: CreateServiceActionResponse -> TestTree
responseCreateServiceAction =
  res
    "CreateServiceActionResponse"
    "fixture/CreateServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceAction)

responseTerminateProvisionedProduct :: TerminateProvisionedProductResponse -> TestTree
responseTerminateProvisionedProduct =
  res
    "TerminateProvisionedProductResponse"
    "fixture/TerminateProvisionedProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateProvisionedProduct)

responseUpdateProvisionedProduct :: UpdateProvisionedProductResponse -> TestTree
responseUpdateProvisionedProduct =
  res
    "UpdateProvisionedProductResponse"
    "fixture/UpdateProvisionedProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProvisionedProduct)

responseDescribeProvisioningArtifact :: DescribeProvisioningArtifactResponse -> TestTree
responseDescribeProvisioningArtifact =
  res
    "DescribeProvisioningArtifactResponse"
    "fixture/DescribeProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisioningArtifact)

responseAssociateServiceActionWithProvisioningArtifact :: AssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseAssociateServiceActionWithProvisioningArtifact =
  res
    "AssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/AssociateServiceActionWithProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateServiceActionWithProvisioningArtifact)

responseListRecordHistory :: ListRecordHistoryResponse -> TestTree
responseListRecordHistory =
  res
    "ListRecordHistoryResponse"
    "fixture/ListRecordHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecordHistory)

responseDescribeProvisionedProductPlan :: DescribeProvisionedProductPlanResponse -> TestTree
responseDescribeProvisionedProductPlan =
  res
    "DescribeProvisionedProductPlanResponse"
    "fixture/DescribeProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisionedProductPlan)

responseAssociateTagOptionWithResource :: AssociateTagOptionWithResourceResponse -> TestTree
responseAssociateTagOptionWithResource =
  res
    "AssociateTagOptionWithResourceResponse"
    "fixture/AssociateTagOptionWithResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTagOptionWithResource)

responseCreateTagOption :: CreateTagOptionResponse -> TestTree
responseCreateTagOption =
  res
    "CreateTagOptionResponse"
    "fixture/CreateTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTagOption)

responseListBudgetsForResource :: ListBudgetsForResourceResponse -> TestTree
responseListBudgetsForResource =
  res
    "ListBudgetsForResourceResponse"
    "fixture/ListBudgetsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBudgetsForResource)

responseDisassociateProductFromPortfolio :: DisassociateProductFromPortfolioResponse -> TestTree
responseDisassociateProductFromPortfolio =
  res
    "DisassociateProductFromPortfolioResponse"
    "fixture/DisassociateProductFromPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateProductFromPortfolio)

responseListConstraintsForPortfolio :: ListConstraintsForPortfolioResponse -> TestTree
responseListConstraintsForPortfolio =
  res
    "ListConstraintsForPortfolioResponse"
    "fixture/ListConstraintsForPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConstraintsForPortfolio)

responseDescribeRecord :: DescribeRecordResponse -> TestTree
responseDescribeRecord =
  res
    "DescribeRecordResponse"
    "fixture/DescribeRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecord)

responseEnableAWSOrganizationsAccess :: EnableAWSOrganizationsAccessResponse -> TestTree
responseEnableAWSOrganizationsAccess =
  res
    "EnableAWSOrganizationsAccessResponse"
    "fixture/EnableAWSOrganizationsAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAWSOrganizationsAccess)

responseDescribeConstraint :: DescribeConstraintResponse -> TestTree
responseDescribeConstraint =
  res
    "DescribeConstraintResponse"
    "fixture/DescribeConstraintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConstraint)

responseCreateProvisioningArtifact :: CreateProvisioningArtifactResponse -> TestTree
responseCreateProvisioningArtifact =
  res
    "CreateProvisioningArtifactResponse"
    "fixture/CreateProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisioningArtifact)

responseListPortfolios :: ListPortfoliosResponse -> TestTree
responseListPortfolios =
  res
    "ListPortfoliosResponse"
    "fixture/ListPortfoliosResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPortfolios)

responseDisassociateBudgetFromResource :: DisassociateBudgetFromResourceResponse -> TestTree
responseDisassociateBudgetFromResource =
  res
    "DisassociateBudgetFromResourceResponse"
    "fixture/DisassociateBudgetFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateBudgetFromResource)

responseDescribeProductView :: DescribeProductViewResponse -> TestTree
responseDescribeProductView =
  res
    "DescribeProductViewResponse"
    "fixture/DescribeProductViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProductView)

responseCreatePortfolioShare :: CreatePortfolioShareResponse -> TestTree
responseCreatePortfolioShare =
  res
    "CreatePortfolioShareResponse"
    "fixture/CreatePortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePortfolioShare)

responseListProvisioningArtifacts :: ListProvisioningArtifactsResponse -> TestTree
responseListProvisioningArtifacts =
  res
    "ListProvisioningArtifactsResponse"
    "fixture/ListProvisioningArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisioningArtifacts)

responseListServiceActionsForProvisioningArtifact :: ListServiceActionsForProvisioningArtifactResponse -> TestTree
responseListServiceActionsForProvisioningArtifact =
  res
    "ListServiceActionsForProvisioningArtifactResponse"
    "fixture/ListServiceActionsForProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceActionsForProvisioningArtifact)

responseSearchProducts :: SearchProductsResponse -> TestTree
responseSearchProducts =
  res
    "SearchProductsResponse"
    "fixture/SearchProductsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchProducts)

responseDescribeServiceActionExecutionParameters :: DescribeServiceActionExecutionParametersResponse -> TestTree
responseDescribeServiceActionExecutionParameters =
  res
    "DescribeServiceActionExecutionParametersResponse"
    "fixture/DescribeServiceActionExecutionParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceActionExecutionParameters)

responseSearchProvisionedProducts :: SearchProvisionedProductsResponse -> TestTree
responseSearchProvisionedProducts =
  res
    "SearchProvisionedProductsResponse"
    "fixture/SearchProvisionedProductsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchProvisionedProducts)

responseListStackInstancesForProvisionedProduct :: ListStackInstancesForProvisionedProductResponse -> TestTree
responseListStackInstancesForProvisionedProduct =
  res
    "ListStackInstancesForProvisionedProductResponse"
    "fixture/ListStackInstancesForProvisionedProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackInstancesForProvisionedProduct)

responseDescribeServiceAction :: DescribeServiceActionResponse -> TestTree
responseDescribeServiceAction =
  res
    "DescribeServiceActionResponse"
    "fixture/DescribeServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceAction)

responseDescribeProduct :: DescribeProductResponse -> TestTree
responseDescribeProduct =
  res
    "DescribeProductResponse"
    "fixture/DescribeProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProduct)

responseDeleteProvisionedProductPlan :: DeleteProvisionedProductPlanResponse -> TestTree
responseDeleteProvisionedProductPlan =
  res
    "DeleteProvisionedProductPlanResponse"
    "fixture/DeleteProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisionedProductPlan)

responseGetProvisionedProductOutputs :: GetProvisionedProductOutputsResponse -> TestTree
responseGetProvisionedProductOutputs =
  res
    "GetProvisionedProductOutputsResponse"
    "fixture/GetProvisionedProductOutputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProvisionedProductOutputs)

responseCreateConstraint :: CreateConstraintResponse -> TestTree
responseCreateConstraint =
  res
    "CreateConstraintResponse"
    "fixture/CreateConstraintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConstraint)

responseListProvisionedProductPlans :: ListProvisionedProductPlansResponse -> TestTree
responseListProvisionedProductPlans =
  res
    "ListProvisionedProductPlansResponse"
    "fixture/ListProvisionedProductPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisionedProductPlans)

responseListPortfolioAccess :: ListPortfolioAccessResponse -> TestTree
responseListPortfolioAccess =
  res
    "ListPortfolioAccessResponse"
    "fixture/ListPortfolioAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPortfolioAccess)

responseBatchDisassociateServiceActionFromProvisioningArtifact :: BatchDisassociateServiceActionFromProvisioningArtifactResponse -> TestTree
responseBatchDisassociateServiceActionFromProvisioningArtifact =
  res
    "BatchDisassociateServiceActionFromProvisioningArtifactResponse"
    "fixture/BatchDisassociateServiceActionFromProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateServiceActionFromProvisioningArtifact)

responseDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolioResponse -> TestTree
responseDisassociatePrincipalFromPortfolio =
  res
    "DisassociatePrincipalFromPortfolioResponse"
    "fixture/DisassociatePrincipalFromPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePrincipalFromPortfolio)

responseDescribeTagOption :: DescribeTagOptionResponse -> TestTree
responseDescribeTagOption =
  res
    "DescribeTagOptionResponse"
    "fixture/DescribeTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTagOption)

responseDisassociateTagOptionFromResource :: DisassociateTagOptionFromResourceResponse -> TestTree
responseDisassociateTagOptionFromResource =
  res
    "DisassociateTagOptionFromResourceResponse"
    "fixture/DisassociateTagOptionFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTagOptionFromResource)

responseDescribePortfolio :: DescribePortfolioResponse -> TestTree
responseDescribePortfolio =
  res
    "DescribePortfolioResponse"
    "fixture/DescribePortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePortfolio)

responseAssociateProductWithPortfolio :: AssociateProductWithPortfolioResponse -> TestTree
responseAssociateProductWithPortfolio =
  res
    "AssociateProductWithPortfolioResponse"
    "fixture/AssociateProductWithPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateProductWithPortfolio)

responseListAcceptedPortfolioShares :: ListAcceptedPortfolioSharesResponse -> TestTree
responseListAcceptedPortfolioShares =
  res
    "ListAcceptedPortfolioSharesResponse"
    "fixture/ListAcceptedPortfolioSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAcceptedPortfolioShares)

responseExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlanResponse -> TestTree
responseExecuteProvisionedProductPlan =
  res
    "ExecuteProvisionedProductPlanResponse"
    "fixture/ExecuteProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteProvisionedProductPlan)

responseAcceptPortfolioShare :: AcceptPortfolioShareResponse -> TestTree
responseAcceptPortfolioShare =
  res
    "AcceptPortfolioShareResponse"
    "fixture/AcceptPortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptPortfolioShare)

responseScanProvisionedProducts :: ScanProvisionedProductsResponse -> TestTree
responseScanProvisionedProducts =
  res
    "ScanProvisionedProductsResponse"
    "fixture/ScanProvisionedProductsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ScanProvisionedProducts)

responseListOrganizationPortfolioAccess :: ListOrganizationPortfolioAccessResponse -> TestTree
responseListOrganizationPortfolioAccess =
  res
    "ListOrganizationPortfolioAccessResponse"
    "fixture/ListOrganizationPortfolioAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationPortfolioAccess)

responseListPrincipalsForPortfolio :: ListPrincipalsForPortfolioResponse -> TestTree
responseListPrincipalsForPortfolio =
  res
    "ListPrincipalsForPortfolioResponse"
    "fixture/ListPrincipalsForPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrincipalsForPortfolio)

responseDeleteProduct :: DeleteProductResponse -> TestTree
responseDeleteProduct =
  res
    "DeleteProductResponse"
    "fixture/DeleteProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProduct)

responseUpdateProduct :: UpdateProductResponse -> TestTree
responseUpdateProduct =
  res
    "UpdateProductResponse"
    "fixture/UpdateProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProduct)

responseListServiceActions :: ListServiceActionsResponse -> TestTree
responseListServiceActions =
  res
    "ListServiceActionsResponse"
    "fixture/ListServiceActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceActions)

responseProvisionProduct :: ProvisionProductResponse -> TestTree
responseProvisionProduct =
  res
    "ProvisionProductResponse"
    "fixture/ProvisionProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionProduct)

responseDeleteServiceAction :: DeleteServiceActionResponse -> TestTree
responseDeleteServiceAction =
  res
    "DeleteServiceActionResponse"
    "fixture/DeleteServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceAction)

responseUpdateServiceAction :: UpdateServiceActionResponse -> TestTree
responseUpdateServiceAction =
  res
    "UpdateServiceActionResponse"
    "fixture/UpdateServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceAction)

responseDisableAWSOrganizationsAccess :: DisableAWSOrganizationsAccessResponse -> TestTree
responseDisableAWSOrganizationsAccess =
  res
    "DisableAWSOrganizationsAccessResponse"
    "fixture/DisableAWSOrganizationsAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAWSOrganizationsAccess)

responseRejectPortfolioShare :: RejectPortfolioShareResponse -> TestTree
responseRejectPortfolioShare =
  res
    "RejectPortfolioShareResponse"
    "fixture/RejectPortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectPortfolioShare)

responseDisassociateServiceActionFromProvisioningArtifact :: DisassociateServiceActionFromProvisioningArtifactResponse -> TestTree
responseDisassociateServiceActionFromProvisioningArtifact =
  res
    "DisassociateServiceActionFromProvisioningArtifactResponse"
    "fixture/DisassociateServiceActionFromProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateServiceActionFromProvisioningArtifact)

responseDeleteTagOption :: DeleteTagOptionResponse -> TestTree
responseDeleteTagOption =
  res
    "DeleteTagOptionResponse"
    "fixture/DeleteTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTagOption)

responseUpdateTagOption :: UpdateTagOptionResponse -> TestTree
responseUpdateTagOption =
  res
    "UpdateTagOptionResponse"
    "fixture/UpdateTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTagOption)

responseListTagOptions :: ListTagOptionsResponse -> TestTree
responseListTagOptions =
  res
    "ListTagOptionsResponse"
    "fixture/ListTagOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagOptions)

responseUpdateProvisionedProductProperties :: UpdateProvisionedProductPropertiesResponse -> TestTree
responseUpdateProvisionedProductProperties =
  res
    "UpdateProvisionedProductPropertiesResponse"
    "fixture/UpdateProvisionedProductPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProvisionedProductProperties)

responseSearchProductsAsAdmin :: SearchProductsAsAdminResponse -> TestTree
responseSearchProductsAsAdmin =
  res
    "SearchProductsAsAdminResponse"
    "fixture/SearchProductsAsAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchProductsAsAdmin)

responseDeletePortfolio :: DeletePortfolioResponse -> TestTree
responseDeletePortfolio =
  res
    "DeletePortfolioResponse"
    "fixture/DeletePortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePortfolio)

responseUpdatePortfolio :: UpdatePortfolioResponse -> TestTree
responseUpdatePortfolio =
  res
    "UpdatePortfolioResponse"
    "fixture/UpdatePortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePortfolio)

responseListPortfoliosForProduct :: ListPortfoliosForProductResponse -> TestTree
responseListPortfoliosForProduct =
  res
    "ListPortfoliosForProductResponse"
    "fixture/ListPortfoliosForProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPortfoliosForProduct)

responseGetAWSOrganizationsAccessStatus :: GetAWSOrganizationsAccessStatusResponse -> TestTree
responseGetAWSOrganizationsAccessStatus =
  res
    "GetAWSOrganizationsAccessStatusResponse"
    "fixture/GetAWSOrganizationsAccessStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAWSOrganizationsAccessStatus)

responseDescribeProductAsAdmin :: DescribeProductAsAdminResponse -> TestTree
responseDescribeProductAsAdmin =
  res
    "DescribeProductAsAdminResponse"
    "fixture/DescribeProductAsAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProductAsAdmin)

responseBatchAssociateServiceActionWithProvisioningArtifact :: BatchAssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseBatchAssociateServiceActionWithProvisioningArtifact =
  res
    "BatchAssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/BatchAssociateServiceActionWithProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateServiceActionWithProvisioningArtifact)

responseDescribeProvisioningParameters :: DescribeProvisioningParametersResponse -> TestTree
responseDescribeProvisioningParameters =
  res
    "DescribeProvisioningParametersResponse"
    "fixture/DescribeProvisioningParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisioningParameters)

responseAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolioResponse -> TestTree
responseAssociatePrincipalWithPortfolio =
  res
    "AssociatePrincipalWithPortfolioResponse"
    "fixture/AssociatePrincipalWithPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePrincipalWithPortfolio)

responseDescribeProvisionedProduct :: DescribeProvisionedProductResponse -> TestTree
responseDescribeProvisionedProduct =
  res
    "DescribeProvisionedProductResponse"
    "fixture/DescribeProvisionedProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisionedProduct)

responseCopyProduct :: CopyProductResponse -> TestTree
responseCopyProduct =
  res
    "CopyProductResponse"
    "fixture/CopyProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyProduct)

responseDescribePortfolioShareStatus :: DescribePortfolioShareStatusResponse -> TestTree
responseDescribePortfolioShareStatus =
  res
    "DescribePortfolioShareStatusResponse"
    "fixture/DescribePortfolioShareStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePortfolioShareStatus)

responseUpdateProvisioningArtifact :: UpdateProvisioningArtifactResponse -> TestTree
responseUpdateProvisioningArtifact =
  res
    "UpdateProvisioningArtifactResponse"
    "fixture/UpdateProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProvisioningArtifact)

responseDeletePortfolioShare :: DeletePortfolioShareResponse -> TestTree
responseDeletePortfolioShare =
  res
    "DeletePortfolioShareResponse"
    "fixture/DeletePortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePortfolioShare)

responseDeleteProvisioningArtifact :: DeleteProvisioningArtifactResponse -> TestTree
responseDeleteProvisioningArtifact =
  res
    "DeleteProvisioningArtifactResponse"
    "fixture/DeleteProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisioningArtifact)

responseUpdatePortfolioShare :: UpdatePortfolioShareResponse -> TestTree
responseUpdatePortfolioShare =
  res
    "UpdatePortfolioShareResponse"
    "fixture/UpdatePortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePortfolioShare)

responseListProvisioningArtifactsForServiceAction :: ListProvisioningArtifactsForServiceActionResponse -> TestTree
responseListProvisioningArtifactsForServiceAction =
  res
    "ListProvisioningArtifactsForServiceActionResponse"
    "fixture/ListProvisioningArtifactsForServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisioningArtifactsForServiceAction)

responseCreatePortfolio :: CreatePortfolioResponse -> TestTree
responseCreatePortfolio =
  res
    "CreatePortfolioResponse"
    "fixture/CreatePortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePortfolio)

responseListLaunchPaths :: ListLaunchPathsResponse -> TestTree
responseListLaunchPaths =
  res
    "ListLaunchPathsResponse"
    "fixture/ListLaunchPathsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLaunchPaths)

responseDescribePortfolioShares :: DescribePortfolioSharesResponse -> TestTree
responseDescribePortfolioShares =
  res
    "DescribePortfolioSharesResponse"
    "fixture/DescribePortfolioSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePortfolioShares)

responseListResourcesForTagOption :: ListResourcesForTagOptionResponse -> TestTree
responseListResourcesForTagOption =
  res
    "ListResourcesForTagOptionResponse"
    "fixture/ListResourcesForTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourcesForTagOption)

responseAssociateBudgetWithResource :: AssociateBudgetWithResourceResponse -> TestTree
responseAssociateBudgetWithResource =
  res
    "AssociateBudgetWithResourceResponse"
    "fixture/AssociateBudgetWithResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateBudgetWithResource)
