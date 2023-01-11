{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ServiceCatalog
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestAcceptPortfolioShare $
--             newAcceptPortfolioShare
--
--         , requestAssociateBudgetWithResource $
--             newAssociateBudgetWithResource
--
--         , requestAssociatePrincipalWithPortfolio $
--             newAssociatePrincipalWithPortfolio
--
--         , requestAssociateProductWithPortfolio $
--             newAssociateProductWithPortfolio
--
--         , requestAssociateServiceActionWithProvisioningArtifact $
--             newAssociateServiceActionWithProvisioningArtifact
--
--         , requestAssociateTagOptionWithResource $
--             newAssociateTagOptionWithResource
--
--         , requestBatchAssociateServiceActionWithProvisioningArtifact $
--             newBatchAssociateServiceActionWithProvisioningArtifact
--
--         , requestBatchDisassociateServiceActionFromProvisioningArtifact $
--             newBatchDisassociateServiceActionFromProvisioningArtifact
--
--         , requestCopyProduct $
--             newCopyProduct
--
--         , requestCreateConstraint $
--             newCreateConstraint
--
--         , requestCreatePortfolio $
--             newCreatePortfolio
--
--         , requestCreatePortfolioShare $
--             newCreatePortfolioShare
--
--         , requestCreateProduct $
--             newCreateProduct
--
--         , requestCreateProvisionedProductPlan $
--             newCreateProvisionedProductPlan
--
--         , requestCreateProvisioningArtifact $
--             newCreateProvisioningArtifact
--
--         , requestCreateServiceAction $
--             newCreateServiceAction
--
--         , requestCreateTagOption $
--             newCreateTagOption
--
--         , requestDeleteConstraint $
--             newDeleteConstraint
--
--         , requestDeletePortfolio $
--             newDeletePortfolio
--
--         , requestDeletePortfolioShare $
--             newDeletePortfolioShare
--
--         , requestDeleteProduct $
--             newDeleteProduct
--
--         , requestDeleteProvisionedProductPlan $
--             newDeleteProvisionedProductPlan
--
--         , requestDeleteProvisioningArtifact $
--             newDeleteProvisioningArtifact
--
--         , requestDeleteServiceAction $
--             newDeleteServiceAction
--
--         , requestDeleteTagOption $
--             newDeleteTagOption
--
--         , requestDescribeConstraint $
--             newDescribeConstraint
--
--         , requestDescribeCopyProductStatus $
--             newDescribeCopyProductStatus
--
--         , requestDescribePortfolio $
--             newDescribePortfolio
--
--         , requestDescribePortfolioShareStatus $
--             newDescribePortfolioShareStatus
--
--         , requestDescribePortfolioShares $
--             newDescribePortfolioShares
--
--         , requestDescribeProduct $
--             newDescribeProduct
--
--         , requestDescribeProductAsAdmin $
--             newDescribeProductAsAdmin
--
--         , requestDescribeProductView $
--             newDescribeProductView
--
--         , requestDescribeProvisionedProduct $
--             newDescribeProvisionedProduct
--
--         , requestDescribeProvisionedProductPlan $
--             newDescribeProvisionedProductPlan
--
--         , requestDescribeProvisioningArtifact $
--             newDescribeProvisioningArtifact
--
--         , requestDescribeProvisioningParameters $
--             newDescribeProvisioningParameters
--
--         , requestDescribeRecord $
--             newDescribeRecord
--
--         , requestDescribeServiceAction $
--             newDescribeServiceAction
--
--         , requestDescribeServiceActionExecutionParameters $
--             newDescribeServiceActionExecutionParameters
--
--         , requestDescribeTagOption $
--             newDescribeTagOption
--
--         , requestDisableAWSOrganizationsAccess $
--             newDisableAWSOrganizationsAccess
--
--         , requestDisassociateBudgetFromResource $
--             newDisassociateBudgetFromResource
--
--         , requestDisassociatePrincipalFromPortfolio $
--             newDisassociatePrincipalFromPortfolio
--
--         , requestDisassociateProductFromPortfolio $
--             newDisassociateProductFromPortfolio
--
--         , requestDisassociateServiceActionFromProvisioningArtifact $
--             newDisassociateServiceActionFromProvisioningArtifact
--
--         , requestDisassociateTagOptionFromResource $
--             newDisassociateTagOptionFromResource
--
--         , requestEnableAWSOrganizationsAccess $
--             newEnableAWSOrganizationsAccess
--
--         , requestExecuteProvisionedProductPlan $
--             newExecuteProvisionedProductPlan
--
--         , requestExecuteProvisionedProductServiceAction $
--             newExecuteProvisionedProductServiceAction
--
--         , requestGetAWSOrganizationsAccessStatus $
--             newGetAWSOrganizationsAccessStatus
--
--         , requestGetProvisionedProductOutputs $
--             newGetProvisionedProductOutputs
--
--         , requestImportAsProvisionedProduct $
--             newImportAsProvisionedProduct
--
--         , requestListAcceptedPortfolioShares $
--             newListAcceptedPortfolioShares
--
--         , requestListBudgetsForResource $
--             newListBudgetsForResource
--
--         , requestListConstraintsForPortfolio $
--             newListConstraintsForPortfolio
--
--         , requestListLaunchPaths $
--             newListLaunchPaths
--
--         , requestListOrganizationPortfolioAccess $
--             newListOrganizationPortfolioAccess
--
--         , requestListPortfolioAccess $
--             newListPortfolioAccess
--
--         , requestListPortfolios $
--             newListPortfolios
--
--         , requestListPortfoliosForProduct $
--             newListPortfoliosForProduct
--
--         , requestListPrincipalsForPortfolio $
--             newListPrincipalsForPortfolio
--
--         , requestListProvisionedProductPlans $
--             newListProvisionedProductPlans
--
--         , requestListProvisioningArtifacts $
--             newListProvisioningArtifacts
--
--         , requestListProvisioningArtifactsForServiceAction $
--             newListProvisioningArtifactsForServiceAction
--
--         , requestListRecordHistory $
--             newListRecordHistory
--
--         , requestListResourcesForTagOption $
--             newListResourcesForTagOption
--
--         , requestListServiceActions $
--             newListServiceActions
--
--         , requestListServiceActionsForProvisioningArtifact $
--             newListServiceActionsForProvisioningArtifact
--
--         , requestListStackInstancesForProvisionedProduct $
--             newListStackInstancesForProvisionedProduct
--
--         , requestListTagOptions $
--             newListTagOptions
--
--         , requestProvisionProduct $
--             newProvisionProduct
--
--         , requestRejectPortfolioShare $
--             newRejectPortfolioShare
--
--         , requestScanProvisionedProducts $
--             newScanProvisionedProducts
--
--         , requestSearchProducts $
--             newSearchProducts
--
--         , requestSearchProductsAsAdmin $
--             newSearchProductsAsAdmin
--
--         , requestSearchProvisionedProducts $
--             newSearchProvisionedProducts
--
--         , requestTerminateProvisionedProduct $
--             newTerminateProvisionedProduct
--
--         , requestUpdateConstraint $
--             newUpdateConstraint
--
--         , requestUpdatePortfolio $
--             newUpdatePortfolio
--
--         , requestUpdatePortfolioShare $
--             newUpdatePortfolioShare
--
--         , requestUpdateProduct $
--             newUpdateProduct
--
--         , requestUpdateProvisionedProduct $
--             newUpdateProvisionedProduct
--
--         , requestUpdateProvisionedProductProperties $
--             newUpdateProvisionedProductProperties
--
--         , requestUpdateProvisioningArtifact $
--             newUpdateProvisioningArtifact
--
--         , requestUpdateServiceAction $
--             newUpdateServiceAction
--
--         , requestUpdateTagOption $
--             newUpdateTagOption
--
--           ]

--     , testGroup "response"
--         [ responseAcceptPortfolioShare $
--             newAcceptPortfolioShareResponse
--
--         , responseAssociateBudgetWithResource $
--             newAssociateBudgetWithResourceResponse
--
--         , responseAssociatePrincipalWithPortfolio $
--             newAssociatePrincipalWithPortfolioResponse
--
--         , responseAssociateProductWithPortfolio $
--             newAssociateProductWithPortfolioResponse
--
--         , responseAssociateServiceActionWithProvisioningArtifact $
--             newAssociateServiceActionWithProvisioningArtifactResponse
--
--         , responseAssociateTagOptionWithResource $
--             newAssociateTagOptionWithResourceResponse
--
--         , responseBatchAssociateServiceActionWithProvisioningArtifact $
--             newBatchAssociateServiceActionWithProvisioningArtifactResponse
--
--         , responseBatchDisassociateServiceActionFromProvisioningArtifact $
--             newBatchDisassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseCopyProduct $
--             newCopyProductResponse
--
--         , responseCreateConstraint $
--             newCreateConstraintResponse
--
--         , responseCreatePortfolio $
--             newCreatePortfolioResponse
--
--         , responseCreatePortfolioShare $
--             newCreatePortfolioShareResponse
--
--         , responseCreateProduct $
--             newCreateProductResponse
--
--         , responseCreateProvisionedProductPlan $
--             newCreateProvisionedProductPlanResponse
--
--         , responseCreateProvisioningArtifact $
--             newCreateProvisioningArtifactResponse
--
--         , responseCreateServiceAction $
--             newCreateServiceActionResponse
--
--         , responseCreateTagOption $
--             newCreateTagOptionResponse
--
--         , responseDeleteConstraint $
--             newDeleteConstraintResponse
--
--         , responseDeletePortfolio $
--             newDeletePortfolioResponse
--
--         , responseDeletePortfolioShare $
--             newDeletePortfolioShareResponse
--
--         , responseDeleteProduct $
--             newDeleteProductResponse
--
--         , responseDeleteProvisionedProductPlan $
--             newDeleteProvisionedProductPlanResponse
--
--         , responseDeleteProvisioningArtifact $
--             newDeleteProvisioningArtifactResponse
--
--         , responseDeleteServiceAction $
--             newDeleteServiceActionResponse
--
--         , responseDeleteTagOption $
--             newDeleteTagOptionResponse
--
--         , responseDescribeConstraint $
--             newDescribeConstraintResponse
--
--         , responseDescribeCopyProductStatus $
--             newDescribeCopyProductStatusResponse
--
--         , responseDescribePortfolio $
--             newDescribePortfolioResponse
--
--         , responseDescribePortfolioShareStatus $
--             newDescribePortfolioShareStatusResponse
--
--         , responseDescribePortfolioShares $
--             newDescribePortfolioSharesResponse
--
--         , responseDescribeProduct $
--             newDescribeProductResponse
--
--         , responseDescribeProductAsAdmin $
--             newDescribeProductAsAdminResponse
--
--         , responseDescribeProductView $
--             newDescribeProductViewResponse
--
--         , responseDescribeProvisionedProduct $
--             newDescribeProvisionedProductResponse
--
--         , responseDescribeProvisionedProductPlan $
--             newDescribeProvisionedProductPlanResponse
--
--         , responseDescribeProvisioningArtifact $
--             newDescribeProvisioningArtifactResponse
--
--         , responseDescribeProvisioningParameters $
--             newDescribeProvisioningParametersResponse
--
--         , responseDescribeRecord $
--             newDescribeRecordResponse
--
--         , responseDescribeServiceAction $
--             newDescribeServiceActionResponse
--
--         , responseDescribeServiceActionExecutionParameters $
--             newDescribeServiceActionExecutionParametersResponse
--
--         , responseDescribeTagOption $
--             newDescribeTagOptionResponse
--
--         , responseDisableAWSOrganizationsAccess $
--             newDisableAWSOrganizationsAccessResponse
--
--         , responseDisassociateBudgetFromResource $
--             newDisassociateBudgetFromResourceResponse
--
--         , responseDisassociatePrincipalFromPortfolio $
--             newDisassociatePrincipalFromPortfolioResponse
--
--         , responseDisassociateProductFromPortfolio $
--             newDisassociateProductFromPortfolioResponse
--
--         , responseDisassociateServiceActionFromProvisioningArtifact $
--             newDisassociateServiceActionFromProvisioningArtifactResponse
--
--         , responseDisassociateTagOptionFromResource $
--             newDisassociateTagOptionFromResourceResponse
--
--         , responseEnableAWSOrganizationsAccess $
--             newEnableAWSOrganizationsAccessResponse
--
--         , responseExecuteProvisionedProductPlan $
--             newExecuteProvisionedProductPlanResponse
--
--         , responseExecuteProvisionedProductServiceAction $
--             newExecuteProvisionedProductServiceActionResponse
--
--         , responseGetAWSOrganizationsAccessStatus $
--             newGetAWSOrganizationsAccessStatusResponse
--
--         , responseGetProvisionedProductOutputs $
--             newGetProvisionedProductOutputsResponse
--
--         , responseImportAsProvisionedProduct $
--             newImportAsProvisionedProductResponse
--
--         , responseListAcceptedPortfolioShares $
--             newListAcceptedPortfolioSharesResponse
--
--         , responseListBudgetsForResource $
--             newListBudgetsForResourceResponse
--
--         , responseListConstraintsForPortfolio $
--             newListConstraintsForPortfolioResponse
--
--         , responseListLaunchPaths $
--             newListLaunchPathsResponse
--
--         , responseListOrganizationPortfolioAccess $
--             newListOrganizationPortfolioAccessResponse
--
--         , responseListPortfolioAccess $
--             newListPortfolioAccessResponse
--
--         , responseListPortfolios $
--             newListPortfoliosResponse
--
--         , responseListPortfoliosForProduct $
--             newListPortfoliosForProductResponse
--
--         , responseListPrincipalsForPortfolio $
--             newListPrincipalsForPortfolioResponse
--
--         , responseListProvisionedProductPlans $
--             newListProvisionedProductPlansResponse
--
--         , responseListProvisioningArtifacts $
--             newListProvisioningArtifactsResponse
--
--         , responseListProvisioningArtifactsForServiceAction $
--             newListProvisioningArtifactsForServiceActionResponse
--
--         , responseListRecordHistory $
--             newListRecordHistoryResponse
--
--         , responseListResourcesForTagOption $
--             newListResourcesForTagOptionResponse
--
--         , responseListServiceActions $
--             newListServiceActionsResponse
--
--         , responseListServiceActionsForProvisioningArtifact $
--             newListServiceActionsForProvisioningArtifactResponse
--
--         , responseListStackInstancesForProvisionedProduct $
--             newListStackInstancesForProvisionedProductResponse
--
--         , responseListTagOptions $
--             newListTagOptionsResponse
--
--         , responseProvisionProduct $
--             newProvisionProductResponse
--
--         , responseRejectPortfolioShare $
--             newRejectPortfolioShareResponse
--
--         , responseScanProvisionedProducts $
--             newScanProvisionedProductsResponse
--
--         , responseSearchProducts $
--             newSearchProductsResponse
--
--         , responseSearchProductsAsAdmin $
--             newSearchProductsAsAdminResponse
--
--         , responseSearchProvisionedProducts $
--             newSearchProvisionedProductsResponse
--
--         , responseTerminateProvisionedProduct $
--             newTerminateProvisionedProductResponse
--
--         , responseUpdateConstraint $
--             newUpdateConstraintResponse
--
--         , responseUpdatePortfolio $
--             newUpdatePortfolioResponse
--
--         , responseUpdatePortfolioShare $
--             newUpdatePortfolioShareResponse
--
--         , responseUpdateProduct $
--             newUpdateProductResponse
--
--         , responseUpdateProvisionedProduct $
--             newUpdateProvisionedProductResponse
--
--         , responseUpdateProvisionedProductProperties $
--             newUpdateProvisionedProductPropertiesResponse
--
--         , responseUpdateProvisioningArtifact $
--             newUpdateProvisioningArtifactResponse
--
--         , responseUpdateServiceAction $
--             newUpdateServiceActionResponse
--
--         , responseUpdateTagOption $
--             newUpdateTagOptionResponse
--
--           ]
--     ]

-- Requests

requestAcceptPortfolioShare :: AcceptPortfolioShare -> TestTree
requestAcceptPortfolioShare =
  req
    "AcceptPortfolioShare"
    "fixture/AcceptPortfolioShare.yaml"

requestAssociateBudgetWithResource :: AssociateBudgetWithResource -> TestTree
requestAssociateBudgetWithResource =
  req
    "AssociateBudgetWithResource"
    "fixture/AssociateBudgetWithResource.yaml"

requestAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolio -> TestTree
requestAssociatePrincipalWithPortfolio =
  req
    "AssociatePrincipalWithPortfolio"
    "fixture/AssociatePrincipalWithPortfolio.yaml"

requestAssociateProductWithPortfolio :: AssociateProductWithPortfolio -> TestTree
requestAssociateProductWithPortfolio =
  req
    "AssociateProductWithPortfolio"
    "fixture/AssociateProductWithPortfolio.yaml"

requestAssociateServiceActionWithProvisioningArtifact :: AssociateServiceActionWithProvisioningArtifact -> TestTree
requestAssociateServiceActionWithProvisioningArtifact =
  req
    "AssociateServiceActionWithProvisioningArtifact"
    "fixture/AssociateServiceActionWithProvisioningArtifact.yaml"

requestAssociateTagOptionWithResource :: AssociateTagOptionWithResource -> TestTree
requestAssociateTagOptionWithResource =
  req
    "AssociateTagOptionWithResource"
    "fixture/AssociateTagOptionWithResource.yaml"

requestBatchAssociateServiceActionWithProvisioningArtifact :: BatchAssociateServiceActionWithProvisioningArtifact -> TestTree
requestBatchAssociateServiceActionWithProvisioningArtifact =
  req
    "BatchAssociateServiceActionWithProvisioningArtifact"
    "fixture/BatchAssociateServiceActionWithProvisioningArtifact.yaml"

requestBatchDisassociateServiceActionFromProvisioningArtifact :: BatchDisassociateServiceActionFromProvisioningArtifact -> TestTree
requestBatchDisassociateServiceActionFromProvisioningArtifact =
  req
    "BatchDisassociateServiceActionFromProvisioningArtifact"
    "fixture/BatchDisassociateServiceActionFromProvisioningArtifact.yaml"

requestCopyProduct :: CopyProduct -> TestTree
requestCopyProduct =
  req
    "CopyProduct"
    "fixture/CopyProduct.yaml"

requestCreateConstraint :: CreateConstraint -> TestTree
requestCreateConstraint =
  req
    "CreateConstraint"
    "fixture/CreateConstraint.yaml"

requestCreatePortfolio :: CreatePortfolio -> TestTree
requestCreatePortfolio =
  req
    "CreatePortfolio"
    "fixture/CreatePortfolio.yaml"

requestCreatePortfolioShare :: CreatePortfolioShare -> TestTree
requestCreatePortfolioShare =
  req
    "CreatePortfolioShare"
    "fixture/CreatePortfolioShare.yaml"

requestCreateProduct :: CreateProduct -> TestTree
requestCreateProduct =
  req
    "CreateProduct"
    "fixture/CreateProduct.yaml"

requestCreateProvisionedProductPlan :: CreateProvisionedProductPlan -> TestTree
requestCreateProvisionedProductPlan =
  req
    "CreateProvisionedProductPlan"
    "fixture/CreateProvisionedProductPlan.yaml"

requestCreateProvisioningArtifact :: CreateProvisioningArtifact -> TestTree
requestCreateProvisioningArtifact =
  req
    "CreateProvisioningArtifact"
    "fixture/CreateProvisioningArtifact.yaml"

requestCreateServiceAction :: CreateServiceAction -> TestTree
requestCreateServiceAction =
  req
    "CreateServiceAction"
    "fixture/CreateServiceAction.yaml"

requestCreateTagOption :: CreateTagOption -> TestTree
requestCreateTagOption =
  req
    "CreateTagOption"
    "fixture/CreateTagOption.yaml"

requestDeleteConstraint :: DeleteConstraint -> TestTree
requestDeleteConstraint =
  req
    "DeleteConstraint"
    "fixture/DeleteConstraint.yaml"

requestDeletePortfolio :: DeletePortfolio -> TestTree
requestDeletePortfolio =
  req
    "DeletePortfolio"
    "fixture/DeletePortfolio.yaml"

requestDeletePortfolioShare :: DeletePortfolioShare -> TestTree
requestDeletePortfolioShare =
  req
    "DeletePortfolioShare"
    "fixture/DeletePortfolioShare.yaml"

requestDeleteProduct :: DeleteProduct -> TestTree
requestDeleteProduct =
  req
    "DeleteProduct"
    "fixture/DeleteProduct.yaml"

requestDeleteProvisionedProductPlan :: DeleteProvisionedProductPlan -> TestTree
requestDeleteProvisionedProductPlan =
  req
    "DeleteProvisionedProductPlan"
    "fixture/DeleteProvisionedProductPlan.yaml"

requestDeleteProvisioningArtifact :: DeleteProvisioningArtifact -> TestTree
requestDeleteProvisioningArtifact =
  req
    "DeleteProvisioningArtifact"
    "fixture/DeleteProvisioningArtifact.yaml"

requestDeleteServiceAction :: DeleteServiceAction -> TestTree
requestDeleteServiceAction =
  req
    "DeleteServiceAction"
    "fixture/DeleteServiceAction.yaml"

requestDeleteTagOption :: DeleteTagOption -> TestTree
requestDeleteTagOption =
  req
    "DeleteTagOption"
    "fixture/DeleteTagOption.yaml"

requestDescribeConstraint :: DescribeConstraint -> TestTree
requestDescribeConstraint =
  req
    "DescribeConstraint"
    "fixture/DescribeConstraint.yaml"

requestDescribeCopyProductStatus :: DescribeCopyProductStatus -> TestTree
requestDescribeCopyProductStatus =
  req
    "DescribeCopyProductStatus"
    "fixture/DescribeCopyProductStatus.yaml"

requestDescribePortfolio :: DescribePortfolio -> TestTree
requestDescribePortfolio =
  req
    "DescribePortfolio"
    "fixture/DescribePortfolio.yaml"

requestDescribePortfolioShareStatus :: DescribePortfolioShareStatus -> TestTree
requestDescribePortfolioShareStatus =
  req
    "DescribePortfolioShareStatus"
    "fixture/DescribePortfolioShareStatus.yaml"

requestDescribePortfolioShares :: DescribePortfolioShares -> TestTree
requestDescribePortfolioShares =
  req
    "DescribePortfolioShares"
    "fixture/DescribePortfolioShares.yaml"

requestDescribeProduct :: DescribeProduct -> TestTree
requestDescribeProduct =
  req
    "DescribeProduct"
    "fixture/DescribeProduct.yaml"

requestDescribeProductAsAdmin :: DescribeProductAsAdmin -> TestTree
requestDescribeProductAsAdmin =
  req
    "DescribeProductAsAdmin"
    "fixture/DescribeProductAsAdmin.yaml"

requestDescribeProductView :: DescribeProductView -> TestTree
requestDescribeProductView =
  req
    "DescribeProductView"
    "fixture/DescribeProductView.yaml"

requestDescribeProvisionedProduct :: DescribeProvisionedProduct -> TestTree
requestDescribeProvisionedProduct =
  req
    "DescribeProvisionedProduct"
    "fixture/DescribeProvisionedProduct.yaml"

requestDescribeProvisionedProductPlan :: DescribeProvisionedProductPlan -> TestTree
requestDescribeProvisionedProductPlan =
  req
    "DescribeProvisionedProductPlan"
    "fixture/DescribeProvisionedProductPlan.yaml"

requestDescribeProvisioningArtifact :: DescribeProvisioningArtifact -> TestTree
requestDescribeProvisioningArtifact =
  req
    "DescribeProvisioningArtifact"
    "fixture/DescribeProvisioningArtifact.yaml"

requestDescribeProvisioningParameters :: DescribeProvisioningParameters -> TestTree
requestDescribeProvisioningParameters =
  req
    "DescribeProvisioningParameters"
    "fixture/DescribeProvisioningParameters.yaml"

requestDescribeRecord :: DescribeRecord -> TestTree
requestDescribeRecord =
  req
    "DescribeRecord"
    "fixture/DescribeRecord.yaml"

requestDescribeServiceAction :: DescribeServiceAction -> TestTree
requestDescribeServiceAction =
  req
    "DescribeServiceAction"
    "fixture/DescribeServiceAction.yaml"

requestDescribeServiceActionExecutionParameters :: DescribeServiceActionExecutionParameters -> TestTree
requestDescribeServiceActionExecutionParameters =
  req
    "DescribeServiceActionExecutionParameters"
    "fixture/DescribeServiceActionExecutionParameters.yaml"

requestDescribeTagOption :: DescribeTagOption -> TestTree
requestDescribeTagOption =
  req
    "DescribeTagOption"
    "fixture/DescribeTagOption.yaml"

requestDisableAWSOrganizationsAccess :: DisableAWSOrganizationsAccess -> TestTree
requestDisableAWSOrganizationsAccess =
  req
    "DisableAWSOrganizationsAccess"
    "fixture/DisableAWSOrganizationsAccess.yaml"

requestDisassociateBudgetFromResource :: DisassociateBudgetFromResource -> TestTree
requestDisassociateBudgetFromResource =
  req
    "DisassociateBudgetFromResource"
    "fixture/DisassociateBudgetFromResource.yaml"

requestDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolio -> TestTree
requestDisassociatePrincipalFromPortfolio =
  req
    "DisassociatePrincipalFromPortfolio"
    "fixture/DisassociatePrincipalFromPortfolio.yaml"

requestDisassociateProductFromPortfolio :: DisassociateProductFromPortfolio -> TestTree
requestDisassociateProductFromPortfolio =
  req
    "DisassociateProductFromPortfolio"
    "fixture/DisassociateProductFromPortfolio.yaml"

requestDisassociateServiceActionFromProvisioningArtifact :: DisassociateServiceActionFromProvisioningArtifact -> TestTree
requestDisassociateServiceActionFromProvisioningArtifact =
  req
    "DisassociateServiceActionFromProvisioningArtifact"
    "fixture/DisassociateServiceActionFromProvisioningArtifact.yaml"

requestDisassociateTagOptionFromResource :: DisassociateTagOptionFromResource -> TestTree
requestDisassociateTagOptionFromResource =
  req
    "DisassociateTagOptionFromResource"
    "fixture/DisassociateTagOptionFromResource.yaml"

requestEnableAWSOrganizationsAccess :: EnableAWSOrganizationsAccess -> TestTree
requestEnableAWSOrganizationsAccess =
  req
    "EnableAWSOrganizationsAccess"
    "fixture/EnableAWSOrganizationsAccess.yaml"

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

requestGetAWSOrganizationsAccessStatus :: GetAWSOrganizationsAccessStatus -> TestTree
requestGetAWSOrganizationsAccessStatus =
  req
    "GetAWSOrganizationsAccessStatus"
    "fixture/GetAWSOrganizationsAccessStatus.yaml"

requestGetProvisionedProductOutputs :: GetProvisionedProductOutputs -> TestTree
requestGetProvisionedProductOutputs =
  req
    "GetProvisionedProductOutputs"
    "fixture/GetProvisionedProductOutputs.yaml"

requestImportAsProvisionedProduct :: ImportAsProvisionedProduct -> TestTree
requestImportAsProvisionedProduct =
  req
    "ImportAsProvisionedProduct"
    "fixture/ImportAsProvisionedProduct.yaml"

requestListAcceptedPortfolioShares :: ListAcceptedPortfolioShares -> TestTree
requestListAcceptedPortfolioShares =
  req
    "ListAcceptedPortfolioShares"
    "fixture/ListAcceptedPortfolioShares.yaml"

requestListBudgetsForResource :: ListBudgetsForResource -> TestTree
requestListBudgetsForResource =
  req
    "ListBudgetsForResource"
    "fixture/ListBudgetsForResource.yaml"

requestListConstraintsForPortfolio :: ListConstraintsForPortfolio -> TestTree
requestListConstraintsForPortfolio =
  req
    "ListConstraintsForPortfolio"
    "fixture/ListConstraintsForPortfolio.yaml"

requestListLaunchPaths :: ListLaunchPaths -> TestTree
requestListLaunchPaths =
  req
    "ListLaunchPaths"
    "fixture/ListLaunchPaths.yaml"

requestListOrganizationPortfolioAccess :: ListOrganizationPortfolioAccess -> TestTree
requestListOrganizationPortfolioAccess =
  req
    "ListOrganizationPortfolioAccess"
    "fixture/ListOrganizationPortfolioAccess.yaml"

requestListPortfolioAccess :: ListPortfolioAccess -> TestTree
requestListPortfolioAccess =
  req
    "ListPortfolioAccess"
    "fixture/ListPortfolioAccess.yaml"

requestListPortfolios :: ListPortfolios -> TestTree
requestListPortfolios =
  req
    "ListPortfolios"
    "fixture/ListPortfolios.yaml"

requestListPortfoliosForProduct :: ListPortfoliosForProduct -> TestTree
requestListPortfoliosForProduct =
  req
    "ListPortfoliosForProduct"
    "fixture/ListPortfoliosForProduct.yaml"

requestListPrincipalsForPortfolio :: ListPrincipalsForPortfolio -> TestTree
requestListPrincipalsForPortfolio =
  req
    "ListPrincipalsForPortfolio"
    "fixture/ListPrincipalsForPortfolio.yaml"

requestListProvisionedProductPlans :: ListProvisionedProductPlans -> TestTree
requestListProvisionedProductPlans =
  req
    "ListProvisionedProductPlans"
    "fixture/ListProvisionedProductPlans.yaml"

requestListProvisioningArtifacts :: ListProvisioningArtifacts -> TestTree
requestListProvisioningArtifacts =
  req
    "ListProvisioningArtifacts"
    "fixture/ListProvisioningArtifacts.yaml"

requestListProvisioningArtifactsForServiceAction :: ListProvisioningArtifactsForServiceAction -> TestTree
requestListProvisioningArtifactsForServiceAction =
  req
    "ListProvisioningArtifactsForServiceAction"
    "fixture/ListProvisioningArtifactsForServiceAction.yaml"

requestListRecordHistory :: ListRecordHistory -> TestTree
requestListRecordHistory =
  req
    "ListRecordHistory"
    "fixture/ListRecordHistory.yaml"

requestListResourcesForTagOption :: ListResourcesForTagOption -> TestTree
requestListResourcesForTagOption =
  req
    "ListResourcesForTagOption"
    "fixture/ListResourcesForTagOption.yaml"

requestListServiceActions :: ListServiceActions -> TestTree
requestListServiceActions =
  req
    "ListServiceActions"
    "fixture/ListServiceActions.yaml"

requestListServiceActionsForProvisioningArtifact :: ListServiceActionsForProvisioningArtifact -> TestTree
requestListServiceActionsForProvisioningArtifact =
  req
    "ListServiceActionsForProvisioningArtifact"
    "fixture/ListServiceActionsForProvisioningArtifact.yaml"

requestListStackInstancesForProvisionedProduct :: ListStackInstancesForProvisionedProduct -> TestTree
requestListStackInstancesForProvisionedProduct =
  req
    "ListStackInstancesForProvisionedProduct"
    "fixture/ListStackInstancesForProvisionedProduct.yaml"

requestListTagOptions :: ListTagOptions -> TestTree
requestListTagOptions =
  req
    "ListTagOptions"
    "fixture/ListTagOptions.yaml"

requestProvisionProduct :: ProvisionProduct -> TestTree
requestProvisionProduct =
  req
    "ProvisionProduct"
    "fixture/ProvisionProduct.yaml"

requestRejectPortfolioShare :: RejectPortfolioShare -> TestTree
requestRejectPortfolioShare =
  req
    "RejectPortfolioShare"
    "fixture/RejectPortfolioShare.yaml"

requestScanProvisionedProducts :: ScanProvisionedProducts -> TestTree
requestScanProvisionedProducts =
  req
    "ScanProvisionedProducts"
    "fixture/ScanProvisionedProducts.yaml"

requestSearchProducts :: SearchProducts -> TestTree
requestSearchProducts =
  req
    "SearchProducts"
    "fixture/SearchProducts.yaml"

requestSearchProductsAsAdmin :: SearchProductsAsAdmin -> TestTree
requestSearchProductsAsAdmin =
  req
    "SearchProductsAsAdmin"
    "fixture/SearchProductsAsAdmin.yaml"

requestSearchProvisionedProducts :: SearchProvisionedProducts -> TestTree
requestSearchProvisionedProducts =
  req
    "SearchProvisionedProducts"
    "fixture/SearchProvisionedProducts.yaml"

requestTerminateProvisionedProduct :: TerminateProvisionedProduct -> TestTree
requestTerminateProvisionedProduct =
  req
    "TerminateProvisionedProduct"
    "fixture/TerminateProvisionedProduct.yaml"

requestUpdateConstraint :: UpdateConstraint -> TestTree
requestUpdateConstraint =
  req
    "UpdateConstraint"
    "fixture/UpdateConstraint.yaml"

requestUpdatePortfolio :: UpdatePortfolio -> TestTree
requestUpdatePortfolio =
  req
    "UpdatePortfolio"
    "fixture/UpdatePortfolio.yaml"

requestUpdatePortfolioShare :: UpdatePortfolioShare -> TestTree
requestUpdatePortfolioShare =
  req
    "UpdatePortfolioShare"
    "fixture/UpdatePortfolioShare.yaml"

requestUpdateProduct :: UpdateProduct -> TestTree
requestUpdateProduct =
  req
    "UpdateProduct"
    "fixture/UpdateProduct.yaml"

requestUpdateProvisionedProduct :: UpdateProvisionedProduct -> TestTree
requestUpdateProvisionedProduct =
  req
    "UpdateProvisionedProduct"
    "fixture/UpdateProvisionedProduct.yaml"

requestUpdateProvisionedProductProperties :: UpdateProvisionedProductProperties -> TestTree
requestUpdateProvisionedProductProperties =
  req
    "UpdateProvisionedProductProperties"
    "fixture/UpdateProvisionedProductProperties.yaml"

requestUpdateProvisioningArtifact :: UpdateProvisioningArtifact -> TestTree
requestUpdateProvisioningArtifact =
  req
    "UpdateProvisioningArtifact"
    "fixture/UpdateProvisioningArtifact.yaml"

requestUpdateServiceAction :: UpdateServiceAction -> TestTree
requestUpdateServiceAction =
  req
    "UpdateServiceAction"
    "fixture/UpdateServiceAction.yaml"

requestUpdateTagOption :: UpdateTagOption -> TestTree
requestUpdateTagOption =
  req
    "UpdateTagOption"
    "fixture/UpdateTagOption.yaml"

-- Responses

responseAcceptPortfolioShare :: AcceptPortfolioShareResponse -> TestTree
responseAcceptPortfolioShare =
  res
    "AcceptPortfolioShareResponse"
    "fixture/AcceptPortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptPortfolioShare)

responseAssociateBudgetWithResource :: AssociateBudgetWithResourceResponse -> TestTree
responseAssociateBudgetWithResource =
  res
    "AssociateBudgetWithResourceResponse"
    "fixture/AssociateBudgetWithResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateBudgetWithResource)

responseAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolioResponse -> TestTree
responseAssociatePrincipalWithPortfolio =
  res
    "AssociatePrincipalWithPortfolioResponse"
    "fixture/AssociatePrincipalWithPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePrincipalWithPortfolio)

responseAssociateProductWithPortfolio :: AssociateProductWithPortfolioResponse -> TestTree
responseAssociateProductWithPortfolio =
  res
    "AssociateProductWithPortfolioResponse"
    "fixture/AssociateProductWithPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateProductWithPortfolio)

responseAssociateServiceActionWithProvisioningArtifact :: AssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseAssociateServiceActionWithProvisioningArtifact =
  res
    "AssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/AssociateServiceActionWithProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateServiceActionWithProvisioningArtifact)

responseAssociateTagOptionWithResource :: AssociateTagOptionWithResourceResponse -> TestTree
responseAssociateTagOptionWithResource =
  res
    "AssociateTagOptionWithResourceResponse"
    "fixture/AssociateTagOptionWithResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTagOptionWithResource)

responseBatchAssociateServiceActionWithProvisioningArtifact :: BatchAssociateServiceActionWithProvisioningArtifactResponse -> TestTree
responseBatchAssociateServiceActionWithProvisioningArtifact =
  res
    "BatchAssociateServiceActionWithProvisioningArtifactResponse"
    "fixture/BatchAssociateServiceActionWithProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateServiceActionWithProvisioningArtifact)

responseBatchDisassociateServiceActionFromProvisioningArtifact :: BatchDisassociateServiceActionFromProvisioningArtifactResponse -> TestTree
responseBatchDisassociateServiceActionFromProvisioningArtifact =
  res
    "BatchDisassociateServiceActionFromProvisioningArtifactResponse"
    "fixture/BatchDisassociateServiceActionFromProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateServiceActionFromProvisioningArtifact)

responseCopyProduct :: CopyProductResponse -> TestTree
responseCopyProduct =
  res
    "CopyProductResponse"
    "fixture/CopyProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyProduct)

responseCreateConstraint :: CreateConstraintResponse -> TestTree
responseCreateConstraint =
  res
    "CreateConstraintResponse"
    "fixture/CreateConstraintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConstraint)

responseCreatePortfolio :: CreatePortfolioResponse -> TestTree
responseCreatePortfolio =
  res
    "CreatePortfolioResponse"
    "fixture/CreatePortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePortfolio)

responseCreatePortfolioShare :: CreatePortfolioShareResponse -> TestTree
responseCreatePortfolioShare =
  res
    "CreatePortfolioShareResponse"
    "fixture/CreatePortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePortfolioShare)

responseCreateProduct :: CreateProductResponse -> TestTree
responseCreateProduct =
  res
    "CreateProductResponse"
    "fixture/CreateProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProduct)

responseCreateProvisionedProductPlan :: CreateProvisionedProductPlanResponse -> TestTree
responseCreateProvisionedProductPlan =
  res
    "CreateProvisionedProductPlanResponse"
    "fixture/CreateProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisionedProductPlan)

responseCreateProvisioningArtifact :: CreateProvisioningArtifactResponse -> TestTree
responseCreateProvisioningArtifact =
  res
    "CreateProvisioningArtifactResponse"
    "fixture/CreateProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProvisioningArtifact)

responseCreateServiceAction :: CreateServiceActionResponse -> TestTree
responseCreateServiceAction =
  res
    "CreateServiceActionResponse"
    "fixture/CreateServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceAction)

responseCreateTagOption :: CreateTagOptionResponse -> TestTree
responseCreateTagOption =
  res
    "CreateTagOptionResponse"
    "fixture/CreateTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTagOption)

responseDeleteConstraint :: DeleteConstraintResponse -> TestTree
responseDeleteConstraint =
  res
    "DeleteConstraintResponse"
    "fixture/DeleteConstraintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConstraint)

responseDeletePortfolio :: DeletePortfolioResponse -> TestTree
responseDeletePortfolio =
  res
    "DeletePortfolioResponse"
    "fixture/DeletePortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePortfolio)

responseDeletePortfolioShare :: DeletePortfolioShareResponse -> TestTree
responseDeletePortfolioShare =
  res
    "DeletePortfolioShareResponse"
    "fixture/DeletePortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePortfolioShare)

responseDeleteProduct :: DeleteProductResponse -> TestTree
responseDeleteProduct =
  res
    "DeleteProductResponse"
    "fixture/DeleteProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProduct)

responseDeleteProvisionedProductPlan :: DeleteProvisionedProductPlanResponse -> TestTree
responseDeleteProvisionedProductPlan =
  res
    "DeleteProvisionedProductPlanResponse"
    "fixture/DeleteProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisionedProductPlan)

responseDeleteProvisioningArtifact :: DeleteProvisioningArtifactResponse -> TestTree
responseDeleteProvisioningArtifact =
  res
    "DeleteProvisioningArtifactResponse"
    "fixture/DeleteProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProvisioningArtifact)

responseDeleteServiceAction :: DeleteServiceActionResponse -> TestTree
responseDeleteServiceAction =
  res
    "DeleteServiceActionResponse"
    "fixture/DeleteServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceAction)

responseDeleteTagOption :: DeleteTagOptionResponse -> TestTree
responseDeleteTagOption =
  res
    "DeleteTagOptionResponse"
    "fixture/DeleteTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTagOption)

responseDescribeConstraint :: DescribeConstraintResponse -> TestTree
responseDescribeConstraint =
  res
    "DescribeConstraintResponse"
    "fixture/DescribeConstraintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConstraint)

responseDescribeCopyProductStatus :: DescribeCopyProductStatusResponse -> TestTree
responseDescribeCopyProductStatus =
  res
    "DescribeCopyProductStatusResponse"
    "fixture/DescribeCopyProductStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCopyProductStatus)

responseDescribePortfolio :: DescribePortfolioResponse -> TestTree
responseDescribePortfolio =
  res
    "DescribePortfolioResponse"
    "fixture/DescribePortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePortfolio)

responseDescribePortfolioShareStatus :: DescribePortfolioShareStatusResponse -> TestTree
responseDescribePortfolioShareStatus =
  res
    "DescribePortfolioShareStatusResponse"
    "fixture/DescribePortfolioShareStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePortfolioShareStatus)

responseDescribePortfolioShares :: DescribePortfolioSharesResponse -> TestTree
responseDescribePortfolioShares =
  res
    "DescribePortfolioSharesResponse"
    "fixture/DescribePortfolioSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePortfolioShares)

responseDescribeProduct :: DescribeProductResponse -> TestTree
responseDescribeProduct =
  res
    "DescribeProductResponse"
    "fixture/DescribeProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProduct)

responseDescribeProductAsAdmin :: DescribeProductAsAdminResponse -> TestTree
responseDescribeProductAsAdmin =
  res
    "DescribeProductAsAdminResponse"
    "fixture/DescribeProductAsAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProductAsAdmin)

responseDescribeProductView :: DescribeProductViewResponse -> TestTree
responseDescribeProductView =
  res
    "DescribeProductViewResponse"
    "fixture/DescribeProductViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProductView)

responseDescribeProvisionedProduct :: DescribeProvisionedProductResponse -> TestTree
responseDescribeProvisionedProduct =
  res
    "DescribeProvisionedProductResponse"
    "fixture/DescribeProvisionedProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisionedProduct)

responseDescribeProvisionedProductPlan :: DescribeProvisionedProductPlanResponse -> TestTree
responseDescribeProvisionedProductPlan =
  res
    "DescribeProvisionedProductPlanResponse"
    "fixture/DescribeProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisionedProductPlan)

responseDescribeProvisioningArtifact :: DescribeProvisioningArtifactResponse -> TestTree
responseDescribeProvisioningArtifact =
  res
    "DescribeProvisioningArtifactResponse"
    "fixture/DescribeProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisioningArtifact)

responseDescribeProvisioningParameters :: DescribeProvisioningParametersResponse -> TestTree
responseDescribeProvisioningParameters =
  res
    "DescribeProvisioningParametersResponse"
    "fixture/DescribeProvisioningParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProvisioningParameters)

responseDescribeRecord :: DescribeRecordResponse -> TestTree
responseDescribeRecord =
  res
    "DescribeRecordResponse"
    "fixture/DescribeRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecord)

responseDescribeServiceAction :: DescribeServiceActionResponse -> TestTree
responseDescribeServiceAction =
  res
    "DescribeServiceActionResponse"
    "fixture/DescribeServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceAction)

responseDescribeServiceActionExecutionParameters :: DescribeServiceActionExecutionParametersResponse -> TestTree
responseDescribeServiceActionExecutionParameters =
  res
    "DescribeServiceActionExecutionParametersResponse"
    "fixture/DescribeServiceActionExecutionParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceActionExecutionParameters)

responseDescribeTagOption :: DescribeTagOptionResponse -> TestTree
responseDescribeTagOption =
  res
    "DescribeTagOptionResponse"
    "fixture/DescribeTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTagOption)

responseDisableAWSOrganizationsAccess :: DisableAWSOrganizationsAccessResponse -> TestTree
responseDisableAWSOrganizationsAccess =
  res
    "DisableAWSOrganizationsAccessResponse"
    "fixture/DisableAWSOrganizationsAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAWSOrganizationsAccess)

responseDisassociateBudgetFromResource :: DisassociateBudgetFromResourceResponse -> TestTree
responseDisassociateBudgetFromResource =
  res
    "DisassociateBudgetFromResourceResponse"
    "fixture/DisassociateBudgetFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateBudgetFromResource)

responseDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolioResponse -> TestTree
responseDisassociatePrincipalFromPortfolio =
  res
    "DisassociatePrincipalFromPortfolioResponse"
    "fixture/DisassociatePrincipalFromPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePrincipalFromPortfolio)

responseDisassociateProductFromPortfolio :: DisassociateProductFromPortfolioResponse -> TestTree
responseDisassociateProductFromPortfolio =
  res
    "DisassociateProductFromPortfolioResponse"
    "fixture/DisassociateProductFromPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateProductFromPortfolio)

responseDisassociateServiceActionFromProvisioningArtifact :: DisassociateServiceActionFromProvisioningArtifactResponse -> TestTree
responseDisassociateServiceActionFromProvisioningArtifact =
  res
    "DisassociateServiceActionFromProvisioningArtifactResponse"
    "fixture/DisassociateServiceActionFromProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateServiceActionFromProvisioningArtifact)

responseDisassociateTagOptionFromResource :: DisassociateTagOptionFromResourceResponse -> TestTree
responseDisassociateTagOptionFromResource =
  res
    "DisassociateTagOptionFromResourceResponse"
    "fixture/DisassociateTagOptionFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTagOptionFromResource)

responseEnableAWSOrganizationsAccess :: EnableAWSOrganizationsAccessResponse -> TestTree
responseEnableAWSOrganizationsAccess =
  res
    "EnableAWSOrganizationsAccessResponse"
    "fixture/EnableAWSOrganizationsAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAWSOrganizationsAccess)

responseExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlanResponse -> TestTree
responseExecuteProvisionedProductPlan =
  res
    "ExecuteProvisionedProductPlanResponse"
    "fixture/ExecuteProvisionedProductPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteProvisionedProductPlan)

responseExecuteProvisionedProductServiceAction :: ExecuteProvisionedProductServiceActionResponse -> TestTree
responseExecuteProvisionedProductServiceAction =
  res
    "ExecuteProvisionedProductServiceActionResponse"
    "fixture/ExecuteProvisionedProductServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteProvisionedProductServiceAction)

responseGetAWSOrganizationsAccessStatus :: GetAWSOrganizationsAccessStatusResponse -> TestTree
responseGetAWSOrganizationsAccessStatus =
  res
    "GetAWSOrganizationsAccessStatusResponse"
    "fixture/GetAWSOrganizationsAccessStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAWSOrganizationsAccessStatus)

responseGetProvisionedProductOutputs :: GetProvisionedProductOutputsResponse -> TestTree
responseGetProvisionedProductOutputs =
  res
    "GetProvisionedProductOutputsResponse"
    "fixture/GetProvisionedProductOutputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProvisionedProductOutputs)

responseImportAsProvisionedProduct :: ImportAsProvisionedProductResponse -> TestTree
responseImportAsProvisionedProduct =
  res
    "ImportAsProvisionedProductResponse"
    "fixture/ImportAsProvisionedProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportAsProvisionedProduct)

responseListAcceptedPortfolioShares :: ListAcceptedPortfolioSharesResponse -> TestTree
responseListAcceptedPortfolioShares =
  res
    "ListAcceptedPortfolioSharesResponse"
    "fixture/ListAcceptedPortfolioSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAcceptedPortfolioShares)

responseListBudgetsForResource :: ListBudgetsForResourceResponse -> TestTree
responseListBudgetsForResource =
  res
    "ListBudgetsForResourceResponse"
    "fixture/ListBudgetsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBudgetsForResource)

responseListConstraintsForPortfolio :: ListConstraintsForPortfolioResponse -> TestTree
responseListConstraintsForPortfolio =
  res
    "ListConstraintsForPortfolioResponse"
    "fixture/ListConstraintsForPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConstraintsForPortfolio)

responseListLaunchPaths :: ListLaunchPathsResponse -> TestTree
responseListLaunchPaths =
  res
    "ListLaunchPathsResponse"
    "fixture/ListLaunchPathsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLaunchPaths)

responseListOrganizationPortfolioAccess :: ListOrganizationPortfolioAccessResponse -> TestTree
responseListOrganizationPortfolioAccess =
  res
    "ListOrganizationPortfolioAccessResponse"
    "fixture/ListOrganizationPortfolioAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationPortfolioAccess)

responseListPortfolioAccess :: ListPortfolioAccessResponse -> TestTree
responseListPortfolioAccess =
  res
    "ListPortfolioAccessResponse"
    "fixture/ListPortfolioAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPortfolioAccess)

responseListPortfolios :: ListPortfoliosResponse -> TestTree
responseListPortfolios =
  res
    "ListPortfoliosResponse"
    "fixture/ListPortfoliosResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPortfolios)

responseListPortfoliosForProduct :: ListPortfoliosForProductResponse -> TestTree
responseListPortfoliosForProduct =
  res
    "ListPortfoliosForProductResponse"
    "fixture/ListPortfoliosForProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPortfoliosForProduct)

responseListPrincipalsForPortfolio :: ListPrincipalsForPortfolioResponse -> TestTree
responseListPrincipalsForPortfolio =
  res
    "ListPrincipalsForPortfolioResponse"
    "fixture/ListPrincipalsForPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrincipalsForPortfolio)

responseListProvisionedProductPlans :: ListProvisionedProductPlansResponse -> TestTree
responseListProvisionedProductPlans =
  res
    "ListProvisionedProductPlansResponse"
    "fixture/ListProvisionedProductPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisionedProductPlans)

responseListProvisioningArtifacts :: ListProvisioningArtifactsResponse -> TestTree
responseListProvisioningArtifacts =
  res
    "ListProvisioningArtifactsResponse"
    "fixture/ListProvisioningArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisioningArtifacts)

responseListProvisioningArtifactsForServiceAction :: ListProvisioningArtifactsForServiceActionResponse -> TestTree
responseListProvisioningArtifactsForServiceAction =
  res
    "ListProvisioningArtifactsForServiceActionResponse"
    "fixture/ListProvisioningArtifactsForServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProvisioningArtifactsForServiceAction)

responseListRecordHistory :: ListRecordHistoryResponse -> TestTree
responseListRecordHistory =
  res
    "ListRecordHistoryResponse"
    "fixture/ListRecordHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecordHistory)

responseListResourcesForTagOption :: ListResourcesForTagOptionResponse -> TestTree
responseListResourcesForTagOption =
  res
    "ListResourcesForTagOptionResponse"
    "fixture/ListResourcesForTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourcesForTagOption)

responseListServiceActions :: ListServiceActionsResponse -> TestTree
responseListServiceActions =
  res
    "ListServiceActionsResponse"
    "fixture/ListServiceActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceActions)

responseListServiceActionsForProvisioningArtifact :: ListServiceActionsForProvisioningArtifactResponse -> TestTree
responseListServiceActionsForProvisioningArtifact =
  res
    "ListServiceActionsForProvisioningArtifactResponse"
    "fixture/ListServiceActionsForProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceActionsForProvisioningArtifact)

responseListStackInstancesForProvisionedProduct :: ListStackInstancesForProvisionedProductResponse -> TestTree
responseListStackInstancesForProvisionedProduct =
  res
    "ListStackInstancesForProvisionedProductResponse"
    "fixture/ListStackInstancesForProvisionedProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStackInstancesForProvisionedProduct)

responseListTagOptions :: ListTagOptionsResponse -> TestTree
responseListTagOptions =
  res
    "ListTagOptionsResponse"
    "fixture/ListTagOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagOptions)

responseProvisionProduct :: ProvisionProductResponse -> TestTree
responseProvisionProduct =
  res
    "ProvisionProductResponse"
    "fixture/ProvisionProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionProduct)

responseRejectPortfolioShare :: RejectPortfolioShareResponse -> TestTree
responseRejectPortfolioShare =
  res
    "RejectPortfolioShareResponse"
    "fixture/RejectPortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectPortfolioShare)

responseScanProvisionedProducts :: ScanProvisionedProductsResponse -> TestTree
responseScanProvisionedProducts =
  res
    "ScanProvisionedProductsResponse"
    "fixture/ScanProvisionedProductsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ScanProvisionedProducts)

responseSearchProducts :: SearchProductsResponse -> TestTree
responseSearchProducts =
  res
    "SearchProductsResponse"
    "fixture/SearchProductsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchProducts)

responseSearchProductsAsAdmin :: SearchProductsAsAdminResponse -> TestTree
responseSearchProductsAsAdmin =
  res
    "SearchProductsAsAdminResponse"
    "fixture/SearchProductsAsAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchProductsAsAdmin)

responseSearchProvisionedProducts :: SearchProvisionedProductsResponse -> TestTree
responseSearchProvisionedProducts =
  res
    "SearchProvisionedProductsResponse"
    "fixture/SearchProvisionedProductsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchProvisionedProducts)

responseTerminateProvisionedProduct :: TerminateProvisionedProductResponse -> TestTree
responseTerminateProvisionedProduct =
  res
    "TerminateProvisionedProductResponse"
    "fixture/TerminateProvisionedProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateProvisionedProduct)

responseUpdateConstraint :: UpdateConstraintResponse -> TestTree
responseUpdateConstraint =
  res
    "UpdateConstraintResponse"
    "fixture/UpdateConstraintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConstraint)

responseUpdatePortfolio :: UpdatePortfolioResponse -> TestTree
responseUpdatePortfolio =
  res
    "UpdatePortfolioResponse"
    "fixture/UpdatePortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePortfolio)

responseUpdatePortfolioShare :: UpdatePortfolioShareResponse -> TestTree
responseUpdatePortfolioShare =
  res
    "UpdatePortfolioShareResponse"
    "fixture/UpdatePortfolioShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePortfolioShare)

responseUpdateProduct :: UpdateProductResponse -> TestTree
responseUpdateProduct =
  res
    "UpdateProductResponse"
    "fixture/UpdateProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProduct)

responseUpdateProvisionedProduct :: UpdateProvisionedProductResponse -> TestTree
responseUpdateProvisionedProduct =
  res
    "UpdateProvisionedProductResponse"
    "fixture/UpdateProvisionedProductResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProvisionedProduct)

responseUpdateProvisionedProductProperties :: UpdateProvisionedProductPropertiesResponse -> TestTree
responseUpdateProvisionedProductProperties =
  res
    "UpdateProvisionedProductPropertiesResponse"
    "fixture/UpdateProvisionedProductPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProvisionedProductProperties)

responseUpdateProvisioningArtifact :: UpdateProvisioningArtifactResponse -> TestTree
responseUpdateProvisioningArtifact =
  res
    "UpdateProvisioningArtifactResponse"
    "fixture/UpdateProvisioningArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProvisioningArtifact)

responseUpdateServiceAction :: UpdateServiceActionResponse -> TestTree
responseUpdateServiceAction =
  res
    "UpdateServiceActionResponse"
    "fixture/UpdateServiceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceAction)

responseUpdateTagOption :: UpdateTagOptionResponse -> TestTree
responseUpdateTagOption =
  res
    "UpdateTagOptionResponse"
    "fixture/UpdateTagOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTagOption)
