{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ServiceCatalog
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDeleteConstraint $
--             deleteConstraint
--
--         , requestUpdateConstraint $
--             updateConstraint
--
--         , requestCreateProvisionedProductPlan $
--             createProvisionedProductPlan
--
--         , requestCreateProduct $
--             createProduct
--
--         , requestDescribeCopyProductStatus $
--             describeCopyProductStatus
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
--         , requestDisassociateProductFromPortfolio $
--             disassociateProductFromPortfolio
--
--         , requestListConstraintsForPortfolio $
--             listConstraintsForPortfolio
--
--         , requestDescribeRecord $
--             describeRecord
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
--         , requestDescribeProductView $
--             describeProductView
--
--         , requestCreatePortfolioShare $
--             createPortfolioShare
--
--         , requestListProvisioningArtifacts $
--             listProvisioningArtifacts
--
--         , requestSearchProducts $
--             searchProducts
--
--         , requestSearchProvisionedProducts $
--             searchProvisionedProducts
--
--         , requestDescribeProduct $
--             describeProduct
--
--         , requestDeleteProvisionedProductPlan $
--             deleteProvisionedProductPlan
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
--         , requestListPrincipalsForPortfolio $
--             listPrincipalsForPortfolio
--
--         , requestDeleteProduct $
--             deleteProduct
--
--         , requestUpdateProduct $
--             updateProduct
--
--         , requestProvisionProduct $
--             provisionProduct
--
--         , requestRejectPortfolioShare $
--             rejectPortfolioShare
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
--         , requestDescribeProductAsAdmin $
--             describeProductAsAdmin
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
--         , requestUpdateProvisioningArtifact $
--             updateProvisioningArtifact
--
--         , requestDeletePortfolioShare $
--             deletePortfolioShare
--
--         , requestDeleteProvisioningArtifact $
--             deleteProvisioningArtifact
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
--           ]

--     , testGroup "response"
--         [ responseDeleteConstraint $
--             deleteConstraintResponse
--
--         , responseUpdateConstraint $
--             updateConstraintResponse
--
--         , responseCreateProvisionedProductPlan $
--             createProvisionedProductPlanResponse
--
--         , responseCreateProduct $
--             createProductResponse
--
--         , responseDescribeCopyProductStatus $
--             describeCopyProductStatusResponse
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
--         , responseDisassociateProductFromPortfolio $
--             disassociateProductFromPortfolioResponse
--
--         , responseListConstraintsForPortfolio $
--             listConstraintsForPortfolioResponse
--
--         , responseDescribeRecord $
--             describeRecordResponse
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
--         , responseDescribeProductView $
--             describeProductViewResponse
--
--         , responseCreatePortfolioShare $
--             createPortfolioShareResponse
--
--         , responseListProvisioningArtifacts $
--             listProvisioningArtifactsResponse
--
--         , responseSearchProducts $
--             searchProductsResponse
--
--         , responseSearchProvisionedProducts $
--             searchProvisionedProductsResponse
--
--         , responseDescribeProduct $
--             describeProductResponse
--
--         , responseDeleteProvisionedProductPlan $
--             deleteProvisionedProductPlanResponse
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
--         , responseListPrincipalsForPortfolio $
--             listPrincipalsForPortfolioResponse
--
--         , responseDeleteProduct $
--             deleteProductResponse
--
--         , responseUpdateProduct $
--             updateProductResponse
--
--         , responseProvisionProduct $
--             provisionProductResponse
--
--         , responseRejectPortfolioShare $
--             rejectPortfolioShareResponse
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
--         , responseDescribeProductAsAdmin $
--             describeProductAsAdminResponse
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
--         , responseUpdateProvisioningArtifact $
--             updateProvisioningArtifactResponse
--
--         , responseDeletePortfolioShare $
--             deletePortfolioShareResponse
--
--         , responseDeleteProvisioningArtifact $
--             deleteProvisioningArtifactResponse
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
--           ]
--     ]

-- Requests

requestDeleteConstraint :: DeleteConstraint -> TestTree
requestDeleteConstraint = req
    "DeleteConstraint"
    "fixture/DeleteConstraint.yaml"

requestUpdateConstraint :: UpdateConstraint -> TestTree
requestUpdateConstraint = req
    "UpdateConstraint"
    "fixture/UpdateConstraint.yaml"

requestCreateProvisionedProductPlan :: CreateProvisionedProductPlan -> TestTree
requestCreateProvisionedProductPlan = req
    "CreateProvisionedProductPlan"
    "fixture/CreateProvisionedProductPlan.yaml"

requestCreateProduct :: CreateProduct -> TestTree
requestCreateProduct = req
    "CreateProduct"
    "fixture/CreateProduct.yaml"

requestDescribeCopyProductStatus :: DescribeCopyProductStatus -> TestTree
requestDescribeCopyProductStatus = req
    "DescribeCopyProductStatus"
    "fixture/DescribeCopyProductStatus.yaml"

requestTerminateProvisionedProduct :: TerminateProvisionedProduct -> TestTree
requestTerminateProvisionedProduct = req
    "TerminateProvisionedProduct"
    "fixture/TerminateProvisionedProduct.yaml"

requestUpdateProvisionedProduct :: UpdateProvisionedProduct -> TestTree
requestUpdateProvisionedProduct = req
    "UpdateProvisionedProduct"
    "fixture/UpdateProvisionedProduct.yaml"

requestDescribeProvisioningArtifact :: DescribeProvisioningArtifact -> TestTree
requestDescribeProvisioningArtifact = req
    "DescribeProvisioningArtifact"
    "fixture/DescribeProvisioningArtifact.yaml"

requestListRecordHistory :: ListRecordHistory -> TestTree
requestListRecordHistory = req
    "ListRecordHistory"
    "fixture/ListRecordHistory.yaml"

requestDescribeProvisionedProductPlan :: DescribeProvisionedProductPlan -> TestTree
requestDescribeProvisionedProductPlan = req
    "DescribeProvisionedProductPlan"
    "fixture/DescribeProvisionedProductPlan.yaml"

requestAssociateTagOptionWithResource :: AssociateTagOptionWithResource -> TestTree
requestAssociateTagOptionWithResource = req
    "AssociateTagOptionWithResource"
    "fixture/AssociateTagOptionWithResource.yaml"

requestCreateTagOption :: CreateTagOption -> TestTree
requestCreateTagOption = req
    "CreateTagOption"
    "fixture/CreateTagOption.yaml"

requestDisassociateProductFromPortfolio :: DisassociateProductFromPortfolio -> TestTree
requestDisassociateProductFromPortfolio = req
    "DisassociateProductFromPortfolio"
    "fixture/DisassociateProductFromPortfolio.yaml"

requestListConstraintsForPortfolio :: ListConstraintsForPortfolio -> TestTree
requestListConstraintsForPortfolio = req
    "ListConstraintsForPortfolio"
    "fixture/ListConstraintsForPortfolio.yaml"

requestDescribeRecord :: DescribeRecord -> TestTree
requestDescribeRecord = req
    "DescribeRecord"
    "fixture/DescribeRecord.yaml"

requestDescribeConstraint :: DescribeConstraint -> TestTree
requestDescribeConstraint = req
    "DescribeConstraint"
    "fixture/DescribeConstraint.yaml"

requestCreateProvisioningArtifact :: CreateProvisioningArtifact -> TestTree
requestCreateProvisioningArtifact = req
    "CreateProvisioningArtifact"
    "fixture/CreateProvisioningArtifact.yaml"

requestListPortfolios :: ListPortfolios -> TestTree
requestListPortfolios = req
    "ListPortfolios"
    "fixture/ListPortfolios.yaml"

requestDescribeProductView :: DescribeProductView -> TestTree
requestDescribeProductView = req
    "DescribeProductView"
    "fixture/DescribeProductView.yaml"

requestCreatePortfolioShare :: CreatePortfolioShare -> TestTree
requestCreatePortfolioShare = req
    "CreatePortfolioShare"
    "fixture/CreatePortfolioShare.yaml"

requestListProvisioningArtifacts :: ListProvisioningArtifacts -> TestTree
requestListProvisioningArtifacts = req
    "ListProvisioningArtifacts"
    "fixture/ListProvisioningArtifacts.yaml"

requestSearchProducts :: SearchProducts -> TestTree
requestSearchProducts = req
    "SearchProducts"
    "fixture/SearchProducts.yaml"

requestSearchProvisionedProducts :: SearchProvisionedProducts -> TestTree
requestSearchProvisionedProducts = req
    "SearchProvisionedProducts"
    "fixture/SearchProvisionedProducts.yaml"

requestDescribeProduct :: DescribeProduct -> TestTree
requestDescribeProduct = req
    "DescribeProduct"
    "fixture/DescribeProduct.yaml"

requestDeleteProvisionedProductPlan :: DeleteProvisionedProductPlan -> TestTree
requestDeleteProvisionedProductPlan = req
    "DeleteProvisionedProductPlan"
    "fixture/DeleteProvisionedProductPlan.yaml"

requestCreateConstraint :: CreateConstraint -> TestTree
requestCreateConstraint = req
    "CreateConstraint"
    "fixture/CreateConstraint.yaml"

requestListProvisionedProductPlans :: ListProvisionedProductPlans -> TestTree
requestListProvisionedProductPlans = req
    "ListProvisionedProductPlans"
    "fixture/ListProvisionedProductPlans.yaml"

requestListPortfolioAccess :: ListPortfolioAccess -> TestTree
requestListPortfolioAccess = req
    "ListPortfolioAccess"
    "fixture/ListPortfolioAccess.yaml"

requestDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolio -> TestTree
requestDisassociatePrincipalFromPortfolio = req
    "DisassociatePrincipalFromPortfolio"
    "fixture/DisassociatePrincipalFromPortfolio.yaml"

requestDescribeTagOption :: DescribeTagOption -> TestTree
requestDescribeTagOption = req
    "DescribeTagOption"
    "fixture/DescribeTagOption.yaml"

requestDisassociateTagOptionFromResource :: DisassociateTagOptionFromResource -> TestTree
requestDisassociateTagOptionFromResource = req
    "DisassociateTagOptionFromResource"
    "fixture/DisassociateTagOptionFromResource.yaml"

requestDescribePortfolio :: DescribePortfolio -> TestTree
requestDescribePortfolio = req
    "DescribePortfolio"
    "fixture/DescribePortfolio.yaml"

requestAssociateProductWithPortfolio :: AssociateProductWithPortfolio -> TestTree
requestAssociateProductWithPortfolio = req
    "AssociateProductWithPortfolio"
    "fixture/AssociateProductWithPortfolio.yaml"

requestListAcceptedPortfolioShares :: ListAcceptedPortfolioShares -> TestTree
requestListAcceptedPortfolioShares = req
    "ListAcceptedPortfolioShares"
    "fixture/ListAcceptedPortfolioShares.yaml"

requestExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlan -> TestTree
requestExecuteProvisionedProductPlan = req
    "ExecuteProvisionedProductPlan"
    "fixture/ExecuteProvisionedProductPlan.yaml"

requestAcceptPortfolioShare :: AcceptPortfolioShare -> TestTree
requestAcceptPortfolioShare = req
    "AcceptPortfolioShare"
    "fixture/AcceptPortfolioShare.yaml"

requestScanProvisionedProducts :: ScanProvisionedProducts -> TestTree
requestScanProvisionedProducts = req
    "ScanProvisionedProducts"
    "fixture/ScanProvisionedProducts.yaml"

requestListPrincipalsForPortfolio :: ListPrincipalsForPortfolio -> TestTree
requestListPrincipalsForPortfolio = req
    "ListPrincipalsForPortfolio"
    "fixture/ListPrincipalsForPortfolio.yaml"

requestDeleteProduct :: DeleteProduct -> TestTree
requestDeleteProduct = req
    "DeleteProduct"
    "fixture/DeleteProduct.yaml"

requestUpdateProduct :: UpdateProduct -> TestTree
requestUpdateProduct = req
    "UpdateProduct"
    "fixture/UpdateProduct.yaml"

requestProvisionProduct :: ProvisionProduct -> TestTree
requestProvisionProduct = req
    "ProvisionProduct"
    "fixture/ProvisionProduct.yaml"

requestRejectPortfolioShare :: RejectPortfolioShare -> TestTree
requestRejectPortfolioShare = req
    "RejectPortfolioShare"
    "fixture/RejectPortfolioShare.yaml"

requestDeleteTagOption :: DeleteTagOption -> TestTree
requestDeleteTagOption = req
    "DeleteTagOption"
    "fixture/DeleteTagOption.yaml"

requestUpdateTagOption :: UpdateTagOption -> TestTree
requestUpdateTagOption = req
    "UpdateTagOption"
    "fixture/UpdateTagOption.yaml"

requestListTagOptions :: ListTagOptions -> TestTree
requestListTagOptions = req
    "ListTagOptions"
    "fixture/ListTagOptions.yaml"

requestSearchProductsAsAdmin :: SearchProductsAsAdmin -> TestTree
requestSearchProductsAsAdmin = req
    "SearchProductsAsAdmin"
    "fixture/SearchProductsAsAdmin.yaml"

requestDeletePortfolio :: DeletePortfolio -> TestTree
requestDeletePortfolio = req
    "DeletePortfolio"
    "fixture/DeletePortfolio.yaml"

requestUpdatePortfolio :: UpdatePortfolio -> TestTree
requestUpdatePortfolio = req
    "UpdatePortfolio"
    "fixture/UpdatePortfolio.yaml"

requestListPortfoliosForProduct :: ListPortfoliosForProduct -> TestTree
requestListPortfoliosForProduct = req
    "ListPortfoliosForProduct"
    "fixture/ListPortfoliosForProduct.yaml"

requestDescribeProductAsAdmin :: DescribeProductAsAdmin -> TestTree
requestDescribeProductAsAdmin = req
    "DescribeProductAsAdmin"
    "fixture/DescribeProductAsAdmin.yaml"

requestDescribeProvisioningParameters :: DescribeProvisioningParameters -> TestTree
requestDescribeProvisioningParameters = req
    "DescribeProvisioningParameters"
    "fixture/DescribeProvisioningParameters.yaml"

requestAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolio -> TestTree
requestAssociatePrincipalWithPortfolio = req
    "AssociatePrincipalWithPortfolio"
    "fixture/AssociatePrincipalWithPortfolio.yaml"

requestDescribeProvisionedProduct :: DescribeProvisionedProduct -> TestTree
requestDescribeProvisionedProduct = req
    "DescribeProvisionedProduct"
    "fixture/DescribeProvisionedProduct.yaml"

requestCopyProduct :: CopyProduct -> TestTree
requestCopyProduct = req
    "CopyProduct"
    "fixture/CopyProduct.yaml"

requestUpdateProvisioningArtifact :: UpdateProvisioningArtifact -> TestTree
requestUpdateProvisioningArtifact = req
    "UpdateProvisioningArtifact"
    "fixture/UpdateProvisioningArtifact.yaml"

requestDeletePortfolioShare :: DeletePortfolioShare -> TestTree
requestDeletePortfolioShare = req
    "DeletePortfolioShare"
    "fixture/DeletePortfolioShare.yaml"

requestDeleteProvisioningArtifact :: DeleteProvisioningArtifact -> TestTree
requestDeleteProvisioningArtifact = req
    "DeleteProvisioningArtifact"
    "fixture/DeleteProvisioningArtifact.yaml"

requestCreatePortfolio :: CreatePortfolio -> TestTree
requestCreatePortfolio = req
    "CreatePortfolio"
    "fixture/CreatePortfolio.yaml"

requestListLaunchPaths :: ListLaunchPaths -> TestTree
requestListLaunchPaths = req
    "ListLaunchPaths"
    "fixture/ListLaunchPaths.yaml"

requestListResourcesForTagOption :: ListResourcesForTagOption -> TestTree
requestListResourcesForTagOption = req
    "ListResourcesForTagOption"
    "fixture/ListResourcesForTagOption.yaml"

-- Responses

responseDeleteConstraint :: DeleteConstraintResponse -> TestTree
responseDeleteConstraint = res
    "DeleteConstraintResponse"
    "fixture/DeleteConstraintResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteConstraint)

responseUpdateConstraint :: UpdateConstraintResponse -> TestTree
responseUpdateConstraint = res
    "UpdateConstraintResponse"
    "fixture/UpdateConstraintResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateConstraint)

responseCreateProvisionedProductPlan :: CreateProvisionedProductPlanResponse -> TestTree
responseCreateProvisionedProductPlan = res
    "CreateProvisionedProductPlanResponse"
    "fixture/CreateProvisionedProductPlanResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateProvisionedProductPlan)

responseCreateProduct :: CreateProductResponse -> TestTree
responseCreateProduct = res
    "CreateProductResponse"
    "fixture/CreateProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateProduct)

responseDescribeCopyProductStatus :: DescribeCopyProductStatusResponse -> TestTree
responseDescribeCopyProductStatus = res
    "DescribeCopyProductStatusResponse"
    "fixture/DescribeCopyProductStatusResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeCopyProductStatus)

responseTerminateProvisionedProduct :: TerminateProvisionedProductResponse -> TestTree
responseTerminateProvisionedProduct = res
    "TerminateProvisionedProductResponse"
    "fixture/TerminateProvisionedProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy TerminateProvisionedProduct)

responseUpdateProvisionedProduct :: UpdateProvisionedProductResponse -> TestTree
responseUpdateProvisionedProduct = res
    "UpdateProvisionedProductResponse"
    "fixture/UpdateProvisionedProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateProvisionedProduct)

responseDescribeProvisioningArtifact :: DescribeProvisioningArtifactResponse -> TestTree
responseDescribeProvisioningArtifact = res
    "DescribeProvisioningArtifactResponse"
    "fixture/DescribeProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProvisioningArtifact)

responseListRecordHistory :: ListRecordHistoryResponse -> TestTree
responseListRecordHistory = res
    "ListRecordHistoryResponse"
    "fixture/ListRecordHistoryResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListRecordHistory)

responseDescribeProvisionedProductPlan :: DescribeProvisionedProductPlanResponse -> TestTree
responseDescribeProvisionedProductPlan = res
    "DescribeProvisionedProductPlanResponse"
    "fixture/DescribeProvisionedProductPlanResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProvisionedProductPlan)

responseAssociateTagOptionWithResource :: AssociateTagOptionWithResourceResponse -> TestTree
responseAssociateTagOptionWithResource = res
    "AssociateTagOptionWithResourceResponse"
    "fixture/AssociateTagOptionWithResourceResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AssociateTagOptionWithResource)

responseCreateTagOption :: CreateTagOptionResponse -> TestTree
responseCreateTagOption = res
    "CreateTagOptionResponse"
    "fixture/CreateTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateTagOption)

responseDisassociateProductFromPortfolio :: DisassociateProductFromPortfolioResponse -> TestTree
responseDisassociateProductFromPortfolio = res
    "DisassociateProductFromPortfolioResponse"
    "fixture/DisassociateProductFromPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DisassociateProductFromPortfolio)

responseListConstraintsForPortfolio :: ListConstraintsForPortfolioResponse -> TestTree
responseListConstraintsForPortfolio = res
    "ListConstraintsForPortfolioResponse"
    "fixture/ListConstraintsForPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListConstraintsForPortfolio)

responseDescribeRecord :: DescribeRecordResponse -> TestTree
responseDescribeRecord = res
    "DescribeRecordResponse"
    "fixture/DescribeRecordResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeRecord)

responseDescribeConstraint :: DescribeConstraintResponse -> TestTree
responseDescribeConstraint = res
    "DescribeConstraintResponse"
    "fixture/DescribeConstraintResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeConstraint)

responseCreateProvisioningArtifact :: CreateProvisioningArtifactResponse -> TestTree
responseCreateProvisioningArtifact = res
    "CreateProvisioningArtifactResponse"
    "fixture/CreateProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateProvisioningArtifact)

responseListPortfolios :: ListPortfoliosResponse -> TestTree
responseListPortfolios = res
    "ListPortfoliosResponse"
    "fixture/ListPortfoliosResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListPortfolios)

responseDescribeProductView :: DescribeProductViewResponse -> TestTree
responseDescribeProductView = res
    "DescribeProductViewResponse"
    "fixture/DescribeProductViewResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProductView)

responseCreatePortfolioShare :: CreatePortfolioShareResponse -> TestTree
responseCreatePortfolioShare = res
    "CreatePortfolioShareResponse"
    "fixture/CreatePortfolioShareResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreatePortfolioShare)

responseListProvisioningArtifacts :: ListProvisioningArtifactsResponse -> TestTree
responseListProvisioningArtifacts = res
    "ListProvisioningArtifactsResponse"
    "fixture/ListProvisioningArtifactsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListProvisioningArtifacts)

responseSearchProducts :: SearchProductsResponse -> TestTree
responseSearchProducts = res
    "SearchProductsResponse"
    "fixture/SearchProductsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy SearchProducts)

responseSearchProvisionedProducts :: SearchProvisionedProductsResponse -> TestTree
responseSearchProvisionedProducts = res
    "SearchProvisionedProductsResponse"
    "fixture/SearchProvisionedProductsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy SearchProvisionedProducts)

responseDescribeProduct :: DescribeProductResponse -> TestTree
responseDescribeProduct = res
    "DescribeProductResponse"
    "fixture/DescribeProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProduct)

responseDeleteProvisionedProductPlan :: DeleteProvisionedProductPlanResponse -> TestTree
responseDeleteProvisionedProductPlan = res
    "DeleteProvisionedProductPlanResponse"
    "fixture/DeleteProvisionedProductPlanResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteProvisionedProductPlan)

responseCreateConstraint :: CreateConstraintResponse -> TestTree
responseCreateConstraint = res
    "CreateConstraintResponse"
    "fixture/CreateConstraintResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreateConstraint)

responseListProvisionedProductPlans :: ListProvisionedProductPlansResponse -> TestTree
responseListProvisionedProductPlans = res
    "ListProvisionedProductPlansResponse"
    "fixture/ListProvisionedProductPlansResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListProvisionedProductPlans)

responseListPortfolioAccess :: ListPortfolioAccessResponse -> TestTree
responseListPortfolioAccess = res
    "ListPortfolioAccessResponse"
    "fixture/ListPortfolioAccessResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListPortfolioAccess)

responseDisassociatePrincipalFromPortfolio :: DisassociatePrincipalFromPortfolioResponse -> TestTree
responseDisassociatePrincipalFromPortfolio = res
    "DisassociatePrincipalFromPortfolioResponse"
    "fixture/DisassociatePrincipalFromPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DisassociatePrincipalFromPortfolio)

responseDescribeTagOption :: DescribeTagOptionResponse -> TestTree
responseDescribeTagOption = res
    "DescribeTagOptionResponse"
    "fixture/DescribeTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeTagOption)

responseDisassociateTagOptionFromResource :: DisassociateTagOptionFromResourceResponse -> TestTree
responseDisassociateTagOptionFromResource = res
    "DisassociateTagOptionFromResourceResponse"
    "fixture/DisassociateTagOptionFromResourceResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DisassociateTagOptionFromResource)

responseDescribePortfolio :: DescribePortfolioResponse -> TestTree
responseDescribePortfolio = res
    "DescribePortfolioResponse"
    "fixture/DescribePortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribePortfolio)

responseAssociateProductWithPortfolio :: AssociateProductWithPortfolioResponse -> TestTree
responseAssociateProductWithPortfolio = res
    "AssociateProductWithPortfolioResponse"
    "fixture/AssociateProductWithPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AssociateProductWithPortfolio)

responseListAcceptedPortfolioShares :: ListAcceptedPortfolioSharesResponse -> TestTree
responseListAcceptedPortfolioShares = res
    "ListAcceptedPortfolioSharesResponse"
    "fixture/ListAcceptedPortfolioSharesResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListAcceptedPortfolioShares)

responseExecuteProvisionedProductPlan :: ExecuteProvisionedProductPlanResponse -> TestTree
responseExecuteProvisionedProductPlan = res
    "ExecuteProvisionedProductPlanResponse"
    "fixture/ExecuteProvisionedProductPlanResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ExecuteProvisionedProductPlan)

responseAcceptPortfolioShare :: AcceptPortfolioShareResponse -> TestTree
responseAcceptPortfolioShare = res
    "AcceptPortfolioShareResponse"
    "fixture/AcceptPortfolioShareResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AcceptPortfolioShare)

responseScanProvisionedProducts :: ScanProvisionedProductsResponse -> TestTree
responseScanProvisionedProducts = res
    "ScanProvisionedProductsResponse"
    "fixture/ScanProvisionedProductsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ScanProvisionedProducts)

responseListPrincipalsForPortfolio :: ListPrincipalsForPortfolioResponse -> TestTree
responseListPrincipalsForPortfolio = res
    "ListPrincipalsForPortfolioResponse"
    "fixture/ListPrincipalsForPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListPrincipalsForPortfolio)

responseDeleteProduct :: DeleteProductResponse -> TestTree
responseDeleteProduct = res
    "DeleteProductResponse"
    "fixture/DeleteProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteProduct)

responseUpdateProduct :: UpdateProductResponse -> TestTree
responseUpdateProduct = res
    "UpdateProductResponse"
    "fixture/UpdateProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateProduct)

responseProvisionProduct :: ProvisionProductResponse -> TestTree
responseProvisionProduct = res
    "ProvisionProductResponse"
    "fixture/ProvisionProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ProvisionProduct)

responseRejectPortfolioShare :: RejectPortfolioShareResponse -> TestTree
responseRejectPortfolioShare = res
    "RejectPortfolioShareResponse"
    "fixture/RejectPortfolioShareResponse.proto"
    serviceCatalog
    (Proxy :: Proxy RejectPortfolioShare)

responseDeleteTagOption :: DeleteTagOptionResponse -> TestTree
responseDeleteTagOption = res
    "DeleteTagOptionResponse"
    "fixture/DeleteTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteTagOption)

responseUpdateTagOption :: UpdateTagOptionResponse -> TestTree
responseUpdateTagOption = res
    "UpdateTagOptionResponse"
    "fixture/UpdateTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateTagOption)

responseListTagOptions :: ListTagOptionsResponse -> TestTree
responseListTagOptions = res
    "ListTagOptionsResponse"
    "fixture/ListTagOptionsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListTagOptions)

responseSearchProductsAsAdmin :: SearchProductsAsAdminResponse -> TestTree
responseSearchProductsAsAdmin = res
    "SearchProductsAsAdminResponse"
    "fixture/SearchProductsAsAdminResponse.proto"
    serviceCatalog
    (Proxy :: Proxy SearchProductsAsAdmin)

responseDeletePortfolio :: DeletePortfolioResponse -> TestTree
responseDeletePortfolio = res
    "DeletePortfolioResponse"
    "fixture/DeletePortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeletePortfolio)

responseUpdatePortfolio :: UpdatePortfolioResponse -> TestTree
responseUpdatePortfolio = res
    "UpdatePortfolioResponse"
    "fixture/UpdatePortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdatePortfolio)

responseListPortfoliosForProduct :: ListPortfoliosForProductResponse -> TestTree
responseListPortfoliosForProduct = res
    "ListPortfoliosForProductResponse"
    "fixture/ListPortfoliosForProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListPortfoliosForProduct)

responseDescribeProductAsAdmin :: DescribeProductAsAdminResponse -> TestTree
responseDescribeProductAsAdmin = res
    "DescribeProductAsAdminResponse"
    "fixture/DescribeProductAsAdminResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProductAsAdmin)

responseDescribeProvisioningParameters :: DescribeProvisioningParametersResponse -> TestTree
responseDescribeProvisioningParameters = res
    "DescribeProvisioningParametersResponse"
    "fixture/DescribeProvisioningParametersResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProvisioningParameters)

responseAssociatePrincipalWithPortfolio :: AssociatePrincipalWithPortfolioResponse -> TestTree
responseAssociatePrincipalWithPortfolio = res
    "AssociatePrincipalWithPortfolioResponse"
    "fixture/AssociatePrincipalWithPortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy AssociatePrincipalWithPortfolio)

responseDescribeProvisionedProduct :: DescribeProvisionedProductResponse -> TestTree
responseDescribeProvisionedProduct = res
    "DescribeProvisionedProductResponse"
    "fixture/DescribeProvisionedProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProvisionedProduct)

responseCopyProduct :: CopyProductResponse -> TestTree
responseCopyProduct = res
    "CopyProductResponse"
    "fixture/CopyProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CopyProduct)

responseUpdateProvisioningArtifact :: UpdateProvisioningArtifactResponse -> TestTree
responseUpdateProvisioningArtifact = res
    "UpdateProvisioningArtifactResponse"
    "fixture/UpdateProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy UpdateProvisioningArtifact)

responseDeletePortfolioShare :: DeletePortfolioShareResponse -> TestTree
responseDeletePortfolioShare = res
    "DeletePortfolioShareResponse"
    "fixture/DeletePortfolioShareResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeletePortfolioShare)

responseDeleteProvisioningArtifact :: DeleteProvisioningArtifactResponse -> TestTree
responseDeleteProvisioningArtifact = res
    "DeleteProvisioningArtifactResponse"
    "fixture/DeleteProvisioningArtifactResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DeleteProvisioningArtifact)

responseCreatePortfolio :: CreatePortfolioResponse -> TestTree
responseCreatePortfolio = res
    "CreatePortfolioResponse"
    "fixture/CreatePortfolioResponse.proto"
    serviceCatalog
    (Proxy :: Proxy CreatePortfolio)

responseListLaunchPaths :: ListLaunchPathsResponse -> TestTree
responseListLaunchPaths = res
    "ListLaunchPathsResponse"
    "fixture/ListLaunchPathsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListLaunchPaths)

responseListResourcesForTagOption :: ListResourcesForTagOptionResponse -> TestTree
responseListResourcesForTagOption = res
    "ListResourcesForTagOptionResponse"
    "fixture/ListResourcesForTagOptionResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListResourcesForTagOption)
