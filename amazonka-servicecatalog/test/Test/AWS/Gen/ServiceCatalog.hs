{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ServiceCatalog
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ServiceCatalog where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ServiceCatalog
import Test.AWS.ServiceCatalog.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestTerminateProvisionedProduct $
--             terminateProvisionedProduct
--
--         , requestUpdateProvisionedProduct $
--             updateProvisionedProduct
--
--         , requestListRecordHistory $
--             listRecordHistory
--
--         , requestDescribeRecord $
--             describeRecord
--
--         , requestDescribeProductView $
--             describeProductView
--
--         , requestSearchProducts $
--             searchProducts
--
--         , requestDescribeProduct $
--             describeProduct
--
--         , requestScanProvisionedProducts $
--             scanProvisionedProducts
--
--         , requestProvisionProduct $
--             provisionProduct
--
--         , requestDescribeProvisioningParameters $
--             describeProvisioningParameters
--
--         , requestListLaunchPaths $
--             listLaunchPaths
--
--           ]

--     , testGroup "response"
--         [ responseTerminateProvisionedProduct $
--             terminateProvisionedProductResponse
--
--         , responseUpdateProvisionedProduct $
--             updateProvisionedProductResponse
--
--         , responseListRecordHistory $
--             listRecordHistoryResponse
--
--         , responseDescribeRecord $
--             describeRecordResponse
--
--         , responseDescribeProductView $
--             describeProductViewResponse
--
--         , responseSearchProducts $
--             searchProductsResponse
--
--         , responseDescribeProduct $
--             describeProductResponse
--
--         , responseScanProvisionedProducts $
--             scanProvisionedProductsResponse
--
--         , responseProvisionProduct $
--             provisionProductResponse
--
--         , responseDescribeProvisioningParameters $
--             describeProvisioningParametersResponse
--
--         , responseListLaunchPaths $
--             listLaunchPathsResponse
--
--           ]
--     ]

-- Requests

requestTerminateProvisionedProduct :: TerminateProvisionedProduct -> TestTree
requestTerminateProvisionedProduct = req
    "TerminateProvisionedProduct"
    "fixture/TerminateProvisionedProduct.yaml"

requestUpdateProvisionedProduct :: UpdateProvisionedProduct -> TestTree
requestUpdateProvisionedProduct = req
    "UpdateProvisionedProduct"
    "fixture/UpdateProvisionedProduct.yaml"

requestListRecordHistory :: ListRecordHistory -> TestTree
requestListRecordHistory = req
    "ListRecordHistory"
    "fixture/ListRecordHistory.yaml"

requestDescribeRecord :: DescribeRecord -> TestTree
requestDescribeRecord = req
    "DescribeRecord"
    "fixture/DescribeRecord.yaml"

requestDescribeProductView :: DescribeProductView -> TestTree
requestDescribeProductView = req
    "DescribeProductView"
    "fixture/DescribeProductView.yaml"

requestSearchProducts :: SearchProducts -> TestTree
requestSearchProducts = req
    "SearchProducts"
    "fixture/SearchProducts.yaml"

requestDescribeProduct :: DescribeProduct -> TestTree
requestDescribeProduct = req
    "DescribeProduct"
    "fixture/DescribeProduct.yaml"

requestScanProvisionedProducts :: ScanProvisionedProducts -> TestTree
requestScanProvisionedProducts = req
    "ScanProvisionedProducts"
    "fixture/ScanProvisionedProducts.yaml"

requestProvisionProduct :: ProvisionProduct -> TestTree
requestProvisionProduct = req
    "ProvisionProduct"
    "fixture/ProvisionProduct.yaml"

requestDescribeProvisioningParameters :: DescribeProvisioningParameters -> TestTree
requestDescribeProvisioningParameters = req
    "DescribeProvisioningParameters"
    "fixture/DescribeProvisioningParameters.yaml"

requestListLaunchPaths :: ListLaunchPaths -> TestTree
requestListLaunchPaths = req
    "ListLaunchPaths"
    "fixture/ListLaunchPaths.yaml"

-- Responses

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

responseListRecordHistory :: ListRecordHistoryResponse -> TestTree
responseListRecordHistory = res
    "ListRecordHistoryResponse"
    "fixture/ListRecordHistoryResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListRecordHistory)

responseDescribeRecord :: DescribeRecordResponse -> TestTree
responseDescribeRecord = res
    "DescribeRecordResponse"
    "fixture/DescribeRecordResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeRecord)

responseDescribeProductView :: DescribeProductViewResponse -> TestTree
responseDescribeProductView = res
    "DescribeProductViewResponse"
    "fixture/DescribeProductViewResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProductView)

responseSearchProducts :: SearchProductsResponse -> TestTree
responseSearchProducts = res
    "SearchProductsResponse"
    "fixture/SearchProductsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy SearchProducts)

responseDescribeProduct :: DescribeProductResponse -> TestTree
responseDescribeProduct = res
    "DescribeProductResponse"
    "fixture/DescribeProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProduct)

responseScanProvisionedProducts :: ScanProvisionedProductsResponse -> TestTree
responseScanProvisionedProducts = res
    "ScanProvisionedProductsResponse"
    "fixture/ScanProvisionedProductsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ScanProvisionedProducts)

responseProvisionProduct :: ProvisionProductResponse -> TestTree
responseProvisionProduct = res
    "ProvisionProductResponse"
    "fixture/ProvisionProductResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ProvisionProduct)

responseDescribeProvisioningParameters :: DescribeProvisioningParametersResponse -> TestTree
responseDescribeProvisioningParameters = res
    "DescribeProvisioningParametersResponse"
    "fixture/DescribeProvisioningParametersResponse.proto"
    serviceCatalog
    (Proxy :: Proxy DescribeProvisioningParameters)

responseListLaunchPaths :: ListLaunchPathsResponse -> TestTree
responseListLaunchPaths = res
    "ListLaunchPathsResponse"
    "fixture/ListLaunchPathsResponse.proto"
    serviceCatalog
    (Proxy :: Proxy ListLaunchPaths)
