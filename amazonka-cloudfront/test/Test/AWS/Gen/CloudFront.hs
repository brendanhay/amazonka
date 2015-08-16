{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFront
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudFront where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudFront
import Test.AWS.CloudFront.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDeleteStreamingDistribution $
--             deleteStreamingDistribution
--
--         , testUpdateStreamingDistribution $
--             updateStreamingDistribution
--
--         , testCreateDistribution $
--             createDistribution
--
--         , testGetDistributionConfig $
--             getDistributionConfig
--
--         , testGetDistribution $
--             getDistribution
--
--         , testDeleteCloudFrontOriginAccessIdentity $
--             deleteCloudFrontOriginAccessIdentity
--
--         , testUpdateCloudFrontOriginAccessIdentity $
--             updateCloudFrontOriginAccessIdentity
--
--         , testListStreamingDistributions $
--             listStreamingDistributions
--
--         , testGetStreamingDistributionConfig $
--             getStreamingDistributionConfig
--
--         , testGetCloudFrontOriginAccessIdentityConfig $
--             getCloudFrontOriginAccessIdentityConfig
--
--         , testCreateStreamingDistribution $
--             createStreamingDistribution
--
--         , testCreateCloudFrontOriginAccessIdentity $
--             createCloudFrontOriginAccessIdentity
--
--         , testListCloudFrontOriginAccessIdentities $
--             listCloudFrontOriginAccessIdentities
--
--         , testGetInvalidation $
--             getInvalidation
--
--         , testListInvalidations $
--             listInvalidations
--
--         , testGetStreamingDistribution $
--             getStreamingDistribution
--
--         , testGetCloudFrontOriginAccessIdentity $
--             getCloudFrontOriginAccessIdentity
--
--         , testCreateInvalidation $
--             createInvalidation
--
--         , testUpdateDistribution $
--             updateDistribution
--
--         , testDeleteDistribution $
--             deleteDistribution
--
--         , testListDistributions $
--             listDistributions
--
--           ]

--     , testGroup "response"
--         [ testDeleteStreamingDistributionResponse $
--             deleteStreamingDistributionResponse
--
--         , testUpdateStreamingDistributionResponse $
--             updateStreamingDistributionResponse
--
--         , testCreateDistributionResponse $
--             createDistributionResponse
--
--         , testGetDistributionConfigResponse $
--             getDistributionConfigResponse
--
--         , testGetDistributionResponse $
--             getDistributionResponse
--
--         , testDeleteCloudFrontOriginAccessIdentityResponse $
--             deleteCloudFrontOriginAccessIdentityResponse
--
--         , testUpdateCloudFrontOriginAccessIdentityResponse $
--             updateCloudFrontOriginAccessIdentityResponse
--
--         , testListStreamingDistributionsResponse $
--             listStreamingDistributionsResponse
--
--         , testGetStreamingDistributionConfigResponse $
--             getStreamingDistributionConfigResponse
--
--         , testGetCloudFrontOriginAccessIdentityConfigResponse $
--             getCloudFrontOriginAccessIdentityConfigResponse
--
--         , testCreateStreamingDistributionResponse $
--             createStreamingDistributionResponse
--
--         , testCreateCloudFrontOriginAccessIdentityResponse $
--             createCloudFrontOriginAccessIdentityResponse
--
--         , testListCloudFrontOriginAccessIdentitiesResponse $
--             listCloudFrontOriginAccessIdentitiesResponse
--
--         , testGetInvalidationResponse $
--             getInvalidationResponse
--
--         , testListInvalidationsResponse $
--             listInvalidationsResponse
--
--         , testGetStreamingDistributionResponse $
--             getStreamingDistributionResponse
--
--         , testGetCloudFrontOriginAccessIdentityResponse $
--             getCloudFrontOriginAccessIdentityResponse
--
--         , testCreateInvalidationResponse $
--             createInvalidationResponse
--
--         , testUpdateDistributionResponse $
--             updateDistributionResponse
--
--         , testDeleteDistributionResponse $
--             deleteDistributionResponse
--
--         , testListDistributionsResponse $
--             listDistributionsResponse
--
--           ]
--     ]

-- Requests

testDeleteStreamingDistribution :: DeleteStreamingDistribution -> TestTree
testDeleteStreamingDistribution = req
    "DeleteStreamingDistribution"
    "fixture/DeleteStreamingDistribution"

testUpdateStreamingDistribution :: UpdateStreamingDistribution -> TestTree
testUpdateStreamingDistribution = req
    "UpdateStreamingDistribution"
    "fixture/UpdateStreamingDistribution"

testCreateDistribution :: CreateDistribution -> TestTree
testCreateDistribution = req
    "CreateDistribution"
    "fixture/CreateDistribution"

testGetDistributionConfig :: GetDistributionConfig -> TestTree
testGetDistributionConfig = req
    "GetDistributionConfig"
    "fixture/GetDistributionConfig"

testGetDistribution :: GetDistribution -> TestTree
testGetDistribution = req
    "GetDistribution"
    "fixture/GetDistribution"

testDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentity -> TestTree
testDeleteCloudFrontOriginAccessIdentity = req
    "DeleteCloudFrontOriginAccessIdentity"
    "fixture/DeleteCloudFrontOriginAccessIdentity"

testUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentity -> TestTree
testUpdateCloudFrontOriginAccessIdentity = req
    "UpdateCloudFrontOriginAccessIdentity"
    "fixture/UpdateCloudFrontOriginAccessIdentity"

testListStreamingDistributions :: ListStreamingDistributions -> TestTree
testListStreamingDistributions = req
    "ListStreamingDistributions"
    "fixture/ListStreamingDistributions"

testGetStreamingDistributionConfig :: GetStreamingDistributionConfig -> TestTree
testGetStreamingDistributionConfig = req
    "GetStreamingDistributionConfig"
    "fixture/GetStreamingDistributionConfig"

testGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfig -> TestTree
testGetCloudFrontOriginAccessIdentityConfig = req
    "GetCloudFrontOriginAccessIdentityConfig"
    "fixture/GetCloudFrontOriginAccessIdentityConfig"

testCreateStreamingDistribution :: CreateStreamingDistribution -> TestTree
testCreateStreamingDistribution = req
    "CreateStreamingDistribution"
    "fixture/CreateStreamingDistribution"

testCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentity -> TestTree
testCreateCloudFrontOriginAccessIdentity = req
    "CreateCloudFrontOriginAccessIdentity"
    "fixture/CreateCloudFrontOriginAccessIdentity"

testListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities -> TestTree
testListCloudFrontOriginAccessIdentities = req
    "ListCloudFrontOriginAccessIdentities"
    "fixture/ListCloudFrontOriginAccessIdentities"

testGetInvalidation :: GetInvalidation -> TestTree
testGetInvalidation = req
    "GetInvalidation"
    "fixture/GetInvalidation"

testListInvalidations :: ListInvalidations -> TestTree
testListInvalidations = req
    "ListInvalidations"
    "fixture/ListInvalidations"

testGetStreamingDistribution :: GetStreamingDistribution -> TestTree
testGetStreamingDistribution = req
    "GetStreamingDistribution"
    "fixture/GetStreamingDistribution"

testGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentity -> TestTree
testGetCloudFrontOriginAccessIdentity = req
    "GetCloudFrontOriginAccessIdentity"
    "fixture/GetCloudFrontOriginAccessIdentity"

testCreateInvalidation :: CreateInvalidation -> TestTree
testCreateInvalidation = req
    "CreateInvalidation"
    "fixture/CreateInvalidation"

testUpdateDistribution :: UpdateDistribution -> TestTree
testUpdateDistribution = req
    "UpdateDistribution"
    "fixture/UpdateDistribution"

testDeleteDistribution :: DeleteDistribution -> TestTree
testDeleteDistribution = req
    "DeleteDistribution"
    "fixture/DeleteDistribution"

testListDistributions :: ListDistributions -> TestTree
testListDistributions = req
    "ListDistributions"
    "fixture/ListDistributions"

-- Responses

testDeleteStreamingDistributionResponse :: DeleteStreamingDistributionResponse -> TestTree
testDeleteStreamingDistributionResponse = res
    "DeleteStreamingDistributionResponse"
    "fixture/DeleteStreamingDistributionResponse"
    (Proxy :: Proxy DeleteStreamingDistribution)

testUpdateStreamingDistributionResponse :: UpdateStreamingDistributionResponse -> TestTree
testUpdateStreamingDistributionResponse = res
    "UpdateStreamingDistributionResponse"
    "fixture/UpdateStreamingDistributionResponse"
    (Proxy :: Proxy UpdateStreamingDistribution)

testCreateDistributionResponse :: CreateDistributionResponse -> TestTree
testCreateDistributionResponse = res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse"
    (Proxy :: Proxy CreateDistribution)

testGetDistributionConfigResponse :: GetDistributionConfigResponse -> TestTree
testGetDistributionConfigResponse = res
    "GetDistributionConfigResponse"
    "fixture/GetDistributionConfigResponse"
    (Proxy :: Proxy GetDistributionConfig)

testGetDistributionResponse :: GetDistributionResponse -> TestTree
testGetDistributionResponse = res
    "GetDistributionResponse"
    "fixture/GetDistributionResponse"
    (Proxy :: Proxy GetDistribution)

testDeleteCloudFrontOriginAccessIdentityResponse :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
testDeleteCloudFrontOriginAccessIdentityResponse = res
    "DeleteCloudFrontOriginAccessIdentityResponse"
    "fixture/DeleteCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy DeleteCloudFrontOriginAccessIdentity)

testUpdateCloudFrontOriginAccessIdentityResponse :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
testUpdateCloudFrontOriginAccessIdentityResponse = res
    "UpdateCloudFrontOriginAccessIdentityResponse"
    "fixture/UpdateCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy UpdateCloudFrontOriginAccessIdentity)

testListStreamingDistributionsResponse :: ListStreamingDistributionsResponse -> TestTree
testListStreamingDistributionsResponse = res
    "ListStreamingDistributionsResponse"
    "fixture/ListStreamingDistributionsResponse"
    (Proxy :: Proxy ListStreamingDistributions)

testGetStreamingDistributionConfigResponse :: GetStreamingDistributionConfigResponse -> TestTree
testGetStreamingDistributionConfigResponse = res
    "GetStreamingDistributionConfigResponse"
    "fixture/GetStreamingDistributionConfigResponse"
    (Proxy :: Proxy GetStreamingDistributionConfig)

testGetCloudFrontOriginAccessIdentityConfigResponse :: GetCloudFrontOriginAccessIdentityConfigResponse -> TestTree
testGetCloudFrontOriginAccessIdentityConfigResponse = res
    "GetCloudFrontOriginAccessIdentityConfigResponse"
    "fixture/GetCloudFrontOriginAccessIdentityConfigResponse"
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentityConfig)

testCreateStreamingDistributionResponse :: CreateStreamingDistributionResponse -> TestTree
testCreateStreamingDistributionResponse = res
    "CreateStreamingDistributionResponse"
    "fixture/CreateStreamingDistributionResponse"
    (Proxy :: Proxy CreateStreamingDistribution)

testCreateCloudFrontOriginAccessIdentityResponse :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
testCreateCloudFrontOriginAccessIdentityResponse = res
    "CreateCloudFrontOriginAccessIdentityResponse"
    "fixture/CreateCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy CreateCloudFrontOriginAccessIdentity)

testListCloudFrontOriginAccessIdentitiesResponse :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
testListCloudFrontOriginAccessIdentitiesResponse = res
    "ListCloudFrontOriginAccessIdentitiesResponse"
    "fixture/ListCloudFrontOriginAccessIdentitiesResponse"
    (Proxy :: Proxy ListCloudFrontOriginAccessIdentities)

testGetInvalidationResponse :: GetInvalidationResponse -> TestTree
testGetInvalidationResponse = res
    "GetInvalidationResponse"
    "fixture/GetInvalidationResponse"
    (Proxy :: Proxy GetInvalidation)

testListInvalidationsResponse :: ListInvalidationsResponse -> TestTree
testListInvalidationsResponse = res
    "ListInvalidationsResponse"
    "fixture/ListInvalidationsResponse"
    (Proxy :: Proxy ListInvalidations)

testGetStreamingDistributionResponse :: GetStreamingDistributionResponse -> TestTree
testGetStreamingDistributionResponse = res
    "GetStreamingDistributionResponse"
    "fixture/GetStreamingDistributionResponse"
    (Proxy :: Proxy GetStreamingDistribution)

testGetCloudFrontOriginAccessIdentityResponse :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
testGetCloudFrontOriginAccessIdentityResponse = res
    "GetCloudFrontOriginAccessIdentityResponse"
    "fixture/GetCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentity)

testCreateInvalidationResponse :: CreateInvalidationResponse -> TestTree
testCreateInvalidationResponse = res
    "CreateInvalidationResponse"
    "fixture/CreateInvalidationResponse"
    (Proxy :: Proxy CreateInvalidation)

testUpdateDistributionResponse :: UpdateDistributionResponse -> TestTree
testUpdateDistributionResponse = res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse"
    (Proxy :: Proxy UpdateDistribution)

testDeleteDistributionResponse :: DeleteDistributionResponse -> TestTree
testDeleteDistributionResponse = res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse"
    (Proxy :: Proxy DeleteDistribution)

testListDistributionsResponse :: ListDistributionsResponse -> TestTree
testListDistributionsResponse = res
    "ListDistributionsResponse"
    "fixture/ListDistributionsResponse"
    (Proxy :: Proxy ListDistributions)
