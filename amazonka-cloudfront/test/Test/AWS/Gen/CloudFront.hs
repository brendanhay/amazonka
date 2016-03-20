{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFront
-- Copyright   : (c) 2013-2016 Brendan Hay
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
--         , testUpdateCloudFrontOriginAccessIdentity $
--             updateCloudFrontOriginAccessIdentity
--
--         , testDeleteCloudFrontOriginAccessIdentity $
--             deleteCloudFrontOriginAccessIdentity
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
--         , testCreateInvalidation $
--             createInvalidation
--
--         , testGetCloudFrontOriginAccessIdentity $
--             getCloudFrontOriginAccessIdentity
--
--         , testGetStreamingDistribution $
--             getStreamingDistribution
--
--         , testUpdateDistribution $
--             updateDistribution
--
--         , testDeleteDistribution $
--             deleteDistribution
--
--         , testListDistributionsByWebACLId $
--             listDistributionsByWebACLId
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
--         , testUpdateCloudFrontOriginAccessIdentityResponse $
--             updateCloudFrontOriginAccessIdentityResponse
--
--         , testDeleteCloudFrontOriginAccessIdentityResponse $
--             deleteCloudFrontOriginAccessIdentityResponse
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
--         , testCreateInvalidationResponse $
--             createInvalidationResponse
--
--         , testGetCloudFrontOriginAccessIdentityResponse $
--             getCloudFrontOriginAccessIdentityResponse
--
--         , testGetStreamingDistributionResponse $
--             getStreamingDistributionResponse
--
--         , testUpdateDistributionResponse $
--             updateDistributionResponse
--
--         , testDeleteDistributionResponse $
--             deleteDistributionResponse
--
--         , testListDistributionsByWebACLIdResponse $
--             listDistributionsByWebACLIdResponse
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
    "fixture/DeleteStreamingDistribution.yaml"

testUpdateStreamingDistribution :: UpdateStreamingDistribution -> TestTree
testUpdateStreamingDistribution = req
    "UpdateStreamingDistribution"
    "fixture/UpdateStreamingDistribution.yaml"

testCreateDistribution :: CreateDistribution -> TestTree
testCreateDistribution = req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

testGetDistributionConfig :: GetDistributionConfig -> TestTree
testGetDistributionConfig = req
    "GetDistributionConfig"
    "fixture/GetDistributionConfig.yaml"

testGetDistribution :: GetDistribution -> TestTree
testGetDistribution = req
    "GetDistribution"
    "fixture/GetDistribution.yaml"

testUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentity -> TestTree
testUpdateCloudFrontOriginAccessIdentity = req
    "UpdateCloudFrontOriginAccessIdentity"
    "fixture/UpdateCloudFrontOriginAccessIdentity.yaml"

testDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentity -> TestTree
testDeleteCloudFrontOriginAccessIdentity = req
    "DeleteCloudFrontOriginAccessIdentity"
    "fixture/DeleteCloudFrontOriginAccessIdentity.yaml"

testListStreamingDistributions :: ListStreamingDistributions -> TestTree
testListStreamingDistributions = req
    "ListStreamingDistributions"
    "fixture/ListStreamingDistributions.yaml"

testGetStreamingDistributionConfig :: GetStreamingDistributionConfig -> TestTree
testGetStreamingDistributionConfig = req
    "GetStreamingDistributionConfig"
    "fixture/GetStreamingDistributionConfig.yaml"

testGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfig -> TestTree
testGetCloudFrontOriginAccessIdentityConfig = req
    "GetCloudFrontOriginAccessIdentityConfig"
    "fixture/GetCloudFrontOriginAccessIdentityConfig.yaml"

testCreateStreamingDistribution :: CreateStreamingDistribution -> TestTree
testCreateStreamingDistribution = req
    "CreateStreamingDistribution"
    "fixture/CreateStreamingDistribution.yaml"

testCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentity -> TestTree
testCreateCloudFrontOriginAccessIdentity = req
    "CreateCloudFrontOriginAccessIdentity"
    "fixture/CreateCloudFrontOriginAccessIdentity.yaml"

testListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities -> TestTree
testListCloudFrontOriginAccessIdentities = req
    "ListCloudFrontOriginAccessIdentities"
    "fixture/ListCloudFrontOriginAccessIdentities.yaml"

testGetInvalidation :: GetInvalidation -> TestTree
testGetInvalidation = req
    "GetInvalidation"
    "fixture/GetInvalidation.yaml"

testListInvalidations :: ListInvalidations -> TestTree
testListInvalidations = req
    "ListInvalidations"
    "fixture/ListInvalidations.yaml"

testCreateInvalidation :: CreateInvalidation -> TestTree
testCreateInvalidation = req
    "CreateInvalidation"
    "fixture/CreateInvalidation.yaml"

testGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentity -> TestTree
testGetCloudFrontOriginAccessIdentity = req
    "GetCloudFrontOriginAccessIdentity"
    "fixture/GetCloudFrontOriginAccessIdentity.yaml"

testGetStreamingDistribution :: GetStreamingDistribution -> TestTree
testGetStreamingDistribution = req
    "GetStreamingDistribution"
    "fixture/GetStreamingDistribution.yaml"

testUpdateDistribution :: UpdateDistribution -> TestTree
testUpdateDistribution = req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

testDeleteDistribution :: DeleteDistribution -> TestTree
testDeleteDistribution = req
    "DeleteDistribution"
    "fixture/DeleteDistribution.yaml"

testListDistributionsByWebACLId :: ListDistributionsByWebACLId -> TestTree
testListDistributionsByWebACLId = req
    "ListDistributionsByWebACLId"
    "fixture/ListDistributionsByWebACLId.yaml"

testListDistributions :: ListDistributions -> TestTree
testListDistributions = req
    "ListDistributions"
    "fixture/ListDistributions.yaml"

-- Responses

testDeleteStreamingDistributionResponse :: DeleteStreamingDistributionResponse -> TestTree
testDeleteStreamingDistributionResponse = res
    "DeleteStreamingDistributionResponse"
    "fixture/DeleteStreamingDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy DeleteStreamingDistribution)

testUpdateStreamingDistributionResponse :: UpdateStreamingDistributionResponse -> TestTree
testUpdateStreamingDistributionResponse = res
    "UpdateStreamingDistributionResponse"
    "fixture/UpdateStreamingDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy UpdateStreamingDistribution)

testCreateDistributionResponse :: CreateDistributionResponse -> TestTree
testCreateDistributionResponse = res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateDistribution)

testGetDistributionConfigResponse :: GetDistributionConfigResponse -> TestTree
testGetDistributionConfigResponse = res
    "GetDistributionConfigResponse"
    "fixture/GetDistributionConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy GetDistributionConfig)

testGetDistributionResponse :: GetDistributionResponse -> TestTree
testGetDistributionResponse = res
    "GetDistributionResponse"
    "fixture/GetDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy GetDistribution)

testUpdateCloudFrontOriginAccessIdentityResponse :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
testUpdateCloudFrontOriginAccessIdentityResponse = res
    "UpdateCloudFrontOriginAccessIdentityResponse"
    "fixture/UpdateCloudFrontOriginAccessIdentityResponse.proto"
    cloudFront
    (Proxy :: Proxy UpdateCloudFrontOriginAccessIdentity)

testDeleteCloudFrontOriginAccessIdentityResponse :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
testDeleteCloudFrontOriginAccessIdentityResponse = res
    "DeleteCloudFrontOriginAccessIdentityResponse"
    "fixture/DeleteCloudFrontOriginAccessIdentityResponse.proto"
    cloudFront
    (Proxy :: Proxy DeleteCloudFrontOriginAccessIdentity)

testListStreamingDistributionsResponse :: ListStreamingDistributionsResponse -> TestTree
testListStreamingDistributionsResponse = res
    "ListStreamingDistributionsResponse"
    "fixture/ListStreamingDistributionsResponse.proto"
    cloudFront
    (Proxy :: Proxy ListStreamingDistributions)

testGetStreamingDistributionConfigResponse :: GetStreamingDistributionConfigResponse -> TestTree
testGetStreamingDistributionConfigResponse = res
    "GetStreamingDistributionConfigResponse"
    "fixture/GetStreamingDistributionConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy GetStreamingDistributionConfig)

testGetCloudFrontOriginAccessIdentityConfigResponse :: GetCloudFrontOriginAccessIdentityConfigResponse -> TestTree
testGetCloudFrontOriginAccessIdentityConfigResponse = res
    "GetCloudFrontOriginAccessIdentityConfigResponse"
    "fixture/GetCloudFrontOriginAccessIdentityConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentityConfig)

testCreateStreamingDistributionResponse :: CreateStreamingDistributionResponse -> TestTree
testCreateStreamingDistributionResponse = res
    "CreateStreamingDistributionResponse"
    "fixture/CreateStreamingDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateStreamingDistribution)

testCreateCloudFrontOriginAccessIdentityResponse :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
testCreateCloudFrontOriginAccessIdentityResponse = res
    "CreateCloudFrontOriginAccessIdentityResponse"
    "fixture/CreateCloudFrontOriginAccessIdentityResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateCloudFrontOriginAccessIdentity)

testListCloudFrontOriginAccessIdentitiesResponse :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
testListCloudFrontOriginAccessIdentitiesResponse = res
    "ListCloudFrontOriginAccessIdentitiesResponse"
    "fixture/ListCloudFrontOriginAccessIdentitiesResponse.proto"
    cloudFront
    (Proxy :: Proxy ListCloudFrontOriginAccessIdentities)

testGetInvalidationResponse :: GetInvalidationResponse -> TestTree
testGetInvalidationResponse = res
    "GetInvalidationResponse"
    "fixture/GetInvalidationResponse.proto"
    cloudFront
    (Proxy :: Proxy GetInvalidation)

testListInvalidationsResponse :: ListInvalidationsResponse -> TestTree
testListInvalidationsResponse = res
    "ListInvalidationsResponse"
    "fixture/ListInvalidationsResponse.proto"
    cloudFront
    (Proxy :: Proxy ListInvalidations)

testCreateInvalidationResponse :: CreateInvalidationResponse -> TestTree
testCreateInvalidationResponse = res
    "CreateInvalidationResponse"
    "fixture/CreateInvalidationResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateInvalidation)

testGetCloudFrontOriginAccessIdentityResponse :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
testGetCloudFrontOriginAccessIdentityResponse = res
    "GetCloudFrontOriginAccessIdentityResponse"
    "fixture/GetCloudFrontOriginAccessIdentityResponse.proto"
    cloudFront
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentity)

testGetStreamingDistributionResponse :: GetStreamingDistributionResponse -> TestTree
testGetStreamingDistributionResponse = res
    "GetStreamingDistributionResponse"
    "fixture/GetStreamingDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy GetStreamingDistribution)

testUpdateDistributionResponse :: UpdateDistributionResponse -> TestTree
testUpdateDistributionResponse = res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy UpdateDistribution)

testDeleteDistributionResponse :: DeleteDistributionResponse -> TestTree
testDeleteDistributionResponse = res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy DeleteDistribution)

testListDistributionsByWebACLIdResponse :: ListDistributionsByWebACLIdResponse -> TestTree
testListDistributionsByWebACLIdResponse = res
    "ListDistributionsByWebACLIdResponse"
    "fixture/ListDistributionsByWebACLIdResponse.proto"
    cloudFront
    (Proxy :: Proxy ListDistributionsByWebACLId)

testListDistributionsResponse :: ListDistributionsResponse -> TestTree
testListDistributionsResponse = res
    "ListDistributionsResponse"
    "fixture/ListDistributionsResponse.proto"
    cloudFront
    (Proxy :: Proxy ListDistributions)
