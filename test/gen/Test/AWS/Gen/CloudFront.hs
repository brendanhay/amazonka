-- Module      : Test.AWS.Gen.CloudFront
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.CloudFront where

import           Data.Proxy
import           Network.AWS.CloudFront
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ deleteStreamingDistributionTest $
--             deleteStreamingDistribution
--
--         , updateStreamingDistributionTest $
--             updateStreamingDistribution
--
--         , createDistributionTest $
--             createDistribution
--
--         , getDistributionConfigTest $
--             getDistributionConfig
--
--         , getDistributionTest $
--             getDistribution
--
--         , deleteCloudFrontOriginAccessIdentityTest $
--             deleteCloudFrontOriginAccessIdentity
--
--         , updateCloudFrontOriginAccessIdentityTest $
--             updateCloudFrontOriginAccessIdentity
--
--         , listStreamingDistributionsTest $
--             listStreamingDistributions
--
--         , getStreamingDistributionConfigTest $
--             getStreamingDistributionConfig
--
--         , getCloudFrontOriginAccessIdentityConfigTest $
--             getCloudFrontOriginAccessIdentityConfig
--
--         , createStreamingDistributionTest $
--             createStreamingDistribution
--
--         , createCloudFrontOriginAccessIdentityTest $
--             createCloudFrontOriginAccessIdentity
--
--         , listCloudFrontOriginAccessIdentitiesTest $
--             listCloudFrontOriginAccessIdentities
--
--         , getInvalidationTest $
--             getInvalidation
--
--         , listInvalidationsTest $
--             listInvalidations
--
--         , getStreamingDistributionTest $
--             getStreamingDistribution
--
--         , getCloudFrontOriginAccessIdentityTest $
--             getCloudFrontOriginAccessIdentity
--
--         , createInvalidationTest $
--             createInvalidation
--
--         , updateDistributionTest $
--             updateDistribution
--
--         , deleteDistributionTest $
--             deleteDistribution
--
--         , listDistributionsTest $
--             listDistributions
--
--           ]

--     , testGroup "response"
--         [ deleteStreamingDistributionResponseTest $
--             deleteStreamingDistributionResponse
--
--         , updateStreamingDistributionResponseTest $
--             updateStreamingDistributionResponse
--
--         , createDistributionResponseTest $
--             createDistributionResponse
--
--         , getDistributionConfigResponseTest $
--             getDistributionConfigResponse
--
--         , getDistributionResponseTest $
--             getDistributionResponse
--
--         , deleteCloudFrontOriginAccessIdentityResponseTest $
--             deleteCloudFrontOriginAccessIdentityResponse
--
--         , updateCloudFrontOriginAccessIdentityResponseTest $
--             updateCloudFrontOriginAccessIdentityResponse
--
--         , listStreamingDistributionsResponseTest $
--             listStreamingDistributionsResponse
--
--         , getStreamingDistributionConfigResponseTest $
--             getStreamingDistributionConfigResponse
--
--         , getCloudFrontOriginAccessIdentityConfigResponseTest $
--             getCloudFrontOriginAccessIdentityConfigResponse
--
--         , createStreamingDistributionResponseTest $
--             createStreamingDistributionResponse
--
--         , createCloudFrontOriginAccessIdentityResponseTest $
--             createCloudFrontOriginAccessIdentityResponse
--
--         , listCloudFrontOriginAccessIdentitiesResponseTest $
--             listCloudFrontOriginAccessIdentitiesResponse
--
--         , getInvalidationResponseTest $
--             getInvalidationResponse
--
--         , listInvalidationsResponseTest $
--             listInvalidationsResponse
--
--         , getStreamingDistributionResponseTest $
--             getStreamingDistributionResponse
--
--         , getCloudFrontOriginAccessIdentityResponseTest $
--             getCloudFrontOriginAccessIdentityResponse
--
--         , createInvalidationResponseTest $
--             createInvalidationResponse
--
--         , updateDistributionResponseTest $
--             updateDistributionResponse
--
--         , deleteDistributionResponseTest $
--             deleteDistributionResponse
--
--         , listDistributionsResponseTest $
--             listDistributionsResponse
--
--           ]
--     ]

-- Requests

deleteStreamingDistributionTest :: DeleteStreamingDistribution -> TestTree
deleteStreamingDistributionTest = undefined

updateStreamingDistributionTest :: UpdateStreamingDistribution -> TestTree
updateStreamingDistributionTest = undefined

createDistributionTest :: CreateDistribution -> TestTree
createDistributionTest = undefined

getDistributionConfigTest :: GetDistributionConfig -> TestTree
getDistributionConfigTest = undefined

getDistributionTest :: GetDistribution -> TestTree
getDistributionTest = undefined

deleteCloudFrontOriginAccessIdentityTest :: DeleteCloudFrontOriginAccessIdentity -> TestTree
deleteCloudFrontOriginAccessIdentityTest = undefined

updateCloudFrontOriginAccessIdentityTest :: UpdateCloudFrontOriginAccessIdentity -> TestTree
updateCloudFrontOriginAccessIdentityTest = undefined

listStreamingDistributionsTest :: ListStreamingDistributions -> TestTree
listStreamingDistributionsTest = undefined

getStreamingDistributionConfigTest :: GetStreamingDistributionConfig -> TestTree
getStreamingDistributionConfigTest = undefined

getCloudFrontOriginAccessIdentityConfigTest :: GetCloudFrontOriginAccessIdentityConfig -> TestTree
getCloudFrontOriginAccessIdentityConfigTest = undefined

createStreamingDistributionTest :: CreateStreamingDistribution -> TestTree
createStreamingDistributionTest = undefined

createCloudFrontOriginAccessIdentityTest :: CreateCloudFrontOriginAccessIdentity -> TestTree
createCloudFrontOriginAccessIdentityTest = undefined

listCloudFrontOriginAccessIdentitiesTest :: ListCloudFrontOriginAccessIdentities -> TestTree
listCloudFrontOriginAccessIdentitiesTest = undefined

getInvalidationTest :: GetInvalidation -> TestTree
getInvalidationTest = undefined

listInvalidationsTest :: ListInvalidations -> TestTree
listInvalidationsTest = undefined

getStreamingDistributionTest :: GetStreamingDistribution -> TestTree
getStreamingDistributionTest = undefined

getCloudFrontOriginAccessIdentityTest :: GetCloudFrontOriginAccessIdentity -> TestTree
getCloudFrontOriginAccessIdentityTest = undefined

createInvalidationTest :: CreateInvalidation -> TestTree
createInvalidationTest = undefined

updateDistributionTest :: UpdateDistribution -> TestTree
updateDistributionTest = undefined

deleteDistributionTest :: DeleteDistribution -> TestTree
deleteDistributionTest = undefined

listDistributionsTest :: ListDistributions -> TestTree
listDistributionsTest = undefined

-- Responses

deleteStreamingDistributionResponseTest :: DeleteStreamingDistributionResponse -> TestTree
deleteStreamingDistributionResponseTest = resp
    "DeleteStreamingDistribution"
    "fixture/CloudFront/DeleteStreamingDistributionResponse"
    (Proxy :: Proxy DeleteStreamingDistribution)

updateStreamingDistributionResponseTest :: UpdateStreamingDistributionResponse -> TestTree
updateStreamingDistributionResponseTest = resp
    "UpdateStreamingDistribution"
    "fixture/CloudFront/UpdateStreamingDistributionResponse"
    (Proxy :: Proxy UpdateStreamingDistribution)

createDistributionResponseTest :: CreateDistributionResponse -> TestTree
createDistributionResponseTest = resp
    "CreateDistribution"
    "fixture/CloudFront/CreateDistributionResponse"
    (Proxy :: Proxy CreateDistribution)

getDistributionConfigResponseTest :: GetDistributionConfigResponse -> TestTree
getDistributionConfigResponseTest = resp
    "GetDistributionConfig"
    "fixture/CloudFront/GetDistributionConfigResponse"
    (Proxy :: Proxy GetDistributionConfig)

getDistributionResponseTest :: GetDistributionResponse -> TestTree
getDistributionResponseTest = resp
    "GetDistribution"
    "fixture/CloudFront/GetDistributionResponse"
    (Proxy :: Proxy GetDistribution)

deleteCloudFrontOriginAccessIdentityResponseTest :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
deleteCloudFrontOriginAccessIdentityResponseTest = resp
    "DeleteCloudFrontOriginAccessIdentity"
    "fixture/CloudFront/DeleteCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy DeleteCloudFrontOriginAccessIdentity)

updateCloudFrontOriginAccessIdentityResponseTest :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
updateCloudFrontOriginAccessIdentityResponseTest = resp
    "UpdateCloudFrontOriginAccessIdentity"
    "fixture/CloudFront/UpdateCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy UpdateCloudFrontOriginAccessIdentity)

listStreamingDistributionsResponseTest :: ListStreamingDistributionsResponse -> TestTree
listStreamingDistributionsResponseTest = resp
    "ListStreamingDistributions"
    "fixture/CloudFront/ListStreamingDistributionsResponse"
    (Proxy :: Proxy ListStreamingDistributions)

getStreamingDistributionConfigResponseTest :: GetStreamingDistributionConfigResponse -> TestTree
getStreamingDistributionConfigResponseTest = resp
    "GetStreamingDistributionConfig"
    "fixture/CloudFront/GetStreamingDistributionConfigResponse"
    (Proxy :: Proxy GetStreamingDistributionConfig)

getCloudFrontOriginAccessIdentityConfigResponseTest :: GetCloudFrontOriginAccessIdentityConfigResponse -> TestTree
getCloudFrontOriginAccessIdentityConfigResponseTest = resp
    "GetCloudFrontOriginAccessIdentityConfig"
    "fixture/CloudFront/GetCloudFrontOriginAccessIdentityConfigResponse"
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentityConfig)

createStreamingDistributionResponseTest :: CreateStreamingDistributionResponse -> TestTree
createStreamingDistributionResponseTest = resp
    "CreateStreamingDistribution"
    "fixture/CloudFront/CreateStreamingDistributionResponse"
    (Proxy :: Proxy CreateStreamingDistribution)

createCloudFrontOriginAccessIdentityResponseTest :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
createCloudFrontOriginAccessIdentityResponseTest = resp
    "CreateCloudFrontOriginAccessIdentity"
    "fixture/CloudFront/CreateCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy CreateCloudFrontOriginAccessIdentity)

listCloudFrontOriginAccessIdentitiesResponseTest :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
listCloudFrontOriginAccessIdentitiesResponseTest = resp
    "ListCloudFrontOriginAccessIdentities"
    "fixture/CloudFront/ListCloudFrontOriginAccessIdentitiesResponse"
    (Proxy :: Proxy ListCloudFrontOriginAccessIdentities)

getInvalidationResponseTest :: GetInvalidationResponse -> TestTree
getInvalidationResponseTest = resp
    "GetInvalidation"
    "fixture/CloudFront/GetInvalidationResponse"
    (Proxy :: Proxy GetInvalidation)

listInvalidationsResponseTest :: ListInvalidationsResponse -> TestTree
listInvalidationsResponseTest = resp
    "ListInvalidations"
    "fixture/CloudFront/ListInvalidationsResponse"
    (Proxy :: Proxy ListInvalidations)

getStreamingDistributionResponseTest :: GetStreamingDistributionResponse -> TestTree
getStreamingDistributionResponseTest = resp
    "GetStreamingDistribution"
    "fixture/CloudFront/GetStreamingDistributionResponse"
    (Proxy :: Proxy GetStreamingDistribution)

getCloudFrontOriginAccessIdentityResponseTest :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
getCloudFrontOriginAccessIdentityResponseTest = resp
    "GetCloudFrontOriginAccessIdentity"
    "fixture/CloudFront/GetCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentity)

createInvalidationResponseTest :: CreateInvalidationResponse -> TestTree
createInvalidationResponseTest = resp
    "CreateInvalidation"
    "fixture/CloudFront/CreateInvalidationResponse"
    (Proxy :: Proxy CreateInvalidation)

updateDistributionResponseTest :: UpdateDistributionResponse -> TestTree
updateDistributionResponseTest = resp
    "UpdateDistribution"
    "fixture/CloudFront/UpdateDistributionResponse"
    (Proxy :: Proxy UpdateDistribution)

deleteDistributionResponseTest :: DeleteDistributionResponse -> TestTree
deleteDistributionResponseTest = resp
    "DeleteDistribution"
    "fixture/CloudFront/DeleteDistributionResponse"
    (Proxy :: Proxy DeleteDistribution)

listDistributionsResponseTest :: ListDistributionsResponse -> TestTree
listDistributionsResponseTest = resp
    "ListDistributions"
    "fixture/CloudFront/ListDistributionsResponse"
    (Proxy :: Proxy ListDistributions)
