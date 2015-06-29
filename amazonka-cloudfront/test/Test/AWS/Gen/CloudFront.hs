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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CloudFront

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ createCloudFrontOriginAccessIdentityTest $
--             createCloudFrontOriginAccessIdentity
--
--         , createDistributionTest $
--             createDistribution
--
--         , createInvalidationTest $
--             createInvalidation
--
--         , createStreamingDistributionTest $
--             createStreamingDistribution
--
--         , deleteCloudFrontOriginAccessIdentityTest $
--             deleteCloudFrontOriginAccessIdentity
--
--         , deleteDistributionTest $
--             deleteDistribution
--
--         , deleteStreamingDistributionTest $
--             deleteStreamingDistribution
--
--         , getCloudFrontOriginAccessIdentityTest $
--             getCloudFrontOriginAccessIdentity
--
--         , getCloudFrontOriginAccessIdentityConfigTest $
--             getCloudFrontOriginAccessIdentityConfig
--
--         , getDistributionTest $
--             getDistribution
--
--         , getDistributionConfigTest $
--             getDistributionConfig
--
--         , getInvalidationTest $
--             getInvalidation
--
--         , getStreamingDistributionTest $
--             getStreamingDistribution
--
--         , getStreamingDistributionConfigTest $
--             getStreamingDistributionConfig
--
--         , listCloudFrontOriginAccessIdentitiesTest $
--             listCloudFrontOriginAccessIdentities
--
--         , listDistributionsTest $
--             listDistributions
--
--         , listInvalidationsTest $
--             listInvalidations
--
--         , listStreamingDistributionsTest $
--             listStreamingDistributions
--
--         , updateCloudFrontOriginAccessIdentityTest $
--             updateCloudFrontOriginAccessIdentity
--
--         , updateDistributionTest $
--             updateDistribution
--
--         , updateStreamingDistributionTest $
--             updateStreamingDistribution
--
--           ]

--     , testGroup "response"
--         [ createCloudFrontOriginAccessIdentityResponseTest $
--             createCloudFrontOriginAccessIdentityResponse
--
--         , createDistributionResponseTest $
--             createDistributionResponse
--
--         , createInvalidationResponseTest $
--             createInvalidationResponse
--
--         , createStreamingDistributionResponseTest $
--             createStreamingDistributionResponse
--
--         , deleteCloudFrontOriginAccessIdentityResponseTest $
--             deleteCloudFrontOriginAccessIdentityResponse
--
--         , deleteDistributionResponseTest $
--             deleteDistributionResponse
--
--         , deleteStreamingDistributionResponseTest $
--             deleteStreamingDistributionResponse
--
--         , getCloudFrontOriginAccessIdentityResponseTest $
--             getCloudFrontOriginAccessIdentityResponse
--
--         , getCloudFrontOriginAccessIdentityConfigResponseTest $
--             getCloudFrontOriginAccessIdentityConfigResponse
--
--         , getDistributionResponseTest $
--             getDistributionResponse
--
--         , getDistributionConfigResponseTest $
--             getDistributionConfigResponse
--
--         , getInvalidationResponseTest $
--             getInvalidationResponse
--
--         , getStreamingDistributionResponseTest $
--             getStreamingDistributionResponse
--
--         , getStreamingDistributionConfigResponseTest $
--             getStreamingDistributionConfigResponse
--
--         , listCloudFrontOriginAccessIdentitiesResponseTest $
--             listCloudFrontOriginAccessIdentitiesResponse
--
--         , listDistributionsResponseTest $
--             listDistributionsResponse
--
--         , listInvalidationsResponseTest $
--             listInvalidationsResponse
--
--         , listStreamingDistributionsResponseTest $
--             listStreamingDistributionsResponse
--
--         , updateCloudFrontOriginAccessIdentityResponseTest $
--             updateCloudFrontOriginAccessIdentityResponse
--
--         , updateDistributionResponseTest $
--             updateDistributionResponse
--
--         , updateStreamingDistributionResponseTest $
--             updateStreamingDistributionResponse
--
--           ]
--     ]

-- Requests

createCloudFrontOriginAccessIdentityTest :: CreateCloudFrontOriginAccessIdentity -> TestTree
createCloudFrontOriginAccessIdentityTest = undefined

createDistributionTest :: CreateDistribution -> TestTree
createDistributionTest = undefined

createInvalidationTest :: CreateInvalidation -> TestTree
createInvalidationTest = undefined

createStreamingDistributionTest :: CreateStreamingDistribution -> TestTree
createStreamingDistributionTest = undefined

deleteCloudFrontOriginAccessIdentityTest :: DeleteCloudFrontOriginAccessIdentity -> TestTree
deleteCloudFrontOriginAccessIdentityTest = undefined

deleteDistributionTest :: DeleteDistribution -> TestTree
deleteDistributionTest = undefined

deleteStreamingDistributionTest :: DeleteStreamingDistribution -> TestTree
deleteStreamingDistributionTest = undefined

getCloudFrontOriginAccessIdentityTest :: GetCloudFrontOriginAccessIdentity -> TestTree
getCloudFrontOriginAccessIdentityTest = undefined

getCloudFrontOriginAccessIdentityConfigTest :: GetCloudFrontOriginAccessIdentityConfig -> TestTree
getCloudFrontOriginAccessIdentityConfigTest = undefined

getDistributionTest :: GetDistribution -> TestTree
getDistributionTest = undefined

getDistributionConfigTest :: GetDistributionConfig -> TestTree
getDistributionConfigTest = undefined

getInvalidationTest :: GetInvalidation -> TestTree
getInvalidationTest = undefined

getStreamingDistributionTest :: GetStreamingDistribution -> TestTree
getStreamingDistributionTest = undefined

getStreamingDistributionConfigTest :: GetStreamingDistributionConfig -> TestTree
getStreamingDistributionConfigTest = undefined

listCloudFrontOriginAccessIdentitiesTest :: ListCloudFrontOriginAccessIdentities -> TestTree
listCloudFrontOriginAccessIdentitiesTest = undefined

listDistributionsTest :: ListDistributions -> TestTree
listDistributionsTest = undefined

listInvalidationsTest :: ListInvalidations -> TestTree
listInvalidationsTest = undefined

listStreamingDistributionsTest :: ListStreamingDistributions -> TestTree
listStreamingDistributionsTest = undefined

updateCloudFrontOriginAccessIdentityTest :: UpdateCloudFrontOriginAccessIdentity -> TestTree
updateCloudFrontOriginAccessIdentityTest = undefined

updateDistributionTest :: UpdateDistribution -> TestTree
updateDistributionTest = undefined

updateStreamingDistributionTest :: UpdateStreamingDistribution -> TestTree
updateStreamingDistributionTest = undefined

-- Responses

createCloudFrontOriginAccessIdentityResponseTest :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
createCloudFrontOriginAccessIdentityResponseTest = resp
    "createCloudFrontOriginAccessIdentityResponse"
    "fixture/CreateCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy CreateCloudFrontOriginAccessIdentity)

createDistributionResponseTest :: CreateDistributionResponse -> TestTree
createDistributionResponseTest = resp
    "createDistributionResponse"
    "fixture/CreateDistributionResponse"
    (Proxy :: Proxy CreateDistribution)

createInvalidationResponseTest :: CreateInvalidationResponse -> TestTree
createInvalidationResponseTest = resp
    "createInvalidationResponse"
    "fixture/CreateInvalidationResponse"
    (Proxy :: Proxy CreateInvalidation)

createStreamingDistributionResponseTest :: CreateStreamingDistributionResponse -> TestTree
createStreamingDistributionResponseTest = resp
    "createStreamingDistributionResponse"
    "fixture/CreateStreamingDistributionResponse"
    (Proxy :: Proxy CreateStreamingDistribution)

deleteCloudFrontOriginAccessIdentityResponseTest :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
deleteCloudFrontOriginAccessIdentityResponseTest = resp
    "deleteCloudFrontOriginAccessIdentityResponse"
    "fixture/DeleteCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy DeleteCloudFrontOriginAccessIdentity)

deleteDistributionResponseTest :: DeleteDistributionResponse -> TestTree
deleteDistributionResponseTest = resp
    "deleteDistributionResponse"
    "fixture/DeleteDistributionResponse"
    (Proxy :: Proxy DeleteDistribution)

deleteStreamingDistributionResponseTest :: DeleteStreamingDistributionResponse -> TestTree
deleteStreamingDistributionResponseTest = resp
    "deleteStreamingDistributionResponse"
    "fixture/DeleteStreamingDistributionResponse"
    (Proxy :: Proxy DeleteStreamingDistribution)

getCloudFrontOriginAccessIdentityResponseTest :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
getCloudFrontOriginAccessIdentityResponseTest = resp
    "getCloudFrontOriginAccessIdentityResponse"
    "fixture/GetCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentity)

getCloudFrontOriginAccessIdentityConfigResponseTest :: GetCloudFrontOriginAccessIdentityConfigResponse -> TestTree
getCloudFrontOriginAccessIdentityConfigResponseTest = resp
    "getCloudFrontOriginAccessIdentityConfigResponse"
    "fixture/GetCloudFrontOriginAccessIdentityConfigResponse"
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentityConfig)

getDistributionResponseTest :: GetDistributionResponse -> TestTree
getDistributionResponseTest = resp
    "getDistributionResponse"
    "fixture/GetDistributionResponse"
    (Proxy :: Proxy GetDistribution)

getDistributionConfigResponseTest :: GetDistributionConfigResponse -> TestTree
getDistributionConfigResponseTest = resp
    "getDistributionConfigResponse"
    "fixture/GetDistributionConfigResponse"
    (Proxy :: Proxy GetDistributionConfig)

getInvalidationResponseTest :: GetInvalidationResponse -> TestTree
getInvalidationResponseTest = resp
    "getInvalidationResponse"
    "fixture/GetInvalidationResponse"
    (Proxy :: Proxy GetInvalidation)

getStreamingDistributionResponseTest :: GetStreamingDistributionResponse -> TestTree
getStreamingDistributionResponseTest = resp
    "getStreamingDistributionResponse"
    "fixture/GetStreamingDistributionResponse"
    (Proxy :: Proxy GetStreamingDistribution)

getStreamingDistributionConfigResponseTest :: GetStreamingDistributionConfigResponse -> TestTree
getStreamingDistributionConfigResponseTest = resp
    "getStreamingDistributionConfigResponse"
    "fixture/GetStreamingDistributionConfigResponse"
    (Proxy :: Proxy GetStreamingDistributionConfig)

listCloudFrontOriginAccessIdentitiesResponseTest :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
listCloudFrontOriginAccessIdentitiesResponseTest = resp
    "listCloudFrontOriginAccessIdentitiesResponse"
    "fixture/ListCloudFrontOriginAccessIdentitiesResponse"
    (Proxy :: Proxy ListCloudFrontOriginAccessIdentities)

listDistributionsResponseTest :: ListDistributionsResponse -> TestTree
listDistributionsResponseTest = resp
    "listDistributionsResponse"
    "fixture/ListDistributionsResponse"
    (Proxy :: Proxy ListDistributions)

listInvalidationsResponseTest :: ListInvalidationsResponse -> TestTree
listInvalidationsResponseTest = resp
    "listInvalidationsResponse"
    "fixture/ListInvalidationsResponse"
    (Proxy :: Proxy ListInvalidations)

listStreamingDistributionsResponseTest :: ListStreamingDistributionsResponse -> TestTree
listStreamingDistributionsResponseTest = resp
    "listStreamingDistributionsResponse"
    "fixture/ListStreamingDistributionsResponse"
    (Proxy :: Proxy ListStreamingDistributions)

updateCloudFrontOriginAccessIdentityResponseTest :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
updateCloudFrontOriginAccessIdentityResponseTest = resp
    "updateCloudFrontOriginAccessIdentityResponse"
    "fixture/UpdateCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy UpdateCloudFrontOriginAccessIdentity)

updateDistributionResponseTest :: UpdateDistributionResponse -> TestTree
updateDistributionResponseTest = resp
    "updateDistributionResponse"
    "fixture/UpdateDistributionResponse"
    (Proxy :: Proxy UpdateDistribution)

updateStreamingDistributionResponseTest :: UpdateStreamingDistributionResponse -> TestTree
updateStreamingDistributionResponseTest = resp
    "updateStreamingDistributionResponse"
    "fixture/UpdateStreamingDistributionResponse"
    (Proxy :: Proxy UpdateStreamingDistribution)
