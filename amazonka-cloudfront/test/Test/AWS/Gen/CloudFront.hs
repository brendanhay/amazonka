{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.CloudFront
-- Copyright   : (c) 2013-2015 Brendan Hay
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
testDeleteStreamingDistribution = undefined

testUpdateStreamingDistribution :: UpdateStreamingDistribution -> TestTree
testUpdateStreamingDistribution = undefined

testCreateDistribution :: CreateDistribution -> TestTree
testCreateDistribution = undefined

testGetDistributionConfig :: GetDistributionConfig -> TestTree
testGetDistributionConfig = undefined

testGetDistribution :: GetDistribution -> TestTree
testGetDistribution = undefined

testDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentity -> TestTree
testDeleteCloudFrontOriginAccessIdentity = undefined

testUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentity -> TestTree
testUpdateCloudFrontOriginAccessIdentity = undefined

testListStreamingDistributions :: ListStreamingDistributions -> TestTree
testListStreamingDistributions = undefined

testGetStreamingDistributionConfig :: GetStreamingDistributionConfig -> TestTree
testGetStreamingDistributionConfig = undefined

testGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfig -> TestTree
testGetCloudFrontOriginAccessIdentityConfig = undefined

testCreateStreamingDistribution :: CreateStreamingDistribution -> TestTree
testCreateStreamingDistribution = undefined

testCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentity -> TestTree
testCreateCloudFrontOriginAccessIdentity = undefined

testListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities -> TestTree
testListCloudFrontOriginAccessIdentities = undefined

testGetInvalidation :: GetInvalidation -> TestTree
testGetInvalidation = undefined

testListInvalidations :: ListInvalidations -> TestTree
testListInvalidations = undefined

testGetStreamingDistribution :: GetStreamingDistribution -> TestTree
testGetStreamingDistribution = undefined

testGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentity -> TestTree
testGetCloudFrontOriginAccessIdentity = undefined

testCreateInvalidation :: CreateInvalidation -> TestTree
testCreateInvalidation = undefined

testUpdateDistribution :: UpdateDistribution -> TestTree
testUpdateDistribution = undefined

testDeleteDistribution :: DeleteDistribution -> TestTree
testDeleteDistribution = undefined

testListDistributions :: ListDistributions -> TestTree
testListDistributions = undefined

-- Responses

testDeleteStreamingDistributionResponse :: DeleteStreamingDistributionResponse -> TestTree
testDeleteStreamingDistributionResponse = resp
    "DeleteStreamingDistributionResponse"
    "fixture/DeleteStreamingDistributionResponse"
    (Proxy :: Proxy DeleteStreamingDistribution)

testUpdateStreamingDistributionResponse :: UpdateStreamingDistributionResponse -> TestTree
testUpdateStreamingDistributionResponse = resp
    "UpdateStreamingDistributionResponse"
    "fixture/UpdateStreamingDistributionResponse"
    (Proxy :: Proxy UpdateStreamingDistribution)

testCreateDistributionResponse :: CreateDistributionResponse -> TestTree
testCreateDistributionResponse = resp
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse"
    (Proxy :: Proxy CreateDistribution)

testGetDistributionConfigResponse :: GetDistributionConfigResponse -> TestTree
testGetDistributionConfigResponse = resp
    "GetDistributionConfigResponse"
    "fixture/GetDistributionConfigResponse"
    (Proxy :: Proxy GetDistributionConfig)

testGetDistributionResponse :: GetDistributionResponse -> TestTree
testGetDistributionResponse = resp
    "GetDistributionResponse"
    "fixture/GetDistributionResponse"
    (Proxy :: Proxy GetDistribution)

testDeleteCloudFrontOriginAccessIdentityResponse :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
testDeleteCloudFrontOriginAccessIdentityResponse = resp
    "DeleteCloudFrontOriginAccessIdentityResponse"
    "fixture/DeleteCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy DeleteCloudFrontOriginAccessIdentity)

testUpdateCloudFrontOriginAccessIdentityResponse :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
testUpdateCloudFrontOriginAccessIdentityResponse = resp
    "UpdateCloudFrontOriginAccessIdentityResponse"
    "fixture/UpdateCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy UpdateCloudFrontOriginAccessIdentity)

testListStreamingDistributionsResponse :: ListStreamingDistributionsResponse -> TestTree
testListStreamingDistributionsResponse = resp
    "ListStreamingDistributionsResponse"
    "fixture/ListStreamingDistributionsResponse"
    (Proxy :: Proxy ListStreamingDistributions)

testGetStreamingDistributionConfigResponse :: GetStreamingDistributionConfigResponse -> TestTree
testGetStreamingDistributionConfigResponse = resp
    "GetStreamingDistributionConfigResponse"
    "fixture/GetStreamingDistributionConfigResponse"
    (Proxy :: Proxy GetStreamingDistributionConfig)

testGetCloudFrontOriginAccessIdentityConfigResponse :: GetCloudFrontOriginAccessIdentityConfigResponse -> TestTree
testGetCloudFrontOriginAccessIdentityConfigResponse = resp
    "GetCloudFrontOriginAccessIdentityConfigResponse"
    "fixture/GetCloudFrontOriginAccessIdentityConfigResponse"
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentityConfig)

testCreateStreamingDistributionResponse :: CreateStreamingDistributionResponse -> TestTree
testCreateStreamingDistributionResponse = resp
    "CreateStreamingDistributionResponse"
    "fixture/CreateStreamingDistributionResponse"
    (Proxy :: Proxy CreateStreamingDistribution)

testCreateCloudFrontOriginAccessIdentityResponse :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
testCreateCloudFrontOriginAccessIdentityResponse = resp
    "CreateCloudFrontOriginAccessIdentityResponse"
    "fixture/CreateCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy CreateCloudFrontOriginAccessIdentity)

testListCloudFrontOriginAccessIdentitiesResponse :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
testListCloudFrontOriginAccessIdentitiesResponse = resp
    "ListCloudFrontOriginAccessIdentitiesResponse"
    "fixture/ListCloudFrontOriginAccessIdentitiesResponse"
    (Proxy :: Proxy ListCloudFrontOriginAccessIdentities)

testGetInvalidationResponse :: GetInvalidationResponse -> TestTree
testGetInvalidationResponse = resp
    "GetInvalidationResponse"
    "fixture/GetInvalidationResponse"
    (Proxy :: Proxy GetInvalidation)

testListInvalidationsResponse :: ListInvalidationsResponse -> TestTree
testListInvalidationsResponse = resp
    "ListInvalidationsResponse"
    "fixture/ListInvalidationsResponse"
    (Proxy :: Proxy ListInvalidations)

testGetStreamingDistributionResponse :: GetStreamingDistributionResponse -> TestTree
testGetStreamingDistributionResponse = resp
    "GetStreamingDistributionResponse"
    "fixture/GetStreamingDistributionResponse"
    (Proxy :: Proxy GetStreamingDistribution)

testGetCloudFrontOriginAccessIdentityResponse :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
testGetCloudFrontOriginAccessIdentityResponse = resp
    "GetCloudFrontOriginAccessIdentityResponse"
    "fixture/GetCloudFrontOriginAccessIdentityResponse"
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentity)

testCreateInvalidationResponse :: CreateInvalidationResponse -> TestTree
testCreateInvalidationResponse = resp
    "CreateInvalidationResponse"
    "fixture/CreateInvalidationResponse"
    (Proxy :: Proxy CreateInvalidation)

testUpdateDistributionResponse :: UpdateDistributionResponse -> TestTree
testUpdateDistributionResponse = resp
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse"
    (Proxy :: Proxy UpdateDistribution)

testDeleteDistributionResponse :: DeleteDistributionResponse -> TestTree
testDeleteDistributionResponse = resp
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse"
    (Proxy :: Proxy DeleteDistribution)

testListDistributionsResponse :: ListDistributionsResponse -> TestTree
testListDistributionsResponse = resp
    "ListDistributionsResponse"
    "fixture/ListDistributionsResponse"
    (Proxy :: Proxy ListDistributions)

instance Out ActiveTrustedSigners
instance Out Aliases
instance Out AllowedMethods
instance Out CacheBehavior
instance Out CacheBehaviors
instance Out CachedMethods
instance Out CloudFrontOriginAccessIdentity
instance Out CloudFrontOriginAccessIdentityConfig
instance Out CloudFrontOriginAccessIdentityList
instance Out CloudFrontOriginAccessIdentitySummary
instance Out CookieNames
instance Out CookiePreference
instance Out CreateCloudFrontOriginAccessIdentity
instance Out CreateCloudFrontOriginAccessIdentityResponse
instance Out CreateDistribution
instance Out CreateDistributionResponse
instance Out CreateInvalidation
instance Out CreateInvalidationResponse
instance Out CreateStreamingDistribution
instance Out CreateStreamingDistributionResponse
instance Out CustomErrorResponse
instance Out CustomErrorResponses
instance Out CustomOriginConfig
instance Out DefaultCacheBehavior
instance Out DeleteCloudFrontOriginAccessIdentity
instance Out DeleteCloudFrontOriginAccessIdentityResponse
instance Out DeleteDistribution
instance Out DeleteDistributionResponse
instance Out DeleteStreamingDistribution
instance Out DeleteStreamingDistributionResponse
instance Out Distribution
instance Out DistributionConfig
instance Out DistributionList
instance Out DistributionSummary
instance Out ForwardedValues
instance Out GeoRestriction
instance Out GeoRestrictionType
instance Out GetCloudFrontOriginAccessIdentity
instance Out GetCloudFrontOriginAccessIdentityConfig
instance Out GetCloudFrontOriginAccessIdentityConfigResponse
instance Out GetCloudFrontOriginAccessIdentityResponse
instance Out GetDistribution
instance Out GetDistributionConfig
instance Out GetDistributionConfigResponse
instance Out GetDistributionResponse
instance Out GetInvalidation
instance Out GetInvalidationResponse
instance Out GetStreamingDistribution
instance Out GetStreamingDistributionConfig
instance Out GetStreamingDistributionConfigResponse
instance Out GetStreamingDistributionResponse
instance Out Headers
instance Out Invalidation
instance Out InvalidationBatch
instance Out InvalidationList
instance Out InvalidationSummary
instance Out ItemSelection
instance Out KeyPairIds
instance Out ListCloudFrontOriginAccessIdentities
instance Out ListCloudFrontOriginAccessIdentitiesResponse
instance Out ListDistributions
instance Out ListDistributionsResponse
instance Out ListInvalidations
instance Out ListInvalidationsResponse
instance Out ListStreamingDistributions
instance Out ListStreamingDistributionsResponse
instance Out LoggingConfig
instance Out Method
instance Out MinimumProtocolVersion
instance Out Origin
instance Out OriginProtocolPolicy
instance Out Origins
instance Out Paths
instance Out PriceClass
instance Out Restrictions
instance Out S3Origin
instance Out S3OriginConfig
instance Out SSLSupportMethod
instance Out Signer
instance Out StreamingDistribution
instance Out StreamingDistributionConfig
instance Out StreamingDistributionList
instance Out StreamingDistributionSummary
instance Out StreamingLoggingConfig
instance Out TrustedSigners
instance Out UpdateCloudFrontOriginAccessIdentity
instance Out UpdateCloudFrontOriginAccessIdentityResponse
instance Out UpdateDistribution
instance Out UpdateDistributionResponse
instance Out UpdateStreamingDistribution
instance Out UpdateStreamingDistributionResponse
instance Out ViewerCertificate
instance Out ViewerProtocolPolicy
