-- Module      : Test.AWS.Gen.Route53
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

module Test.AWS.Gen.Route53 where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Route53

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ associateVPCWithHostedZoneTest $
--             associateVPCWithHostedZone
--
--         , changeResourceRecordSetsTest $
--             changeResourceRecordSets
--
--         , changeTagsForResourceTest $
--             changeTagsForResource
--
--         , createHealthCheckTest $
--             createHealthCheck
--
--         , createHostedZoneTest $
--             createHostedZone
--
--         , createReusableDelegationSetTest $
--             createReusableDelegationSet
--
--         , deleteHealthCheckTest $
--             deleteHealthCheck
--
--         , deleteHostedZoneTest $
--             deleteHostedZone
--
--         , deleteReusableDelegationSetTest $
--             deleteReusableDelegationSet
--
--         , disassociateVPCFromHostedZoneTest $
--             disassociateVPCFromHostedZone
--
--         , getChangeTest $
--             getChange
--
--         , getCheckerIPRangesTest $
--             getCheckerIPRanges
--
--         , getGeoLocationTest $
--             getGeoLocation
--
--         , getHealthCheckTest $
--             getHealthCheck
--
--         , getHealthCheckCountTest $
--             getHealthCheckCount
--
--         , getHealthCheckLastFailureReasonTest $
--             getHealthCheckLastFailureReason
--
--         , getHealthCheckStatusTest $
--             getHealthCheckStatus
--
--         , getHostedZoneTest $
--             getHostedZone
--
--         , getHostedZoneCountTest $
--             getHostedZoneCount
--
--         , getReusableDelegationSetTest $
--             getReusableDelegationSet
--
--         , listGeoLocationsTest $
--             listGeoLocations
--
--         , listHealthChecksTest $
--             listHealthChecks
--
--         , listHostedZonesTest $
--             listHostedZones
--
--         , listHostedZonesByNameTest $
--             listHostedZonesByName
--
--         , listResourceRecordSetsTest $
--             listResourceRecordSets
--
--         , listReusableDelegationSetsTest $
--             listReusableDelegationSets
--
--         , listTagsForResourceTest $
--             listTagsForResource
--
--         , listTagsForResourcesTest $
--             listTagsForResources
--
--         , updateHealthCheckTest $
--             updateHealthCheck
--
--         , updateHostedZoneCommentTest $
--             updateHostedZoneComment
--
--           ]

--     , testGroup "response"
--         [ associateVPCWithHostedZoneResponseTest $
--             associateVPCWithHostedZoneResponse
--
--         , changeResourceRecordSetsResponseTest $
--             changeResourceRecordSetsResponse
--
--         , changeTagsForResourceResponseTest $
--             changeTagsForResourceResponse
--
--         , createHealthCheckResponseTest $
--             createHealthCheckResponse
--
--         , createHostedZoneResponseTest $
--             createHostedZoneResponse
--
--         , createReusableDelegationSetResponseTest $
--             createReusableDelegationSetResponse
--
--         , deleteHealthCheckResponseTest $
--             deleteHealthCheckResponse
--
--         , deleteHostedZoneResponseTest $
--             deleteHostedZoneResponse
--
--         , deleteReusableDelegationSetResponseTest $
--             deleteReusableDelegationSetResponse
--
--         , disassociateVPCFromHostedZoneResponseTest $
--             disassociateVPCFromHostedZoneResponse
--
--         , getChangeResponseTest $
--             getChangeResponse
--
--         , getCheckerIPRangesResponseTest $
--             getCheckerIPRangesResponse
--
--         , getGeoLocationResponseTest $
--             getGeoLocationResponse
--
--         , getHealthCheckResponseTest $
--             getHealthCheckResponse
--
--         , getHealthCheckCountResponseTest $
--             getHealthCheckCountResponse
--
--         , getHealthCheckLastFailureReasonResponseTest $
--             getHealthCheckLastFailureReasonResponse
--
--         , getHealthCheckStatusResponseTest $
--             getHealthCheckStatusResponse
--
--         , getHostedZoneResponseTest $
--             getHostedZoneResponse
--
--         , getHostedZoneCountResponseTest $
--             getHostedZoneCountResponse
--
--         , getReusableDelegationSetResponseTest $
--             getReusableDelegationSetResponse
--
--         , listGeoLocationsResponseTest $
--             listGeoLocationsResponse
--
--         , listHealthChecksResponseTest $
--             listHealthChecksResponse
--
--         , listHostedZonesResponseTest $
--             listHostedZonesResponse
--
--         , listHostedZonesByNameResponseTest $
--             listHostedZonesByNameResponse
--
--         , listResourceRecordSetsResponseTest $
--             listResourceRecordSetsResponse
--
--         , listReusableDelegationSetsResponseTest $
--             listReusableDelegationSetsResponse
--
--         , listTagsForResourceResponseTest $
--             listTagsForResourceResponse
--
--         , listTagsForResourcesResponseTest $
--             listTagsForResourcesResponse
--
--         , updateHealthCheckResponseTest $
--             updateHealthCheckResponse
--
--         , updateHostedZoneCommentResponseTest $
--             updateHostedZoneCommentResponse
--
--           ]
--     ]

-- Requests

associateVPCWithHostedZoneTest :: AssociateVPCWithHostedZone -> TestTree
associateVPCWithHostedZoneTest = undefined

changeResourceRecordSetsTest :: ChangeResourceRecordSets -> TestTree
changeResourceRecordSetsTest = undefined

changeTagsForResourceTest :: ChangeTagsForResource -> TestTree
changeTagsForResourceTest = undefined

createHealthCheckTest :: CreateHealthCheck -> TestTree
createHealthCheckTest = undefined

createHostedZoneTest :: CreateHostedZone -> TestTree
createHostedZoneTest = undefined

createReusableDelegationSetTest :: CreateReusableDelegationSet -> TestTree
createReusableDelegationSetTest = undefined

deleteHealthCheckTest :: DeleteHealthCheck -> TestTree
deleteHealthCheckTest = undefined

deleteHostedZoneTest :: DeleteHostedZone -> TestTree
deleteHostedZoneTest = undefined

deleteReusableDelegationSetTest :: DeleteReusableDelegationSet -> TestTree
deleteReusableDelegationSetTest = undefined

disassociateVPCFromHostedZoneTest :: DisassociateVPCFromHostedZone -> TestTree
disassociateVPCFromHostedZoneTest = undefined

getChangeTest :: GetChange -> TestTree
getChangeTest = undefined

getCheckerIPRangesTest :: GetCheckerIPRanges -> TestTree
getCheckerIPRangesTest = undefined

getGeoLocationTest :: GetGeoLocation -> TestTree
getGeoLocationTest = undefined

getHealthCheckTest :: GetHealthCheck -> TestTree
getHealthCheckTest = undefined

getHealthCheckCountTest :: GetHealthCheckCount -> TestTree
getHealthCheckCountTest = undefined

getHealthCheckLastFailureReasonTest :: GetHealthCheckLastFailureReason -> TestTree
getHealthCheckLastFailureReasonTest = undefined

getHealthCheckStatusTest :: GetHealthCheckStatus -> TestTree
getHealthCheckStatusTest = undefined

getHostedZoneTest :: GetHostedZone -> TestTree
getHostedZoneTest = undefined

getHostedZoneCountTest :: GetHostedZoneCount -> TestTree
getHostedZoneCountTest = undefined

getReusableDelegationSetTest :: GetReusableDelegationSet -> TestTree
getReusableDelegationSetTest = undefined

listGeoLocationsTest :: ListGeoLocations -> TestTree
listGeoLocationsTest = undefined

listHealthChecksTest :: ListHealthChecks -> TestTree
listHealthChecksTest = undefined

listHostedZonesTest :: ListHostedZones -> TestTree
listHostedZonesTest = undefined

listHostedZonesByNameTest :: ListHostedZonesByName -> TestTree
listHostedZonesByNameTest = undefined

listResourceRecordSetsTest :: ListResourceRecordSets -> TestTree
listResourceRecordSetsTest = undefined

listReusableDelegationSetsTest :: ListReusableDelegationSets -> TestTree
listReusableDelegationSetsTest = undefined

listTagsForResourceTest :: ListTagsForResource -> TestTree
listTagsForResourceTest = undefined

listTagsForResourcesTest :: ListTagsForResources -> TestTree
listTagsForResourcesTest = undefined

updateHealthCheckTest :: UpdateHealthCheck -> TestTree
updateHealthCheckTest = undefined

updateHostedZoneCommentTest :: UpdateHostedZoneComment -> TestTree
updateHostedZoneCommentTest = undefined

-- Responses

associateVPCWithHostedZoneResponseTest :: AssociateVPCWithHostedZoneResponse -> TestTree
associateVPCWithHostedZoneResponseTest = resp
    "associateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse"
    (Proxy :: Proxy AssociateVPCWithHostedZone)

changeResourceRecordSetsResponseTest :: ChangeResourceRecordSetsResponse -> TestTree
changeResourceRecordSetsResponseTest = resp
    "changeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse"
    (Proxy :: Proxy ChangeResourceRecordSets)

changeTagsForResourceResponseTest :: ChangeTagsForResourceResponse -> TestTree
changeTagsForResourceResponseTest = resp
    "changeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse"
    (Proxy :: Proxy ChangeTagsForResource)

createHealthCheckResponseTest :: CreateHealthCheckResponse -> TestTree
createHealthCheckResponseTest = resp
    "createHealthCheckResponse"
    "fixture/CreateHealthCheckResponse"
    (Proxy :: Proxy CreateHealthCheck)

createHostedZoneResponseTest :: CreateHostedZoneResponse -> TestTree
createHostedZoneResponseTest = resp
    "createHostedZoneResponse"
    "fixture/CreateHostedZoneResponse"
    (Proxy :: Proxy CreateHostedZone)

createReusableDelegationSetResponseTest :: CreateReusableDelegationSetResponse -> TestTree
createReusableDelegationSetResponseTest = resp
    "createReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse"
    (Proxy :: Proxy CreateReusableDelegationSet)

deleteHealthCheckResponseTest :: DeleteHealthCheckResponse -> TestTree
deleteHealthCheckResponseTest = resp
    "deleteHealthCheckResponse"
    "fixture/DeleteHealthCheckResponse"
    (Proxy :: Proxy DeleteHealthCheck)

deleteHostedZoneResponseTest :: DeleteHostedZoneResponse -> TestTree
deleteHostedZoneResponseTest = resp
    "deleteHostedZoneResponse"
    "fixture/DeleteHostedZoneResponse"
    (Proxy :: Proxy DeleteHostedZone)

deleteReusableDelegationSetResponseTest :: DeleteReusableDelegationSetResponse -> TestTree
deleteReusableDelegationSetResponseTest = resp
    "deleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse"
    (Proxy :: Proxy DeleteReusableDelegationSet)

disassociateVPCFromHostedZoneResponseTest :: DisassociateVPCFromHostedZoneResponse -> TestTree
disassociateVPCFromHostedZoneResponseTest = resp
    "disassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse"
    (Proxy :: Proxy DisassociateVPCFromHostedZone)

getChangeResponseTest :: GetChangeResponse -> TestTree
getChangeResponseTest = resp
    "getChangeResponse"
    "fixture/GetChangeResponse"
    (Proxy :: Proxy GetChange)

getCheckerIPRangesResponseTest :: GetCheckerIPRangesResponse -> TestTree
getCheckerIPRangesResponseTest = resp
    "getCheckerIPRangesResponse"
    "fixture/GetCheckerIPRangesResponse"
    (Proxy :: Proxy GetCheckerIPRanges)

getGeoLocationResponseTest :: GetGeoLocationResponse -> TestTree
getGeoLocationResponseTest = resp
    "getGeoLocationResponse"
    "fixture/GetGeoLocationResponse"
    (Proxy :: Proxy GetGeoLocation)

getHealthCheckResponseTest :: GetHealthCheckResponse -> TestTree
getHealthCheckResponseTest = resp
    "getHealthCheckResponse"
    "fixture/GetHealthCheckResponse"
    (Proxy :: Proxy GetHealthCheck)

getHealthCheckCountResponseTest :: GetHealthCheckCountResponse -> TestTree
getHealthCheckCountResponseTest = resp
    "getHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse"
    (Proxy :: Proxy GetHealthCheckCount)

getHealthCheckLastFailureReasonResponseTest :: GetHealthCheckLastFailureReasonResponse -> TestTree
getHealthCheckLastFailureReasonResponseTest = resp
    "getHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse"
    (Proxy :: Proxy GetHealthCheckLastFailureReason)

getHealthCheckStatusResponseTest :: GetHealthCheckStatusResponse -> TestTree
getHealthCheckStatusResponseTest = resp
    "getHealthCheckStatusResponse"
    "fixture/GetHealthCheckStatusResponse"
    (Proxy :: Proxy GetHealthCheckStatus)

getHostedZoneResponseTest :: GetHostedZoneResponse -> TestTree
getHostedZoneResponseTest = resp
    "getHostedZoneResponse"
    "fixture/GetHostedZoneResponse"
    (Proxy :: Proxy GetHostedZone)

getHostedZoneCountResponseTest :: GetHostedZoneCountResponse -> TestTree
getHostedZoneCountResponseTest = resp
    "getHostedZoneCountResponse"
    "fixture/GetHostedZoneCountResponse"
    (Proxy :: Proxy GetHostedZoneCount)

getReusableDelegationSetResponseTest :: GetReusableDelegationSetResponse -> TestTree
getReusableDelegationSetResponseTest = resp
    "getReusableDelegationSetResponse"
    "fixture/GetReusableDelegationSetResponse"
    (Proxy :: Proxy GetReusableDelegationSet)

listGeoLocationsResponseTest :: ListGeoLocationsResponse -> TestTree
listGeoLocationsResponseTest = resp
    "listGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse"
    (Proxy :: Proxy ListGeoLocations)

listHealthChecksResponseTest :: ListHealthChecksResponse -> TestTree
listHealthChecksResponseTest = resp
    "listHealthChecksResponse"
    "fixture/ListHealthChecksResponse"
    (Proxy :: Proxy ListHealthChecks)

listHostedZonesResponseTest :: ListHostedZonesResponse -> TestTree
listHostedZonesResponseTest = resp
    "listHostedZonesResponse"
    "fixture/ListHostedZonesResponse"
    (Proxy :: Proxy ListHostedZones)

listHostedZonesByNameResponseTest :: ListHostedZonesByNameResponse -> TestTree
listHostedZonesByNameResponseTest = resp
    "listHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse"
    (Proxy :: Proxy ListHostedZonesByName)

listResourceRecordSetsResponseTest :: ListResourceRecordSetsResponse -> TestTree
listResourceRecordSetsResponseTest = resp
    "listResourceRecordSetsResponse"
    "fixture/ListResourceRecordSetsResponse"
    (Proxy :: Proxy ListResourceRecordSets)

listReusableDelegationSetsResponseTest :: ListReusableDelegationSetsResponse -> TestTree
listReusableDelegationSetsResponseTest = resp
    "listReusableDelegationSetsResponse"
    "fixture/ListReusableDelegationSetsResponse"
    (Proxy :: Proxy ListReusableDelegationSets)

listTagsForResourceResponseTest :: ListTagsForResourceResponse -> TestTree
listTagsForResourceResponseTest = resp
    "listTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse"
    (Proxy :: Proxy ListTagsForResource)

listTagsForResourcesResponseTest :: ListTagsForResourcesResponse -> TestTree
listTagsForResourcesResponseTest = resp
    "listTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse"
    (Proxy :: Proxy ListTagsForResources)

updateHealthCheckResponseTest :: UpdateHealthCheckResponse -> TestTree
updateHealthCheckResponseTest = resp
    "updateHealthCheckResponse"
    "fixture/UpdateHealthCheckResponse"
    (Proxy :: Proxy UpdateHealthCheck)

updateHostedZoneCommentResponseTest :: UpdateHostedZoneCommentResponse -> TestTree
updateHostedZoneCommentResponseTest = resp
    "updateHostedZoneCommentResponse"
    "fixture/UpdateHostedZoneCommentResponse"
    (Proxy :: Proxy UpdateHostedZoneComment)
