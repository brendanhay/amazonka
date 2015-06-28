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

import           Data.Proxy
import           Network.AWS.Route53
import           Test.AWS.Fixture
import           Test.Tasty

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
--         , getHealthCheckLastFailureReasonTest $
--             getHealthCheckLastFailureReason
--
--         , listHostedZonesByNameTest $
--             listHostedZonesByName
--
--         , deleteReusableDelegationSetTest $
--             deleteReusableDelegationSet
--
--         , listReusableDelegationSetsTest $
--             listReusableDelegationSets
--
--         , getCheckerIPRangesTest $
--             getCheckerIPRanges
--
--         , listTagsForResourceTest $
--             listTagsForResource
--
--         , changeResourceRecordSetsTest $
--             changeResourceRecordSets
--
--         , getChangeTest $
--             getChange
--
--         , createHostedZoneTest $
--             createHostedZone
--
--         , deleteHealthCheckTest $
--             deleteHealthCheck
--
--         , updateHealthCheckTest $
--             updateHealthCheck
--
--         , changeTagsForResourceTest $
--             changeTagsForResource
--
--         , createHealthCheckTest $
--             createHealthCheck
--
--         , listHostedZonesTest $
--             listHostedZones
--
--         , disassociateVPCFromHostedZoneTest $
--             disassociateVPCFromHostedZone
--
--         , getHostedZoneTest $
--             getHostedZone
--
--         , listGeoLocationsTest $
--             listGeoLocations
--
--         , getHealthCheckTest $
--             getHealthCheck
--
--         , listResourceRecordSetsTest $
--             listResourceRecordSets
--
--         , getHealthCheckCountTest $
--             getHealthCheckCount
--
--         , createReusableDelegationSetTest $
--             createReusableDelegationSet
--
--         , getHostedZoneCountTest $
--             getHostedZoneCount
--
--         , getReusableDelegationSetTest $
--             getReusableDelegationSet
--
--         , updateHostedZoneCommentTest $
--             updateHostedZoneComment
--
--         , getHealthCheckStatusTest $
--             getHealthCheckStatus
--
--         , listHealthChecksTest $
--             listHealthChecks
--
--         , deleteHostedZoneTest $
--             deleteHostedZone
--
--         , getGeoLocationTest $
--             getGeoLocation
--
--         , listTagsForResourcesTest $
--             listTagsForResources
--
--           ]

--     , testGroup "response"
--         [ associateVPCWithHostedZoneResponseTest $
--             associateVPCWithHostedZoneResponse
--
--         , getHealthCheckLastFailureReasonResponseTest $
--             getHealthCheckLastFailureReasonResponse
--
--         , listHostedZonesByNameResponseTest $
--             listHostedZonesByNameResponse
--
--         , deleteReusableDelegationSetResponseTest $
--             deleteReusableDelegationSetResponse
--
--         , listReusableDelegationSetsResponseTest $
--             listReusableDelegationSetsResponse
--
--         , getCheckerIPRangesResponseTest $
--             getCheckerIPRangesResponse
--
--         , listTagsForResourceResponseTest $
--             listTagsForResourceResponse
--
--         , changeResourceRecordSetsResponseTest $
--             changeResourceRecordSetsResponse
--
--         , getChangeResponseTest $
--             getChangeResponse
--
--         , createHostedZoneResponseTest $
--             createHostedZoneResponse
--
--         , deleteHealthCheckResponseTest $
--             deleteHealthCheckResponse
--
--         , updateHealthCheckResponseTest $
--             updateHealthCheckResponse
--
--         , changeTagsForResourceResponseTest $
--             changeTagsForResourceResponse
--
--         , createHealthCheckResponseTest $
--             createHealthCheckResponse
--
--         , listHostedZonesResponseTest $
--             listHostedZonesResponse
--
--         , disassociateVPCFromHostedZoneResponseTest $
--             disassociateVPCFromHostedZoneResponse
--
--         , getHostedZoneResponseTest $
--             getHostedZoneResponse
--
--         , listGeoLocationsResponseTest $
--             listGeoLocationsResponse
--
--         , getHealthCheckResponseTest $
--             getHealthCheckResponse
--
--         , listResourceRecordSetsResponseTest $
--             listResourceRecordSetsResponse
--
--         , getHealthCheckCountResponseTest $
--             getHealthCheckCountResponse
--
--         , createReusableDelegationSetResponseTest $
--             createReusableDelegationSetResponse
--
--         , getHostedZoneCountResponseTest $
--             getHostedZoneCountResponse
--
--         , getReusableDelegationSetResponseTest $
--             getReusableDelegationSetResponse
--
--         , updateHostedZoneCommentResponseTest $
--             updateHostedZoneCommentResponse
--
--         , getHealthCheckStatusResponseTest $
--             getHealthCheckStatusResponse
--
--         , listHealthChecksResponseTest $
--             listHealthChecksResponse
--
--         , deleteHostedZoneResponseTest $
--             deleteHostedZoneResponse
--
--         , getGeoLocationResponseTest $
--             getGeoLocationResponse
--
--         , listTagsForResourcesResponseTest $
--             listTagsForResourcesResponse
--
--           ]
--     ]

-- Requests

associateVPCWithHostedZoneTest :: AssociateVPCWithHostedZone -> TestTree
associateVPCWithHostedZoneTest = undefined

getHealthCheckLastFailureReasonTest :: GetHealthCheckLastFailureReason -> TestTree
getHealthCheckLastFailureReasonTest = undefined

listHostedZonesByNameTest :: ListHostedZonesByName -> TestTree
listHostedZonesByNameTest = undefined

deleteReusableDelegationSetTest :: DeleteReusableDelegationSet -> TestTree
deleteReusableDelegationSetTest = undefined

listReusableDelegationSetsTest :: ListReusableDelegationSets -> TestTree
listReusableDelegationSetsTest = undefined

getCheckerIPRangesTest :: GetCheckerIPRanges -> TestTree
getCheckerIPRangesTest = undefined

listTagsForResourceTest :: ListTagsForResource -> TestTree
listTagsForResourceTest = undefined

changeResourceRecordSetsTest :: ChangeResourceRecordSets -> TestTree
changeResourceRecordSetsTest = undefined

getChangeTest :: GetChange -> TestTree
getChangeTest = undefined

createHostedZoneTest :: CreateHostedZone -> TestTree
createHostedZoneTest = undefined

deleteHealthCheckTest :: DeleteHealthCheck -> TestTree
deleteHealthCheckTest = undefined

updateHealthCheckTest :: UpdateHealthCheck -> TestTree
updateHealthCheckTest = undefined

changeTagsForResourceTest :: ChangeTagsForResource -> TestTree
changeTagsForResourceTest = undefined

createHealthCheckTest :: CreateHealthCheck -> TestTree
createHealthCheckTest = undefined

listHostedZonesTest :: ListHostedZones -> TestTree
listHostedZonesTest = undefined

disassociateVPCFromHostedZoneTest :: DisassociateVPCFromHostedZone -> TestTree
disassociateVPCFromHostedZoneTest = undefined

getHostedZoneTest :: GetHostedZone -> TestTree
getHostedZoneTest = undefined

listGeoLocationsTest :: ListGeoLocations -> TestTree
listGeoLocationsTest = undefined

getHealthCheckTest :: GetHealthCheck -> TestTree
getHealthCheckTest = undefined

listResourceRecordSetsTest :: ListResourceRecordSets -> TestTree
listResourceRecordSetsTest = undefined

getHealthCheckCountTest :: GetHealthCheckCount -> TestTree
getHealthCheckCountTest = undefined

createReusableDelegationSetTest :: CreateReusableDelegationSet -> TestTree
createReusableDelegationSetTest = undefined

getHostedZoneCountTest :: GetHostedZoneCount -> TestTree
getHostedZoneCountTest = undefined

getReusableDelegationSetTest :: GetReusableDelegationSet -> TestTree
getReusableDelegationSetTest = undefined

updateHostedZoneCommentTest :: UpdateHostedZoneComment -> TestTree
updateHostedZoneCommentTest = undefined

getHealthCheckStatusTest :: GetHealthCheckStatus -> TestTree
getHealthCheckStatusTest = undefined

listHealthChecksTest :: ListHealthChecks -> TestTree
listHealthChecksTest = undefined

deleteHostedZoneTest :: DeleteHostedZone -> TestTree
deleteHostedZoneTest = undefined

getGeoLocationTest :: GetGeoLocation -> TestTree
getGeoLocationTest = undefined

listTagsForResourcesTest :: ListTagsForResources -> TestTree
listTagsForResourcesTest = undefined

-- Responses

associateVPCWithHostedZoneResponseTest :: AssociateVPCWithHostedZoneResponse -> TestTree
associateVPCWithHostedZoneResponseTest = resp
    "AssociateVPCWithHostedZone"
    "fixture/Route53/AssociateVPCWithHostedZoneResponse"
    (Proxy :: Proxy AssociateVPCWithHostedZone)

getHealthCheckLastFailureReasonResponseTest :: GetHealthCheckLastFailureReasonResponse -> TestTree
getHealthCheckLastFailureReasonResponseTest = resp
    "GetHealthCheckLastFailureReason"
    "fixture/Route53/GetHealthCheckLastFailureReasonResponse"
    (Proxy :: Proxy GetHealthCheckLastFailureReason)

listHostedZonesByNameResponseTest :: ListHostedZonesByNameResponse -> TestTree
listHostedZonesByNameResponseTest = resp
    "ListHostedZonesByName"
    "fixture/Route53/ListHostedZonesByNameResponse"
    (Proxy :: Proxy ListHostedZonesByName)

deleteReusableDelegationSetResponseTest :: DeleteReusableDelegationSetResponse -> TestTree
deleteReusableDelegationSetResponseTest = resp
    "DeleteReusableDelegationSet"
    "fixture/Route53/DeleteReusableDelegationSetResponse"
    (Proxy :: Proxy DeleteReusableDelegationSet)

listReusableDelegationSetsResponseTest :: ListReusableDelegationSetsResponse -> TestTree
listReusableDelegationSetsResponseTest = resp
    "ListReusableDelegationSets"
    "fixture/Route53/ListReusableDelegationSetsResponse"
    (Proxy :: Proxy ListReusableDelegationSets)

getCheckerIPRangesResponseTest :: GetCheckerIPRangesResponse -> TestTree
getCheckerIPRangesResponseTest = resp
    "GetCheckerIPRanges"
    "fixture/Route53/GetCheckerIPRangesResponse"
    (Proxy :: Proxy GetCheckerIPRanges)

listTagsForResourceResponseTest :: ListTagsForResourceResponse -> TestTree
listTagsForResourceResponseTest = resp
    "ListTagsForResource"
    "fixture/Route53/ListTagsForResourceResponse"
    (Proxy :: Proxy ListTagsForResource)

changeResourceRecordSetsResponseTest :: ChangeResourceRecordSetsResponse -> TestTree
changeResourceRecordSetsResponseTest = resp
    "ChangeResourceRecordSets"
    "fixture/Route53/ChangeResourceRecordSetsResponse"
    (Proxy :: Proxy ChangeResourceRecordSets)

getChangeResponseTest :: GetChangeResponse -> TestTree
getChangeResponseTest = resp
    "GetChange"
    "fixture/Route53/GetChangeResponse"
    (Proxy :: Proxy GetChange)

createHostedZoneResponseTest :: CreateHostedZoneResponse -> TestTree
createHostedZoneResponseTest = resp
    "CreateHostedZone"
    "fixture/Route53/CreateHostedZoneResponse"
    (Proxy :: Proxy CreateHostedZone)

deleteHealthCheckResponseTest :: DeleteHealthCheckResponse -> TestTree
deleteHealthCheckResponseTest = resp
    "DeleteHealthCheck"
    "fixture/Route53/DeleteHealthCheckResponse"
    (Proxy :: Proxy DeleteHealthCheck)

updateHealthCheckResponseTest :: UpdateHealthCheckResponse -> TestTree
updateHealthCheckResponseTest = resp
    "UpdateHealthCheck"
    "fixture/Route53/UpdateHealthCheckResponse"
    (Proxy :: Proxy UpdateHealthCheck)

changeTagsForResourceResponseTest :: ChangeTagsForResourceResponse -> TestTree
changeTagsForResourceResponseTest = resp
    "ChangeTagsForResource"
    "fixture/Route53/ChangeTagsForResourceResponse"
    (Proxy :: Proxy ChangeTagsForResource)

createHealthCheckResponseTest :: CreateHealthCheckResponse -> TestTree
createHealthCheckResponseTest = resp
    "CreateHealthCheck"
    "fixture/Route53/CreateHealthCheckResponse"
    (Proxy :: Proxy CreateHealthCheck)

listHostedZonesResponseTest :: ListHostedZonesResponse -> TestTree
listHostedZonesResponseTest = resp
    "ListHostedZones"
    "fixture/Route53/ListHostedZonesResponse"
    (Proxy :: Proxy ListHostedZones)

disassociateVPCFromHostedZoneResponseTest :: DisassociateVPCFromHostedZoneResponse -> TestTree
disassociateVPCFromHostedZoneResponseTest = resp
    "DisassociateVPCFromHostedZone"
    "fixture/Route53/DisassociateVPCFromHostedZoneResponse"
    (Proxy :: Proxy DisassociateVPCFromHostedZone)

getHostedZoneResponseTest :: GetHostedZoneResponse -> TestTree
getHostedZoneResponseTest = resp
    "GetHostedZone"
    "fixture/Route53/GetHostedZoneResponse"
    (Proxy :: Proxy GetHostedZone)

listGeoLocationsResponseTest :: ListGeoLocationsResponse -> TestTree
listGeoLocationsResponseTest = resp
    "ListGeoLocations"
    "fixture/Route53/ListGeoLocationsResponse"
    (Proxy :: Proxy ListGeoLocations)

getHealthCheckResponseTest :: GetHealthCheckResponse -> TestTree
getHealthCheckResponseTest = resp
    "GetHealthCheck"
    "fixture/Route53/GetHealthCheckResponse"
    (Proxy :: Proxy GetHealthCheck)

listResourceRecordSetsResponseTest :: ListResourceRecordSetsResponse -> TestTree
listResourceRecordSetsResponseTest = resp
    "ListResourceRecordSets"
    "fixture/Route53/ListResourceRecordSetsResponse"
    (Proxy :: Proxy ListResourceRecordSets)

getHealthCheckCountResponseTest :: GetHealthCheckCountResponse -> TestTree
getHealthCheckCountResponseTest = resp
    "GetHealthCheckCount"
    "fixture/Route53/GetHealthCheckCountResponse"
    (Proxy :: Proxy GetHealthCheckCount)

createReusableDelegationSetResponseTest :: CreateReusableDelegationSetResponse -> TestTree
createReusableDelegationSetResponseTest = resp
    "CreateReusableDelegationSet"
    "fixture/Route53/CreateReusableDelegationSetResponse"
    (Proxy :: Proxy CreateReusableDelegationSet)

getHostedZoneCountResponseTest :: GetHostedZoneCountResponse -> TestTree
getHostedZoneCountResponseTest = resp
    "GetHostedZoneCount"
    "fixture/Route53/GetHostedZoneCountResponse"
    (Proxy :: Proxy GetHostedZoneCount)

getReusableDelegationSetResponseTest :: GetReusableDelegationSetResponse -> TestTree
getReusableDelegationSetResponseTest = resp
    "GetReusableDelegationSet"
    "fixture/Route53/GetReusableDelegationSetResponse"
    (Proxy :: Proxy GetReusableDelegationSet)

updateHostedZoneCommentResponseTest :: UpdateHostedZoneCommentResponse -> TestTree
updateHostedZoneCommentResponseTest = resp
    "UpdateHostedZoneComment"
    "fixture/Route53/UpdateHostedZoneCommentResponse"
    (Proxy :: Proxy UpdateHostedZoneComment)

getHealthCheckStatusResponseTest :: GetHealthCheckStatusResponse -> TestTree
getHealthCheckStatusResponseTest = resp
    "GetHealthCheckStatus"
    "fixture/Route53/GetHealthCheckStatusResponse"
    (Proxy :: Proxy GetHealthCheckStatus)

listHealthChecksResponseTest :: ListHealthChecksResponse -> TestTree
listHealthChecksResponseTest = resp
    "ListHealthChecks"
    "fixture/Route53/ListHealthChecksResponse"
    (Proxy :: Proxy ListHealthChecks)

deleteHostedZoneResponseTest :: DeleteHostedZoneResponse -> TestTree
deleteHostedZoneResponseTest = resp
    "DeleteHostedZone"
    "fixture/Route53/DeleteHostedZoneResponse"
    (Proxy :: Proxy DeleteHostedZone)

getGeoLocationResponseTest :: GetGeoLocationResponse -> TestTree
getGeoLocationResponseTest = resp
    "GetGeoLocation"
    "fixture/Route53/GetGeoLocationResponse"
    (Proxy :: Proxy GetGeoLocation)

listTagsForResourcesResponseTest :: ListTagsForResourcesResponse -> TestTree
listTagsForResourcesResponseTest = resp
    "ListTagsForResources"
    "fixture/Route53/ListTagsForResourcesResponse"
    (Proxy :: Proxy ListTagsForResources)
