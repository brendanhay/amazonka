-- Module      : Test.AWS.Gen.ELB
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

module Test.AWS.Gen.ELB where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.ELB

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ addTagsTest $
--             addTags
--
--         , applySecurityGroupsToLoadBalancerTest $
--             applySecurityGroupsToLoadBalancer
--
--         , attachLoadBalancerToSubnetsTest $
--             attachLoadBalancerToSubnets
--
--         , configureHealthCheckTest $
--             configureHealthCheck
--
--         , createAppCookieStickinessPolicyTest $
--             createAppCookieStickinessPolicy
--
--         , createLBCookieStickinessPolicyTest $
--             createLBCookieStickinessPolicy
--
--         , createLoadBalancerTest $
--             createLoadBalancer
--
--         , createLoadBalancerListenersTest $
--             createLoadBalancerListeners
--
--         , createLoadBalancerPolicyTest $
--             createLoadBalancerPolicy
--
--         , deleteLoadBalancerTest $
--             deleteLoadBalancer
--
--         , deleteLoadBalancerListenersTest $
--             deleteLoadBalancerListeners
--
--         , deleteLoadBalancerPolicyTest $
--             deleteLoadBalancerPolicy
--
--         , deregisterInstancesFromLoadBalancerTest $
--             deregisterInstancesFromLoadBalancer
--
--         , describeInstanceHealthTest $
--             describeInstanceHealth
--
--         , describeLoadBalancerAttributesTest $
--             describeLoadBalancerAttributes
--
--         , describeLoadBalancerPoliciesTest $
--             describeLoadBalancerPolicies
--
--         , describeLoadBalancerPolicyTypesTest $
--             describeLoadBalancerPolicyTypes
--
--         , describeLoadBalancersTest $
--             describeLoadBalancers
--
--         , describeTagsTest $
--             describeTags
--
--         , detachLoadBalancerFromSubnetsTest $
--             detachLoadBalancerFromSubnets
--
--         , disableAvailabilityZonesForLoadBalancerTest $
--             disableAvailabilityZonesForLoadBalancer
--
--         , enableAvailabilityZonesForLoadBalancerTest $
--             enableAvailabilityZonesForLoadBalancer
--
--         , modifyLoadBalancerAttributesTest $
--             modifyLoadBalancerAttributes
--
--         , registerInstancesWithLoadBalancerTest $
--             registerInstancesWithLoadBalancer
--
--         , removeTagsTest $
--             removeTags
--
--         , setLoadBalancerListenerSSLCertificateTest $
--             setLoadBalancerListenerSSLCertificate
--
--         , setLoadBalancerPoliciesForBackendServerTest $
--             setLoadBalancerPoliciesForBackendServer
--
--         , setLoadBalancerPoliciesOfListenerTest $
--             setLoadBalancerPoliciesOfListener
--
--           ]

--     , testGroup "response"
--         [ addTagsResponseTest $
--             addTagsResponse
--
--         , applySecurityGroupsToLoadBalancerResponseTest $
--             applySecurityGroupsToLoadBalancerResponse
--
--         , attachLoadBalancerToSubnetsResponseTest $
--             attachLoadBalancerToSubnetsResponse
--
--         , configureHealthCheckResponseTest $
--             configureHealthCheckResponse
--
--         , createAppCookieStickinessPolicyResponseTest $
--             createAppCookieStickinessPolicyResponse
--
--         , createLBCookieStickinessPolicyResponseTest $
--             createLBCookieStickinessPolicyResponse
--
--         , createLoadBalancerResponseTest $
--             createLoadBalancerResponse
--
--         , createLoadBalancerListenersResponseTest $
--             createLoadBalancerListenersResponse
--
--         , createLoadBalancerPolicyResponseTest $
--             createLoadBalancerPolicyResponse
--
--         , deleteLoadBalancerResponseTest $
--             deleteLoadBalancerResponse
--
--         , deleteLoadBalancerListenersResponseTest $
--             deleteLoadBalancerListenersResponse
--
--         , deleteLoadBalancerPolicyResponseTest $
--             deleteLoadBalancerPolicyResponse
--
--         , deregisterInstancesFromLoadBalancerResponseTest $
--             deregisterInstancesFromLoadBalancerResponse
--
--         , describeInstanceHealthResponseTest $
--             describeInstanceHealthResponse
--
--         , describeLoadBalancerAttributesResponseTest $
--             describeLoadBalancerAttributesResponse
--
--         , describeLoadBalancerPoliciesResponseTest $
--             describeLoadBalancerPoliciesResponse
--
--         , describeLoadBalancerPolicyTypesResponseTest $
--             describeLoadBalancerPolicyTypesResponse
--
--         , describeLoadBalancersResponseTest $
--             describeLoadBalancersResponse
--
--         , describeTagsResponseTest $
--             describeTagsResponse
--
--         , detachLoadBalancerFromSubnetsResponseTest $
--             detachLoadBalancerFromSubnetsResponse
--
--         , disableAvailabilityZonesForLoadBalancerResponseTest $
--             disableAvailabilityZonesForLoadBalancerResponse
--
--         , enableAvailabilityZonesForLoadBalancerResponseTest $
--             enableAvailabilityZonesForLoadBalancerResponse
--
--         , modifyLoadBalancerAttributesResponseTest $
--             modifyLoadBalancerAttributesResponse
--
--         , registerInstancesWithLoadBalancerResponseTest $
--             registerInstancesWithLoadBalancerResponse
--
--         , removeTagsResponseTest $
--             removeTagsResponse
--
--         , setLoadBalancerListenerSSLCertificateResponseTest $
--             setLoadBalancerListenerSSLCertificateResponse
--
--         , setLoadBalancerPoliciesForBackendServerResponseTest $
--             setLoadBalancerPoliciesForBackendServerResponse
--
--         , setLoadBalancerPoliciesOfListenerResponseTest $
--             setLoadBalancerPoliciesOfListenerResponse
--
--           ]
--     ]

-- Requests

addTagsTest :: AddTags -> TestTree
addTagsTest = undefined

applySecurityGroupsToLoadBalancerTest :: ApplySecurityGroupsToLoadBalancer -> TestTree
applySecurityGroupsToLoadBalancerTest = undefined

attachLoadBalancerToSubnetsTest :: AttachLoadBalancerToSubnets -> TestTree
attachLoadBalancerToSubnetsTest = undefined

configureHealthCheckTest :: ConfigureHealthCheck -> TestTree
configureHealthCheckTest = undefined

createAppCookieStickinessPolicyTest :: CreateAppCookieStickinessPolicy -> TestTree
createAppCookieStickinessPolicyTest = undefined

createLBCookieStickinessPolicyTest :: CreateLBCookieStickinessPolicy -> TestTree
createLBCookieStickinessPolicyTest = undefined

createLoadBalancerTest :: CreateLoadBalancer -> TestTree
createLoadBalancerTest = undefined

createLoadBalancerListenersTest :: CreateLoadBalancerListeners -> TestTree
createLoadBalancerListenersTest = undefined

createLoadBalancerPolicyTest :: CreateLoadBalancerPolicy -> TestTree
createLoadBalancerPolicyTest = undefined

deleteLoadBalancerTest :: DeleteLoadBalancer -> TestTree
deleteLoadBalancerTest = undefined

deleteLoadBalancerListenersTest :: DeleteLoadBalancerListeners -> TestTree
deleteLoadBalancerListenersTest = undefined

deleteLoadBalancerPolicyTest :: DeleteLoadBalancerPolicy -> TestTree
deleteLoadBalancerPolicyTest = undefined

deregisterInstancesFromLoadBalancerTest :: DeregisterInstancesFromLoadBalancer -> TestTree
deregisterInstancesFromLoadBalancerTest = undefined

describeInstanceHealthTest :: DescribeInstanceHealth -> TestTree
describeInstanceHealthTest = undefined

describeLoadBalancerAttributesTest :: DescribeLoadBalancerAttributes -> TestTree
describeLoadBalancerAttributesTest = undefined

describeLoadBalancerPoliciesTest :: DescribeLoadBalancerPolicies -> TestTree
describeLoadBalancerPoliciesTest = undefined

describeLoadBalancerPolicyTypesTest :: DescribeLoadBalancerPolicyTypes -> TestTree
describeLoadBalancerPolicyTypesTest = undefined

describeLoadBalancersTest :: DescribeLoadBalancers -> TestTree
describeLoadBalancersTest = undefined

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

detachLoadBalancerFromSubnetsTest :: DetachLoadBalancerFromSubnets -> TestTree
detachLoadBalancerFromSubnetsTest = undefined

disableAvailabilityZonesForLoadBalancerTest :: DisableAvailabilityZonesForLoadBalancer -> TestTree
disableAvailabilityZonesForLoadBalancerTest = undefined

enableAvailabilityZonesForLoadBalancerTest :: EnableAvailabilityZonesForLoadBalancer -> TestTree
enableAvailabilityZonesForLoadBalancerTest = undefined

modifyLoadBalancerAttributesTest :: ModifyLoadBalancerAttributes -> TestTree
modifyLoadBalancerAttributesTest = undefined

registerInstancesWithLoadBalancerTest :: RegisterInstancesWithLoadBalancer -> TestTree
registerInstancesWithLoadBalancerTest = undefined

removeTagsTest :: RemoveTags -> TestTree
removeTagsTest = undefined

setLoadBalancerListenerSSLCertificateTest :: SetLoadBalancerListenerSSLCertificate -> TestTree
setLoadBalancerListenerSSLCertificateTest = undefined

setLoadBalancerPoliciesForBackendServerTest :: SetLoadBalancerPoliciesForBackendServer -> TestTree
setLoadBalancerPoliciesForBackendServerTest = undefined

setLoadBalancerPoliciesOfListenerTest :: SetLoadBalancerPoliciesOfListener -> TestTree
setLoadBalancerPoliciesOfListenerTest = undefined

-- Responses

addTagsResponseTest :: AddTagsResponse -> TestTree
addTagsResponseTest = resp
    "addTagsResponse"
    "fixture/AddTagsResponse"
    (Proxy :: Proxy AddTags)

applySecurityGroupsToLoadBalancerResponseTest :: ApplySecurityGroupsToLoadBalancerResponse -> TestTree
applySecurityGroupsToLoadBalancerResponseTest = resp
    "applySecurityGroupsToLoadBalancerResponse"
    "fixture/ApplySecurityGroupsToLoadBalancerResponse"
    (Proxy :: Proxy ApplySecurityGroupsToLoadBalancer)

attachLoadBalancerToSubnetsResponseTest :: AttachLoadBalancerToSubnetsResponse -> TestTree
attachLoadBalancerToSubnetsResponseTest = resp
    "attachLoadBalancerToSubnetsResponse"
    "fixture/AttachLoadBalancerToSubnetsResponse"
    (Proxy :: Proxy AttachLoadBalancerToSubnets)

configureHealthCheckResponseTest :: ConfigureHealthCheckResponse -> TestTree
configureHealthCheckResponseTest = resp
    "configureHealthCheckResponse"
    "fixture/ConfigureHealthCheckResponse"
    (Proxy :: Proxy ConfigureHealthCheck)

createAppCookieStickinessPolicyResponseTest :: CreateAppCookieStickinessPolicyResponse -> TestTree
createAppCookieStickinessPolicyResponseTest = resp
    "createAppCookieStickinessPolicyResponse"
    "fixture/CreateAppCookieStickinessPolicyResponse"
    (Proxy :: Proxy CreateAppCookieStickinessPolicy)

createLBCookieStickinessPolicyResponseTest :: CreateLBCookieStickinessPolicyResponse -> TestTree
createLBCookieStickinessPolicyResponseTest = resp
    "createLBCookieStickinessPolicyResponse"
    "fixture/CreateLBCookieStickinessPolicyResponse"
    (Proxy :: Proxy CreateLBCookieStickinessPolicy)

createLoadBalancerResponseTest :: CreateLoadBalancerResponse -> TestTree
createLoadBalancerResponseTest = resp
    "createLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse"
    (Proxy :: Proxy CreateLoadBalancer)

createLoadBalancerListenersResponseTest :: CreateLoadBalancerListenersResponse -> TestTree
createLoadBalancerListenersResponseTest = resp
    "createLoadBalancerListenersResponse"
    "fixture/CreateLoadBalancerListenersResponse"
    (Proxy :: Proxy CreateLoadBalancerListeners)

createLoadBalancerPolicyResponseTest :: CreateLoadBalancerPolicyResponse -> TestTree
createLoadBalancerPolicyResponseTest = resp
    "createLoadBalancerPolicyResponse"
    "fixture/CreateLoadBalancerPolicyResponse"
    (Proxy :: Proxy CreateLoadBalancerPolicy)

deleteLoadBalancerResponseTest :: DeleteLoadBalancerResponse -> TestTree
deleteLoadBalancerResponseTest = resp
    "deleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse"
    (Proxy :: Proxy DeleteLoadBalancer)

deleteLoadBalancerListenersResponseTest :: DeleteLoadBalancerListenersResponse -> TestTree
deleteLoadBalancerListenersResponseTest = resp
    "deleteLoadBalancerListenersResponse"
    "fixture/DeleteLoadBalancerListenersResponse"
    (Proxy :: Proxy DeleteLoadBalancerListeners)

deleteLoadBalancerPolicyResponseTest :: DeleteLoadBalancerPolicyResponse -> TestTree
deleteLoadBalancerPolicyResponseTest = resp
    "deleteLoadBalancerPolicyResponse"
    "fixture/DeleteLoadBalancerPolicyResponse"
    (Proxy :: Proxy DeleteLoadBalancerPolicy)

deregisterInstancesFromLoadBalancerResponseTest :: DeregisterInstancesFromLoadBalancerResponse -> TestTree
deregisterInstancesFromLoadBalancerResponseTest = resp
    "deregisterInstancesFromLoadBalancerResponse"
    "fixture/DeregisterInstancesFromLoadBalancerResponse"
    (Proxy :: Proxy DeregisterInstancesFromLoadBalancer)

describeInstanceHealthResponseTest :: DescribeInstanceHealthResponse -> TestTree
describeInstanceHealthResponseTest = resp
    "describeInstanceHealthResponse"
    "fixture/DescribeInstanceHealthResponse"
    (Proxy :: Proxy DescribeInstanceHealth)

describeLoadBalancerAttributesResponseTest :: DescribeLoadBalancerAttributesResponse -> TestTree
describeLoadBalancerAttributesResponseTest = resp
    "describeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse"
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

describeLoadBalancerPoliciesResponseTest :: DescribeLoadBalancerPoliciesResponse -> TestTree
describeLoadBalancerPoliciesResponseTest = resp
    "describeLoadBalancerPoliciesResponse"
    "fixture/DescribeLoadBalancerPoliciesResponse"
    (Proxy :: Proxy DescribeLoadBalancerPolicies)

describeLoadBalancerPolicyTypesResponseTest :: DescribeLoadBalancerPolicyTypesResponse -> TestTree
describeLoadBalancerPolicyTypesResponseTest = resp
    "describeLoadBalancerPolicyTypesResponse"
    "fixture/DescribeLoadBalancerPolicyTypesResponse"
    (Proxy :: Proxy DescribeLoadBalancerPolicyTypes)

describeLoadBalancersResponseTest :: DescribeLoadBalancersResponse -> TestTree
describeLoadBalancersResponseTest = resp
    "describeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse"
    (Proxy :: Proxy DescribeLoadBalancers)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "describeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

detachLoadBalancerFromSubnetsResponseTest :: DetachLoadBalancerFromSubnetsResponse -> TestTree
detachLoadBalancerFromSubnetsResponseTest = resp
    "detachLoadBalancerFromSubnetsResponse"
    "fixture/DetachLoadBalancerFromSubnetsResponse"
    (Proxy :: Proxy DetachLoadBalancerFromSubnets)

disableAvailabilityZonesForLoadBalancerResponseTest :: DisableAvailabilityZonesForLoadBalancerResponse -> TestTree
disableAvailabilityZonesForLoadBalancerResponseTest = resp
    "disableAvailabilityZonesForLoadBalancerResponse"
    "fixture/DisableAvailabilityZonesForLoadBalancerResponse"
    (Proxy :: Proxy DisableAvailabilityZonesForLoadBalancer)

enableAvailabilityZonesForLoadBalancerResponseTest :: EnableAvailabilityZonesForLoadBalancerResponse -> TestTree
enableAvailabilityZonesForLoadBalancerResponseTest = resp
    "enableAvailabilityZonesForLoadBalancerResponse"
    "fixture/EnableAvailabilityZonesForLoadBalancerResponse"
    (Proxy :: Proxy EnableAvailabilityZonesForLoadBalancer)

modifyLoadBalancerAttributesResponseTest :: ModifyLoadBalancerAttributesResponse -> TestTree
modifyLoadBalancerAttributesResponseTest = resp
    "modifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse"
    (Proxy :: Proxy ModifyLoadBalancerAttributes)

registerInstancesWithLoadBalancerResponseTest :: RegisterInstancesWithLoadBalancerResponse -> TestTree
registerInstancesWithLoadBalancerResponseTest = resp
    "registerInstancesWithLoadBalancerResponse"
    "fixture/RegisterInstancesWithLoadBalancerResponse"
    (Proxy :: Proxy RegisterInstancesWithLoadBalancer)

removeTagsResponseTest :: RemoveTagsResponse -> TestTree
removeTagsResponseTest = resp
    "removeTagsResponse"
    "fixture/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

setLoadBalancerListenerSSLCertificateResponseTest :: SetLoadBalancerListenerSSLCertificateResponse -> TestTree
setLoadBalancerListenerSSLCertificateResponseTest = resp
    "setLoadBalancerListenerSSLCertificateResponse"
    "fixture/SetLoadBalancerListenerSSLCertificateResponse"
    (Proxy :: Proxy SetLoadBalancerListenerSSLCertificate)

setLoadBalancerPoliciesForBackendServerResponseTest :: SetLoadBalancerPoliciesForBackendServerResponse -> TestTree
setLoadBalancerPoliciesForBackendServerResponseTest = resp
    "setLoadBalancerPoliciesForBackendServerResponse"
    "fixture/SetLoadBalancerPoliciesForBackendServerResponse"
    (Proxy :: Proxy SetLoadBalancerPoliciesForBackendServer)

setLoadBalancerPoliciesOfListenerResponseTest :: SetLoadBalancerPoliciesOfListenerResponse -> TestTree
setLoadBalancerPoliciesOfListenerResponseTest = resp
    "setLoadBalancerPoliciesOfListenerResponse"
    "fixture/SetLoadBalancerPoliciesOfListenerResponse"
    (Proxy :: Proxy SetLoadBalancerPoliciesOfListener)
