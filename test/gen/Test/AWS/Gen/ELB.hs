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

import           Data.Proxy
import           Network.AWS.ELB
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeLoadBalancerPolicyTypesTest $
--             describeLoadBalancerPolicyTypes
--
--         , describeTagsTest $
--             describeTags
--
--         , describeLoadBalancersTest $
--             describeLoadBalancers
--
--         , applySecurityGroupsToLoadBalancerTest $
--             applySecurityGroupsToLoadBalancer
--
--         , removeTagsTest $
--             removeTags
--
--         , createLBCookieStickinessPolicyTest $
--             createLBCookieStickinessPolicy
--
--         , deleteLoadBalancerTest $
--             deleteLoadBalancer
--
--         , createLoadBalancerPolicyTest $
--             createLoadBalancerPolicy
--
--         , deregisterInstancesFromLoadBalancerTest $
--             deregisterInstancesFromLoadBalancer
--
--         , describeLoadBalancerPoliciesTest $
--             describeLoadBalancerPolicies
--
--         , disableAvailabilityZonesForLoadBalancerTest $
--             disableAvailabilityZonesForLoadBalancer
--
--         , setLoadBalancerPoliciesForBackendServerTest $
--             setLoadBalancerPoliciesForBackendServer
--
--         , enableAvailabilityZonesForLoadBalancerTest $
--             enableAvailabilityZonesForLoadBalancer
--
--         , setLoadBalancerListenerSSLCertificateTest $
--             setLoadBalancerListenerSSLCertificate
--
--         , configureHealthCheckTest $
--             configureHealthCheck
--
--         , attachLoadBalancerToSubnetsTest $
--             attachLoadBalancerToSubnets
--
--         , modifyLoadBalancerAttributesTest $
--             modifyLoadBalancerAttributes
--
--         , createAppCookieStickinessPolicyTest $
--             createAppCookieStickinessPolicy
--
--         , addTagsTest $
--             addTags
--
--         , describeLoadBalancerAttributesTest $
--             describeLoadBalancerAttributes
--
--         , describeInstanceHealthTest $
--             describeInstanceHealth
--
--         , detachLoadBalancerFromSubnetsTest $
--             detachLoadBalancerFromSubnets
--
--         , registerInstancesWithLoadBalancerTest $
--             registerInstancesWithLoadBalancer
--
--         , deleteLoadBalancerPolicyTest $
--             deleteLoadBalancerPolicy
--
--         , createLoadBalancerListenersTest $
--             createLoadBalancerListeners
--
--         , deleteLoadBalancerListenersTest $
--             deleteLoadBalancerListeners
--
--         , createLoadBalancerTest $
--             createLoadBalancer
--
--         , setLoadBalancerPoliciesOfListenerTest $
--             setLoadBalancerPoliciesOfListener
--
--           ]

--     , testGroup "response"
--         [ describeLoadBalancerPolicyTypesResponseTest $
--             describeLoadBalancerPolicyTypesResponse
--
--         , describeTagsResponseTest $
--             describeTagsResponse
--
--         , describeLoadBalancersResponseTest $
--             describeLoadBalancersResponse
--
--         , applySecurityGroupsToLoadBalancerResponseTest $
--             applySecurityGroupsToLoadBalancerResponse
--
--         , removeTagsResponseTest $
--             removeTagsResponse
--
--         , createLBCookieStickinessPolicyResponseTest $
--             createLBCookieStickinessPolicyResponse
--
--         , deleteLoadBalancerResponseTest $
--             deleteLoadBalancerResponse
--
--         , createLoadBalancerPolicyResponseTest $
--             createLoadBalancerPolicyResponse
--
--         , deregisterInstancesFromLoadBalancerResponseTest $
--             deregisterInstancesFromLoadBalancerResponse
--
--         , describeLoadBalancerPoliciesResponseTest $
--             describeLoadBalancerPoliciesResponse
--
--         , disableAvailabilityZonesForLoadBalancerResponseTest $
--             disableAvailabilityZonesForLoadBalancerResponse
--
--         , setLoadBalancerPoliciesForBackendServerResponseTest $
--             setLoadBalancerPoliciesForBackendServerResponse
--
--         , enableAvailabilityZonesForLoadBalancerResponseTest $
--             enableAvailabilityZonesForLoadBalancerResponse
--
--         , setLoadBalancerListenerSSLCertificateResponseTest $
--             setLoadBalancerListenerSSLCertificateResponse
--
--         , configureHealthCheckResponseTest $
--             configureHealthCheckResponse
--
--         , attachLoadBalancerToSubnetsResponseTest $
--             attachLoadBalancerToSubnetsResponse
--
--         , modifyLoadBalancerAttributesResponseTest $
--             modifyLoadBalancerAttributesResponse
--
--         , createAppCookieStickinessPolicyResponseTest $
--             createAppCookieStickinessPolicyResponse
--
--         , addTagsResponseTest $
--             addTagsResponse
--
--         , describeLoadBalancerAttributesResponseTest $
--             describeLoadBalancerAttributesResponse
--
--         , describeInstanceHealthResponseTest $
--             describeInstanceHealthResponse
--
--         , detachLoadBalancerFromSubnetsResponseTest $
--             detachLoadBalancerFromSubnetsResponse
--
--         , registerInstancesWithLoadBalancerResponseTest $
--             registerInstancesWithLoadBalancerResponse
--
--         , deleteLoadBalancerPolicyResponseTest $
--             deleteLoadBalancerPolicyResponse
--
--         , createLoadBalancerListenersResponseTest $
--             createLoadBalancerListenersResponse
--
--         , deleteLoadBalancerListenersResponseTest $
--             deleteLoadBalancerListenersResponse
--
--         , createLoadBalancerResponseTest $
--             createLoadBalancerResponse
--
--         , setLoadBalancerPoliciesOfListenerResponseTest $
--             setLoadBalancerPoliciesOfListenerResponse
--
--           ]
--     ]

-- Requests

describeLoadBalancerPolicyTypesTest :: DescribeLoadBalancerPolicyTypes -> TestTree
describeLoadBalancerPolicyTypesTest = undefined

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

describeLoadBalancersTest :: DescribeLoadBalancers -> TestTree
describeLoadBalancersTest = undefined

applySecurityGroupsToLoadBalancerTest :: ApplySecurityGroupsToLoadBalancer -> TestTree
applySecurityGroupsToLoadBalancerTest = undefined

removeTagsTest :: RemoveTags -> TestTree
removeTagsTest = undefined

createLBCookieStickinessPolicyTest :: CreateLBCookieStickinessPolicy -> TestTree
createLBCookieStickinessPolicyTest = undefined

deleteLoadBalancerTest :: DeleteLoadBalancer -> TestTree
deleteLoadBalancerTest = undefined

createLoadBalancerPolicyTest :: CreateLoadBalancerPolicy -> TestTree
createLoadBalancerPolicyTest = undefined

deregisterInstancesFromLoadBalancerTest :: DeregisterInstancesFromLoadBalancer -> TestTree
deregisterInstancesFromLoadBalancerTest = undefined

describeLoadBalancerPoliciesTest :: DescribeLoadBalancerPolicies -> TestTree
describeLoadBalancerPoliciesTest = undefined

disableAvailabilityZonesForLoadBalancerTest :: DisableAvailabilityZonesForLoadBalancer -> TestTree
disableAvailabilityZonesForLoadBalancerTest = undefined

setLoadBalancerPoliciesForBackendServerTest :: SetLoadBalancerPoliciesForBackendServer -> TestTree
setLoadBalancerPoliciesForBackendServerTest = undefined

enableAvailabilityZonesForLoadBalancerTest :: EnableAvailabilityZonesForLoadBalancer -> TestTree
enableAvailabilityZonesForLoadBalancerTest = undefined

setLoadBalancerListenerSSLCertificateTest :: SetLoadBalancerListenerSSLCertificate -> TestTree
setLoadBalancerListenerSSLCertificateTest = undefined

configureHealthCheckTest :: ConfigureHealthCheck -> TestTree
configureHealthCheckTest = undefined

attachLoadBalancerToSubnetsTest :: AttachLoadBalancerToSubnets -> TestTree
attachLoadBalancerToSubnetsTest = undefined

modifyLoadBalancerAttributesTest :: ModifyLoadBalancerAttributes -> TestTree
modifyLoadBalancerAttributesTest = undefined

createAppCookieStickinessPolicyTest :: CreateAppCookieStickinessPolicy -> TestTree
createAppCookieStickinessPolicyTest = undefined

addTagsTest :: AddTags -> TestTree
addTagsTest = undefined

describeLoadBalancerAttributesTest :: DescribeLoadBalancerAttributes -> TestTree
describeLoadBalancerAttributesTest = undefined

describeInstanceHealthTest :: DescribeInstanceHealth -> TestTree
describeInstanceHealthTest = undefined

detachLoadBalancerFromSubnetsTest :: DetachLoadBalancerFromSubnets -> TestTree
detachLoadBalancerFromSubnetsTest = undefined

registerInstancesWithLoadBalancerTest :: RegisterInstancesWithLoadBalancer -> TestTree
registerInstancesWithLoadBalancerTest = undefined

deleteLoadBalancerPolicyTest :: DeleteLoadBalancerPolicy -> TestTree
deleteLoadBalancerPolicyTest = undefined

createLoadBalancerListenersTest :: CreateLoadBalancerListeners -> TestTree
createLoadBalancerListenersTest = undefined

deleteLoadBalancerListenersTest :: DeleteLoadBalancerListeners -> TestTree
deleteLoadBalancerListenersTest = undefined

createLoadBalancerTest :: CreateLoadBalancer -> TestTree
createLoadBalancerTest = undefined

setLoadBalancerPoliciesOfListenerTest :: SetLoadBalancerPoliciesOfListener -> TestTree
setLoadBalancerPoliciesOfListenerTest = undefined

-- Responses

describeLoadBalancerPolicyTypesResponseTest :: DescribeLoadBalancerPolicyTypesResponse -> TestTree
describeLoadBalancerPolicyTypesResponseTest = resp
    "DescribeLoadBalancerPolicyTypesResponse"
    "fixture/ELB/DescribeLoadBalancerPolicyTypesResponse"
    (Proxy :: Proxy DescribeLoadBalancerPolicyTypes)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "DescribeTagsResponse"
    "fixture/ELB/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

describeLoadBalancersResponseTest :: DescribeLoadBalancersResponse -> TestTree
describeLoadBalancersResponseTest = resp
    "DescribeLoadBalancersResponse"
    "fixture/ELB/DescribeLoadBalancersResponse"
    (Proxy :: Proxy DescribeLoadBalancers)

applySecurityGroupsToLoadBalancerResponseTest :: ApplySecurityGroupsToLoadBalancerResponse -> TestTree
applySecurityGroupsToLoadBalancerResponseTest = resp
    "ApplySecurityGroupsToLoadBalancerResponse"
    "fixture/ELB/ApplySecurityGroupsToLoadBalancerResponse"
    (Proxy :: Proxy ApplySecurityGroupsToLoadBalancer)

removeTagsResponseTest :: RemoveTagsResponse -> TestTree
removeTagsResponseTest = resp
    "RemoveTagsResponse"
    "fixture/ELB/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

createLBCookieStickinessPolicyResponseTest :: CreateLBCookieStickinessPolicyResponse -> TestTree
createLBCookieStickinessPolicyResponseTest = resp
    "CreateLBCookieStickinessPolicyResponse"
    "fixture/ELB/CreateLBCookieStickinessPolicyResponse"
    (Proxy :: Proxy CreateLBCookieStickinessPolicy)

deleteLoadBalancerResponseTest :: DeleteLoadBalancerResponse -> TestTree
deleteLoadBalancerResponseTest = resp
    "DeleteLoadBalancerResponse"
    "fixture/ELB/DeleteLoadBalancerResponse"
    (Proxy :: Proxy DeleteLoadBalancer)

createLoadBalancerPolicyResponseTest :: CreateLoadBalancerPolicyResponse -> TestTree
createLoadBalancerPolicyResponseTest = resp
    "CreateLoadBalancerPolicyResponse"
    "fixture/ELB/CreateLoadBalancerPolicyResponse"
    (Proxy :: Proxy CreateLoadBalancerPolicy)

deregisterInstancesFromLoadBalancerResponseTest :: DeregisterInstancesFromLoadBalancerResponse -> TestTree
deregisterInstancesFromLoadBalancerResponseTest = resp
    "DeregisterInstancesFromLoadBalancerResponse"
    "fixture/ELB/DeregisterInstancesFromLoadBalancerResponse"
    (Proxy :: Proxy DeregisterInstancesFromLoadBalancer)

describeLoadBalancerPoliciesResponseTest :: DescribeLoadBalancerPoliciesResponse -> TestTree
describeLoadBalancerPoliciesResponseTest = resp
    "DescribeLoadBalancerPoliciesResponse"
    "fixture/ELB/DescribeLoadBalancerPoliciesResponse"
    (Proxy :: Proxy DescribeLoadBalancerPolicies)

disableAvailabilityZonesForLoadBalancerResponseTest :: DisableAvailabilityZonesForLoadBalancerResponse -> TestTree
disableAvailabilityZonesForLoadBalancerResponseTest = resp
    "DisableAvailabilityZonesForLoadBalancerResponse"
    "fixture/ELB/DisableAvailabilityZonesForLoadBalancerResponse"
    (Proxy :: Proxy DisableAvailabilityZonesForLoadBalancer)

setLoadBalancerPoliciesForBackendServerResponseTest :: SetLoadBalancerPoliciesForBackendServerResponse -> TestTree
setLoadBalancerPoliciesForBackendServerResponseTest = resp
    "SetLoadBalancerPoliciesForBackendServerResponse"
    "fixture/ELB/SetLoadBalancerPoliciesForBackendServerResponse"
    (Proxy :: Proxy SetLoadBalancerPoliciesForBackendServer)

enableAvailabilityZonesForLoadBalancerResponseTest :: EnableAvailabilityZonesForLoadBalancerResponse -> TestTree
enableAvailabilityZonesForLoadBalancerResponseTest = resp
    "EnableAvailabilityZonesForLoadBalancerResponse"
    "fixture/ELB/EnableAvailabilityZonesForLoadBalancerResponse"
    (Proxy :: Proxy EnableAvailabilityZonesForLoadBalancer)

setLoadBalancerListenerSSLCertificateResponseTest :: SetLoadBalancerListenerSSLCertificateResponse -> TestTree
setLoadBalancerListenerSSLCertificateResponseTest = resp
    "SetLoadBalancerListenerSSLCertificateResponse"
    "fixture/ELB/SetLoadBalancerListenerSSLCertificateResponse"
    (Proxy :: Proxy SetLoadBalancerListenerSSLCertificate)

configureHealthCheckResponseTest :: ConfigureHealthCheckResponse -> TestTree
configureHealthCheckResponseTest = resp
    "ConfigureHealthCheckResponse"
    "fixture/ELB/ConfigureHealthCheckResponse"
    (Proxy :: Proxy ConfigureHealthCheck)

attachLoadBalancerToSubnetsResponseTest :: AttachLoadBalancerToSubnetsResponse -> TestTree
attachLoadBalancerToSubnetsResponseTest = resp
    "AttachLoadBalancerToSubnetsResponse"
    "fixture/ELB/AttachLoadBalancerToSubnetsResponse"
    (Proxy :: Proxy AttachLoadBalancerToSubnets)

modifyLoadBalancerAttributesResponseTest :: ModifyLoadBalancerAttributesResponse -> TestTree
modifyLoadBalancerAttributesResponseTest = resp
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ELB/ModifyLoadBalancerAttributesResponse"
    (Proxy :: Proxy ModifyLoadBalancerAttributes)

createAppCookieStickinessPolicyResponseTest :: CreateAppCookieStickinessPolicyResponse -> TestTree
createAppCookieStickinessPolicyResponseTest = resp
    "CreateAppCookieStickinessPolicyResponse"
    "fixture/ELB/CreateAppCookieStickinessPolicyResponse"
    (Proxy :: Proxy CreateAppCookieStickinessPolicy)

addTagsResponseTest :: AddTagsResponse -> TestTree
addTagsResponseTest = resp
    "AddTagsResponse"
    "fixture/ELB/AddTagsResponse"
    (Proxy :: Proxy AddTags)

describeLoadBalancerAttributesResponseTest :: DescribeLoadBalancerAttributesResponse -> TestTree
describeLoadBalancerAttributesResponseTest = resp
    "DescribeLoadBalancerAttributesResponse"
    "fixture/ELB/DescribeLoadBalancerAttributesResponse"
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

describeInstanceHealthResponseTest :: DescribeInstanceHealthResponse -> TestTree
describeInstanceHealthResponseTest = resp
    "DescribeInstanceHealthResponse"
    "fixture/ELB/DescribeInstanceHealthResponse"
    (Proxy :: Proxy DescribeInstanceHealth)

detachLoadBalancerFromSubnetsResponseTest :: DetachLoadBalancerFromSubnetsResponse -> TestTree
detachLoadBalancerFromSubnetsResponseTest = resp
    "DetachLoadBalancerFromSubnetsResponse"
    "fixture/ELB/DetachLoadBalancerFromSubnetsResponse"
    (Proxy :: Proxy DetachLoadBalancerFromSubnets)

registerInstancesWithLoadBalancerResponseTest :: RegisterInstancesWithLoadBalancerResponse -> TestTree
registerInstancesWithLoadBalancerResponseTest = resp
    "RegisterInstancesWithLoadBalancerResponse"
    "fixture/ELB/RegisterInstancesWithLoadBalancerResponse"
    (Proxy :: Proxy RegisterInstancesWithLoadBalancer)

deleteLoadBalancerPolicyResponseTest :: DeleteLoadBalancerPolicyResponse -> TestTree
deleteLoadBalancerPolicyResponseTest = resp
    "DeleteLoadBalancerPolicyResponse"
    "fixture/ELB/DeleteLoadBalancerPolicyResponse"
    (Proxy :: Proxy DeleteLoadBalancerPolicy)

createLoadBalancerListenersResponseTest :: CreateLoadBalancerListenersResponse -> TestTree
createLoadBalancerListenersResponseTest = resp
    "CreateLoadBalancerListenersResponse"
    "fixture/ELB/CreateLoadBalancerListenersResponse"
    (Proxy :: Proxy CreateLoadBalancerListeners)

deleteLoadBalancerListenersResponseTest :: DeleteLoadBalancerListenersResponse -> TestTree
deleteLoadBalancerListenersResponseTest = resp
    "DeleteLoadBalancerListenersResponse"
    "fixture/ELB/DeleteLoadBalancerListenersResponse"
    (Proxy :: Proxy DeleteLoadBalancerListeners)

createLoadBalancerResponseTest :: CreateLoadBalancerResponse -> TestTree
createLoadBalancerResponseTest = resp
    "CreateLoadBalancerResponse"
    "fixture/ELB/CreateLoadBalancerResponse"
    (Proxy :: Proxy CreateLoadBalancer)

setLoadBalancerPoliciesOfListenerResponseTest :: SetLoadBalancerPoliciesOfListenerResponse -> TestTree
setLoadBalancerPoliciesOfListenerResponseTest = resp
    "SetLoadBalancerPoliciesOfListenerResponse"
    "fixture/ELB/SetLoadBalancerPoliciesOfListenerResponse"
    (Proxy :: Proxy SetLoadBalancerPoliciesOfListener)
