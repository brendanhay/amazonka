{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ELB
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ELB where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ELB
import Test.AWS.ELB.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeLoadBalancers $
--             describeLoadBalancers
--
--         , testDescribeTags $
--             describeTags
--
--         , testDescribeLoadBalancerPolicyTypes $
--             describeLoadBalancerPolicyTypes
--
--         , testApplySecurityGroupsToLoadBalancer $
--             applySecurityGroupsToLoadBalancer
--
--         , testRemoveTags $
--             removeTags
--
--         , testCreateLBCookieStickinessPolicy $
--             createLBCookieStickinessPolicy
--
--         , testDeleteLoadBalancer $
--             deleteLoadBalancer
--
--         , testDeregisterInstancesFromLoadBalancer $
--             deregisterInstancesFromLoadBalancer
--
--         , testCreateLoadBalancerPolicy $
--             createLoadBalancerPolicy
--
--         , testDescribeLoadBalancerPolicies $
--             describeLoadBalancerPolicies
--
--         , testDisableAvailabilityZonesForLoadBalancer $
--             disableAvailabilityZonesForLoadBalancer
--
--         , testEnableAvailabilityZonesForLoadBalancer $
--             enableAvailabilityZonesForLoadBalancer
--
--         , testSetLoadBalancerPoliciesForBackendServer $
--             setLoadBalancerPoliciesForBackendServer
--
--         , testSetLoadBalancerListenerSSLCertificate $
--             setLoadBalancerListenerSSLCertificate
--
--         , testAttachLoadBalancerToSubnets $
--             attachLoadBalancerToSubnets
--
--         , testConfigureHealthCheck $
--             configureHealthCheck
--
--         , testModifyLoadBalancerAttributes $
--             modifyLoadBalancerAttributes
--
--         , testCreateAppCookieStickinessPolicy $
--             createAppCookieStickinessPolicy
--
--         , testDescribeInstanceHealth $
--             describeInstanceHealth
--
--         , testAddTags $
--             addTags
--
--         , testDescribeLoadBalancerAttributes $
--             describeLoadBalancerAttributes
--
--         , testCreateLoadBalancerListeners $
--             createLoadBalancerListeners
--
--         , testDeleteLoadBalancerPolicy $
--             deleteLoadBalancerPolicy
--
--         , testDetachLoadBalancerFromSubnets $
--             detachLoadBalancerFromSubnets
--
--         , testRegisterInstancesWithLoadBalancer $
--             registerInstancesWithLoadBalancer
--
--         , testCreateLoadBalancer $
--             createLoadBalancer
--
--         , testDeleteLoadBalancerListeners $
--             deleteLoadBalancerListeners
--
--         , testSetLoadBalancerPoliciesOfListener $
--             setLoadBalancerPoliciesOfListener
--
--           ]

--     , testGroup "response"
--         [ testDescribeLoadBalancersResponse $
--             describeLoadBalancersResponse
--
--         , testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testDescribeLoadBalancerPolicyTypesResponse $
--             describeLoadBalancerPolicyTypesResponse
--
--         , testApplySecurityGroupsToLoadBalancerResponse $
--             applySecurityGroupsToLoadBalancerResponse
--
--         , testRemoveTagsResponse $
--             removeTagsResponse
--
--         , testCreateLBCookieStickinessPolicyResponse $
--             createLBCookieStickinessPolicyResponse
--
--         , testDeleteLoadBalancerResponse $
--             deleteLoadBalancerResponse
--
--         , testDeregisterInstancesFromLoadBalancerResponse $
--             deregisterInstancesFromLoadBalancerResponse
--
--         , testCreateLoadBalancerPolicyResponse $
--             createLoadBalancerPolicyResponse
--
--         , testDescribeLoadBalancerPoliciesResponse $
--             describeLoadBalancerPoliciesResponse
--
--         , testDisableAvailabilityZonesForLoadBalancerResponse $
--             disableAvailabilityZonesForLoadBalancerResponse
--
--         , testEnableAvailabilityZonesForLoadBalancerResponse $
--             enableAvailabilityZonesForLoadBalancerResponse
--
--         , testSetLoadBalancerPoliciesForBackendServerResponse $
--             setLoadBalancerPoliciesForBackendServerResponse
--
--         , testSetLoadBalancerListenerSSLCertificateResponse $
--             setLoadBalancerListenerSSLCertificateResponse
--
--         , testAttachLoadBalancerToSubnetsResponse $
--             attachLoadBalancerToSubnetsResponse
--
--         , testConfigureHealthCheckResponse $
--             configureHealthCheckResponse
--
--         , testModifyLoadBalancerAttributesResponse $
--             modifyLoadBalancerAttributesResponse
--
--         , testCreateAppCookieStickinessPolicyResponse $
--             createAppCookieStickinessPolicyResponse
--
--         , testDescribeInstanceHealthResponse $
--             describeInstanceHealthResponse
--
--         , testAddTagsResponse $
--             addTagsResponse
--
--         , testDescribeLoadBalancerAttributesResponse $
--             describeLoadBalancerAttributesResponse
--
--         , testCreateLoadBalancerListenersResponse $
--             createLoadBalancerListenersResponse
--
--         , testDeleteLoadBalancerPolicyResponse $
--             deleteLoadBalancerPolicyResponse
--
--         , testDetachLoadBalancerFromSubnetsResponse $
--             detachLoadBalancerFromSubnetsResponse
--
--         , testRegisterInstancesWithLoadBalancerResponse $
--             registerInstancesWithLoadBalancerResponse
--
--         , testCreateLoadBalancerResponse $
--             createLoadBalancerResponse
--
--         , testDeleteLoadBalancerListenersResponse $
--             deleteLoadBalancerListenersResponse
--
--         , testSetLoadBalancerPoliciesOfListenerResponse $
--             setLoadBalancerPoliciesOfListenerResponse
--
--           ]
--     ]

-- Requests

testDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
testDescribeLoadBalancers = req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

testDescribeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypes -> TestTree
testDescribeLoadBalancerPolicyTypes = req
    "DescribeLoadBalancerPolicyTypes"
    "fixture/DescribeLoadBalancerPolicyTypes.yaml"

testApplySecurityGroupsToLoadBalancer :: ApplySecurityGroupsToLoadBalancer -> TestTree
testApplySecurityGroupsToLoadBalancer = req
    "ApplySecurityGroupsToLoadBalancer"
    "fixture/ApplySecurityGroupsToLoadBalancer.yaml"

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

testCreateLBCookieStickinessPolicy :: CreateLBCookieStickinessPolicy -> TestTree
testCreateLBCookieStickinessPolicy = req
    "CreateLBCookieStickinessPolicy"
    "fixture/CreateLBCookieStickinessPolicy.yaml"

testDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
testDeleteLoadBalancer = req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

testDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancer -> TestTree
testDeregisterInstancesFromLoadBalancer = req
    "DeregisterInstancesFromLoadBalancer"
    "fixture/DeregisterInstancesFromLoadBalancer.yaml"

testCreateLoadBalancerPolicy :: CreateLoadBalancerPolicy -> TestTree
testCreateLoadBalancerPolicy = req
    "CreateLoadBalancerPolicy"
    "fixture/CreateLoadBalancerPolicy.yaml"

testDescribeLoadBalancerPolicies :: DescribeLoadBalancerPolicies -> TestTree
testDescribeLoadBalancerPolicies = req
    "DescribeLoadBalancerPolicies"
    "fixture/DescribeLoadBalancerPolicies.yaml"

testDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancer -> TestTree
testDisableAvailabilityZonesForLoadBalancer = req
    "DisableAvailabilityZonesForLoadBalancer"
    "fixture/DisableAvailabilityZonesForLoadBalancer.yaml"

testEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancer -> TestTree
testEnableAvailabilityZonesForLoadBalancer = req
    "EnableAvailabilityZonesForLoadBalancer"
    "fixture/EnableAvailabilityZonesForLoadBalancer.yaml"

testSetLoadBalancerPoliciesForBackendServer :: SetLoadBalancerPoliciesForBackendServer -> TestTree
testSetLoadBalancerPoliciesForBackendServer = req
    "SetLoadBalancerPoliciesForBackendServer"
    "fixture/SetLoadBalancerPoliciesForBackendServer.yaml"

testSetLoadBalancerListenerSSLCertificate :: SetLoadBalancerListenerSSLCertificate -> TestTree
testSetLoadBalancerListenerSSLCertificate = req
    "SetLoadBalancerListenerSSLCertificate"
    "fixture/SetLoadBalancerListenerSSLCertificate.yaml"

testAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnets -> TestTree
testAttachLoadBalancerToSubnets = req
    "AttachLoadBalancerToSubnets"
    "fixture/AttachLoadBalancerToSubnets.yaml"

testConfigureHealthCheck :: ConfigureHealthCheck -> TestTree
testConfigureHealthCheck = req
    "ConfigureHealthCheck"
    "fixture/ConfigureHealthCheck.yaml"

testModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributes -> TestTree
testModifyLoadBalancerAttributes = req
    "ModifyLoadBalancerAttributes"
    "fixture/ModifyLoadBalancerAttributes.yaml"

testCreateAppCookieStickinessPolicy :: CreateAppCookieStickinessPolicy -> TestTree
testCreateAppCookieStickinessPolicy = req
    "CreateAppCookieStickinessPolicy"
    "fixture/CreateAppCookieStickinessPolicy.yaml"

testDescribeInstanceHealth :: DescribeInstanceHealth -> TestTree
testDescribeInstanceHealth = req
    "DescribeInstanceHealth"
    "fixture/DescribeInstanceHealth.yaml"

testAddTags :: AddTags -> TestTree
testAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

testDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
testDescribeLoadBalancerAttributes = req
    "DescribeLoadBalancerAttributes"
    "fixture/DescribeLoadBalancerAttributes.yaml"

testCreateLoadBalancerListeners :: CreateLoadBalancerListeners -> TestTree
testCreateLoadBalancerListeners = req
    "CreateLoadBalancerListeners"
    "fixture/CreateLoadBalancerListeners.yaml"

testDeleteLoadBalancerPolicy :: DeleteLoadBalancerPolicy -> TestTree
testDeleteLoadBalancerPolicy = req
    "DeleteLoadBalancerPolicy"
    "fixture/DeleteLoadBalancerPolicy.yaml"

testDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnets -> TestTree
testDetachLoadBalancerFromSubnets = req
    "DetachLoadBalancerFromSubnets"
    "fixture/DetachLoadBalancerFromSubnets.yaml"

testRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancer -> TestTree
testRegisterInstancesWithLoadBalancer = req
    "RegisterInstancesWithLoadBalancer"
    "fixture/RegisterInstancesWithLoadBalancer.yaml"

testCreateLoadBalancer :: CreateLoadBalancer -> TestTree
testCreateLoadBalancer = req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

testDeleteLoadBalancerListeners :: DeleteLoadBalancerListeners -> TestTree
testDeleteLoadBalancerListeners = req
    "DeleteLoadBalancerListeners"
    "fixture/DeleteLoadBalancerListeners.yaml"

testSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListener -> TestTree
testSetLoadBalancerPoliciesOfListener = req
    "SetLoadBalancerPoliciesOfListener"
    "fixture/SetLoadBalancerPoliciesOfListener.yaml"

-- Responses

testDescribeLoadBalancersResponse :: DescribeLoadBalancersResponse -> TestTree
testDescribeLoadBalancersResponse = res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    eLB
    (Proxy :: Proxy DescribeLoadBalancers)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    eLB
    (Proxy :: Proxy DescribeTags)

testDescribeLoadBalancerPolicyTypesResponse :: DescribeLoadBalancerPolicyTypesResponse -> TestTree
testDescribeLoadBalancerPolicyTypesResponse = res
    "DescribeLoadBalancerPolicyTypesResponse"
    "fixture/DescribeLoadBalancerPolicyTypesResponse.proto"
    eLB
    (Proxy :: Proxy DescribeLoadBalancerPolicyTypes)

testApplySecurityGroupsToLoadBalancerResponse :: ApplySecurityGroupsToLoadBalancerResponse -> TestTree
testApplySecurityGroupsToLoadBalancerResponse = res
    "ApplySecurityGroupsToLoadBalancerResponse"
    "fixture/ApplySecurityGroupsToLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy ApplySecurityGroupsToLoadBalancer)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    eLB
    (Proxy :: Proxy RemoveTags)

testCreateLBCookieStickinessPolicyResponse :: CreateLBCookieStickinessPolicyResponse -> TestTree
testCreateLBCookieStickinessPolicyResponse = res
    "CreateLBCookieStickinessPolicyResponse"
    "fixture/CreateLBCookieStickinessPolicyResponse.proto"
    eLB
    (Proxy :: Proxy CreateLBCookieStickinessPolicy)

testDeleteLoadBalancerResponse :: DeleteLoadBalancerResponse -> TestTree
testDeleteLoadBalancerResponse = res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy DeleteLoadBalancer)

testDeregisterInstancesFromLoadBalancerResponse :: DeregisterInstancesFromLoadBalancerResponse -> TestTree
testDeregisterInstancesFromLoadBalancerResponse = res
    "DeregisterInstancesFromLoadBalancerResponse"
    "fixture/DeregisterInstancesFromLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy DeregisterInstancesFromLoadBalancer)

testCreateLoadBalancerPolicyResponse :: CreateLoadBalancerPolicyResponse -> TestTree
testCreateLoadBalancerPolicyResponse = res
    "CreateLoadBalancerPolicyResponse"
    "fixture/CreateLoadBalancerPolicyResponse.proto"
    eLB
    (Proxy :: Proxy CreateLoadBalancerPolicy)

testDescribeLoadBalancerPoliciesResponse :: DescribeLoadBalancerPoliciesResponse -> TestTree
testDescribeLoadBalancerPoliciesResponse = res
    "DescribeLoadBalancerPoliciesResponse"
    "fixture/DescribeLoadBalancerPoliciesResponse.proto"
    eLB
    (Proxy :: Proxy DescribeLoadBalancerPolicies)

testDisableAvailabilityZonesForLoadBalancerResponse :: DisableAvailabilityZonesForLoadBalancerResponse -> TestTree
testDisableAvailabilityZonesForLoadBalancerResponse = res
    "DisableAvailabilityZonesForLoadBalancerResponse"
    "fixture/DisableAvailabilityZonesForLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy DisableAvailabilityZonesForLoadBalancer)

testEnableAvailabilityZonesForLoadBalancerResponse :: EnableAvailabilityZonesForLoadBalancerResponse -> TestTree
testEnableAvailabilityZonesForLoadBalancerResponse = res
    "EnableAvailabilityZonesForLoadBalancerResponse"
    "fixture/EnableAvailabilityZonesForLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy EnableAvailabilityZonesForLoadBalancer)

testSetLoadBalancerPoliciesForBackendServerResponse :: SetLoadBalancerPoliciesForBackendServerResponse -> TestTree
testSetLoadBalancerPoliciesForBackendServerResponse = res
    "SetLoadBalancerPoliciesForBackendServerResponse"
    "fixture/SetLoadBalancerPoliciesForBackendServerResponse.proto"
    eLB
    (Proxy :: Proxy SetLoadBalancerPoliciesForBackendServer)

testSetLoadBalancerListenerSSLCertificateResponse :: SetLoadBalancerListenerSSLCertificateResponse -> TestTree
testSetLoadBalancerListenerSSLCertificateResponse = res
    "SetLoadBalancerListenerSSLCertificateResponse"
    "fixture/SetLoadBalancerListenerSSLCertificateResponse.proto"
    eLB
    (Proxy :: Proxy SetLoadBalancerListenerSSLCertificate)

testAttachLoadBalancerToSubnetsResponse :: AttachLoadBalancerToSubnetsResponse -> TestTree
testAttachLoadBalancerToSubnetsResponse = res
    "AttachLoadBalancerToSubnetsResponse"
    "fixture/AttachLoadBalancerToSubnetsResponse.proto"
    eLB
    (Proxy :: Proxy AttachLoadBalancerToSubnets)

testConfigureHealthCheckResponse :: ConfigureHealthCheckResponse -> TestTree
testConfigureHealthCheckResponse = res
    "ConfigureHealthCheckResponse"
    "fixture/ConfigureHealthCheckResponse.proto"
    eLB
    (Proxy :: Proxy ConfigureHealthCheck)

testModifyLoadBalancerAttributesResponse :: ModifyLoadBalancerAttributesResponse -> TestTree
testModifyLoadBalancerAttributesResponse = res
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse.proto"
    eLB
    (Proxy :: Proxy ModifyLoadBalancerAttributes)

testCreateAppCookieStickinessPolicyResponse :: CreateAppCookieStickinessPolicyResponse -> TestTree
testCreateAppCookieStickinessPolicyResponse = res
    "CreateAppCookieStickinessPolicyResponse"
    "fixture/CreateAppCookieStickinessPolicyResponse.proto"
    eLB
    (Proxy :: Proxy CreateAppCookieStickinessPolicy)

testDescribeInstanceHealthResponse :: DescribeInstanceHealthResponse -> TestTree
testDescribeInstanceHealthResponse = res
    "DescribeInstanceHealthResponse"
    "fixture/DescribeInstanceHealthResponse.proto"
    eLB
    (Proxy :: Proxy DescribeInstanceHealth)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    eLB
    (Proxy :: Proxy AddTags)

testDescribeLoadBalancerAttributesResponse :: DescribeLoadBalancerAttributesResponse -> TestTree
testDescribeLoadBalancerAttributesResponse = res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse.proto"
    eLB
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

testCreateLoadBalancerListenersResponse :: CreateLoadBalancerListenersResponse -> TestTree
testCreateLoadBalancerListenersResponse = res
    "CreateLoadBalancerListenersResponse"
    "fixture/CreateLoadBalancerListenersResponse.proto"
    eLB
    (Proxy :: Proxy CreateLoadBalancerListeners)

testDeleteLoadBalancerPolicyResponse :: DeleteLoadBalancerPolicyResponse -> TestTree
testDeleteLoadBalancerPolicyResponse = res
    "DeleteLoadBalancerPolicyResponse"
    "fixture/DeleteLoadBalancerPolicyResponse.proto"
    eLB
    (Proxy :: Proxy DeleteLoadBalancerPolicy)

testDetachLoadBalancerFromSubnetsResponse :: DetachLoadBalancerFromSubnetsResponse -> TestTree
testDetachLoadBalancerFromSubnetsResponse = res
    "DetachLoadBalancerFromSubnetsResponse"
    "fixture/DetachLoadBalancerFromSubnetsResponse.proto"
    eLB
    (Proxy :: Proxy DetachLoadBalancerFromSubnets)

testRegisterInstancesWithLoadBalancerResponse :: RegisterInstancesWithLoadBalancerResponse -> TestTree
testRegisterInstancesWithLoadBalancerResponse = res
    "RegisterInstancesWithLoadBalancerResponse"
    "fixture/RegisterInstancesWithLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy RegisterInstancesWithLoadBalancer)

testCreateLoadBalancerResponse :: CreateLoadBalancerResponse -> TestTree
testCreateLoadBalancerResponse = res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy CreateLoadBalancer)

testDeleteLoadBalancerListenersResponse :: DeleteLoadBalancerListenersResponse -> TestTree
testDeleteLoadBalancerListenersResponse = res
    "DeleteLoadBalancerListenersResponse"
    "fixture/DeleteLoadBalancerListenersResponse.proto"
    eLB
    (Proxy :: Proxy DeleteLoadBalancerListeners)

testSetLoadBalancerPoliciesOfListenerResponse :: SetLoadBalancerPoliciesOfListenerResponse -> TestTree
testSetLoadBalancerPoliciesOfListenerResponse = res
    "SetLoadBalancerPoliciesOfListenerResponse"
    "fixture/SetLoadBalancerPoliciesOfListenerResponse.proto"
    eLB
    (Proxy :: Proxy SetLoadBalancerPoliciesOfListener)
