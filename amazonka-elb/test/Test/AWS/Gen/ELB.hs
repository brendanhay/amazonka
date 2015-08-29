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
--         [ testDescribeLoadBalancerPolicyTypes $
--             describeLoadBalancerPolicyTypes
--
--         , testDescribeTags $
--             describeTags
--
--         , testDescribeLoadBalancers $
--             describeLoadBalancers
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
--         , testCreateLoadBalancerPolicy $
--             createLoadBalancerPolicy
--
--         , testDeregisterInstancesFromLoadBalancer $
--             deregisterInstancesFromLoadBalancer
--
--         , testDescribeLoadBalancerPolicies $
--             describeLoadBalancerPolicies
--
--         , testDisableAvailabilityZonesForLoadBalancer $
--             disableAvailabilityZonesForLoadBalancer
--
--         , testSetLoadBalancerPoliciesForBackendServer $
--             setLoadBalancerPoliciesForBackendServer
--
--         , testEnableAvailabilityZonesForLoadBalancer $
--             enableAvailabilityZonesForLoadBalancer
--
--         , testSetLoadBalancerListenerSSLCertificate $
--             setLoadBalancerListenerSSLCertificate
--
--         , testConfigureHealthCheck $
--             configureHealthCheck
--
--         , testAttachLoadBalancerToSubnets $
--             attachLoadBalancerToSubnets
--
--         , testModifyLoadBalancerAttributes $
--             modifyLoadBalancerAttributes
--
--         , testCreateAppCookieStickinessPolicy $
--             createAppCookieStickinessPolicy
--
--         , testAddTags $
--             addTags
--
--         , testDescribeLoadBalancerAttributes $
--             describeLoadBalancerAttributes
--
--         , testDescribeInstanceHealth $
--             describeInstanceHealth
--
--         , testDetachLoadBalancerFromSubnets $
--             detachLoadBalancerFromSubnets
--
--         , testRegisterInstancesWithLoadBalancer $
--             registerInstancesWithLoadBalancer
--
--         , testDeleteLoadBalancerPolicy $
--             deleteLoadBalancerPolicy
--
--         , testCreateLoadBalancerListeners $
--             createLoadBalancerListeners
--
--         , testDeleteLoadBalancerListeners $
--             deleteLoadBalancerListeners
--
--         , testCreateLoadBalancer $
--             createLoadBalancer
--
--         , testSetLoadBalancerPoliciesOfListener $
--             setLoadBalancerPoliciesOfListener
--
--           ]

--     , testGroup "response"
--         [ testDescribeLoadBalancerPolicyTypesResponse $
--             describeLoadBalancerPolicyTypesResponse
--
--         , testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testDescribeLoadBalancersResponse $
--             describeLoadBalancersResponse
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
--         , testCreateLoadBalancerPolicyResponse $
--             createLoadBalancerPolicyResponse
--
--         , testDeregisterInstancesFromLoadBalancerResponse $
--             deregisterInstancesFromLoadBalancerResponse
--
--         , testDescribeLoadBalancerPoliciesResponse $
--             describeLoadBalancerPoliciesResponse
--
--         , testDisableAvailabilityZonesForLoadBalancerResponse $
--             disableAvailabilityZonesForLoadBalancerResponse
--
--         , testSetLoadBalancerPoliciesForBackendServerResponse $
--             setLoadBalancerPoliciesForBackendServerResponse
--
--         , testEnableAvailabilityZonesForLoadBalancerResponse $
--             enableAvailabilityZonesForLoadBalancerResponse
--
--         , testSetLoadBalancerListenerSSLCertificateResponse $
--             setLoadBalancerListenerSSLCertificateResponse
--
--         , testConfigureHealthCheckResponse $
--             configureHealthCheckResponse
--
--         , testAttachLoadBalancerToSubnetsResponse $
--             attachLoadBalancerToSubnetsResponse
--
--         , testModifyLoadBalancerAttributesResponse $
--             modifyLoadBalancerAttributesResponse
--
--         , testCreateAppCookieStickinessPolicyResponse $
--             createAppCookieStickinessPolicyResponse
--
--         , testAddTagsResponse $
--             addTagsResponse
--
--         , testDescribeLoadBalancerAttributesResponse $
--             describeLoadBalancerAttributesResponse
--
--         , testDescribeInstanceHealthResponse $
--             describeInstanceHealthResponse
--
--         , testDetachLoadBalancerFromSubnetsResponse $
--             detachLoadBalancerFromSubnetsResponse
--
--         , testRegisterInstancesWithLoadBalancerResponse $
--             registerInstancesWithLoadBalancerResponse
--
--         , testDeleteLoadBalancerPolicyResponse $
--             deleteLoadBalancerPolicyResponse
--
--         , testCreateLoadBalancerListenersResponse $
--             createLoadBalancerListenersResponse
--
--         , testDeleteLoadBalancerListenersResponse $
--             deleteLoadBalancerListenersResponse
--
--         , testCreateLoadBalancerResponse $
--             createLoadBalancerResponse
--
--         , testSetLoadBalancerPoliciesOfListenerResponse $
--             setLoadBalancerPoliciesOfListenerResponse
--
--           ]
--     ]

-- Requests

testDescribeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypes -> TestTree
testDescribeLoadBalancerPolicyTypes = req
    "DescribeLoadBalancerPolicyTypes"
    "fixture/DescribeLoadBalancerPolicyTypes.yaml"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

testDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
testDescribeLoadBalancers = req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

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

testCreateLoadBalancerPolicy :: CreateLoadBalancerPolicy -> TestTree
testCreateLoadBalancerPolicy = req
    "CreateLoadBalancerPolicy"
    "fixture/CreateLoadBalancerPolicy.yaml"

testDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancer -> TestTree
testDeregisterInstancesFromLoadBalancer = req
    "DeregisterInstancesFromLoadBalancer"
    "fixture/DeregisterInstancesFromLoadBalancer.yaml"

testDescribeLoadBalancerPolicies :: DescribeLoadBalancerPolicies -> TestTree
testDescribeLoadBalancerPolicies = req
    "DescribeLoadBalancerPolicies"
    "fixture/DescribeLoadBalancerPolicies.yaml"

testDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancer -> TestTree
testDisableAvailabilityZonesForLoadBalancer = req
    "DisableAvailabilityZonesForLoadBalancer"
    "fixture/DisableAvailabilityZonesForLoadBalancer.yaml"

testSetLoadBalancerPoliciesForBackendServer :: SetLoadBalancerPoliciesForBackendServer -> TestTree
testSetLoadBalancerPoliciesForBackendServer = req
    "SetLoadBalancerPoliciesForBackendServer"
    "fixture/SetLoadBalancerPoliciesForBackendServer.yaml"

testEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancer -> TestTree
testEnableAvailabilityZonesForLoadBalancer = req
    "EnableAvailabilityZonesForLoadBalancer"
    "fixture/EnableAvailabilityZonesForLoadBalancer.yaml"

testSetLoadBalancerListenerSSLCertificate :: SetLoadBalancerListenerSSLCertificate -> TestTree
testSetLoadBalancerListenerSSLCertificate = req
    "SetLoadBalancerListenerSSLCertificate"
    "fixture/SetLoadBalancerListenerSSLCertificate.yaml"

testConfigureHealthCheck :: ConfigureHealthCheck -> TestTree
testConfigureHealthCheck = req
    "ConfigureHealthCheck"
    "fixture/ConfigureHealthCheck.yaml"

testAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnets -> TestTree
testAttachLoadBalancerToSubnets = req
    "AttachLoadBalancerToSubnets"
    "fixture/AttachLoadBalancerToSubnets.yaml"

testModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributes -> TestTree
testModifyLoadBalancerAttributes = req
    "ModifyLoadBalancerAttributes"
    "fixture/ModifyLoadBalancerAttributes.yaml"

testCreateAppCookieStickinessPolicy :: CreateAppCookieStickinessPolicy -> TestTree
testCreateAppCookieStickinessPolicy = req
    "CreateAppCookieStickinessPolicy"
    "fixture/CreateAppCookieStickinessPolicy.yaml"

testAddTags :: AddTags -> TestTree
testAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

testDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
testDescribeLoadBalancerAttributes = req
    "DescribeLoadBalancerAttributes"
    "fixture/DescribeLoadBalancerAttributes.yaml"

testDescribeInstanceHealth :: DescribeInstanceHealth -> TestTree
testDescribeInstanceHealth = req
    "DescribeInstanceHealth"
    "fixture/DescribeInstanceHealth.yaml"

testDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnets -> TestTree
testDetachLoadBalancerFromSubnets = req
    "DetachLoadBalancerFromSubnets"
    "fixture/DetachLoadBalancerFromSubnets.yaml"

testRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancer -> TestTree
testRegisterInstancesWithLoadBalancer = req
    "RegisterInstancesWithLoadBalancer"
    "fixture/RegisterInstancesWithLoadBalancer.yaml"

testDeleteLoadBalancerPolicy :: DeleteLoadBalancerPolicy -> TestTree
testDeleteLoadBalancerPolicy = req
    "DeleteLoadBalancerPolicy"
    "fixture/DeleteLoadBalancerPolicy.yaml"

testCreateLoadBalancerListeners :: CreateLoadBalancerListeners -> TestTree
testCreateLoadBalancerListeners = req
    "CreateLoadBalancerListeners"
    "fixture/CreateLoadBalancerListeners.yaml"

testDeleteLoadBalancerListeners :: DeleteLoadBalancerListeners -> TestTree
testDeleteLoadBalancerListeners = req
    "DeleteLoadBalancerListeners"
    "fixture/DeleteLoadBalancerListeners.yaml"

testCreateLoadBalancer :: CreateLoadBalancer -> TestTree
testCreateLoadBalancer = req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

testSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListener -> TestTree
testSetLoadBalancerPoliciesOfListener = req
    "SetLoadBalancerPoliciesOfListener"
    "fixture/SetLoadBalancerPoliciesOfListener.yaml"

-- Responses

testDescribeLoadBalancerPolicyTypesResponse :: DescribeLoadBalancerPolicyTypesResponse -> TestTree
testDescribeLoadBalancerPolicyTypesResponse = res
    "DescribeLoadBalancerPolicyTypesResponse"
    "fixture/DescribeLoadBalancerPolicyTypesResponse.proto"
    eLB
    (Proxy :: Proxy DescribeLoadBalancerPolicyTypes)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    eLB
    (Proxy :: Proxy DescribeTags)

testDescribeLoadBalancersResponse :: DescribeLoadBalancersResponse -> TestTree
testDescribeLoadBalancersResponse = res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    eLB
    (Proxy :: Proxy DescribeLoadBalancers)

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

testCreateLoadBalancerPolicyResponse :: CreateLoadBalancerPolicyResponse -> TestTree
testCreateLoadBalancerPolicyResponse = res
    "CreateLoadBalancerPolicyResponse"
    "fixture/CreateLoadBalancerPolicyResponse.proto"
    eLB
    (Proxy :: Proxy CreateLoadBalancerPolicy)

testDeregisterInstancesFromLoadBalancerResponse :: DeregisterInstancesFromLoadBalancerResponse -> TestTree
testDeregisterInstancesFromLoadBalancerResponse = res
    "DeregisterInstancesFromLoadBalancerResponse"
    "fixture/DeregisterInstancesFromLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy DeregisterInstancesFromLoadBalancer)

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

testSetLoadBalancerPoliciesForBackendServerResponse :: SetLoadBalancerPoliciesForBackendServerResponse -> TestTree
testSetLoadBalancerPoliciesForBackendServerResponse = res
    "SetLoadBalancerPoliciesForBackendServerResponse"
    "fixture/SetLoadBalancerPoliciesForBackendServerResponse.proto"
    eLB
    (Proxy :: Proxy SetLoadBalancerPoliciesForBackendServer)

testEnableAvailabilityZonesForLoadBalancerResponse :: EnableAvailabilityZonesForLoadBalancerResponse -> TestTree
testEnableAvailabilityZonesForLoadBalancerResponse = res
    "EnableAvailabilityZonesForLoadBalancerResponse"
    "fixture/EnableAvailabilityZonesForLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy EnableAvailabilityZonesForLoadBalancer)

testSetLoadBalancerListenerSSLCertificateResponse :: SetLoadBalancerListenerSSLCertificateResponse -> TestTree
testSetLoadBalancerListenerSSLCertificateResponse = res
    "SetLoadBalancerListenerSSLCertificateResponse"
    "fixture/SetLoadBalancerListenerSSLCertificateResponse.proto"
    eLB
    (Proxy :: Proxy SetLoadBalancerListenerSSLCertificate)

testConfigureHealthCheckResponse :: ConfigureHealthCheckResponse -> TestTree
testConfigureHealthCheckResponse = res
    "ConfigureHealthCheckResponse"
    "fixture/ConfigureHealthCheckResponse.proto"
    eLB
    (Proxy :: Proxy ConfigureHealthCheck)

testAttachLoadBalancerToSubnetsResponse :: AttachLoadBalancerToSubnetsResponse -> TestTree
testAttachLoadBalancerToSubnetsResponse = res
    "AttachLoadBalancerToSubnetsResponse"
    "fixture/AttachLoadBalancerToSubnetsResponse.proto"
    eLB
    (Proxy :: Proxy AttachLoadBalancerToSubnets)

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

testDescribeInstanceHealthResponse :: DescribeInstanceHealthResponse -> TestTree
testDescribeInstanceHealthResponse = res
    "DescribeInstanceHealthResponse"
    "fixture/DescribeInstanceHealthResponse.proto"
    eLB
    (Proxy :: Proxy DescribeInstanceHealth)

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

testDeleteLoadBalancerPolicyResponse :: DeleteLoadBalancerPolicyResponse -> TestTree
testDeleteLoadBalancerPolicyResponse = res
    "DeleteLoadBalancerPolicyResponse"
    "fixture/DeleteLoadBalancerPolicyResponse.proto"
    eLB
    (Proxy :: Proxy DeleteLoadBalancerPolicy)

testCreateLoadBalancerListenersResponse :: CreateLoadBalancerListenersResponse -> TestTree
testCreateLoadBalancerListenersResponse = res
    "CreateLoadBalancerListenersResponse"
    "fixture/CreateLoadBalancerListenersResponse.proto"
    eLB
    (Proxy :: Proxy CreateLoadBalancerListeners)

testDeleteLoadBalancerListenersResponse :: DeleteLoadBalancerListenersResponse -> TestTree
testDeleteLoadBalancerListenersResponse = res
    "DeleteLoadBalancerListenersResponse"
    "fixture/DeleteLoadBalancerListenersResponse.proto"
    eLB
    (Proxy :: Proxy DeleteLoadBalancerListeners)

testCreateLoadBalancerResponse :: CreateLoadBalancerResponse -> TestTree
testCreateLoadBalancerResponse = res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    eLB
    (Proxy :: Proxy CreateLoadBalancer)

testSetLoadBalancerPoliciesOfListenerResponse :: SetLoadBalancerPoliciesOfListenerResponse -> TestTree
testSetLoadBalancerPoliciesOfListenerResponse = res
    "SetLoadBalancerPoliciesOfListenerResponse"
    "fixture/SetLoadBalancerPoliciesOfListenerResponse.proto"
    eLB
    (Proxy :: Proxy SetLoadBalancerPoliciesOfListener)
