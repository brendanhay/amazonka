{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ELB
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    "fixture/DescribeLoadBalancerPolicyTypes"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags"

testDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
testDescribeLoadBalancers = req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers"

testApplySecurityGroupsToLoadBalancer :: ApplySecurityGroupsToLoadBalancer -> TestTree
testApplySecurityGroupsToLoadBalancer = req
    "ApplySecurityGroupsToLoadBalancer"
    "fixture/ApplySecurityGroupsToLoadBalancer"

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags"

testCreateLBCookieStickinessPolicy :: CreateLBCookieStickinessPolicy -> TestTree
testCreateLBCookieStickinessPolicy = req
    "CreateLBCookieStickinessPolicy"
    "fixture/CreateLBCookieStickinessPolicy"

testDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
testDeleteLoadBalancer = req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer"

testCreateLoadBalancerPolicy :: CreateLoadBalancerPolicy -> TestTree
testCreateLoadBalancerPolicy = req
    "CreateLoadBalancerPolicy"
    "fixture/CreateLoadBalancerPolicy"

testDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancer -> TestTree
testDeregisterInstancesFromLoadBalancer = req
    "DeregisterInstancesFromLoadBalancer"
    "fixture/DeregisterInstancesFromLoadBalancer"

testDescribeLoadBalancerPolicies :: DescribeLoadBalancerPolicies -> TestTree
testDescribeLoadBalancerPolicies = req
    "DescribeLoadBalancerPolicies"
    "fixture/DescribeLoadBalancerPolicies"

testDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancer -> TestTree
testDisableAvailabilityZonesForLoadBalancer = req
    "DisableAvailabilityZonesForLoadBalancer"
    "fixture/DisableAvailabilityZonesForLoadBalancer"

testSetLoadBalancerPoliciesForBackendServer :: SetLoadBalancerPoliciesForBackendServer -> TestTree
testSetLoadBalancerPoliciesForBackendServer = req
    "SetLoadBalancerPoliciesForBackendServer"
    "fixture/SetLoadBalancerPoliciesForBackendServer"

testEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancer -> TestTree
testEnableAvailabilityZonesForLoadBalancer = req
    "EnableAvailabilityZonesForLoadBalancer"
    "fixture/EnableAvailabilityZonesForLoadBalancer"

testSetLoadBalancerListenerSSLCertificate :: SetLoadBalancerListenerSSLCertificate -> TestTree
testSetLoadBalancerListenerSSLCertificate = req
    "SetLoadBalancerListenerSSLCertificate"
    "fixture/SetLoadBalancerListenerSSLCertificate"

testConfigureHealthCheck :: ConfigureHealthCheck -> TestTree
testConfigureHealthCheck = req
    "ConfigureHealthCheck"
    "fixture/ConfigureHealthCheck"

testAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnets -> TestTree
testAttachLoadBalancerToSubnets = req
    "AttachLoadBalancerToSubnets"
    "fixture/AttachLoadBalancerToSubnets"

testModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributes -> TestTree
testModifyLoadBalancerAttributes = req
    "ModifyLoadBalancerAttributes"
    "fixture/ModifyLoadBalancerAttributes"

testCreateAppCookieStickinessPolicy :: CreateAppCookieStickinessPolicy -> TestTree
testCreateAppCookieStickinessPolicy = req
    "CreateAppCookieStickinessPolicy"
    "fixture/CreateAppCookieStickinessPolicy"

testAddTags :: AddTags -> TestTree
testAddTags = req
    "AddTags"
    "fixture/AddTags"

testDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
testDescribeLoadBalancerAttributes = req
    "DescribeLoadBalancerAttributes"
    "fixture/DescribeLoadBalancerAttributes"

testDescribeInstanceHealth :: DescribeInstanceHealth -> TestTree
testDescribeInstanceHealth = req
    "DescribeInstanceHealth"
    "fixture/DescribeInstanceHealth"

testDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnets -> TestTree
testDetachLoadBalancerFromSubnets = req
    "DetachLoadBalancerFromSubnets"
    "fixture/DetachLoadBalancerFromSubnets"

testRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancer -> TestTree
testRegisterInstancesWithLoadBalancer = req
    "RegisterInstancesWithLoadBalancer"
    "fixture/RegisterInstancesWithLoadBalancer"

testDeleteLoadBalancerPolicy :: DeleteLoadBalancerPolicy -> TestTree
testDeleteLoadBalancerPolicy = req
    "DeleteLoadBalancerPolicy"
    "fixture/DeleteLoadBalancerPolicy"

testCreateLoadBalancerListeners :: CreateLoadBalancerListeners -> TestTree
testCreateLoadBalancerListeners = req
    "CreateLoadBalancerListeners"
    "fixture/CreateLoadBalancerListeners"

testDeleteLoadBalancerListeners :: DeleteLoadBalancerListeners -> TestTree
testDeleteLoadBalancerListeners = req
    "DeleteLoadBalancerListeners"
    "fixture/DeleteLoadBalancerListeners"

testCreateLoadBalancer :: CreateLoadBalancer -> TestTree
testCreateLoadBalancer = req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer"

testSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListener -> TestTree
testSetLoadBalancerPoliciesOfListener = req
    "SetLoadBalancerPoliciesOfListener"
    "fixture/SetLoadBalancerPoliciesOfListener"

-- Responses

testDescribeLoadBalancerPolicyTypesResponse :: DescribeLoadBalancerPolicyTypesResponse -> TestTree
testDescribeLoadBalancerPolicyTypesResponse = res
    "DescribeLoadBalancerPolicyTypesResponse"
    "fixture/DescribeLoadBalancerPolicyTypesResponse"
    (Proxy :: Proxy DescribeLoadBalancerPolicyTypes)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

testDescribeLoadBalancersResponse :: DescribeLoadBalancersResponse -> TestTree
testDescribeLoadBalancersResponse = res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse"
    (Proxy :: Proxy DescribeLoadBalancers)

testApplySecurityGroupsToLoadBalancerResponse :: ApplySecurityGroupsToLoadBalancerResponse -> TestTree
testApplySecurityGroupsToLoadBalancerResponse = res
    "ApplySecurityGroupsToLoadBalancerResponse"
    "fixture/ApplySecurityGroupsToLoadBalancerResponse"
    (Proxy :: Proxy ApplySecurityGroupsToLoadBalancer)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

testCreateLBCookieStickinessPolicyResponse :: CreateLBCookieStickinessPolicyResponse -> TestTree
testCreateLBCookieStickinessPolicyResponse = res
    "CreateLBCookieStickinessPolicyResponse"
    "fixture/CreateLBCookieStickinessPolicyResponse"
    (Proxy :: Proxy CreateLBCookieStickinessPolicy)

testDeleteLoadBalancerResponse :: DeleteLoadBalancerResponse -> TestTree
testDeleteLoadBalancerResponse = res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse"
    (Proxy :: Proxy DeleteLoadBalancer)

testCreateLoadBalancerPolicyResponse :: CreateLoadBalancerPolicyResponse -> TestTree
testCreateLoadBalancerPolicyResponse = res
    "CreateLoadBalancerPolicyResponse"
    "fixture/CreateLoadBalancerPolicyResponse"
    (Proxy :: Proxy CreateLoadBalancerPolicy)

testDeregisterInstancesFromLoadBalancerResponse :: DeregisterInstancesFromLoadBalancerResponse -> TestTree
testDeregisterInstancesFromLoadBalancerResponse = res
    "DeregisterInstancesFromLoadBalancerResponse"
    "fixture/DeregisterInstancesFromLoadBalancerResponse"
    (Proxy :: Proxy DeregisterInstancesFromLoadBalancer)

testDescribeLoadBalancerPoliciesResponse :: DescribeLoadBalancerPoliciesResponse -> TestTree
testDescribeLoadBalancerPoliciesResponse = res
    "DescribeLoadBalancerPoliciesResponse"
    "fixture/DescribeLoadBalancerPoliciesResponse"
    (Proxy :: Proxy DescribeLoadBalancerPolicies)

testDisableAvailabilityZonesForLoadBalancerResponse :: DisableAvailabilityZonesForLoadBalancerResponse -> TestTree
testDisableAvailabilityZonesForLoadBalancerResponse = res
    "DisableAvailabilityZonesForLoadBalancerResponse"
    "fixture/DisableAvailabilityZonesForLoadBalancerResponse"
    (Proxy :: Proxy DisableAvailabilityZonesForLoadBalancer)

testSetLoadBalancerPoliciesForBackendServerResponse :: SetLoadBalancerPoliciesForBackendServerResponse -> TestTree
testSetLoadBalancerPoliciesForBackendServerResponse = res
    "SetLoadBalancerPoliciesForBackendServerResponse"
    "fixture/SetLoadBalancerPoliciesForBackendServerResponse"
    (Proxy :: Proxy SetLoadBalancerPoliciesForBackendServer)

testEnableAvailabilityZonesForLoadBalancerResponse :: EnableAvailabilityZonesForLoadBalancerResponse -> TestTree
testEnableAvailabilityZonesForLoadBalancerResponse = res
    "EnableAvailabilityZonesForLoadBalancerResponse"
    "fixture/EnableAvailabilityZonesForLoadBalancerResponse"
    (Proxy :: Proxy EnableAvailabilityZonesForLoadBalancer)

testSetLoadBalancerListenerSSLCertificateResponse :: SetLoadBalancerListenerSSLCertificateResponse -> TestTree
testSetLoadBalancerListenerSSLCertificateResponse = res
    "SetLoadBalancerListenerSSLCertificateResponse"
    "fixture/SetLoadBalancerListenerSSLCertificateResponse"
    (Proxy :: Proxy SetLoadBalancerListenerSSLCertificate)

testConfigureHealthCheckResponse :: ConfigureHealthCheckResponse -> TestTree
testConfigureHealthCheckResponse = res
    "ConfigureHealthCheckResponse"
    "fixture/ConfigureHealthCheckResponse"
    (Proxy :: Proxy ConfigureHealthCheck)

testAttachLoadBalancerToSubnetsResponse :: AttachLoadBalancerToSubnetsResponse -> TestTree
testAttachLoadBalancerToSubnetsResponse = res
    "AttachLoadBalancerToSubnetsResponse"
    "fixture/AttachLoadBalancerToSubnetsResponse"
    (Proxy :: Proxy AttachLoadBalancerToSubnets)

testModifyLoadBalancerAttributesResponse :: ModifyLoadBalancerAttributesResponse -> TestTree
testModifyLoadBalancerAttributesResponse = res
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse"
    (Proxy :: Proxy ModifyLoadBalancerAttributes)

testCreateAppCookieStickinessPolicyResponse :: CreateAppCookieStickinessPolicyResponse -> TestTree
testCreateAppCookieStickinessPolicyResponse = res
    "CreateAppCookieStickinessPolicyResponse"
    "fixture/CreateAppCookieStickinessPolicyResponse"
    (Proxy :: Proxy CreateAppCookieStickinessPolicy)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = res
    "AddTagsResponse"
    "fixture/AddTagsResponse"
    (Proxy :: Proxy AddTags)

testDescribeLoadBalancerAttributesResponse :: DescribeLoadBalancerAttributesResponse -> TestTree
testDescribeLoadBalancerAttributesResponse = res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse"
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

testDescribeInstanceHealthResponse :: DescribeInstanceHealthResponse -> TestTree
testDescribeInstanceHealthResponse = res
    "DescribeInstanceHealthResponse"
    "fixture/DescribeInstanceHealthResponse"
    (Proxy :: Proxy DescribeInstanceHealth)

testDetachLoadBalancerFromSubnetsResponse :: DetachLoadBalancerFromSubnetsResponse -> TestTree
testDetachLoadBalancerFromSubnetsResponse = res
    "DetachLoadBalancerFromSubnetsResponse"
    "fixture/DetachLoadBalancerFromSubnetsResponse"
    (Proxy :: Proxy DetachLoadBalancerFromSubnets)

testRegisterInstancesWithLoadBalancerResponse :: RegisterInstancesWithLoadBalancerResponse -> TestTree
testRegisterInstancesWithLoadBalancerResponse = res
    "RegisterInstancesWithLoadBalancerResponse"
    "fixture/RegisterInstancesWithLoadBalancerResponse"
    (Proxy :: Proxy RegisterInstancesWithLoadBalancer)

testDeleteLoadBalancerPolicyResponse :: DeleteLoadBalancerPolicyResponse -> TestTree
testDeleteLoadBalancerPolicyResponse = res
    "DeleteLoadBalancerPolicyResponse"
    "fixture/DeleteLoadBalancerPolicyResponse"
    (Proxy :: Proxy DeleteLoadBalancerPolicy)

testCreateLoadBalancerListenersResponse :: CreateLoadBalancerListenersResponse -> TestTree
testCreateLoadBalancerListenersResponse = res
    "CreateLoadBalancerListenersResponse"
    "fixture/CreateLoadBalancerListenersResponse"
    (Proxy :: Proxy CreateLoadBalancerListeners)

testDeleteLoadBalancerListenersResponse :: DeleteLoadBalancerListenersResponse -> TestTree
testDeleteLoadBalancerListenersResponse = res
    "DeleteLoadBalancerListenersResponse"
    "fixture/DeleteLoadBalancerListenersResponse"
    (Proxy :: Proxy DeleteLoadBalancerListeners)

testCreateLoadBalancerResponse :: CreateLoadBalancerResponse -> TestTree
testCreateLoadBalancerResponse = res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse"
    (Proxy :: Proxy CreateLoadBalancer)

testSetLoadBalancerPoliciesOfListenerResponse :: SetLoadBalancerPoliciesOfListenerResponse -> TestTree
testSetLoadBalancerPoliciesOfListenerResponse = res
    "SetLoadBalancerPoliciesOfListenerResponse"
    "fixture/SetLoadBalancerPoliciesOfListenerResponse"
    (Proxy :: Proxy SetLoadBalancerPoliciesOfListener)
