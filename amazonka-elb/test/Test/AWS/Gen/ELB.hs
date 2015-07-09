{-# OPTIONS_GHC -fno-warn-orphans #-}

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
testDescribeLoadBalancerPolicyTypes = undefined

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = undefined

testDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
testDescribeLoadBalancers = undefined

testApplySecurityGroupsToLoadBalancer :: ApplySecurityGroupsToLoadBalancer -> TestTree
testApplySecurityGroupsToLoadBalancer = undefined

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = undefined

testCreateLBCookieStickinessPolicy :: CreateLBCookieStickinessPolicy -> TestTree
testCreateLBCookieStickinessPolicy = undefined

testDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
testDeleteLoadBalancer = undefined

testCreateLoadBalancerPolicy :: CreateLoadBalancerPolicy -> TestTree
testCreateLoadBalancerPolicy = undefined

testDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancer -> TestTree
testDeregisterInstancesFromLoadBalancer = undefined

testDescribeLoadBalancerPolicies :: DescribeLoadBalancerPolicies -> TestTree
testDescribeLoadBalancerPolicies = undefined

testDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancer -> TestTree
testDisableAvailabilityZonesForLoadBalancer = undefined

testSetLoadBalancerPoliciesForBackendServer :: SetLoadBalancerPoliciesForBackendServer -> TestTree
testSetLoadBalancerPoliciesForBackendServer = undefined

testEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancer -> TestTree
testEnableAvailabilityZonesForLoadBalancer = undefined

testSetLoadBalancerListenerSSLCertificate :: SetLoadBalancerListenerSSLCertificate -> TestTree
testSetLoadBalancerListenerSSLCertificate = undefined

testConfigureHealthCheck :: ConfigureHealthCheck -> TestTree
testConfigureHealthCheck = undefined

testAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnets -> TestTree
testAttachLoadBalancerToSubnets = undefined

testModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributes -> TestTree
testModifyLoadBalancerAttributes = undefined

testCreateAppCookieStickinessPolicy :: CreateAppCookieStickinessPolicy -> TestTree
testCreateAppCookieStickinessPolicy = undefined

testAddTags :: AddTags -> TestTree
testAddTags = undefined

testDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
testDescribeLoadBalancerAttributes = undefined

testDescribeInstanceHealth :: DescribeInstanceHealth -> TestTree
testDescribeInstanceHealth = undefined

testDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnets -> TestTree
testDetachLoadBalancerFromSubnets = undefined

testRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancer -> TestTree
testRegisterInstancesWithLoadBalancer = undefined

testDeleteLoadBalancerPolicy :: DeleteLoadBalancerPolicy -> TestTree
testDeleteLoadBalancerPolicy = undefined

testCreateLoadBalancerListeners :: CreateLoadBalancerListeners -> TestTree
testCreateLoadBalancerListeners = undefined

testDeleteLoadBalancerListeners :: DeleteLoadBalancerListeners -> TestTree
testDeleteLoadBalancerListeners = undefined

testCreateLoadBalancer :: CreateLoadBalancer -> TestTree
testCreateLoadBalancer = undefined

testSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListener -> TestTree
testSetLoadBalancerPoliciesOfListener = undefined

-- Responses

testDescribeLoadBalancerPolicyTypesResponse :: DescribeLoadBalancerPolicyTypesResponse -> TestTree
testDescribeLoadBalancerPolicyTypesResponse = resp
    "DescribeLoadBalancerPolicyTypesResponse"
    "fixture/DescribeLoadBalancerPolicyTypesResponse"
    (Proxy :: Proxy DescribeLoadBalancerPolicyTypes)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = resp
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

testDescribeLoadBalancersResponse :: DescribeLoadBalancersResponse -> TestTree
testDescribeLoadBalancersResponse = resp
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse"
    (Proxy :: Proxy DescribeLoadBalancers)

testApplySecurityGroupsToLoadBalancerResponse :: ApplySecurityGroupsToLoadBalancerResponse -> TestTree
testApplySecurityGroupsToLoadBalancerResponse = resp
    "ApplySecurityGroupsToLoadBalancerResponse"
    "fixture/ApplySecurityGroupsToLoadBalancerResponse"
    (Proxy :: Proxy ApplySecurityGroupsToLoadBalancer)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = resp
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

testCreateLBCookieStickinessPolicyResponse :: CreateLBCookieStickinessPolicyResponse -> TestTree
testCreateLBCookieStickinessPolicyResponse = resp
    "CreateLBCookieStickinessPolicyResponse"
    "fixture/CreateLBCookieStickinessPolicyResponse"
    (Proxy :: Proxy CreateLBCookieStickinessPolicy)

testDeleteLoadBalancerResponse :: DeleteLoadBalancerResponse -> TestTree
testDeleteLoadBalancerResponse = resp
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse"
    (Proxy :: Proxy DeleteLoadBalancer)

testCreateLoadBalancerPolicyResponse :: CreateLoadBalancerPolicyResponse -> TestTree
testCreateLoadBalancerPolicyResponse = resp
    "CreateLoadBalancerPolicyResponse"
    "fixture/CreateLoadBalancerPolicyResponse"
    (Proxy :: Proxy CreateLoadBalancerPolicy)

testDeregisterInstancesFromLoadBalancerResponse :: DeregisterInstancesFromLoadBalancerResponse -> TestTree
testDeregisterInstancesFromLoadBalancerResponse = resp
    "DeregisterInstancesFromLoadBalancerResponse"
    "fixture/DeregisterInstancesFromLoadBalancerResponse"
    (Proxy :: Proxy DeregisterInstancesFromLoadBalancer)

testDescribeLoadBalancerPoliciesResponse :: DescribeLoadBalancerPoliciesResponse -> TestTree
testDescribeLoadBalancerPoliciesResponse = resp
    "DescribeLoadBalancerPoliciesResponse"
    "fixture/DescribeLoadBalancerPoliciesResponse"
    (Proxy :: Proxy DescribeLoadBalancerPolicies)

testDisableAvailabilityZonesForLoadBalancerResponse :: DisableAvailabilityZonesForLoadBalancerResponse -> TestTree
testDisableAvailabilityZonesForLoadBalancerResponse = resp
    "DisableAvailabilityZonesForLoadBalancerResponse"
    "fixture/DisableAvailabilityZonesForLoadBalancerResponse"
    (Proxy :: Proxy DisableAvailabilityZonesForLoadBalancer)

testSetLoadBalancerPoliciesForBackendServerResponse :: SetLoadBalancerPoliciesForBackendServerResponse -> TestTree
testSetLoadBalancerPoliciesForBackendServerResponse = resp
    "SetLoadBalancerPoliciesForBackendServerResponse"
    "fixture/SetLoadBalancerPoliciesForBackendServerResponse"
    (Proxy :: Proxy SetLoadBalancerPoliciesForBackendServer)

testEnableAvailabilityZonesForLoadBalancerResponse :: EnableAvailabilityZonesForLoadBalancerResponse -> TestTree
testEnableAvailabilityZonesForLoadBalancerResponse = resp
    "EnableAvailabilityZonesForLoadBalancerResponse"
    "fixture/EnableAvailabilityZonesForLoadBalancerResponse"
    (Proxy :: Proxy EnableAvailabilityZonesForLoadBalancer)

testSetLoadBalancerListenerSSLCertificateResponse :: SetLoadBalancerListenerSSLCertificateResponse -> TestTree
testSetLoadBalancerListenerSSLCertificateResponse = resp
    "SetLoadBalancerListenerSSLCertificateResponse"
    "fixture/SetLoadBalancerListenerSSLCertificateResponse"
    (Proxy :: Proxy SetLoadBalancerListenerSSLCertificate)

testConfigureHealthCheckResponse :: ConfigureHealthCheckResponse -> TestTree
testConfigureHealthCheckResponse = resp
    "ConfigureHealthCheckResponse"
    "fixture/ConfigureHealthCheckResponse"
    (Proxy :: Proxy ConfigureHealthCheck)

testAttachLoadBalancerToSubnetsResponse :: AttachLoadBalancerToSubnetsResponse -> TestTree
testAttachLoadBalancerToSubnetsResponse = resp
    "AttachLoadBalancerToSubnetsResponse"
    "fixture/AttachLoadBalancerToSubnetsResponse"
    (Proxy :: Proxy AttachLoadBalancerToSubnets)

testModifyLoadBalancerAttributesResponse :: ModifyLoadBalancerAttributesResponse -> TestTree
testModifyLoadBalancerAttributesResponse = resp
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse"
    (Proxy :: Proxy ModifyLoadBalancerAttributes)

testCreateAppCookieStickinessPolicyResponse :: CreateAppCookieStickinessPolicyResponse -> TestTree
testCreateAppCookieStickinessPolicyResponse = resp
    "CreateAppCookieStickinessPolicyResponse"
    "fixture/CreateAppCookieStickinessPolicyResponse"
    (Proxy :: Proxy CreateAppCookieStickinessPolicy)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = resp
    "AddTagsResponse"
    "fixture/AddTagsResponse"
    (Proxy :: Proxy AddTags)

testDescribeLoadBalancerAttributesResponse :: DescribeLoadBalancerAttributesResponse -> TestTree
testDescribeLoadBalancerAttributesResponse = resp
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse"
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

testDescribeInstanceHealthResponse :: DescribeInstanceHealthResponse -> TestTree
testDescribeInstanceHealthResponse = resp
    "DescribeInstanceHealthResponse"
    "fixture/DescribeInstanceHealthResponse"
    (Proxy :: Proxy DescribeInstanceHealth)

testDetachLoadBalancerFromSubnetsResponse :: DetachLoadBalancerFromSubnetsResponse -> TestTree
testDetachLoadBalancerFromSubnetsResponse = resp
    "DetachLoadBalancerFromSubnetsResponse"
    "fixture/DetachLoadBalancerFromSubnetsResponse"
    (Proxy :: Proxy DetachLoadBalancerFromSubnets)

testRegisterInstancesWithLoadBalancerResponse :: RegisterInstancesWithLoadBalancerResponse -> TestTree
testRegisterInstancesWithLoadBalancerResponse = resp
    "RegisterInstancesWithLoadBalancerResponse"
    "fixture/RegisterInstancesWithLoadBalancerResponse"
    (Proxy :: Proxy RegisterInstancesWithLoadBalancer)

testDeleteLoadBalancerPolicyResponse :: DeleteLoadBalancerPolicyResponse -> TestTree
testDeleteLoadBalancerPolicyResponse = resp
    "DeleteLoadBalancerPolicyResponse"
    "fixture/DeleteLoadBalancerPolicyResponse"
    (Proxy :: Proxy DeleteLoadBalancerPolicy)

testCreateLoadBalancerListenersResponse :: CreateLoadBalancerListenersResponse -> TestTree
testCreateLoadBalancerListenersResponse = resp
    "CreateLoadBalancerListenersResponse"
    "fixture/CreateLoadBalancerListenersResponse"
    (Proxy :: Proxy CreateLoadBalancerListeners)

testDeleteLoadBalancerListenersResponse :: DeleteLoadBalancerListenersResponse -> TestTree
testDeleteLoadBalancerListenersResponse = resp
    "DeleteLoadBalancerListenersResponse"
    "fixture/DeleteLoadBalancerListenersResponse"
    (Proxy :: Proxy DeleteLoadBalancerListeners)

testCreateLoadBalancerResponse :: CreateLoadBalancerResponse -> TestTree
testCreateLoadBalancerResponse = resp
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse"
    (Proxy :: Proxy CreateLoadBalancer)

testSetLoadBalancerPoliciesOfListenerResponse :: SetLoadBalancerPoliciesOfListenerResponse -> TestTree
testSetLoadBalancerPoliciesOfListenerResponse = resp
    "SetLoadBalancerPoliciesOfListenerResponse"
    "fixture/SetLoadBalancerPoliciesOfListenerResponse"
    (Proxy :: Proxy SetLoadBalancerPoliciesOfListener)

instance Out AccessLog
instance Out AddTags
instance Out AddTagsResponse
instance Out AdditionalAttribute
instance Out AppCookieStickinessPolicy
instance Out ApplySecurityGroupsToLoadBalancer
instance Out ApplySecurityGroupsToLoadBalancerResponse
instance Out AttachLoadBalancerToSubnets
instance Out AttachLoadBalancerToSubnetsResponse
instance Out BackendServerDescription
instance Out ConfigureHealthCheck
instance Out ConfigureHealthCheckResponse
instance Out ConnectionDraining
instance Out ConnectionSettings
instance Out CreateAppCookieStickinessPolicy
instance Out CreateAppCookieStickinessPolicyResponse
instance Out CreateLBCookieStickinessPolicy
instance Out CreateLBCookieStickinessPolicyResponse
instance Out CreateLoadBalancer
instance Out CreateLoadBalancerListeners
instance Out CreateLoadBalancerListenersResponse
instance Out CreateLoadBalancerPolicy
instance Out CreateLoadBalancerPolicyResponse
instance Out CreateLoadBalancerResponse
instance Out CrossZoneLoadBalancing
instance Out DeleteLoadBalancer
instance Out DeleteLoadBalancerListeners
instance Out DeleteLoadBalancerListenersResponse
instance Out DeleteLoadBalancerPolicy
instance Out DeleteLoadBalancerPolicyResponse
instance Out DeleteLoadBalancerResponse
instance Out DeregisterInstancesFromLoadBalancer
instance Out DeregisterInstancesFromLoadBalancerResponse
instance Out DescribeInstanceHealth
instance Out DescribeInstanceHealthResponse
instance Out DescribeLoadBalancerAttributes
instance Out DescribeLoadBalancerAttributesResponse
instance Out DescribeLoadBalancerPolicies
instance Out DescribeLoadBalancerPoliciesResponse
instance Out DescribeLoadBalancerPolicyTypes
instance Out DescribeLoadBalancerPolicyTypesResponse
instance Out DescribeLoadBalancers
instance Out DescribeLoadBalancersResponse
instance Out DescribeTags
instance Out DescribeTagsResponse
instance Out DetachLoadBalancerFromSubnets
instance Out DetachLoadBalancerFromSubnetsResponse
instance Out DisableAvailabilityZonesForLoadBalancer
instance Out DisableAvailabilityZonesForLoadBalancerResponse
instance Out EnableAvailabilityZonesForLoadBalancer
instance Out EnableAvailabilityZonesForLoadBalancerResponse
instance Out HealthCheck
instance Out Instance
instance Out InstanceState
instance Out LBCookieStickinessPolicy
instance Out Listener
instance Out ListenerDescription
instance Out LoadBalancerAttributes
instance Out LoadBalancerDescription
instance Out ModifyLoadBalancerAttributes
instance Out ModifyLoadBalancerAttributesResponse
instance Out Policies
instance Out PolicyAttribute
instance Out PolicyAttributeDescription
instance Out PolicyAttributeTypeDescription
instance Out PolicyDescription
instance Out PolicyTypeDescription
instance Out RegisterInstancesWithLoadBalancer
instance Out RegisterInstancesWithLoadBalancerResponse
instance Out RemoveTags
instance Out RemoveTagsResponse
instance Out SetLoadBalancerListenerSSLCertificate
instance Out SetLoadBalancerListenerSSLCertificateResponse
instance Out SetLoadBalancerPoliciesForBackendServer
instance Out SetLoadBalancerPoliciesForBackendServerResponse
instance Out SetLoadBalancerPoliciesOfListener
instance Out SetLoadBalancerPoliciesOfListenerResponse
instance Out SourceSecurityGroup
instance Out Tag
instance Out TagDescription
instance Out TagKeyOnly
