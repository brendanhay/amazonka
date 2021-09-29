{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ELB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ELB where

import Data.Proxy
import Network.AWS.ELB
import Test.AWS.ELB.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSetLoadBalancerListenerSSLCertificate $
--             newSetLoadBalancerListenerSSLCertificate
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestSetLoadBalancerPoliciesForBackendServer $
--             newSetLoadBalancerPoliciesForBackendServer
--
--         , requestSetLoadBalancerPoliciesOfListener $
--             newSetLoadBalancerPoliciesOfListener
--
--         , requestDescribeLoadBalancerPolicies $
--             newDescribeLoadBalancerPolicies
--
--         , requestCreateLoadBalancer $
--             newCreateLoadBalancer
--
--         , requestDeregisterInstancesFromLoadBalancer $
--             newDeregisterInstancesFromLoadBalancer
--
--         , requestRegisterInstancesWithLoadBalancer $
--             newRegisterInstancesWithLoadBalancer
--
--         , requestDetachLoadBalancerFromSubnets $
--             newDetachLoadBalancerFromSubnets
--
--         , requestCreateLoadBalancerListeners $
--             newCreateLoadBalancerListeners
--
--         , requestDeleteLoadBalancer $
--             newDeleteLoadBalancer
--
--         , requestAddTags $
--             newAddTags
--
--         , requestModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributes
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestCreateLBCookieStickinessPolicy $
--             newCreateLBCookieStickinessPolicy
--
--         , requestAttachLoadBalancerToSubnets $
--             newAttachLoadBalancerToSubnets
--
--         , requestApplySecurityGroupsToLoadBalancer $
--             newApplySecurityGroupsToLoadBalancer
--
--         , requestDescribeLoadBalancerPolicyTypes $
--             newDescribeLoadBalancerPolicyTypes
--
--         , requestDescribeLoadBalancers $
--             newDescribeLoadBalancers
--
--         , requestEnableAvailabilityZonesForLoadBalancer $
--             newEnableAvailabilityZonesForLoadBalancer
--
--         , requestDisableAvailabilityZonesForLoadBalancer $
--             newDisableAvailabilityZonesForLoadBalancer
--
--         , requestDeleteLoadBalancerListeners $
--             newDeleteLoadBalancerListeners
--
--         , requestCreateLoadBalancerPolicy $
--             newCreateLoadBalancerPolicy
--
--         , requestDeleteLoadBalancerPolicy $
--             newDeleteLoadBalancerPolicy
--
--         , requestDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributes
--
--         , requestDescribeInstanceHealth $
--             newDescribeInstanceHealth
--
--         , requestCreateAppCookieStickinessPolicy $
--             newCreateAppCookieStickinessPolicy
--
--         , requestConfigureHealthCheck $
--             newConfigureHealthCheck
--
--           ]

--     , testGroup "response"
--         [ responseSetLoadBalancerListenerSSLCertificate $
--             newSetLoadBalancerListenerSSLCertificateResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseSetLoadBalancerPoliciesForBackendServer $
--             newSetLoadBalancerPoliciesForBackendServerResponse
--
--         , responseSetLoadBalancerPoliciesOfListener $
--             newSetLoadBalancerPoliciesOfListenerResponse
--
--         , responseDescribeLoadBalancerPolicies $
--             newDescribeLoadBalancerPoliciesResponse
--
--         , responseCreateLoadBalancer $
--             newCreateLoadBalancerResponse
--
--         , responseDeregisterInstancesFromLoadBalancer $
--             newDeregisterInstancesFromLoadBalancerResponse
--
--         , responseRegisterInstancesWithLoadBalancer $
--             newRegisterInstancesWithLoadBalancerResponse
--
--         , responseDetachLoadBalancerFromSubnets $
--             newDetachLoadBalancerFromSubnetsResponse
--
--         , responseCreateLoadBalancerListeners $
--             newCreateLoadBalancerListenersResponse
--
--         , responseDeleteLoadBalancer $
--             newDeleteLoadBalancerResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributesResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseCreateLBCookieStickinessPolicy $
--             newCreateLBCookieStickinessPolicyResponse
--
--         , responseAttachLoadBalancerToSubnets $
--             newAttachLoadBalancerToSubnetsResponse
--
--         , responseApplySecurityGroupsToLoadBalancer $
--             newApplySecurityGroupsToLoadBalancerResponse
--
--         , responseDescribeLoadBalancerPolicyTypes $
--             newDescribeLoadBalancerPolicyTypesResponse
--
--         , responseDescribeLoadBalancers $
--             newDescribeLoadBalancersResponse
--
--         , responseEnableAvailabilityZonesForLoadBalancer $
--             newEnableAvailabilityZonesForLoadBalancerResponse
--
--         , responseDisableAvailabilityZonesForLoadBalancer $
--             newDisableAvailabilityZonesForLoadBalancerResponse
--
--         , responseDeleteLoadBalancerListeners $
--             newDeleteLoadBalancerListenersResponse
--
--         , responseCreateLoadBalancerPolicy $
--             newCreateLoadBalancerPolicyResponse
--
--         , responseDeleteLoadBalancerPolicy $
--             newDeleteLoadBalancerPolicyResponse
--
--         , responseDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributesResponse
--
--         , responseDescribeInstanceHealth $
--             newDescribeInstanceHealthResponse
--
--         , responseCreateAppCookieStickinessPolicy $
--             newCreateAppCookieStickinessPolicyResponse
--
--         , responseConfigureHealthCheck $
--             newConfigureHealthCheckResponse
--
--           ]
--     ]

-- Requests

requestSetLoadBalancerListenerSSLCertificate :: SetLoadBalancerListenerSSLCertificate -> TestTree
requestSetLoadBalancerListenerSSLCertificate =
  req
    "SetLoadBalancerListenerSSLCertificate"
    "fixture/SetLoadBalancerListenerSSLCertificate.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestSetLoadBalancerPoliciesForBackendServer :: SetLoadBalancerPoliciesForBackendServer -> TestTree
requestSetLoadBalancerPoliciesForBackendServer =
  req
    "SetLoadBalancerPoliciesForBackendServer"
    "fixture/SetLoadBalancerPoliciesForBackendServer.yaml"

requestSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListener -> TestTree
requestSetLoadBalancerPoliciesOfListener =
  req
    "SetLoadBalancerPoliciesOfListener"
    "fixture/SetLoadBalancerPoliciesOfListener.yaml"

requestDescribeLoadBalancerPolicies :: DescribeLoadBalancerPolicies -> TestTree
requestDescribeLoadBalancerPolicies =
  req
    "DescribeLoadBalancerPolicies"
    "fixture/DescribeLoadBalancerPolicies.yaml"

requestCreateLoadBalancer :: CreateLoadBalancer -> TestTree
requestCreateLoadBalancer =
  req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

requestDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancer -> TestTree
requestDeregisterInstancesFromLoadBalancer =
  req
    "DeregisterInstancesFromLoadBalancer"
    "fixture/DeregisterInstancesFromLoadBalancer.yaml"

requestRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancer -> TestTree
requestRegisterInstancesWithLoadBalancer =
  req
    "RegisterInstancesWithLoadBalancer"
    "fixture/RegisterInstancesWithLoadBalancer.yaml"

requestDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnets -> TestTree
requestDetachLoadBalancerFromSubnets =
  req
    "DetachLoadBalancerFromSubnets"
    "fixture/DetachLoadBalancerFromSubnets.yaml"

requestCreateLoadBalancerListeners :: CreateLoadBalancerListeners -> TestTree
requestCreateLoadBalancerListeners =
  req
    "CreateLoadBalancerListeners"
    "fixture/CreateLoadBalancerListeners.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer =
  req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributes -> TestTree
requestModifyLoadBalancerAttributes =
  req
    "ModifyLoadBalancerAttributes"
    "fixture/ModifyLoadBalancerAttributes.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestCreateLBCookieStickinessPolicy :: CreateLBCookieStickinessPolicy -> TestTree
requestCreateLBCookieStickinessPolicy =
  req
    "CreateLBCookieStickinessPolicy"
    "fixture/CreateLBCookieStickinessPolicy.yaml"

requestAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnets -> TestTree
requestAttachLoadBalancerToSubnets =
  req
    "AttachLoadBalancerToSubnets"
    "fixture/AttachLoadBalancerToSubnets.yaml"

requestApplySecurityGroupsToLoadBalancer :: ApplySecurityGroupsToLoadBalancer -> TestTree
requestApplySecurityGroupsToLoadBalancer =
  req
    "ApplySecurityGroupsToLoadBalancer"
    "fixture/ApplySecurityGroupsToLoadBalancer.yaml"

requestDescribeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypes -> TestTree
requestDescribeLoadBalancerPolicyTypes =
  req
    "DescribeLoadBalancerPolicyTypes"
    "fixture/DescribeLoadBalancerPolicyTypes.yaml"

requestDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
requestDescribeLoadBalancers =
  req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

requestEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancer -> TestTree
requestEnableAvailabilityZonesForLoadBalancer =
  req
    "EnableAvailabilityZonesForLoadBalancer"
    "fixture/EnableAvailabilityZonesForLoadBalancer.yaml"

requestDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancer -> TestTree
requestDisableAvailabilityZonesForLoadBalancer =
  req
    "DisableAvailabilityZonesForLoadBalancer"
    "fixture/DisableAvailabilityZonesForLoadBalancer.yaml"

requestDeleteLoadBalancerListeners :: DeleteLoadBalancerListeners -> TestTree
requestDeleteLoadBalancerListeners =
  req
    "DeleteLoadBalancerListeners"
    "fixture/DeleteLoadBalancerListeners.yaml"

requestCreateLoadBalancerPolicy :: CreateLoadBalancerPolicy -> TestTree
requestCreateLoadBalancerPolicy =
  req
    "CreateLoadBalancerPolicy"
    "fixture/CreateLoadBalancerPolicy.yaml"

requestDeleteLoadBalancerPolicy :: DeleteLoadBalancerPolicy -> TestTree
requestDeleteLoadBalancerPolicy =
  req
    "DeleteLoadBalancerPolicy"
    "fixture/DeleteLoadBalancerPolicy.yaml"

requestDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
requestDescribeLoadBalancerAttributes =
  req
    "DescribeLoadBalancerAttributes"
    "fixture/DescribeLoadBalancerAttributes.yaml"

requestDescribeInstanceHealth :: DescribeInstanceHealth -> TestTree
requestDescribeInstanceHealth =
  req
    "DescribeInstanceHealth"
    "fixture/DescribeInstanceHealth.yaml"

requestCreateAppCookieStickinessPolicy :: CreateAppCookieStickinessPolicy -> TestTree
requestCreateAppCookieStickinessPolicy =
  req
    "CreateAppCookieStickinessPolicy"
    "fixture/CreateAppCookieStickinessPolicy.yaml"

requestConfigureHealthCheck :: ConfigureHealthCheck -> TestTree
requestConfigureHealthCheck =
  req
    "ConfigureHealthCheck"
    "fixture/ConfigureHealthCheck.yaml"

-- Responses

responseSetLoadBalancerListenerSSLCertificate :: SetLoadBalancerListenerSSLCertificateResponse -> TestTree
responseSetLoadBalancerListenerSSLCertificate =
  res
    "SetLoadBalancerListenerSSLCertificateResponse"
    "fixture/SetLoadBalancerListenerSSLCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoadBalancerListenerSSLCertificate)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTags)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseSetLoadBalancerPoliciesForBackendServer :: SetLoadBalancerPoliciesForBackendServerResponse -> TestTree
responseSetLoadBalancerPoliciesForBackendServer =
  res
    "SetLoadBalancerPoliciesForBackendServerResponse"
    "fixture/SetLoadBalancerPoliciesForBackendServerResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoadBalancerPoliciesForBackendServer)

responseSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListenerResponse -> TestTree
responseSetLoadBalancerPoliciesOfListener =
  res
    "SetLoadBalancerPoliciesOfListenerResponse"
    "fixture/SetLoadBalancerPoliciesOfListenerResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoadBalancerPoliciesOfListener)

responseDescribeLoadBalancerPolicies :: DescribeLoadBalancerPoliciesResponse -> TestTree
responseDescribeLoadBalancerPolicies =
  res
    "DescribeLoadBalancerPoliciesResponse"
    "fixture/DescribeLoadBalancerPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancerPolicies)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoadBalancer)

responseDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancerResponse -> TestTree
responseDeregisterInstancesFromLoadBalancer =
  res
    "DeregisterInstancesFromLoadBalancerResponse"
    "fixture/DeregisterInstancesFromLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterInstancesFromLoadBalancer)

responseRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancerResponse -> TestTree
responseRegisterInstancesWithLoadBalancer =
  res
    "RegisterInstancesWithLoadBalancerResponse"
    "fixture/RegisterInstancesWithLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterInstancesWithLoadBalancer)

responseDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnetsResponse -> TestTree
responseDetachLoadBalancerFromSubnets =
  res
    "DetachLoadBalancerFromSubnetsResponse"
    "fixture/DetachLoadBalancerFromSubnetsResponse.proto"
    defaultService
    (Proxy :: Proxy DetachLoadBalancerFromSubnets)

responseCreateLoadBalancerListeners :: CreateLoadBalancerListenersResponse -> TestTree
responseCreateLoadBalancerListeners =
  res
    "CreateLoadBalancerListenersResponse"
    "fixture/CreateLoadBalancerListenersResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoadBalancerListeners)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancer)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy :: Proxy AddTags)

responseModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributesResponse -> TestTree
responseModifyLoadBalancerAttributes =
  res
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyLoadBalancerAttributes)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountLimits)

responseCreateLBCookieStickinessPolicy :: CreateLBCookieStickinessPolicyResponse -> TestTree
responseCreateLBCookieStickinessPolicy =
  res
    "CreateLBCookieStickinessPolicyResponse"
    "fixture/CreateLBCookieStickinessPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLBCookieStickinessPolicy)

responseAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnetsResponse -> TestTree
responseAttachLoadBalancerToSubnets =
  res
    "AttachLoadBalancerToSubnetsResponse"
    "fixture/AttachLoadBalancerToSubnetsResponse.proto"
    defaultService
    (Proxy :: Proxy AttachLoadBalancerToSubnets)

responseApplySecurityGroupsToLoadBalancer :: ApplySecurityGroupsToLoadBalancerResponse -> TestTree
responseApplySecurityGroupsToLoadBalancer =
  res
    "ApplySecurityGroupsToLoadBalancerResponse"
    "fixture/ApplySecurityGroupsToLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy ApplySecurityGroupsToLoadBalancer)

responseDescribeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypesResponse -> TestTree
responseDescribeLoadBalancerPolicyTypes =
  res
    "DescribeLoadBalancerPolicyTypesResponse"
    "fixture/DescribeLoadBalancerPolicyTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancerPolicyTypes)

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers =
  res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancers)

responseEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancerResponse -> TestTree
responseEnableAvailabilityZonesForLoadBalancer =
  res
    "EnableAvailabilityZonesForLoadBalancerResponse"
    "fixture/EnableAvailabilityZonesForLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy EnableAvailabilityZonesForLoadBalancer)

responseDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancerResponse -> TestTree
responseDisableAvailabilityZonesForLoadBalancer =
  res
    "DisableAvailabilityZonesForLoadBalancerResponse"
    "fixture/DisableAvailabilityZonesForLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAvailabilityZonesForLoadBalancer)

responseDeleteLoadBalancerListeners :: DeleteLoadBalancerListenersResponse -> TestTree
responseDeleteLoadBalancerListeners =
  res
    "DeleteLoadBalancerListenersResponse"
    "fixture/DeleteLoadBalancerListenersResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancerListeners)

responseCreateLoadBalancerPolicy :: CreateLoadBalancerPolicyResponse -> TestTree
responseCreateLoadBalancerPolicy =
  res
    "CreateLoadBalancerPolicyResponse"
    "fixture/CreateLoadBalancerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoadBalancerPolicy)

responseDeleteLoadBalancerPolicy :: DeleteLoadBalancerPolicyResponse -> TestTree
responseDeleteLoadBalancerPolicy =
  res
    "DeleteLoadBalancerPolicyResponse"
    "fixture/DeleteLoadBalancerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancerPolicy)

responseDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributesResponse -> TestTree
responseDescribeLoadBalancerAttributes =
  res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

responseDescribeInstanceHealth :: DescribeInstanceHealthResponse -> TestTree
responseDescribeInstanceHealth =
  res
    "DescribeInstanceHealthResponse"
    "fixture/DescribeInstanceHealthResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceHealth)

responseCreateAppCookieStickinessPolicy :: CreateAppCookieStickinessPolicyResponse -> TestTree
responseCreateAppCookieStickinessPolicy =
  res
    "CreateAppCookieStickinessPolicyResponse"
    "fixture/CreateAppCookieStickinessPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAppCookieStickinessPolicy)

responseConfigureHealthCheck :: ConfigureHealthCheckResponse -> TestTree
responseConfigureHealthCheck =
  res
    "ConfigureHealthCheckResponse"
    "fixture/ConfigureHealthCheckResponse.proto"
    defaultService
    (Proxy :: Proxy ConfigureHealthCheck)
