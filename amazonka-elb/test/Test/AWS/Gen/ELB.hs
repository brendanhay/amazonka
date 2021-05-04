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
--         [ requestRemoveTags $
--             newRemoveTags
--
--         , requestSetLoadBalancerListenerSSLCertificate $
--             newSetLoadBalancerListenerSSLCertificate
--
--         , requestSetLoadBalancerPoliciesForBackendServer $
--             newSetLoadBalancerPoliciesForBackendServer
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeLoadBalancerPolicies $
--             newDescribeLoadBalancerPolicies
--
--         , requestSetLoadBalancerPoliciesOfListener $
--             newSetLoadBalancerPoliciesOfListener
--
--         , requestDeregisterInstancesFromLoadBalancer $
--             newDeregisterInstancesFromLoadBalancer
--
--         , requestCreateLoadBalancer $
--             newCreateLoadBalancer
--
--         , requestDetachLoadBalancerFromSubnets $
--             newDetachLoadBalancerFromSubnets
--
--         , requestRegisterInstancesWithLoadBalancer $
--             newRegisterInstancesWithLoadBalancer
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
--         , requestAttachLoadBalancerToSubnets $
--             newAttachLoadBalancerToSubnets
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestCreateLBCookieStickinessPolicy $
--             newCreateLBCookieStickinessPolicy
--
--         , requestApplySecurityGroupsToLoadBalancer $
--             newApplySecurityGroupsToLoadBalancer
--
--         , requestEnableAvailabilityZonesForLoadBalancer $
--             newEnableAvailabilityZonesForLoadBalancer
--
--         , requestDescribeLoadBalancers $
--             newDescribeLoadBalancers
--
--         , requestDescribeLoadBalancerPolicyTypes $
--             newDescribeLoadBalancerPolicyTypes
--
--         , requestDisableAvailabilityZonesForLoadBalancer $
--             newDisableAvailabilityZonesForLoadBalancer
--
--         , requestCreateLoadBalancerPolicy $
--             newCreateLoadBalancerPolicy
--
--         , requestDeleteLoadBalancerListeners $
--             newDeleteLoadBalancerListeners
--
--         , requestDeleteLoadBalancerPolicy $
--             newDeleteLoadBalancerPolicy
--
--         , requestDescribeInstanceHealth $
--             newDescribeInstanceHealth
--
--         , requestDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributes
--
--         , requestCreateAppCookieStickinessPolicy $
--             newCreateAppCookieStickinessPolicy
--
--         , requestConfigureHealthCheck $
--             newConfigureHealthCheck
--
--           ]

--     , testGroup "response"
--         [ responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseSetLoadBalancerListenerSSLCertificate $
--             newSetLoadBalancerListenerSSLCertificateResponse
--
--         , responseSetLoadBalancerPoliciesForBackendServer $
--             newSetLoadBalancerPoliciesForBackendServerResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeLoadBalancerPolicies $
--             newDescribeLoadBalancerPoliciesResponse
--
--         , responseSetLoadBalancerPoliciesOfListener $
--             newSetLoadBalancerPoliciesOfListenerResponse
--
--         , responseDeregisterInstancesFromLoadBalancer $
--             newDeregisterInstancesFromLoadBalancerResponse
--
--         , responseCreateLoadBalancer $
--             newCreateLoadBalancerResponse
--
--         , responseDetachLoadBalancerFromSubnets $
--             newDetachLoadBalancerFromSubnetsResponse
--
--         , responseRegisterInstancesWithLoadBalancer $
--             newRegisterInstancesWithLoadBalancerResponse
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
--         , responseAttachLoadBalancerToSubnets $
--             newAttachLoadBalancerToSubnetsResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseCreateLBCookieStickinessPolicy $
--             newCreateLBCookieStickinessPolicyResponse
--
--         , responseApplySecurityGroupsToLoadBalancer $
--             newApplySecurityGroupsToLoadBalancerResponse
--
--         , responseEnableAvailabilityZonesForLoadBalancer $
--             newEnableAvailabilityZonesForLoadBalancerResponse
--
--         , responseDescribeLoadBalancers $
--             newDescribeLoadBalancersResponse
--
--         , responseDescribeLoadBalancerPolicyTypes $
--             newDescribeLoadBalancerPolicyTypesResponse
--
--         , responseDisableAvailabilityZonesForLoadBalancer $
--             newDisableAvailabilityZonesForLoadBalancerResponse
--
--         , responseCreateLoadBalancerPolicy $
--             newCreateLoadBalancerPolicyResponse
--
--         , responseDeleteLoadBalancerListeners $
--             newDeleteLoadBalancerListenersResponse
--
--         , responseDeleteLoadBalancerPolicy $
--             newDeleteLoadBalancerPolicyResponse
--
--         , responseDescribeInstanceHealth $
--             newDescribeInstanceHealthResponse
--
--         , responseDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributesResponse
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

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestSetLoadBalancerListenerSSLCertificate :: SetLoadBalancerListenerSSLCertificate -> TestTree
requestSetLoadBalancerListenerSSLCertificate =
  req
    "SetLoadBalancerListenerSSLCertificate"
    "fixture/SetLoadBalancerListenerSSLCertificate.yaml"

requestSetLoadBalancerPoliciesForBackendServer :: SetLoadBalancerPoliciesForBackendServer -> TestTree
requestSetLoadBalancerPoliciesForBackendServer =
  req
    "SetLoadBalancerPoliciesForBackendServer"
    "fixture/SetLoadBalancerPoliciesForBackendServer.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeLoadBalancerPolicies :: DescribeLoadBalancerPolicies -> TestTree
requestDescribeLoadBalancerPolicies =
  req
    "DescribeLoadBalancerPolicies"
    "fixture/DescribeLoadBalancerPolicies.yaml"

requestSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListener -> TestTree
requestSetLoadBalancerPoliciesOfListener =
  req
    "SetLoadBalancerPoliciesOfListener"
    "fixture/SetLoadBalancerPoliciesOfListener.yaml"

requestDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancer -> TestTree
requestDeregisterInstancesFromLoadBalancer =
  req
    "DeregisterInstancesFromLoadBalancer"
    "fixture/DeregisterInstancesFromLoadBalancer.yaml"

requestCreateLoadBalancer :: CreateLoadBalancer -> TestTree
requestCreateLoadBalancer =
  req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

requestDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnets -> TestTree
requestDetachLoadBalancerFromSubnets =
  req
    "DetachLoadBalancerFromSubnets"
    "fixture/DetachLoadBalancerFromSubnets.yaml"

requestRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancer -> TestTree
requestRegisterInstancesWithLoadBalancer =
  req
    "RegisterInstancesWithLoadBalancer"
    "fixture/RegisterInstancesWithLoadBalancer.yaml"

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

requestAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnets -> TestTree
requestAttachLoadBalancerToSubnets =
  req
    "AttachLoadBalancerToSubnets"
    "fixture/AttachLoadBalancerToSubnets.yaml"

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

requestApplySecurityGroupsToLoadBalancer :: ApplySecurityGroupsToLoadBalancer -> TestTree
requestApplySecurityGroupsToLoadBalancer =
  req
    "ApplySecurityGroupsToLoadBalancer"
    "fixture/ApplySecurityGroupsToLoadBalancer.yaml"

requestEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancer -> TestTree
requestEnableAvailabilityZonesForLoadBalancer =
  req
    "EnableAvailabilityZonesForLoadBalancer"
    "fixture/EnableAvailabilityZonesForLoadBalancer.yaml"

requestDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
requestDescribeLoadBalancers =
  req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

requestDescribeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypes -> TestTree
requestDescribeLoadBalancerPolicyTypes =
  req
    "DescribeLoadBalancerPolicyTypes"
    "fixture/DescribeLoadBalancerPolicyTypes.yaml"

requestDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancer -> TestTree
requestDisableAvailabilityZonesForLoadBalancer =
  req
    "DisableAvailabilityZonesForLoadBalancer"
    "fixture/DisableAvailabilityZonesForLoadBalancer.yaml"

requestCreateLoadBalancerPolicy :: CreateLoadBalancerPolicy -> TestTree
requestCreateLoadBalancerPolicy =
  req
    "CreateLoadBalancerPolicy"
    "fixture/CreateLoadBalancerPolicy.yaml"

requestDeleteLoadBalancerListeners :: DeleteLoadBalancerListeners -> TestTree
requestDeleteLoadBalancerListeners =
  req
    "DeleteLoadBalancerListeners"
    "fixture/DeleteLoadBalancerListeners.yaml"

requestDeleteLoadBalancerPolicy :: DeleteLoadBalancerPolicy -> TestTree
requestDeleteLoadBalancerPolicy =
  req
    "DeleteLoadBalancerPolicy"
    "fixture/DeleteLoadBalancerPolicy.yaml"

requestDescribeInstanceHealth :: DescribeInstanceHealth -> TestTree
requestDescribeInstanceHealth =
  req
    "DescribeInstanceHealth"
    "fixture/DescribeInstanceHealth.yaml"

requestDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
requestDescribeLoadBalancerAttributes =
  req
    "DescribeLoadBalancerAttributes"
    "fixture/DescribeLoadBalancerAttributes.yaml"

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

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTags)

responseSetLoadBalancerListenerSSLCertificate :: SetLoadBalancerListenerSSLCertificateResponse -> TestTree
responseSetLoadBalancerListenerSSLCertificate =
  res
    "SetLoadBalancerListenerSSLCertificateResponse"
    "fixture/SetLoadBalancerListenerSSLCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoadBalancerListenerSSLCertificate)

responseSetLoadBalancerPoliciesForBackendServer :: SetLoadBalancerPoliciesForBackendServerResponse -> TestTree
responseSetLoadBalancerPoliciesForBackendServer =
  res
    "SetLoadBalancerPoliciesForBackendServerResponse"
    "fixture/SetLoadBalancerPoliciesForBackendServerResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoadBalancerPoliciesForBackendServer)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseDescribeLoadBalancerPolicies :: DescribeLoadBalancerPoliciesResponse -> TestTree
responseDescribeLoadBalancerPolicies =
  res
    "DescribeLoadBalancerPoliciesResponse"
    "fixture/DescribeLoadBalancerPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancerPolicies)

responseSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListenerResponse -> TestTree
responseSetLoadBalancerPoliciesOfListener =
  res
    "SetLoadBalancerPoliciesOfListenerResponse"
    "fixture/SetLoadBalancerPoliciesOfListenerResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoadBalancerPoliciesOfListener)

responseDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancerResponse -> TestTree
responseDeregisterInstancesFromLoadBalancer =
  res
    "DeregisterInstancesFromLoadBalancerResponse"
    "fixture/DeregisterInstancesFromLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterInstancesFromLoadBalancer)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoadBalancer)

responseDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnetsResponse -> TestTree
responseDetachLoadBalancerFromSubnets =
  res
    "DetachLoadBalancerFromSubnetsResponse"
    "fixture/DetachLoadBalancerFromSubnetsResponse.proto"
    defaultService
    (Proxy :: Proxy DetachLoadBalancerFromSubnets)

responseRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancerResponse -> TestTree
responseRegisterInstancesWithLoadBalancer =
  res
    "RegisterInstancesWithLoadBalancerResponse"
    "fixture/RegisterInstancesWithLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterInstancesWithLoadBalancer)

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

responseAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnetsResponse -> TestTree
responseAttachLoadBalancerToSubnets =
  res
    "AttachLoadBalancerToSubnetsResponse"
    "fixture/AttachLoadBalancerToSubnetsResponse.proto"
    defaultService
    (Proxy :: Proxy AttachLoadBalancerToSubnets)

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

responseApplySecurityGroupsToLoadBalancer :: ApplySecurityGroupsToLoadBalancerResponse -> TestTree
responseApplySecurityGroupsToLoadBalancer =
  res
    "ApplySecurityGroupsToLoadBalancerResponse"
    "fixture/ApplySecurityGroupsToLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy ApplySecurityGroupsToLoadBalancer)

responseEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancerResponse -> TestTree
responseEnableAvailabilityZonesForLoadBalancer =
  res
    "EnableAvailabilityZonesForLoadBalancerResponse"
    "fixture/EnableAvailabilityZonesForLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy EnableAvailabilityZonesForLoadBalancer)

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers =
  res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancers)

responseDescribeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypesResponse -> TestTree
responseDescribeLoadBalancerPolicyTypes =
  res
    "DescribeLoadBalancerPolicyTypesResponse"
    "fixture/DescribeLoadBalancerPolicyTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancerPolicyTypes)

responseDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancerResponse -> TestTree
responseDisableAvailabilityZonesForLoadBalancer =
  res
    "DisableAvailabilityZonesForLoadBalancerResponse"
    "fixture/DisableAvailabilityZonesForLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAvailabilityZonesForLoadBalancer)

responseCreateLoadBalancerPolicy :: CreateLoadBalancerPolicyResponse -> TestTree
responseCreateLoadBalancerPolicy =
  res
    "CreateLoadBalancerPolicyResponse"
    "fixture/CreateLoadBalancerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoadBalancerPolicy)

responseDeleteLoadBalancerListeners :: DeleteLoadBalancerListenersResponse -> TestTree
responseDeleteLoadBalancerListeners =
  res
    "DeleteLoadBalancerListenersResponse"
    "fixture/DeleteLoadBalancerListenersResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancerListeners)

responseDeleteLoadBalancerPolicy :: DeleteLoadBalancerPolicyResponse -> TestTree
responseDeleteLoadBalancerPolicy =
  res
    "DeleteLoadBalancerPolicyResponse"
    "fixture/DeleteLoadBalancerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancerPolicy)

responseDescribeInstanceHealth :: DescribeInstanceHealthResponse -> TestTree
responseDescribeInstanceHealth =
  res
    "DescribeInstanceHealthResponse"
    "fixture/DescribeInstanceHealthResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceHealth)

responseDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributesResponse -> TestTree
responseDescribeLoadBalancerAttributes =
  res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

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
