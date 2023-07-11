{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ELB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ELB where

import Amazonka.ELB
import qualified Data.Proxy as Proxy
import Test.Amazonka.ELB.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddTags $
--             newAddTags
--
--         , requestApplySecurityGroupsToLoadBalancer $
--             newApplySecurityGroupsToLoadBalancer
--
--         , requestAttachLoadBalancerToSubnets $
--             newAttachLoadBalancerToSubnets
--
--         , requestConfigureHealthCheck $
--             newConfigureHealthCheck
--
--         , requestCreateAppCookieStickinessPolicy $
--             newCreateAppCookieStickinessPolicy
--
--         , requestCreateLBCookieStickinessPolicy $
--             newCreateLBCookieStickinessPolicy
--
--         , requestCreateLoadBalancer $
--             newCreateLoadBalancer
--
--         , requestCreateLoadBalancerListeners $
--             newCreateLoadBalancerListeners
--
--         , requestCreateLoadBalancerPolicy $
--             newCreateLoadBalancerPolicy
--
--         , requestDeleteLoadBalancer $
--             newDeleteLoadBalancer
--
--         , requestDeleteLoadBalancerListeners $
--             newDeleteLoadBalancerListeners
--
--         , requestDeleteLoadBalancerPolicy $
--             newDeleteLoadBalancerPolicy
--
--         , requestDeregisterInstancesFromLoadBalancer $
--             newDeregisterInstancesFromLoadBalancer
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestDescribeInstanceHealth $
--             newDescribeInstanceHealth
--
--         , requestDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributes
--
--         , requestDescribeLoadBalancerPolicies $
--             newDescribeLoadBalancerPolicies
--
--         , requestDescribeLoadBalancerPolicyTypes $
--             newDescribeLoadBalancerPolicyTypes
--
--         , requestDescribeLoadBalancers $
--             newDescribeLoadBalancers
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDetachLoadBalancerFromSubnets $
--             newDetachLoadBalancerFromSubnets
--
--         , requestDisableAvailabilityZonesForLoadBalancer $
--             newDisableAvailabilityZonesForLoadBalancer
--
--         , requestEnableAvailabilityZonesForLoadBalancer $
--             newEnableAvailabilityZonesForLoadBalancer
--
--         , requestModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributes
--
--         , requestRegisterInstancesWithLoadBalancer $
--             newRegisterInstancesWithLoadBalancer
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestSetLoadBalancerListenerSSLCertificate $
--             newSetLoadBalancerListenerSSLCertificate
--
--         , requestSetLoadBalancerPoliciesForBackendServer $
--             newSetLoadBalancerPoliciesForBackendServer
--
--         , requestSetLoadBalancerPoliciesOfListener $
--             newSetLoadBalancerPoliciesOfListener
--
--           ]

--     , testGroup "response"
--         [ responseAddTags $
--             newAddTagsResponse
--
--         , responseApplySecurityGroupsToLoadBalancer $
--             newApplySecurityGroupsToLoadBalancerResponse
--
--         , responseAttachLoadBalancerToSubnets $
--             newAttachLoadBalancerToSubnetsResponse
--
--         , responseConfigureHealthCheck $
--             newConfigureHealthCheckResponse
--
--         , responseCreateAppCookieStickinessPolicy $
--             newCreateAppCookieStickinessPolicyResponse
--
--         , responseCreateLBCookieStickinessPolicy $
--             newCreateLBCookieStickinessPolicyResponse
--
--         , responseCreateLoadBalancer $
--             newCreateLoadBalancerResponse
--
--         , responseCreateLoadBalancerListeners $
--             newCreateLoadBalancerListenersResponse
--
--         , responseCreateLoadBalancerPolicy $
--             newCreateLoadBalancerPolicyResponse
--
--         , responseDeleteLoadBalancer $
--             newDeleteLoadBalancerResponse
--
--         , responseDeleteLoadBalancerListeners $
--             newDeleteLoadBalancerListenersResponse
--
--         , responseDeleteLoadBalancerPolicy $
--             newDeleteLoadBalancerPolicyResponse
--
--         , responseDeregisterInstancesFromLoadBalancer $
--             newDeregisterInstancesFromLoadBalancerResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseDescribeInstanceHealth $
--             newDescribeInstanceHealthResponse
--
--         , responseDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributesResponse
--
--         , responseDescribeLoadBalancerPolicies $
--             newDescribeLoadBalancerPoliciesResponse
--
--         , responseDescribeLoadBalancerPolicyTypes $
--             newDescribeLoadBalancerPolicyTypesResponse
--
--         , responseDescribeLoadBalancers $
--             newDescribeLoadBalancersResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDetachLoadBalancerFromSubnets $
--             newDetachLoadBalancerFromSubnetsResponse
--
--         , responseDisableAvailabilityZonesForLoadBalancer $
--             newDisableAvailabilityZonesForLoadBalancerResponse
--
--         , responseEnableAvailabilityZonesForLoadBalancer $
--             newEnableAvailabilityZonesForLoadBalancerResponse
--
--         , responseModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributesResponse
--
--         , responseRegisterInstancesWithLoadBalancer $
--             newRegisterInstancesWithLoadBalancerResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseSetLoadBalancerListenerSSLCertificate $
--             newSetLoadBalancerListenerSSLCertificateResponse
--
--         , responseSetLoadBalancerPoliciesForBackendServer $
--             newSetLoadBalancerPoliciesForBackendServerResponse
--
--         , responseSetLoadBalancerPoliciesOfListener $
--             newSetLoadBalancerPoliciesOfListenerResponse
--
--           ]
--     ]

-- Requests

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestApplySecurityGroupsToLoadBalancer :: ApplySecurityGroupsToLoadBalancer -> TestTree
requestApplySecurityGroupsToLoadBalancer =
  req
    "ApplySecurityGroupsToLoadBalancer"
    "fixture/ApplySecurityGroupsToLoadBalancer.yaml"

requestAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnets -> TestTree
requestAttachLoadBalancerToSubnets =
  req
    "AttachLoadBalancerToSubnets"
    "fixture/AttachLoadBalancerToSubnets.yaml"

requestConfigureHealthCheck :: ConfigureHealthCheck -> TestTree
requestConfigureHealthCheck =
  req
    "ConfigureHealthCheck"
    "fixture/ConfigureHealthCheck.yaml"

requestCreateAppCookieStickinessPolicy :: CreateAppCookieStickinessPolicy -> TestTree
requestCreateAppCookieStickinessPolicy =
  req
    "CreateAppCookieStickinessPolicy"
    "fixture/CreateAppCookieStickinessPolicy.yaml"

requestCreateLBCookieStickinessPolicy :: CreateLBCookieStickinessPolicy -> TestTree
requestCreateLBCookieStickinessPolicy =
  req
    "CreateLBCookieStickinessPolicy"
    "fixture/CreateLBCookieStickinessPolicy.yaml"

requestCreateLoadBalancer :: CreateLoadBalancer -> TestTree
requestCreateLoadBalancer =
  req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

requestCreateLoadBalancerListeners :: CreateLoadBalancerListeners -> TestTree
requestCreateLoadBalancerListeners =
  req
    "CreateLoadBalancerListeners"
    "fixture/CreateLoadBalancerListeners.yaml"

requestCreateLoadBalancerPolicy :: CreateLoadBalancerPolicy -> TestTree
requestCreateLoadBalancerPolicy =
  req
    "CreateLoadBalancerPolicy"
    "fixture/CreateLoadBalancerPolicy.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer =
  req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

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

requestDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancer -> TestTree
requestDeregisterInstancesFromLoadBalancer =
  req
    "DeregisterInstancesFromLoadBalancer"
    "fixture/DeregisterInstancesFromLoadBalancer.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

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

requestDescribeLoadBalancerPolicies :: DescribeLoadBalancerPolicies -> TestTree
requestDescribeLoadBalancerPolicies =
  req
    "DescribeLoadBalancerPolicies"
    "fixture/DescribeLoadBalancerPolicies.yaml"

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

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnets -> TestTree
requestDetachLoadBalancerFromSubnets =
  req
    "DetachLoadBalancerFromSubnets"
    "fixture/DetachLoadBalancerFromSubnets.yaml"

requestDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancer -> TestTree
requestDisableAvailabilityZonesForLoadBalancer =
  req
    "DisableAvailabilityZonesForLoadBalancer"
    "fixture/DisableAvailabilityZonesForLoadBalancer.yaml"

requestEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancer -> TestTree
requestEnableAvailabilityZonesForLoadBalancer =
  req
    "EnableAvailabilityZonesForLoadBalancer"
    "fixture/EnableAvailabilityZonesForLoadBalancer.yaml"

requestModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributes -> TestTree
requestModifyLoadBalancerAttributes =
  req
    "ModifyLoadBalancerAttributes"
    "fixture/ModifyLoadBalancerAttributes.yaml"

requestRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancer -> TestTree
requestRegisterInstancesWithLoadBalancer =
  req
    "RegisterInstancesWithLoadBalancer"
    "fixture/RegisterInstancesWithLoadBalancer.yaml"

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

requestSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListener -> TestTree
requestSetLoadBalancerPoliciesOfListener =
  req
    "SetLoadBalancerPoliciesOfListener"
    "fixture/SetLoadBalancerPoliciesOfListener.yaml"

-- Responses

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseApplySecurityGroupsToLoadBalancer :: ApplySecurityGroupsToLoadBalancerResponse -> TestTree
responseApplySecurityGroupsToLoadBalancer =
  res
    "ApplySecurityGroupsToLoadBalancerResponse"
    "fixture/ApplySecurityGroupsToLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApplySecurityGroupsToLoadBalancer)

responseAttachLoadBalancerToSubnets :: AttachLoadBalancerToSubnetsResponse -> TestTree
responseAttachLoadBalancerToSubnets =
  res
    "AttachLoadBalancerToSubnetsResponse"
    "fixture/AttachLoadBalancerToSubnetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachLoadBalancerToSubnets)

responseConfigureHealthCheck :: ConfigureHealthCheckResponse -> TestTree
responseConfigureHealthCheck =
  res
    "ConfigureHealthCheckResponse"
    "fixture/ConfigureHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfigureHealthCheck)

responseCreateAppCookieStickinessPolicy :: CreateAppCookieStickinessPolicyResponse -> TestTree
responseCreateAppCookieStickinessPolicy =
  res
    "CreateAppCookieStickinessPolicyResponse"
    "fixture/CreateAppCookieStickinessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppCookieStickinessPolicy)

responseCreateLBCookieStickinessPolicy :: CreateLBCookieStickinessPolicyResponse -> TestTree
responseCreateLBCookieStickinessPolicy =
  res
    "CreateLBCookieStickinessPolicyResponse"
    "fixture/CreateLBCookieStickinessPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLBCookieStickinessPolicy)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoadBalancer)

responseCreateLoadBalancerListeners :: CreateLoadBalancerListenersResponse -> TestTree
responseCreateLoadBalancerListeners =
  res
    "CreateLoadBalancerListenersResponse"
    "fixture/CreateLoadBalancerListenersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoadBalancerListeners)

responseCreateLoadBalancerPolicy :: CreateLoadBalancerPolicyResponse -> TestTree
responseCreateLoadBalancerPolicy =
  res
    "CreateLoadBalancerPolicyResponse"
    "fixture/CreateLoadBalancerPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoadBalancerPolicy)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoadBalancer)

responseDeleteLoadBalancerListeners :: DeleteLoadBalancerListenersResponse -> TestTree
responseDeleteLoadBalancerListeners =
  res
    "DeleteLoadBalancerListenersResponse"
    "fixture/DeleteLoadBalancerListenersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoadBalancerListeners)

responseDeleteLoadBalancerPolicy :: DeleteLoadBalancerPolicyResponse -> TestTree
responseDeleteLoadBalancerPolicy =
  res
    "DeleteLoadBalancerPolicyResponse"
    "fixture/DeleteLoadBalancerPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoadBalancerPolicy)

responseDeregisterInstancesFromLoadBalancer :: DeregisterInstancesFromLoadBalancerResponse -> TestTree
responseDeregisterInstancesFromLoadBalancer =
  res
    "DeregisterInstancesFromLoadBalancerResponse"
    "fixture/DeregisterInstancesFromLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterInstancesFromLoadBalancer)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountLimits)

responseDescribeInstanceHealth :: DescribeInstanceHealthResponse -> TestTree
responseDescribeInstanceHealth =
  res
    "DescribeInstanceHealthResponse"
    "fixture/DescribeInstanceHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceHealth)

responseDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributesResponse -> TestTree
responseDescribeLoadBalancerAttributes =
  res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancerAttributes)

responseDescribeLoadBalancerPolicies :: DescribeLoadBalancerPoliciesResponse -> TestTree
responseDescribeLoadBalancerPolicies =
  res
    "DescribeLoadBalancerPoliciesResponse"
    "fixture/DescribeLoadBalancerPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancerPolicies)

responseDescribeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypesResponse -> TestTree
responseDescribeLoadBalancerPolicyTypes =
  res
    "DescribeLoadBalancerPolicyTypesResponse"
    "fixture/DescribeLoadBalancerPolicyTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancerPolicyTypes)

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers =
  res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancers)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseDetachLoadBalancerFromSubnets :: DetachLoadBalancerFromSubnetsResponse -> TestTree
responseDetachLoadBalancerFromSubnets =
  res
    "DetachLoadBalancerFromSubnetsResponse"
    "fixture/DetachLoadBalancerFromSubnetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachLoadBalancerFromSubnets)

responseDisableAvailabilityZonesForLoadBalancer :: DisableAvailabilityZonesForLoadBalancerResponse -> TestTree
responseDisableAvailabilityZonesForLoadBalancer =
  res
    "DisableAvailabilityZonesForLoadBalancerResponse"
    "fixture/DisableAvailabilityZonesForLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAvailabilityZonesForLoadBalancer)

responseEnableAvailabilityZonesForLoadBalancer :: EnableAvailabilityZonesForLoadBalancerResponse -> TestTree
responseEnableAvailabilityZonesForLoadBalancer =
  res
    "EnableAvailabilityZonesForLoadBalancerResponse"
    "fixture/EnableAvailabilityZonesForLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAvailabilityZonesForLoadBalancer)

responseModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributesResponse -> TestTree
responseModifyLoadBalancerAttributes =
  res
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyLoadBalancerAttributes)

responseRegisterInstancesWithLoadBalancer :: RegisterInstancesWithLoadBalancerResponse -> TestTree
responseRegisterInstancesWithLoadBalancer =
  res
    "RegisterInstancesWithLoadBalancerResponse"
    "fixture/RegisterInstancesWithLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterInstancesWithLoadBalancer)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTags)

responseSetLoadBalancerListenerSSLCertificate :: SetLoadBalancerListenerSSLCertificateResponse -> TestTree
responseSetLoadBalancerListenerSSLCertificate =
  res
    "SetLoadBalancerListenerSSLCertificateResponse"
    "fixture/SetLoadBalancerListenerSSLCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetLoadBalancerListenerSSLCertificate)

responseSetLoadBalancerPoliciesForBackendServer :: SetLoadBalancerPoliciesForBackendServerResponse -> TestTree
responseSetLoadBalancerPoliciesForBackendServer =
  res
    "SetLoadBalancerPoliciesForBackendServerResponse"
    "fixture/SetLoadBalancerPoliciesForBackendServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetLoadBalancerPoliciesForBackendServer)

responseSetLoadBalancerPoliciesOfListener :: SetLoadBalancerPoliciesOfListenerResponse -> TestTree
responseSetLoadBalancerPoliciesOfListener =
  res
    "SetLoadBalancerPoliciesOfListenerResponse"
    "fixture/SetLoadBalancerPoliciesOfListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetLoadBalancerPoliciesOfListener)
