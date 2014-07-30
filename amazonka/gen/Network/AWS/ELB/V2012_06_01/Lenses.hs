{-# LANGUAGE TemplateHaskell             #-}

-- Module      : Network.AWS.ELB.V2012_06_01.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ELB.V2012_06_01.Lenses where

import Control.Lens.TH
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.ELB.V2012_06_01.DescribeLoadBalancers
import Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerPolicyTypes
import Network.AWS.ELB.V2012_06_01.ApplySecurityGroupsToLoadBalancer
import Network.AWS.ELB.V2012_06_01.CreateLBCookieStickinessPolicy
import Network.AWS.ELB.V2012_06_01.DeleteLoadBalancer
import Network.AWS.ELB.V2012_06_01.DeregisterInstancesFromLoadBalancer
import Network.AWS.ELB.V2012_06_01.CreateLoadBalancerPolicy
import Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerPolicies
import Network.AWS.ELB.V2012_06_01.DisableAvailabilityZonesForLoadBalancer
import Network.AWS.ELB.V2012_06_01.EnableAvailabilityZonesForLoadBalancer
import Network.AWS.ELB.V2012_06_01.SetLoadBalancerPoliciesForBackendServer
import Network.AWS.ELB.V2012_06_01.SetLoadBalancerListenerSSLCertificate
import Network.AWS.ELB.V2012_06_01.AttachLoadBalancerToSubnets
import Network.AWS.ELB.V2012_06_01.ConfigureHealthCheck
import Network.AWS.ELB.V2012_06_01.ModifyLoadBalancerAttributes
import Network.AWS.ELB.V2012_06_01.CreateAppCookieStickinessPolicy
import Network.AWS.ELB.V2012_06_01.DescribeInstanceHealth
import Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerAttributes
import Network.AWS.ELB.V2012_06_01.CreateLoadBalancerListeners
import Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerPolicy
import Network.AWS.ELB.V2012_06_01.DetachLoadBalancerFromSubnets
import Network.AWS.ELB.V2012_06_01.RegisterInstancesWithLoadBalancer
import Network.AWS.ELB.V2012_06_01.CreateLoadBalancer
import Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerListeners
import Network.AWS.ELB.V2012_06_01.SetLoadBalancerPoliciesOfListener

-- Newtypes
makeIso ''ConnectionSettings
makeIso ''CrossZoneLoadBalancing
makeIso ''Instance

-- Products
makeLenses ''AccessLog
makeLenses ''AppCookieStickinessPolicy
makeLenses ''BackendServerDescription
makeLenses ''ConnectionDraining
makeLenses ''HealthCheck
makeLenses ''InstanceState
makeLenses ''LBCookieStickinessPolicy
makeLenses ''Listener
makeLenses ''ListenerDescription
makeLenses ''LoadBalancerAttributes
makeLenses ''LoadBalancerDescription
makeLenses ''Policies
makeLenses ''PolicyAttribute
makeLenses ''PolicyAttributeDescription
makeLenses ''PolicyAttributeTypeDescription
makeLenses ''PolicyDescription
makeLenses ''PolicyTypeDescription
makeLenses ''SourceSecurityGroup

-- Requests
makeLenses ''DescribeLoadBalancers
makeLenses ''DescribeLoadBalancerPolicyTypes
makeLenses ''ApplySecurityGroupsToLoadBalancer
makeLenses ''CreateLBCookieStickinessPolicy
makeLenses ''DeleteLoadBalancer
makeLenses ''DeregisterInstancesFromLoadBalancer
makeLenses ''CreateLoadBalancerPolicy
makeLenses ''DescribeLoadBalancerPolicies
makeLenses ''DisableAvailabilityZonesForLoadBalancer
makeLenses ''EnableAvailabilityZonesForLoadBalancer
makeLenses ''SetLoadBalancerPoliciesForBackendServer
makeLenses ''SetLoadBalancerListenerSSLCertificate
makeLenses ''AttachLoadBalancerToSubnets
makeLenses ''ConfigureHealthCheck
makeLenses ''ModifyLoadBalancerAttributes
makeLenses ''CreateAppCookieStickinessPolicy
makeLenses ''DescribeInstanceHealth
makeLenses ''DescribeLoadBalancerAttributes
makeLenses ''CreateLoadBalancerListeners
makeLenses ''DeleteLoadBalancerPolicy
makeLenses ''DetachLoadBalancerFromSubnets
makeLenses ''RegisterInstancesWithLoadBalancer
makeLenses ''CreateLoadBalancer
makeLenses ''DeleteLoadBalancerListeners
makeLenses ''SetLoadBalancerPoliciesOfListener

-- Responses
makeLenses ''DescribeLoadBalancersResponse
makeLenses ''DescribeLoadBalancerPolicyTypesResponse
makeLenses ''ApplySecurityGroupsToLoadBalancerResponse
makeLenses ''CreateLBCookieStickinessPolicyResponse
makeLenses ''DeleteLoadBalancerResponse
makeLenses ''DeregisterInstancesFromLoadBalancerResponse
makeLenses ''CreateLoadBalancerPolicyResponse
makeLenses ''DescribeLoadBalancerPoliciesResponse
makeLenses ''DisableAvailabilityZonesForLoadBalancerResponse
makeLenses ''EnableAvailabilityZonesForLoadBalancerResponse
makeLenses ''SetLoadBalancerPoliciesForBackendServerResponse
makeLenses ''SetLoadBalancerListenerSSLCertificateResponse
makeLenses ''AttachLoadBalancerToSubnetsResponse
makeLenses ''ConfigureHealthCheckResponse
makeLenses ''ModifyLoadBalancerAttributesResponse
makeLenses ''CreateAppCookieStickinessPolicyResponse
makeLenses ''DescribeInstanceHealthResponse
makeLenses ''DescribeLoadBalancerAttributesResponse
makeLenses ''CreateLoadBalancerListenersResponse
makeLenses ''DeleteLoadBalancerPolicyResponse
makeLenses ''DetachLoadBalancerFromSubnetsResponse
makeLenses ''RegisterInstancesWithLoadBalancerResponse
makeLenses ''CreateLoadBalancerResponse
makeLenses ''DeleteLoadBalancerListenersResponse
makeLenses ''SetLoadBalancerPoliciesOfListenerResponse
