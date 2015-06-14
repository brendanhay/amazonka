-- Module      : Network.AWS.ELB
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

-- | Elastic Load Balancing
--
-- Elastic Load Balancing automatically distributes incoming web traffic
-- across multiple Amazon EC2 instances.
--
-- All Elastic Load Balancing actions and commands are /idempotent/, which
-- means that they complete no more than one time. If you repeat a request
-- or a command, the action succeeds with a 200 OK response code.
--
-- For detailed information about the features of Elastic Load Balancing,
-- see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/UserScenarios.html Managing Load Balancers>
-- in the /Elastic Load Balancing Developer Guide/.
module Network.AWS.ELB
    ( module Export
    ) where

import Network.AWS.ELB.AddTags as Export
import Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer as Export
import Network.AWS.ELB.AttachLoadBalancerToSubnets as Export
import Network.AWS.ELB.ConfigureHealthCheck as Export
import Network.AWS.ELB.CreateAppCookieStickinessPolicy as Export
import Network.AWS.ELB.CreateLBCookieStickinessPolicy as Export
import Network.AWS.ELB.CreateLoadBalancer as Export
import Network.AWS.ELB.CreateLoadBalancerListeners as Export
import Network.AWS.ELB.CreateLoadBalancerPolicy as Export
import Network.AWS.ELB.DeleteLoadBalancer as Export
import Network.AWS.ELB.DeleteLoadBalancerListeners as Export
import Network.AWS.ELB.DeleteLoadBalancerPolicy as Export
import Network.AWS.ELB.DeregisterInstancesFromLoadBalancer as Export
import Network.AWS.ELB.DescribeInstanceHealth as Export
import Network.AWS.ELB.DescribeLoadBalancerAttributes as Export
import Network.AWS.ELB.DescribeLoadBalancerPolicies as Export
import Network.AWS.ELB.DescribeLoadBalancerPolicyTypes as Export
import Network.AWS.ELB.DescribeLoadBalancers as Export
import Network.AWS.ELB.DescribeTags as Export
import Network.AWS.ELB.DetachLoadBalancerFromSubnets as Export
import Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer as Export
import Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer as Export
import Network.AWS.ELB.ModifyLoadBalancerAttributes as Export
import Network.AWS.ELB.RegisterInstancesWithLoadBalancer as Export
import Network.AWS.ELB.RemoveTags as Export
import Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate as Export
import Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer as Export
import Network.AWS.ELB.SetLoadBalancerPoliciesOfListener as Export
import Network.AWS.ELB.Types as Export
import Network.AWS.ELB.Waiters as Export
