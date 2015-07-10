{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Elastic Load Balancing
--
-- Elastic Load Balancing distributes incoming traffic across your EC2
-- instances.
--
-- For information about the features of Elastic Load Balancing, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elastic-load-balancing.html What Is Elastic Load Balancing?>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- For information about the AWS regions supported by Elastic Load
-- Balancing, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#elb_region Regions and Endpoints - Elastic Load Balancing>
-- in the /Amazon Web Services General Reference/.
--
-- All Elastic Load Balancing operations are /idempotent/, which means that
-- they complete at most one time. If you repeat an operation, it succeeds
-- with a 200 OK response code.
module Network.AWS.ELB
    ( module Export
    ) where

import           Network.AWS.ELB.AddTags                                 as Export
import           Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer       as Export
import           Network.AWS.ELB.AttachLoadBalancerToSubnets             as Export
import           Network.AWS.ELB.ConfigureHealthCheck                    as Export
import           Network.AWS.ELB.CreateAppCookieStickinessPolicy         as Export
import           Network.AWS.ELB.CreateLBCookieStickinessPolicy          as Export
import           Network.AWS.ELB.CreateLoadBalancer                      as Export
import           Network.AWS.ELB.CreateLoadBalancerListeners             as Export
import           Network.AWS.ELB.CreateLoadBalancerPolicy                as Export
import           Network.AWS.ELB.DeleteLoadBalancer                      as Export
import           Network.AWS.ELB.DeleteLoadBalancerListeners             as Export
import           Network.AWS.ELB.DeleteLoadBalancerPolicy                as Export
import           Network.AWS.ELB.DeregisterInstancesFromLoadBalancer     as Export
import           Network.AWS.ELB.DescribeInstanceHealth                  as Export
import           Network.AWS.ELB.DescribeLoadBalancerAttributes          as Export
import           Network.AWS.ELB.DescribeLoadBalancerPolicies            as Export
import           Network.AWS.ELB.DescribeLoadBalancerPolicyTypes         as Export
import           Network.AWS.ELB.DescribeLoadBalancers                   as Export
import           Network.AWS.ELB.DescribeTags                            as Export
import           Network.AWS.ELB.DetachLoadBalancerFromSubnets           as Export
import           Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer as Export
import           Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer  as Export
import           Network.AWS.ELB.ModifyLoadBalancerAttributes            as Export
import           Network.AWS.ELB.RegisterInstancesWithLoadBalancer       as Export
import           Network.AWS.ELB.RemoveTags                              as Export
import           Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate   as Export
import           Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer as Export
import           Network.AWS.ELB.SetLoadBalancerPoliciesOfListener       as Export
import           Network.AWS.ELB.Types                                   as Export
import           Network.AWS.ELB.Waiters                                 as Export
