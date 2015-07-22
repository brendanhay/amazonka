{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a load balancer.
--
-- If the call completes successfully, a new load balancer is created with
-- a unique Domain Name Service (DNS) name. The load balancer receives
-- incoming traffic and routes it to the registered instances. For more
-- information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/how-elb-works.html How Elastic Load Balancing Works>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- You can create up to 20 load balancers per region per account. You can
-- request an increase for the number of load balancers for your account.
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-limits.html Elastic Load Balancing Limits>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancer.html>
module Network.AWS.ELB.CreateLoadBalancer
    (
    -- * Request
      CreateLoadBalancer
    -- ** Request constructor
    , createLoadBalancer
    -- ** Request lenses
    , clbrqSecurityGroups
    , clbrqSubnets
    , clbrqAvailabilityZones
    , clbrqScheme
    , clbrqTags
    , clbrqLoadBalancerName
    , clbrqListeners

    -- * Response
    , CreateLoadBalancerResponse
    -- ** Response constructor
    , createLoadBalancerResponse
    -- ** Response lenses
    , clbrsDNSName
    , clbrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbrqSecurityGroups'
--
-- * 'clbrqSubnets'
--
-- * 'clbrqAvailabilityZones'
--
-- * 'clbrqScheme'
--
-- * 'clbrqTags'
--
-- * 'clbrqLoadBalancerName'
--
-- * 'clbrqListeners'
data CreateLoadBalancer = CreateLoadBalancer'
    { _clbrqSecurityGroups    :: !(Maybe [Text])
    , _clbrqSubnets           :: !(Maybe [Text])
    , _clbrqAvailabilityZones :: !(Maybe [Text])
    , _clbrqScheme            :: !(Maybe Text)
    , _clbrqTags              :: !(Maybe (List1 Tag))
    , _clbrqLoadBalancerName  :: !Text
    , _clbrqListeners         :: ![Listener]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLoadBalancer' smart constructor.
createLoadBalancer :: Text -> CreateLoadBalancer
createLoadBalancer pLoadBalancerName_ =
    CreateLoadBalancer'
    { _clbrqSecurityGroups = Nothing
    , _clbrqSubnets = Nothing
    , _clbrqAvailabilityZones = Nothing
    , _clbrqScheme = Nothing
    , _clbrqTags = Nothing
    , _clbrqLoadBalancerName = pLoadBalancerName_
    , _clbrqListeners = mempty
    }

-- | The IDs of the security groups to assign to the load balancer.
clbrqSecurityGroups :: Lens' CreateLoadBalancer [Text]
clbrqSecurityGroups = lens _clbrqSecurityGroups (\ s a -> s{_clbrqSecurityGroups = a}) . _Default;

-- | The IDs of the subnets in your VPC to attach to the load balancer.
-- Specify one subnet per Availability Zone specified in
-- @AvailabilityZones@.
clbrqSubnets :: Lens' CreateLoadBalancer [Text]
clbrqSubnets = lens _clbrqSubnets (\ s a -> s{_clbrqSubnets = a}) . _Default;

-- | One or more Availability Zones from the same region as the load
-- balancer. Traffic is equally distributed across all specified
-- Availability Zones.
--
-- You must specify at least one Availability Zone.
--
-- You can add more Availability Zones after you create the load balancer
-- using EnableAvailabilityZonesForLoadBalancer.
clbrqAvailabilityZones :: Lens' CreateLoadBalancer [Text]
clbrqAvailabilityZones = lens _clbrqAvailabilityZones (\ s a -> s{_clbrqAvailabilityZones = a}) . _Default;

-- | The type of a load balancer. Valid only for load balancers in a VPC.
--
-- By default, Elastic Load Balancing creates an Internet-facing load
-- balancer with a publicly resolvable DNS name, which resolves to public
-- IP addresses. For more information about Internet-facing and Internal
-- load balancers, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/vpc-loadbalancer-types.html Internet-facing and Internal Load Balancers>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- Specify @internal@ to create an internal load balancer with a DNS name
-- that resolves to private IP addresses.
clbrqScheme :: Lens' CreateLoadBalancer (Maybe Text)
clbrqScheme = lens _clbrqScheme (\ s a -> s{_clbrqScheme = a});

-- | A list of tags to assign to the load balancer.
--
-- For more information about tagging your load balancer, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#tagging-elb Tagging>
-- in the /Elastic Load Balancing Developer Guide/.
clbrqTags :: Lens' CreateLoadBalancer (Maybe (NonEmpty Tag))
clbrqTags = lens _clbrqTags (\ s a -> s{_clbrqTags = a}) . mapping _List1;

-- | The name of the load balancer.
--
-- This name must be unique within your AWS account, must have a maximum of
-- 32 characters, must contain only alphanumeric characters or hyphens, and
-- cannot begin or end with a hyphen.
clbrqLoadBalancerName :: Lens' CreateLoadBalancer Text
clbrqLoadBalancerName = lens _clbrqLoadBalancerName (\ s a -> s{_clbrqLoadBalancerName = a});

-- | The listeners.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-listener-config.html Listeners for Your Load Balancer>
-- in the /Elastic Load Balancing Developer Guide/.
clbrqListeners :: Lens' CreateLoadBalancer [Listener]
clbrqListeners = lens _clbrqListeners (\ s a -> s{_clbrqListeners = a});

instance AWSRequest CreateLoadBalancer where
        type Sv CreateLoadBalancer = ELB
        type Rs CreateLoadBalancer =
             CreateLoadBalancerResponse
        request = post
        response
          = receiveXMLWrapper "CreateLoadBalancerResult"
              (\ s h x ->
                 CreateLoadBalancerResponse' <$>
                   (x .@? "DNSName") <*> (pure (fromEnum s)))

instance ToHeaders CreateLoadBalancer where
        toHeaders = const mempty

instance ToPath CreateLoadBalancer where
        toPath = const "/"

instance ToQuery CreateLoadBalancer where
        toQuery CreateLoadBalancer'{..}
          = mconcat
              ["Action" =: ("CreateLoadBalancer" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "SecurityGroups" =:
                 toQuery
                   (toQueryList "member" <$> _clbrqSecurityGroups),
               "Subnets" =:
                 toQuery (toQueryList "member" <$> _clbrqSubnets),
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "member" <$> _clbrqAvailabilityZones),
               "Scheme" =: _clbrqScheme,
               "Tags" =:
                 toQuery (toQueryList "member" <$> _clbrqTags),
               "LoadBalancerName" =: _clbrqLoadBalancerName,
               "Listeners" =: toQueryList "member" _clbrqListeners]

-- | /See:/ 'createLoadBalancerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbrsDNSName'
--
-- * 'clbrsStatus'
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
    { _clbrsDNSName :: !(Maybe Text)
    , _clbrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLoadBalancerResponse' smart constructor.
createLoadBalancerResponse :: Int -> CreateLoadBalancerResponse
createLoadBalancerResponse pStatus_ =
    CreateLoadBalancerResponse'
    { _clbrsDNSName = Nothing
    , _clbrsStatus = pStatus_
    }

-- | The DNS name of the load balancer.
clbrsDNSName :: Lens' CreateLoadBalancerResponse (Maybe Text)
clbrsDNSName = lens _clbrsDNSName (\ s a -> s{_clbrsDNSName = a});

-- | FIXME: Undocumented member.
clbrsStatus :: Lens' CreateLoadBalancerResponse Int
clbrsStatus = lens _clbrsStatus (\ s a -> s{_clbrsStatus = a});
