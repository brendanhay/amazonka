{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateLoadBalancer
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
module Network.AWS.ELB.CreateLoadBalancer
    (
    -- * Creating a Request
      createLoadBalancer
    , CreateLoadBalancer
    -- * Request Lenses
    , clbSecurityGroups
    , clbSubnets
    , clbAvailabilityZones
    , clbScheme
    , clbTags
    , clbLoadBalancerName
    , clbListeners

    -- * Destructuring the Response
    , createLoadBalancerResponse
    , CreateLoadBalancerResponse
    -- * Response Lenses
    , clbrsDNSName
    , clbrsResponseStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
    { _clbSecurityGroups    :: !(Maybe [Text])
    , _clbSubnets           :: !(Maybe [Text])
    , _clbAvailabilityZones :: !(Maybe [Text])
    , _clbScheme            :: !(Maybe Text)
    , _clbTags              :: !(Maybe (List1 Tag))
    , _clbLoadBalancerName  :: !Text
    , _clbListeners         :: ![Listener]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbSecurityGroups'
--
-- * 'clbSubnets'
--
-- * 'clbAvailabilityZones'
--
-- * 'clbScheme'
--
-- * 'clbTags'
--
-- * 'clbLoadBalancerName'
--
-- * 'clbListeners'
createLoadBalancer
    :: Text -- ^ 'clbLoadBalancerName'
    -> CreateLoadBalancer
createLoadBalancer pLoadBalancerName_ =
    CreateLoadBalancer'
    { _clbSecurityGroups = Nothing
    , _clbSubnets = Nothing
    , _clbAvailabilityZones = Nothing
    , _clbScheme = Nothing
    , _clbTags = Nothing
    , _clbLoadBalancerName = pLoadBalancerName_
    , _clbListeners = mempty
    }

-- | The IDs of the security groups to assign to the load balancer.
clbSecurityGroups :: Lens' CreateLoadBalancer [Text]
clbSecurityGroups = lens _clbSecurityGroups (\ s a -> s{_clbSecurityGroups = a}) . _Default . _Coerce;

-- | The IDs of the subnets in your VPC to attach to the load balancer.
-- Specify one subnet per Availability Zone specified in
-- 'AvailabilityZones'.
clbSubnets :: Lens' CreateLoadBalancer [Text]
clbSubnets = lens _clbSubnets (\ s a -> s{_clbSubnets = a}) . _Default . _Coerce;

-- | One or more Availability Zones from the same region as the load
-- balancer. Traffic is equally distributed across all specified
-- Availability Zones.
--
-- You must specify at least one Availability Zone.
--
-- You can add more Availability Zones after you create the load balancer
-- using < EnableAvailabilityZonesForLoadBalancer>.
clbAvailabilityZones :: Lens' CreateLoadBalancer [Text]
clbAvailabilityZones = lens _clbAvailabilityZones (\ s a -> s{_clbAvailabilityZones = a}) . _Default . _Coerce;

-- | The type of a load balancer. Valid only for load balancers in a VPC.
--
-- By default, Elastic Load Balancing creates an Internet-facing load
-- balancer with a publicly resolvable DNS name, which resolves to public
-- IP addresses. For more information about Internet-facing and Internal
-- load balancers, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/vpc-loadbalancer-types.html Internet-facing and Internal Load Balancers>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- Specify 'internal' to create an internal load balancer with a DNS name
-- that resolves to private IP addresses.
clbScheme :: Lens' CreateLoadBalancer (Maybe Text)
clbScheme = lens _clbScheme (\ s a -> s{_clbScheme = a});

-- | A list of tags to assign to the load balancer.
--
-- For more information about tagging your load balancer, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#tagging-elb Tagging>
-- in the /Elastic Load Balancing Developer Guide/.
clbTags :: Lens' CreateLoadBalancer (Maybe (NonEmpty Tag))
clbTags = lens _clbTags (\ s a -> s{_clbTags = a}) . mapping _List1;

-- | The name of the load balancer.
--
-- This name must be unique within your set of load balancers for the
-- region, must have a maximum of 32 characters, must contain only
-- alphanumeric characters or hyphens, and cannot begin or end with a
-- hyphen.
clbLoadBalancerName :: Lens' CreateLoadBalancer Text
clbLoadBalancerName = lens _clbLoadBalancerName (\ s a -> s{_clbLoadBalancerName = a});

-- | The listeners.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-listener-config.html Listeners for Your Load Balancer>
-- in the /Elastic Load Balancing Developer Guide/.
clbListeners :: Lens' CreateLoadBalancer [Listener]
clbListeners = lens _clbListeners (\ s a -> s{_clbListeners = a}) . _Coerce;

instance AWSRequest CreateLoadBalancer where
        type Rs CreateLoadBalancer =
             CreateLoadBalancerResponse
        request = postQuery elb
        response
          = receiveXMLWrapper "CreateLoadBalancerResult"
              (\ s h x ->
                 CreateLoadBalancerResponse' <$>
                   (x .@? "DNSName") <*> (pure (fromEnum s)))

instance Hashable CreateLoadBalancer

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
                   (toQueryList "member" <$> _clbSecurityGroups),
               "Subnets" =:
                 toQuery (toQueryList "member" <$> _clbSubnets),
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "member" <$> _clbAvailabilityZones),
               "Scheme" =: _clbScheme,
               "Tags" =:
                 toQuery (toQueryList "member" <$> _clbTags),
               "LoadBalancerName" =: _clbLoadBalancerName,
               "Listeners" =: toQueryList "member" _clbListeners]

-- | /See:/ 'createLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
    { _clbrsDNSName        :: !(Maybe Text)
    , _clbrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbrsDNSName'
--
-- * 'clbrsResponseStatus'
createLoadBalancerResponse
    :: Int -- ^ 'clbrsResponseStatus'
    -> CreateLoadBalancerResponse
createLoadBalancerResponse pResponseStatus_ =
    CreateLoadBalancerResponse'
    { _clbrsDNSName = Nothing
    , _clbrsResponseStatus = pResponseStatus_
    }

-- | The DNS name of the load balancer.
clbrsDNSName :: Lens' CreateLoadBalancerResponse (Maybe Text)
clbrsDNSName = lens _clbrsDNSName (\ s a -> s{_clbrsDNSName = a});

-- | The response status code.
clbrsResponseStatus :: Lens' CreateLoadBalancerResponse Int
clbrsResponseStatus = lens _clbrsResponseStatus (\ s a -> s{_clbrsResponseStatus = a});
