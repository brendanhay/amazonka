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
-- Module      : Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified Availability Zones to the set of Availability Zones
-- for the specified load balancer.
--
-- The load balancer evenly distributes requests across all its registered
-- Availability Zones that contain instances.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/US_AddLBAvailabilityZone.html Add Availability Zone>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_EnableAvailabilityZonesForLoadBalancer.html AWS API Reference> for EnableAvailabilityZonesForLoadBalancer.
module Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
    (
    -- * Creating a Request
      enableAvailabilityZonesForLoadBalancer
    , EnableAvailabilityZonesForLoadBalancer
    -- * Request Lenses
    , eazflbLoadBalancerName
    , eazflbAvailabilityZones

    -- * Destructuring the Response
    , enableAvailabilityZonesForLoadBalancerResponse
    , EnableAvailabilityZonesForLoadBalancerResponse
    -- * Response Lenses
    , eazflbrsAvailabilityZones
    , eazflbrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableAvailabilityZonesForLoadBalancer' smart constructor.
data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer'
    { _eazflbLoadBalancerName  :: !Text
    , _eazflbAvailabilityZones :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableAvailabilityZonesForLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eazflbLoadBalancerName'
--
-- * 'eazflbAvailabilityZones'
enableAvailabilityZonesForLoadBalancer
    :: Text -- ^ 'eazflbLoadBalancerName'
    -> EnableAvailabilityZonesForLoadBalancer
enableAvailabilityZonesForLoadBalancer pLoadBalancerName_ =
    EnableAvailabilityZonesForLoadBalancer'
    { _eazflbLoadBalancerName = pLoadBalancerName_
    , _eazflbAvailabilityZones = mempty
    }

-- | The name of the load balancer.
eazflbLoadBalancerName :: Lens' EnableAvailabilityZonesForLoadBalancer Text
eazflbLoadBalancerName = lens _eazflbLoadBalancerName (\ s a -> s{_eazflbLoadBalancerName = a});

-- | The Availability Zones. These must be in the same region as the load
-- balancer.
eazflbAvailabilityZones :: Lens' EnableAvailabilityZonesForLoadBalancer [Text]
eazflbAvailabilityZones = lens _eazflbAvailabilityZones (\ s a -> s{_eazflbAvailabilityZones = a}) . _Coerce;

instance AWSRequest
         EnableAvailabilityZonesForLoadBalancer where
        type Rs EnableAvailabilityZonesForLoadBalancer =
             EnableAvailabilityZonesForLoadBalancerResponse
        request = postQuery eLB
        response
          = receiveXMLWrapper
              "EnableAvailabilityZonesForLoadBalancerResult"
              (\ s h x ->
                 EnableAvailabilityZonesForLoadBalancerResponse' <$>
                   (x .@? "AvailabilityZones" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders
         EnableAvailabilityZonesForLoadBalancer where
        toHeaders = const mempty

instance ToPath
         EnableAvailabilityZonesForLoadBalancer where
        toPath = const "/"

instance ToQuery
         EnableAvailabilityZonesForLoadBalancer where
        toQuery EnableAvailabilityZonesForLoadBalancer'{..}
          = mconcat
              ["Action" =:
                 ("EnableAvailabilityZonesForLoadBalancer" ::
                    ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _eazflbLoadBalancerName,
               "AvailabilityZones" =:
                 toQueryList "member" _eazflbAvailabilityZones]

-- | /See:/ 'enableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse'
    { _eazflbrsAvailabilityZones :: !(Maybe [Text])
    , _eazflbrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableAvailabilityZonesForLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eazflbrsAvailabilityZones'
--
-- * 'eazflbrsStatus'
enableAvailabilityZonesForLoadBalancerResponse
    :: Int -- ^ 'eazflbrsStatus'
    -> EnableAvailabilityZonesForLoadBalancerResponse
enableAvailabilityZonesForLoadBalancerResponse pStatus_ =
    EnableAvailabilityZonesForLoadBalancerResponse'
    { _eazflbrsAvailabilityZones = Nothing
    , _eazflbrsStatus = pStatus_
    }

-- | The updated list of Availability Zones for the load balancer.
eazflbrsAvailabilityZones :: Lens' EnableAvailabilityZonesForLoadBalancerResponse [Text]
eazflbrsAvailabilityZones = lens _eazflbrsAvailabilityZones (\ s a -> s{_eazflbrsAvailabilityZones = a}) . _Default . _Coerce;

-- | The response status code.
eazflbrsStatus :: Lens' EnableAvailabilityZonesForLoadBalancerResponse Int
eazflbrsStatus = lens _eazflbrsStatus (\ s a -> s{_eazflbrsStatus = a});
