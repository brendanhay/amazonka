{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds the specified Availability Zones to the set of Availability Zones
-- for the specified load balancer.
--
-- The load balancer evenly distributes requests across all its registered
-- Availability Zones that contain instances.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/US_AddLBAvailabilityZone.html Add Availability Zone>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_EnableAvailabilityZonesForLoadBalancer.html>
module Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
    (
    -- * Request
      EnableAvailabilityZonesForLoadBalancer
    -- ** Request constructor
    , enableAvailabilityZonesForLoadBalancer
    -- ** Request lenses
    , eazflbLoadBalancerName
    , eazflbAvailabilityZones

    -- * Response
    , EnableAvailabilityZonesForLoadBalancerResponse
    -- ** Response constructor
    , enableAvailabilityZonesForLoadBalancerResponse
    -- ** Response lenses
    , eazflbrAvailabilityZones
    , eazflbrStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableAvailabilityZonesForLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eazflbLoadBalancerName'
--
-- * 'eazflbAvailabilityZones'
data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer'
    { _eazflbLoadBalancerName  :: !Text
    , _eazflbAvailabilityZones :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableAvailabilityZonesForLoadBalancer' smart constructor.
enableAvailabilityZonesForLoadBalancer :: Text -> EnableAvailabilityZonesForLoadBalancer
enableAvailabilityZonesForLoadBalancer pLoadBalancerName =
    EnableAvailabilityZonesForLoadBalancer'
    { _eazflbLoadBalancerName = pLoadBalancerName
    , _eazflbAvailabilityZones = mempty
    }

-- | The name of the load balancer.
eazflbLoadBalancerName :: Lens' EnableAvailabilityZonesForLoadBalancer Text
eazflbLoadBalancerName = lens _eazflbLoadBalancerName (\ s a -> s{_eazflbLoadBalancerName = a});

-- | The Availability Zones. These must be in the same region as the load
-- balancer.
eazflbAvailabilityZones :: Lens' EnableAvailabilityZonesForLoadBalancer [Text]
eazflbAvailabilityZones = lens _eazflbAvailabilityZones (\ s a -> s{_eazflbAvailabilityZones = a});

instance AWSRequest
         EnableAvailabilityZonesForLoadBalancer where
        type Sv EnableAvailabilityZonesForLoadBalancer = ELB
        type Rs EnableAvailabilityZonesForLoadBalancer =
             EnableAvailabilityZonesForLoadBalancerResponse
        request = post
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eazflbrAvailabilityZones'
--
-- * 'eazflbrStatus'
data EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse'
    { _eazflbrAvailabilityZones :: !(Maybe [Text])
    , _eazflbrStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableAvailabilityZonesForLoadBalancerResponse' smart constructor.
enableAvailabilityZonesForLoadBalancerResponse :: Int -> EnableAvailabilityZonesForLoadBalancerResponse
enableAvailabilityZonesForLoadBalancerResponse pStatus =
    EnableAvailabilityZonesForLoadBalancerResponse'
    { _eazflbrAvailabilityZones = Nothing
    , _eazflbrStatus = pStatus
    }

-- | The updated list of Availability Zones for the load balancer.
eazflbrAvailabilityZones :: Lens' EnableAvailabilityZonesForLoadBalancerResponse [Text]
eazflbrAvailabilityZones = lens _eazflbrAvailabilityZones (\ s a -> s{_eazflbrAvailabilityZones = a}) . _Default;

-- | FIXME: Undocumented member.
eazflbrStatus :: Lens' EnableAvailabilityZonesForLoadBalancerResponse Int
eazflbrStatus = lens _eazflbrStatus (\ s a -> s{_eazflbrStatus = a});
