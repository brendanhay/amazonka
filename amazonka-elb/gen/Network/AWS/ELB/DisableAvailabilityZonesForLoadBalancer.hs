{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Removes the specified Availability Zones from the set of Availability
-- Zones for the specified load balancer.
--
-- There must be at least one Availability Zone registered with a load
-- balancer at all times. After an Availability Zone is removed, all
-- instances registered with the load balancer that are in the removed
-- Availability Zone go into the @OutOfService@ state. Then, the load
-- balancer attempts to equally balance the traffic among its remaining
-- Availability Zones.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/US_ShrinkLBApp04.html Disable an Availability Zone from a Load-Balanced Application>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DisableAvailabilityZonesForLoadBalancer.html>
module Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
    (
    -- * Request
      DisableAvailabilityZonesForLoadBalancer
    -- ** Request constructor
    , disableAvailabilityZonesForLoadBalancer
    -- ** Request lenses
    , dazflbLoadBalancerName
    , dazflbAvailabilityZones

    -- * Response
    , DisableAvailabilityZonesForLoadBalancerResponse
    -- ** Response constructor
    , disableAvailabilityZonesForLoadBalancerResponse
    -- ** Response lenses
    , dazflbrAvailabilityZones
    , dazflbrStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disableAvailabilityZonesForLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazflbLoadBalancerName'
--
-- * 'dazflbAvailabilityZones'
data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer'
    { _dazflbLoadBalancerName  :: !Text
    , _dazflbAvailabilityZones :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableAvailabilityZonesForLoadBalancer' smart constructor.
disableAvailabilityZonesForLoadBalancer :: Text -> DisableAvailabilityZonesForLoadBalancer
disableAvailabilityZonesForLoadBalancer pLoadBalancerName =
    DisableAvailabilityZonesForLoadBalancer'
    { _dazflbLoadBalancerName = pLoadBalancerName
    , _dazflbAvailabilityZones = mempty
    }

-- | The name of the load balancer.
dazflbLoadBalancerName :: Lens' DisableAvailabilityZonesForLoadBalancer Text
dazflbLoadBalancerName = lens _dazflbLoadBalancerName (\ s a -> s{_dazflbLoadBalancerName = a});

-- | The Availability Zones.
dazflbAvailabilityZones :: Lens' DisableAvailabilityZonesForLoadBalancer [Text]
dazflbAvailabilityZones = lens _dazflbAvailabilityZones (\ s a -> s{_dazflbAvailabilityZones = a});

instance AWSRequest
         DisableAvailabilityZonesForLoadBalancer where
        type Sv DisableAvailabilityZonesForLoadBalancer = ELB
        type Rs DisableAvailabilityZonesForLoadBalancer =
             DisableAvailabilityZonesForLoadBalancerResponse
        request = post
        response
          = receiveXMLWrapper
              "DisableAvailabilityZonesForLoadBalancerResult"
              (\ s h x ->
                 DisableAvailabilityZonesForLoadBalancerResponse' <$>
                   (x .@? "AvailabilityZones" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders
         DisableAvailabilityZonesForLoadBalancer where
        toHeaders = const mempty

instance ToPath
         DisableAvailabilityZonesForLoadBalancer where
        toPath = const "/"

instance ToQuery
         DisableAvailabilityZonesForLoadBalancer where
        toQuery DisableAvailabilityZonesForLoadBalancer'{..}
          = mconcat
              ["Action" =:
                 ("DisableAvailabilityZonesForLoadBalancer" ::
                    ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _dazflbLoadBalancerName,
               "AvailabilityZones" =:
                 toQueryList "member" _dazflbAvailabilityZones]

-- | /See:/ 'disableAvailabilityZonesForLoadBalancerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazflbrAvailabilityZones'
--
-- * 'dazflbrStatus'
data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse'
    { _dazflbrAvailabilityZones :: !(Maybe [Text])
    , _dazflbrStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableAvailabilityZonesForLoadBalancerResponse' smart constructor.
disableAvailabilityZonesForLoadBalancerResponse :: Int -> DisableAvailabilityZonesForLoadBalancerResponse
disableAvailabilityZonesForLoadBalancerResponse pStatus =
    DisableAvailabilityZonesForLoadBalancerResponse'
    { _dazflbrAvailabilityZones = Nothing
    , _dazflbrStatus = pStatus
    }

-- | The remaining Availability Zones for the load balancer.
dazflbrAvailabilityZones :: Lens' DisableAvailabilityZonesForLoadBalancerResponse [Text]
dazflbrAvailabilityZones = lens _dazflbrAvailabilityZones (\ s a -> s{_dazflbrAvailabilityZones = a}) . _Default;

-- | FIXME: Undocumented member.
dazflbrStatus :: Lens' DisableAvailabilityZonesForLoadBalancerResponse Int
dazflbrStatus = lens _dazflbrStatus (\ s a -> s{_dazflbrStatus = a});
