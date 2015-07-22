{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified Availability Zones from the set of Availability
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
    , dazflbrqLoadBalancerName
    , dazflbrqAvailabilityZones

    -- * Response
    , DisableAvailabilityZonesForLoadBalancerResponse
    -- ** Response constructor
    , disableAvailabilityZonesForLoadBalancerResponse
    -- ** Response lenses
    , dazflbrsAvailabilityZones
    , dazflbrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disableAvailabilityZonesForLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazflbrqLoadBalancerName'
--
-- * 'dazflbrqAvailabilityZones'
data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer'
    { _dazflbrqLoadBalancerName  :: !Text
    , _dazflbrqAvailabilityZones :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableAvailabilityZonesForLoadBalancer' smart constructor.
disableAvailabilityZonesForLoadBalancer :: Text -> DisableAvailabilityZonesForLoadBalancer
disableAvailabilityZonesForLoadBalancer pLoadBalancerName =
    DisableAvailabilityZonesForLoadBalancer'
    { _dazflbrqLoadBalancerName = pLoadBalancerName
    , _dazflbrqAvailabilityZones = mempty
    }

-- | The name of the load balancer.
dazflbrqLoadBalancerName :: Lens' DisableAvailabilityZonesForLoadBalancer Text
dazflbrqLoadBalancerName = lens _dazflbrqLoadBalancerName (\ s a -> s{_dazflbrqLoadBalancerName = a});

-- | The Availability Zones.
dazflbrqAvailabilityZones :: Lens' DisableAvailabilityZonesForLoadBalancer [Text]
dazflbrqAvailabilityZones = lens _dazflbrqAvailabilityZones (\ s a -> s{_dazflbrqAvailabilityZones = a});

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
               "LoadBalancerName" =: _dazflbrqLoadBalancerName,
               "AvailabilityZones" =:
                 toQueryList "member" _dazflbrqAvailabilityZones]

-- | /See:/ 'disableAvailabilityZonesForLoadBalancerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazflbrsAvailabilityZones'
--
-- * 'dazflbrsStatus'
data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse'
    { _dazflbrsAvailabilityZones :: !(Maybe [Text])
    , _dazflbrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableAvailabilityZonesForLoadBalancerResponse' smart constructor.
disableAvailabilityZonesForLoadBalancerResponse :: Int -> DisableAvailabilityZonesForLoadBalancerResponse
disableAvailabilityZonesForLoadBalancerResponse pStatus =
    DisableAvailabilityZonesForLoadBalancerResponse'
    { _dazflbrsAvailabilityZones = Nothing
    , _dazflbrsStatus = pStatus
    }

-- | The remaining Availability Zones for the load balancer.
dazflbrsAvailabilityZones :: Lens' DisableAvailabilityZonesForLoadBalancerResponse [Text]
dazflbrsAvailabilityZones = lens _dazflbrsAvailabilityZones (\ s a -> s{_dazflbrsAvailabilityZones = a}) . _Default;

-- | FIXME: Undocumented member.
dazflbrsStatus :: Lens' DisableAvailabilityZonesForLoadBalancerResponse Int
dazflbrsStatus = lens _dazflbrsStatus (\ s a -> s{_dazflbrsStatus = a});
