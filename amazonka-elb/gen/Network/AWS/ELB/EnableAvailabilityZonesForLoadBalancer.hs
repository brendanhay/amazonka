{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_EnableAvailabilityZonesForLoadBalancer.html>
module Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
    (
    -- * Request
      EnableAvailabilityZonesForLoadBalancer
    -- ** Request constructor
    , enableAvailabilityZonesForLoadBalancer
    -- ** Request lenses
    , eazflbrqLoadBalancerName
    , eazflbrqAvailabilityZones

    -- * Response
    , EnableAvailabilityZonesForLoadBalancerResponse
    -- ** Response constructor
    , enableAvailabilityZonesForLoadBalancerResponse
    -- ** Response lenses
    , eazflbrsAvailabilityZones
    , eazflbrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableAvailabilityZonesForLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eazflbrqLoadBalancerName'
--
-- * 'eazflbrqAvailabilityZones'
data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer'
    { _eazflbrqLoadBalancerName  :: !Text
    , _eazflbrqAvailabilityZones :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableAvailabilityZonesForLoadBalancer' smart constructor.
enableAvailabilityZonesForLoadBalancer :: Text -> EnableAvailabilityZonesForLoadBalancer
enableAvailabilityZonesForLoadBalancer pLoadBalancerName_ =
    EnableAvailabilityZonesForLoadBalancer'
    { _eazflbrqLoadBalancerName = pLoadBalancerName_
    , _eazflbrqAvailabilityZones = mempty
    }

-- | The name of the load balancer.
eazflbrqLoadBalancerName :: Lens' EnableAvailabilityZonesForLoadBalancer Text
eazflbrqLoadBalancerName = lens _eazflbrqLoadBalancerName (\ s a -> s{_eazflbrqLoadBalancerName = a});

-- | The Availability Zones. These must be in the same region as the load
-- balancer.
eazflbrqAvailabilityZones :: Lens' EnableAvailabilityZonesForLoadBalancer [Text]
eazflbrqAvailabilityZones = lens _eazflbrqAvailabilityZones (\ s a -> s{_eazflbrqAvailabilityZones = a});

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
               "LoadBalancerName" =: _eazflbrqLoadBalancerName,
               "AvailabilityZones" =:
                 toQueryList "member" _eazflbrqAvailabilityZones]

-- | /See:/ 'enableAvailabilityZonesForLoadBalancerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eazflbrsAvailabilityZones'
--
-- * 'eazflbrsStatus'
data EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse'
    { _eazflbrsAvailabilityZones :: !(Maybe [Text])
    , _eazflbrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableAvailabilityZonesForLoadBalancerResponse' smart constructor.
enableAvailabilityZonesForLoadBalancerResponse :: Int -> EnableAvailabilityZonesForLoadBalancerResponse
enableAvailabilityZonesForLoadBalancerResponse pStatus_ =
    EnableAvailabilityZonesForLoadBalancerResponse'
    { _eazflbrsAvailabilityZones = Nothing
    , _eazflbrsStatus = pStatus_
    }

-- | The updated list of Availability Zones for the load balancer.
eazflbrsAvailabilityZones :: Lens' EnableAvailabilityZonesForLoadBalancerResponse [Text]
eazflbrsAvailabilityZones = lens _eazflbrsAvailabilityZones (\ s a -> s{_eazflbrsAvailabilityZones = a}) . _Default;

-- | FIXME: Undocumented member.
eazflbrsStatus :: Lens' EnableAvailabilityZonesForLoadBalancerResponse Int
eazflbrsStatus = lens _eazflbrsStatus (\ s a -> s{_eazflbrsStatus = a});
