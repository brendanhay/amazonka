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
-- Module      : Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified Availability Zones from the set of Availability Zones for the specified load balancer.
--
--
-- There must be at least one Availability Zone registered with a load balancer at all times. After an Availability Zone is removed, all instances registered with the load balancer that are in the removed Availability Zone go into the @OutOfService@ state. Then, the load balancer attempts to equally balance the traffic among its remaining Availability Zones.
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html Add or Remove Availability Zones> in the /Classic Load Balancer Guide/ .
--
module Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
    (
    -- * Creating a Request
      disableAvailabilityZonesForLoadBalancer
    , DisableAvailabilityZonesForLoadBalancer
    -- * Request Lenses
    , dazflbLoadBalancerName
    , dazflbAvailabilityZones

    -- * Destructuring the Response
    , disableAvailabilityZonesForLoadBalancerResponse
    , DisableAvailabilityZonesForLoadBalancerResponse
    -- * Response Lenses
    , dazflbrsAvailabilityZones
    , dazflbrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DisableAvailabilityZonesForLoadBalancer.
--
--
--
-- /See:/ 'disableAvailabilityZonesForLoadBalancer' smart constructor.
data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer'
  { _dazflbLoadBalancerName  :: !Text
  , _dazflbAvailabilityZones :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableAvailabilityZonesForLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dazflbLoadBalancerName' - The name of the load balancer.
--
-- * 'dazflbAvailabilityZones' - The Availability Zones.
disableAvailabilityZonesForLoadBalancer
    :: Text -- ^ 'dazflbLoadBalancerName'
    -> DisableAvailabilityZonesForLoadBalancer
disableAvailabilityZonesForLoadBalancer pLoadBalancerName_ =
  DisableAvailabilityZonesForLoadBalancer'
    { _dazflbLoadBalancerName = pLoadBalancerName_
    , _dazflbAvailabilityZones = mempty
    }


-- | The name of the load balancer.
dazflbLoadBalancerName :: Lens' DisableAvailabilityZonesForLoadBalancer Text
dazflbLoadBalancerName = lens _dazflbLoadBalancerName (\ s a -> s{_dazflbLoadBalancerName = a})

-- | The Availability Zones.
dazflbAvailabilityZones :: Lens' DisableAvailabilityZonesForLoadBalancer [Text]
dazflbAvailabilityZones = lens _dazflbAvailabilityZones (\ s a -> s{_dazflbAvailabilityZones = a}) . _Coerce

instance AWSRequest
           DisableAvailabilityZonesForLoadBalancer
         where
        type Rs DisableAvailabilityZonesForLoadBalancer =
             DisableAvailabilityZonesForLoadBalancerResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "DisableAvailabilityZonesForLoadBalancerResult"
              (\ s h x ->
                 DisableAvailabilityZonesForLoadBalancerResponse' <$>
                   (x .@? "AvailabilityZones" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable
           DisableAvailabilityZonesForLoadBalancer
         where

instance NFData
           DisableAvailabilityZonesForLoadBalancer
         where

instance ToHeaders
           DisableAvailabilityZonesForLoadBalancer
         where
        toHeaders = const mempty

instance ToPath
           DisableAvailabilityZonesForLoadBalancer
         where
        toPath = const "/"

instance ToQuery
           DisableAvailabilityZonesForLoadBalancer
         where
        toQuery DisableAvailabilityZonesForLoadBalancer'{..}
          = mconcat
              ["Action" =:
                 ("DisableAvailabilityZonesForLoadBalancer" ::
                    ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _dazflbLoadBalancerName,
               "AvailabilityZones" =:
                 toQueryList "member" _dazflbAvailabilityZones]

-- | Contains the output for DisableAvailabilityZonesForLoadBalancer.
--
--
--
-- /See:/ 'disableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse'
  { _dazflbrsAvailabilityZones :: !(Maybe [Text])
  , _dazflbrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableAvailabilityZonesForLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dazflbrsAvailabilityZones' - The remaining Availability Zones for the load balancer.
--
-- * 'dazflbrsResponseStatus' - -- | The response status code.
disableAvailabilityZonesForLoadBalancerResponse
    :: Int -- ^ 'dazflbrsResponseStatus'
    -> DisableAvailabilityZonesForLoadBalancerResponse
disableAvailabilityZonesForLoadBalancerResponse pResponseStatus_ =
  DisableAvailabilityZonesForLoadBalancerResponse'
    { _dazflbrsAvailabilityZones = Nothing
    , _dazflbrsResponseStatus = pResponseStatus_
    }


-- | The remaining Availability Zones for the load balancer.
dazflbrsAvailabilityZones :: Lens' DisableAvailabilityZonesForLoadBalancerResponse [Text]
dazflbrsAvailabilityZones = lens _dazflbrsAvailabilityZones (\ s a -> s{_dazflbrsAvailabilityZones = a}) . _Default . _Coerce

-- | -- | The response status code.
dazflbrsResponseStatus :: Lens' DisableAvailabilityZonesForLoadBalancerResponse Int
dazflbrsResponseStatus = lens _dazflbrsResponseStatus (\ s a -> s{_dazflbrsResponseStatus = a})

instance NFData
           DisableAvailabilityZonesForLoadBalancerResponse
         where
