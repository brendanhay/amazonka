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
-- Module      : Network.AWS.ELBv2.SetSubnets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the Availability Zone for the specified public subnets for the specified Application Load Balancer. The specified subnets replace the previously enabled subnets.
--
--
-- Note that you can't change the subnets for a Network Load Balancer.
--
module Network.AWS.ELBv2.SetSubnets
    (
    -- * Creating a Request
      setSubnets
    , SetSubnets
    -- * Request Lenses
    , ssSubnetMappings
    , ssLoadBalancerARN
    , ssSubnets

    -- * Destructuring the Response
    , setSubnetsResponse
    , SetSubnetsResponse
    -- * Response Lenses
    , ssrsAvailabilityZones
    , ssrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setSubnets' smart constructor.
data SetSubnets = SetSubnets'
  { _ssSubnetMappings  :: !(Maybe [SubnetMapping])
  , _ssLoadBalancerARN :: !Text
  , _ssSubnets         :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetSubnets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssSubnetMappings' - The IDs of the public subnets. You must specify subnets from at least two Availability Zones. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings. You cannot specify Elastic IP addresses for your subnets.
--
-- * 'ssLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'ssSubnets' - The IDs of the public subnets. You must specify subnets from at least two Availability Zones. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
setSubnets
    :: Text -- ^ 'ssLoadBalancerARN'
    -> SetSubnets
setSubnets pLoadBalancerARN_ =
  SetSubnets'
    { _ssSubnetMappings = Nothing
    , _ssLoadBalancerARN = pLoadBalancerARN_
    , _ssSubnets = mempty
    }


-- | The IDs of the public subnets. You must specify subnets from at least two Availability Zones. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings. You cannot specify Elastic IP addresses for your subnets.
ssSubnetMappings :: Lens' SetSubnets [SubnetMapping]
ssSubnetMappings = lens _ssSubnetMappings (\ s a -> s{_ssSubnetMappings = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
ssLoadBalancerARN :: Lens' SetSubnets Text
ssLoadBalancerARN = lens _ssLoadBalancerARN (\ s a -> s{_ssLoadBalancerARN = a})

-- | The IDs of the public subnets. You must specify subnets from at least two Availability Zones. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
ssSubnets :: Lens' SetSubnets [Text]
ssSubnets = lens _ssSubnets (\ s a -> s{_ssSubnets = a}) . _Coerce

instance AWSRequest SetSubnets where
        type Rs SetSubnets = SetSubnetsResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "SetSubnetsResult"
              (\ s h x ->
                 SetSubnetsResponse' <$>
                   (x .@? "AvailabilityZones" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable SetSubnets where

instance NFData SetSubnets where

instance ToHeaders SetSubnets where
        toHeaders = const mempty

instance ToPath SetSubnets where
        toPath = const "/"

instance ToQuery SetSubnets where
        toQuery SetSubnets'{..}
          = mconcat
              ["Action" =: ("SetSubnets" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "SubnetMappings" =:
                 toQuery (toQueryList "member" <$> _ssSubnetMappings),
               "LoadBalancerArn" =: _ssLoadBalancerARN,
               "Subnets" =: toQueryList "member" _ssSubnets]

-- | /See:/ 'setSubnetsResponse' smart constructor.
data SetSubnetsResponse = SetSubnetsResponse'
  { _ssrsAvailabilityZones :: !(Maybe [AvailabilityZone])
  , _ssrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetSubnetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssrsAvailabilityZones' - Information about the subnet and Availability Zone.
--
-- * 'ssrsResponseStatus' - -- | The response status code.
setSubnetsResponse
    :: Int -- ^ 'ssrsResponseStatus'
    -> SetSubnetsResponse
setSubnetsResponse pResponseStatus_ =
  SetSubnetsResponse'
    {_ssrsAvailabilityZones = Nothing, _ssrsResponseStatus = pResponseStatus_}


-- | Information about the subnet and Availability Zone.
ssrsAvailabilityZones :: Lens' SetSubnetsResponse [AvailabilityZone]
ssrsAvailabilityZones = lens _ssrsAvailabilityZones (\ s a -> s{_ssrsAvailabilityZones = a}) . _Default . _Coerce

-- | -- | The response status code.
ssrsResponseStatus :: Lens' SetSubnetsResponse Int
ssrsResponseStatus = lens _ssrsResponseStatus (\ s a -> s{_ssrsResponseStatus = a})

instance NFData SetSubnetsResponse where
