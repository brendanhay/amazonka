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
-- Module      : Network.AWS.ELBV2.SetSubnets
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the Availability Zone for the specified subnets for the specified load balancer. The specified subnets replace the previously enabled subnets.
module Network.AWS.ELBV2.SetSubnets
    (
    -- * Creating a Request
      setSubnets
    , SetSubnets
    -- * Request Lenses
    , ssLoadBalancerARN
    , ssSubnets

    -- * Destructuring the Response
    , setSubnetsResponse
    , SetSubnetsResponse
    -- * Response Lenses
    , ssrsAvailabilityZones
    , ssrsResponseStatus
    ) where

import           Network.AWS.ELBV2.Types
import           Network.AWS.ELBV2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for SetSubnets.
--
-- /See:/ 'setSubnets' smart constructor.
data SetSubnets = SetSubnets'
    { _ssLoadBalancerARN :: !Text
    , _ssSubnets         :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetSubnets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssLoadBalancerARN'
--
-- * 'ssSubnets'
setSubnets
    :: Text -- ^ 'ssLoadBalancerARN'
    -> SetSubnets
setSubnets pLoadBalancerARN_ =
    SetSubnets'
    { _ssLoadBalancerARN = pLoadBalancerARN_
    , _ssSubnets = mempty
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
ssLoadBalancerARN :: Lens' SetSubnets Text
ssLoadBalancerARN = lens _ssLoadBalancerARN (\ s a -> s{_ssLoadBalancerARN = a});

-- | The IDs of the subnets. You must specify at least two subnets. You can add only one subnet per Availability Zone.
ssSubnets :: Lens' SetSubnets [Text]
ssSubnets = lens _ssSubnets (\ s a -> s{_ssSubnets = a}) . _Coerce;

instance AWSRequest SetSubnets where
        type Rs SetSubnets = SetSubnetsResponse
        request = postQuery elbv2
        response
          = receiveXMLWrapper "SetSubnetsResult"
              (\ s h x ->
                 SetSubnetsResponse' <$>
                   (x .@? "AvailabilityZones" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable SetSubnets

instance NFData SetSubnets

instance ToHeaders SetSubnets where
        toHeaders = const mempty

instance ToPath SetSubnets where
        toPath = const "/"

instance ToQuery SetSubnets where
        toQuery SetSubnets'{..}
          = mconcat
              ["Action" =: ("SetSubnets" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "LoadBalancerArn" =: _ssLoadBalancerARN,
               "Subnets" =: toQueryList "member" _ssSubnets]

-- | Contains the output of SetSubnets.
--
-- /See:/ 'setSubnetsResponse' smart constructor.
data SetSubnetsResponse = SetSubnetsResponse'
    { _ssrsAvailabilityZones :: !(Maybe [AvailabilityZone])
    , _ssrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetSubnetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssrsAvailabilityZones'
--
-- * 'ssrsResponseStatus'
setSubnetsResponse
    :: Int -- ^ 'ssrsResponseStatus'
    -> SetSubnetsResponse
setSubnetsResponse pResponseStatus_ =
    SetSubnetsResponse'
    { _ssrsAvailabilityZones = Nothing
    , _ssrsResponseStatus = pResponseStatus_
    }

-- | Information about the subnet and Availability Zone.
ssrsAvailabilityZones :: Lens' SetSubnetsResponse [AvailabilityZone]
ssrsAvailabilityZones = lens _ssrsAvailabilityZones (\ s a -> s{_ssrsAvailabilityZones = a}) . _Default . _Coerce;

-- | The response status code.
ssrsResponseStatus :: Lens' SetSubnetsResponse Int
ssrsResponseStatus = lens _ssrsResponseStatus (\ s a -> s{_ssrsResponseStatus = a});

instance NFData SetSubnetsResponse
