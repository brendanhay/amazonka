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
-- Module      : Network.AWS.ELB.DetachLoadBalancerFromSubnets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified subnets from the set of configured subnets for the
-- load balancer.
--
-- After a subnet is removed, all EC2 instances registered with the load
-- balancer in the removed subnet go into the 'OutOfService' state. Then,
-- the load balancer balances the traffic among the remaining routable
-- subnets.
module Network.AWS.ELB.DetachLoadBalancerFromSubnets
    (
    -- * Creating a Request
      detachLoadBalancerFromSubnets
    , DetachLoadBalancerFromSubnets
    -- * Request Lenses
    , dlbfsLoadBalancerName
    , dlbfsSubnets

    -- * Destructuring the Response
    , detachLoadBalancerFromSubnetsResponse
    , DetachLoadBalancerFromSubnetsResponse
    -- * Response Lenses
    , dlbfsrsSubnets
    , dlbfsrsResponseStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachLoadBalancerFromSubnets' smart constructor.
data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets'
    { _dlbfsLoadBalancerName :: !Text
    , _dlbfsSubnets          :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachLoadBalancerFromSubnets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbfsLoadBalancerName'
--
-- * 'dlbfsSubnets'
detachLoadBalancerFromSubnets
    :: Text -- ^ 'dlbfsLoadBalancerName'
    -> DetachLoadBalancerFromSubnets
detachLoadBalancerFromSubnets pLoadBalancerName_ =
    DetachLoadBalancerFromSubnets'
    { _dlbfsLoadBalancerName = pLoadBalancerName_
    , _dlbfsSubnets = mempty
    }

-- | The name of the load balancer.
dlbfsLoadBalancerName :: Lens' DetachLoadBalancerFromSubnets Text
dlbfsLoadBalancerName = lens _dlbfsLoadBalancerName (\ s a -> s{_dlbfsLoadBalancerName = a});

-- | The IDs of the subnets.
dlbfsSubnets :: Lens' DetachLoadBalancerFromSubnets [Text]
dlbfsSubnets = lens _dlbfsSubnets (\ s a -> s{_dlbfsSubnets = a}) . _Coerce;

instance AWSRequest DetachLoadBalancerFromSubnets
         where
        type Rs DetachLoadBalancerFromSubnets =
             DetachLoadBalancerFromSubnetsResponse
        request = postQuery eLB
        response
          = receiveXMLWrapper
              "DetachLoadBalancerFromSubnetsResult"
              (\ s h x ->
                 DetachLoadBalancerFromSubnetsResponse' <$>
                   (x .@? "Subnets" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DetachLoadBalancerFromSubnets
         where
        toHeaders = const mempty

instance ToPath DetachLoadBalancerFromSubnets where
        toPath = const "/"

instance ToQuery DetachLoadBalancerFromSubnets where
        toQuery DetachLoadBalancerFromSubnets'{..}
          = mconcat
              ["Action" =:
                 ("DetachLoadBalancerFromSubnets" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _dlbfsLoadBalancerName,
               "Subnets" =: toQueryList "member" _dlbfsSubnets]

-- | /See:/ 'detachLoadBalancerFromSubnetsResponse' smart constructor.
data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse'
    { _dlbfsrsSubnets        :: !(Maybe [Text])
    , _dlbfsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachLoadBalancerFromSubnetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbfsrsSubnets'
--
-- * 'dlbfsrsResponseStatus'
detachLoadBalancerFromSubnetsResponse
    :: Int -- ^ 'dlbfsrsResponseStatus'
    -> DetachLoadBalancerFromSubnetsResponse
detachLoadBalancerFromSubnetsResponse pResponseStatus_ =
    DetachLoadBalancerFromSubnetsResponse'
    { _dlbfsrsSubnets = Nothing
    , _dlbfsrsResponseStatus = pResponseStatus_
    }

-- | The IDs of the remaining subnets for the load balancer.
dlbfsrsSubnets :: Lens' DetachLoadBalancerFromSubnetsResponse [Text]
dlbfsrsSubnets = lens _dlbfsrsSubnets (\ s a -> s{_dlbfsrsSubnets = a}) . _Default . _Coerce;

-- | The response status code.
dlbfsrsResponseStatus :: Lens' DetachLoadBalancerFromSubnetsResponse Int
dlbfsrsResponseStatus = lens _dlbfsrsResponseStatus (\ s a -> s{_dlbfsrsResponseStatus = a});
