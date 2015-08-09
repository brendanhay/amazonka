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
-- balancer in the removed subnet go into the @OutOfService@ state. Then,
-- the load balancer balances the traffic among the remaining routable
-- subnets.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DetachLoadBalancerFromSubnets.html AWS API Reference> for DetachLoadBalancerFromSubnets.
module Network.AWS.ELB.DetachLoadBalancerFromSubnets
    (
    -- * Creating a Request
      DetachLoadBalancerFromSubnets
    , detachLoadBalancerFromSubnets
    -- * Request Lenses
    , dlbfsLoadBalancerName
    , dlbfsSubnets

    -- * Destructuring the Response
    , DetachLoadBalancerFromSubnetsResponse
    , detachLoadBalancerFromSubnetsResponse
    -- * Response Lenses
    , dlbfsrsSubnets
    , dlbfsrsStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachLoadBalancerFromSubnets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbfsLoadBalancerName'
--
-- * 'dlbfsSubnets'
data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets'
    { _dlbfsLoadBalancerName :: !Text
    , _dlbfsSubnets :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachLoadBalancerFromSubnets' smart constructor.
detachLoadBalancerFromSubnets :: Text -> DetachLoadBalancerFromSubnets
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
        type Sv DetachLoadBalancerFromSubnets = ELB
        type Rs DetachLoadBalancerFromSubnets =
             DetachLoadBalancerFromSubnetsResponse
        request = postQuery
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbfsrsSubnets'
--
-- * 'dlbfsrsStatus'
data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse'
    { _dlbfsrsSubnets :: !(Maybe [Text])
    , _dlbfsrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachLoadBalancerFromSubnetsResponse' smart constructor.
detachLoadBalancerFromSubnetsResponse :: Int -> DetachLoadBalancerFromSubnetsResponse
detachLoadBalancerFromSubnetsResponse pStatus_ = 
    DetachLoadBalancerFromSubnetsResponse'
    { _dlbfsrsSubnets = Nothing
    , _dlbfsrsStatus = pStatus_
    }

-- | The IDs of the remaining subnets for the load balancer.
dlbfsrsSubnets :: Lens' DetachLoadBalancerFromSubnetsResponse [Text]
dlbfsrsSubnets = lens _dlbfsrsSubnets (\ s a -> s{_dlbfsrsSubnets = a}) . _Default . _Coerce;

-- | Undocumented member.
dlbfsrsStatus :: Lens' DetachLoadBalancerFromSubnetsResponse Int
dlbfsrsStatus = lens _dlbfsrsStatus (\ s a -> s{_dlbfsrsStatus = a});
