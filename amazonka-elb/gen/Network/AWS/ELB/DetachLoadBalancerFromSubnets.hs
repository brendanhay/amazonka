{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.DetachLoadBalancerFromSubnets
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Removes the specified subnets from the set of configured subnets for the
-- load balancer.
--
-- After a subnet is removed, all EC2 instances registered with the load
-- balancer in the removed subnet go into the @OutOfService@ state. Then,
-- the load balancer balances the traffic among the remaining routable
-- subnets.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DetachLoadBalancerFromSubnets.html>
module Network.AWS.ELB.DetachLoadBalancerFromSubnets
    (
    -- * Request
      DetachLoadBalancerFromSubnets
    -- ** Request constructor
    , detachLoadBalancerFromSubnets
    -- ** Request lenses
    , dlbfsLoadBalancerName
    , dlbfsSubnets

    -- * Response
    , DetachLoadBalancerFromSubnetsResponse
    -- ** Response constructor
    , detachLoadBalancerFromSubnetsResponse
    -- ** Response lenses
    , dlbfsrSubnets
    , dlbfsrStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachLoadBalancerFromSubnets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbfsLoadBalancerName'
--
-- * 'dlbfsSubnets'
data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets'
    { _dlbfsLoadBalancerName :: !Text
    , _dlbfsSubnets          :: ![Text]
    } deriving (Eq,Read,Show)

-- | 'DetachLoadBalancerFromSubnets' smart constructor.
detachLoadBalancerFromSubnets :: Text -> DetachLoadBalancerFromSubnets
detachLoadBalancerFromSubnets pLoadBalancerName =
    DetachLoadBalancerFromSubnets'
    { _dlbfsLoadBalancerName = pLoadBalancerName
    , _dlbfsSubnets = mempty
    }

-- | The name of the load balancer.
dlbfsLoadBalancerName :: Lens' DetachLoadBalancerFromSubnets Text
dlbfsLoadBalancerName = lens _dlbfsLoadBalancerName (\ s a -> s{_dlbfsLoadBalancerName = a});

-- | The IDs of the subnets.
dlbfsSubnets :: Lens' DetachLoadBalancerFromSubnets [Text]
dlbfsSubnets = lens _dlbfsSubnets (\ s a -> s{_dlbfsSubnets = a});

instance AWSRequest DetachLoadBalancerFromSubnets
         where
        type Sv DetachLoadBalancerFromSubnets = ELB
        type Rs DetachLoadBalancerFromSubnets =
             DetachLoadBalancerFromSubnetsResponse
        request = post
        response
          = receiveXMLWrapper
              "DetachLoadBalancerFromSubnetsResult"
              (\ s h x ->
                 DetachLoadBalancerFromSubnetsResponse' <$>
                   (x .@? "Subnets" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure s))

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
-- * 'dlbfsrSubnets'
--
-- * 'dlbfsrStatus'
data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse'
    { _dlbfsrSubnets :: !(Maybe [Text])
    , _dlbfsrStatus  :: !Status
    } deriving (Eq,Show)

-- | 'DetachLoadBalancerFromSubnetsResponse' smart constructor.
detachLoadBalancerFromSubnetsResponse :: Status -> DetachLoadBalancerFromSubnetsResponse
detachLoadBalancerFromSubnetsResponse pStatus =
    DetachLoadBalancerFromSubnetsResponse'
    { _dlbfsrSubnets = Nothing
    , _dlbfsrStatus = pStatus
    }

-- | The IDs of the remaining subnets for the load balancer.
dlbfsrSubnets :: Lens' DetachLoadBalancerFromSubnetsResponse [Text]
dlbfsrSubnets = lens _dlbfsrSubnets (\ s a -> s{_dlbfsrSubnets = a}) . _Default;

-- | FIXME: Undocumented member.
dlbfsrStatus :: Lens' DetachLoadBalancerFromSubnetsResponse Status
dlbfsrStatus = lens _dlbfsrStatus (\ s a -> s{_dlbfsrStatus = a});
