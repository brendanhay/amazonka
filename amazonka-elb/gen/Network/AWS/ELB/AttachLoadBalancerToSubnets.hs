{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.AttachLoadBalancerToSubnets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Adds one or more subnets to the set of configured subnets for the
-- specified load balancer.
--
-- The load balancer evenly distributes requests across all registered
-- subnets. For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-manage-subnets.html Add or Remove Subnets for Your Load Balancer in a VPC>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_AttachLoadBalancerToSubnets.html>
module Network.AWS.ELB.AttachLoadBalancerToSubnets
    (
    -- * Request
      AttachLoadBalancerToSubnets
    -- ** Request constructor
    , attachLoadBalancerToSubnets
    -- ** Request lenses
    , albtsLoadBalancerName
    , albtsSubnets

    -- * Response
    , AttachLoadBalancerToSubnetsResponse
    -- ** Response constructor
    , attachLoadBalancerToSubnetsResponse
    -- ** Response lenses
    , albtsrSubnets
    , albtsrStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachLoadBalancerToSubnets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'albtsLoadBalancerName'
--
-- * 'albtsSubnets'
data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets'
    { _albtsLoadBalancerName :: !Text
    , _albtsSubnets          :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachLoadBalancerToSubnets' smart constructor.
attachLoadBalancerToSubnets :: Text -> AttachLoadBalancerToSubnets
attachLoadBalancerToSubnets pLoadBalancerName =
    AttachLoadBalancerToSubnets'
    { _albtsLoadBalancerName = pLoadBalancerName
    , _albtsSubnets = mempty
    }

-- | The name of the load balancer.
albtsLoadBalancerName :: Lens' AttachLoadBalancerToSubnets Text
albtsLoadBalancerName = lens _albtsLoadBalancerName (\ s a -> s{_albtsLoadBalancerName = a});

-- | The IDs of the subnets to add for the load balancer. You can add only
-- one subnet per Availability Zone.
albtsSubnets :: Lens' AttachLoadBalancerToSubnets [Text]
albtsSubnets = lens _albtsSubnets (\ s a -> s{_albtsSubnets = a});

instance AWSRequest AttachLoadBalancerToSubnets where
        type Sv AttachLoadBalancerToSubnets = ELB
        type Rs AttachLoadBalancerToSubnets =
             AttachLoadBalancerToSubnetsResponse
        request = post
        response
          = receiveXMLWrapper
              "AttachLoadBalancerToSubnetsResult"
              (\ s h x ->
                 AttachLoadBalancerToSubnetsResponse' <$>
                   (x .@? "Subnets" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders AttachLoadBalancerToSubnets where
        toHeaders = const mempty

instance ToPath AttachLoadBalancerToSubnets where
        toPath = const "/"

instance ToQuery AttachLoadBalancerToSubnets where
        toQuery AttachLoadBalancerToSubnets'{..}
          = mconcat
              ["Action" =:
                 ("AttachLoadBalancerToSubnets" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _albtsLoadBalancerName,
               "Subnets" =: toQueryList "member" _albtsSubnets]

-- | /See:/ 'attachLoadBalancerToSubnetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'albtsrSubnets'
--
-- * 'albtsrStatus'
data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse'
    { _albtsrSubnets :: !(Maybe [Text])
    , _albtsrStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachLoadBalancerToSubnetsResponse' smart constructor.
attachLoadBalancerToSubnetsResponse :: Int -> AttachLoadBalancerToSubnetsResponse
attachLoadBalancerToSubnetsResponse pStatus =
    AttachLoadBalancerToSubnetsResponse'
    { _albtsrSubnets = Nothing
    , _albtsrStatus = pStatus
    }

-- | The IDs of the subnets attached to the load balancer.
albtsrSubnets :: Lens' AttachLoadBalancerToSubnetsResponse [Text]
albtsrSubnets = lens _albtsrSubnets (\ s a -> s{_albtsrSubnets = a}) . _Default;

-- | FIXME: Undocumented member.
albtsrStatus :: Lens' AttachLoadBalancerToSubnetsResponse Int
albtsrStatus = lens _albtsrStatus (\ s a -> s{_albtsrStatus = a});
