{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.AttachLoadBalancerToSubnets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more subnets to the set of configured subnets for the
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
    , albtsrqLoadBalancerName
    , albtsrqSubnets

    -- * Response
    , AttachLoadBalancerToSubnetsResponse
    -- ** Response constructor
    , attachLoadBalancerToSubnetsResponse
    -- ** Response lenses
    , albtsrsSubnets
    , albtsrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachLoadBalancerToSubnets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'albtsrqLoadBalancerName'
--
-- * 'albtsrqSubnets'
data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets'
    { _albtsrqLoadBalancerName :: !Text
    , _albtsrqSubnets          :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachLoadBalancerToSubnets' smart constructor.
attachLoadBalancerToSubnets :: Text -> AttachLoadBalancerToSubnets
attachLoadBalancerToSubnets pLoadBalancerName =
    AttachLoadBalancerToSubnets'
    { _albtsrqLoadBalancerName = pLoadBalancerName
    , _albtsrqSubnets = mempty
    }

-- | The name of the load balancer.
albtsrqLoadBalancerName :: Lens' AttachLoadBalancerToSubnets Text
albtsrqLoadBalancerName = lens _albtsrqLoadBalancerName (\ s a -> s{_albtsrqLoadBalancerName = a});

-- | The IDs of the subnets to add for the load balancer. You can add only
-- one subnet per Availability Zone.
albtsrqSubnets :: Lens' AttachLoadBalancerToSubnets [Text]
albtsrqSubnets = lens _albtsrqSubnets (\ s a -> s{_albtsrqSubnets = a});

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
               "LoadBalancerName" =: _albtsrqLoadBalancerName,
               "Subnets" =: toQueryList "member" _albtsrqSubnets]

-- | /See:/ 'attachLoadBalancerToSubnetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'albtsrsSubnets'
--
-- * 'albtsrsStatus'
data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse'
    { _albtsrsSubnets :: !(Maybe [Text])
    , _albtsrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachLoadBalancerToSubnetsResponse' smart constructor.
attachLoadBalancerToSubnetsResponse :: Int -> AttachLoadBalancerToSubnetsResponse
attachLoadBalancerToSubnetsResponse pStatus =
    AttachLoadBalancerToSubnetsResponse'
    { _albtsrsSubnets = Nothing
    , _albtsrsStatus = pStatus
    }

-- | The IDs of the subnets attached to the load balancer.
albtsrsSubnets :: Lens' AttachLoadBalancerToSubnetsResponse [Text]
albtsrsSubnets = lens _albtsrsSubnets (\ s a -> s{_albtsrsSubnets = a}) . _Default;

-- | FIXME: Undocumented member.
albtsrsStatus :: Lens' AttachLoadBalancerToSubnetsResponse Int
albtsrsStatus = lens _albtsrsStatus (\ s a -> s{_albtsrsStatus = a});
