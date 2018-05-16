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
-- Module      : Network.AWS.ELB.AttachLoadBalancerToSubnets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more subnets to the set of configured subnets for the specified load balancer.
--
--
-- The load balancer evenly distributes requests across all registered subnets. For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-manage-subnets.html Add or Remove Subnets for Your Load Balancer in a VPC> in the /Classic Load Balancer Guide/ .
--
module Network.AWS.ELB.AttachLoadBalancerToSubnets
    (
    -- * Creating a Request
      attachLoadBalancerToSubnets
    , AttachLoadBalancerToSubnets
    -- * Request Lenses
    , albtsLoadBalancerName
    , albtsSubnets

    -- * Destructuring the Response
    , attachLoadBalancerToSubnetsResponse
    , AttachLoadBalancerToSubnetsResponse
    -- * Response Lenses
    , albtsrsSubnets
    , albtsrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for AttachLoaBalancerToSubnets.
--
--
--
-- /See:/ 'attachLoadBalancerToSubnets' smart constructor.
data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets'
  { _albtsLoadBalancerName :: !Text
  , _albtsSubnets          :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachLoadBalancerToSubnets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'albtsLoadBalancerName' - The name of the load balancer.
--
-- * 'albtsSubnets' - The IDs of the subnets to add. You can add only one subnet per Availability Zone.
attachLoadBalancerToSubnets
    :: Text -- ^ 'albtsLoadBalancerName'
    -> AttachLoadBalancerToSubnets
attachLoadBalancerToSubnets pLoadBalancerName_ =
  AttachLoadBalancerToSubnets'
    {_albtsLoadBalancerName = pLoadBalancerName_, _albtsSubnets = mempty}


-- | The name of the load balancer.
albtsLoadBalancerName :: Lens' AttachLoadBalancerToSubnets Text
albtsLoadBalancerName = lens _albtsLoadBalancerName (\ s a -> s{_albtsLoadBalancerName = a})

-- | The IDs of the subnets to add. You can add only one subnet per Availability Zone.
albtsSubnets :: Lens' AttachLoadBalancerToSubnets [Text]
albtsSubnets = lens _albtsSubnets (\ s a -> s{_albtsSubnets = a}) . _Coerce

instance AWSRequest AttachLoadBalancerToSubnets where
        type Rs AttachLoadBalancerToSubnets =
             AttachLoadBalancerToSubnetsResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "AttachLoadBalancerToSubnetsResult"
              (\ s h x ->
                 AttachLoadBalancerToSubnetsResponse' <$>
                   (x .@? "Subnets" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable AttachLoadBalancerToSubnets where

instance NFData AttachLoadBalancerToSubnets where

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

-- | Contains the output of AttachLoadBalancerToSubnets.
--
--
--
-- /See:/ 'attachLoadBalancerToSubnetsResponse' smart constructor.
data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse'
  { _albtsrsSubnets        :: !(Maybe [Text])
  , _albtsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachLoadBalancerToSubnetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'albtsrsSubnets' - The IDs of the subnets attached to the load balancer.
--
-- * 'albtsrsResponseStatus' - -- | The response status code.
attachLoadBalancerToSubnetsResponse
    :: Int -- ^ 'albtsrsResponseStatus'
    -> AttachLoadBalancerToSubnetsResponse
attachLoadBalancerToSubnetsResponse pResponseStatus_ =
  AttachLoadBalancerToSubnetsResponse'
    {_albtsrsSubnets = Nothing, _albtsrsResponseStatus = pResponseStatus_}


-- | The IDs of the subnets attached to the load balancer.
albtsrsSubnets :: Lens' AttachLoadBalancerToSubnetsResponse [Text]
albtsrsSubnets = lens _albtsrsSubnets (\ s a -> s{_albtsrsSubnets = a}) . _Default . _Coerce

-- | -- | The response status code.
albtsrsResponseStatus :: Lens' AttachLoadBalancerToSubnetsResponse Int
albtsrsResponseStatus = lens _albtsrsResponseStatus (\ s a -> s{_albtsrsResponseStatus = a})

instance NFData AttachLoadBalancerToSubnetsResponse
         where
