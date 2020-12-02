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
-- Module      : Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more security groups with your load balancer in a virtual private cloud (VPC). The specified security groups override the previously associated security groups.
--
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-security-groups.html#elb-vpc-security-groups Security Groups for Load Balancers in a VPC> in the /Classic Load Balancer Guide/ .
--
module Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
    (
    -- * Creating a Request
      applySecurityGroupsToLoadBalancer
    , ApplySecurityGroupsToLoadBalancer
    -- * Request Lenses
    , asgtlbLoadBalancerName
    , asgtlbSecurityGroups

    -- * Destructuring the Response
    , applySecurityGroupsToLoadBalancerResponse
    , ApplySecurityGroupsToLoadBalancerResponse
    -- * Response Lenses
    , asgtlbrsSecurityGroups
    , asgtlbrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ApplySecurityGroupsToLoadBalancer.
--
--
--
-- /See:/ 'applySecurityGroupsToLoadBalancer' smart constructor.
data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer'
  { _asgtlbLoadBalancerName :: !Text
  , _asgtlbSecurityGroups   :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplySecurityGroupsToLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgtlbLoadBalancerName' - The name of the load balancer.
--
-- * 'asgtlbSecurityGroups' - The IDs of the security groups to associate with the load balancer. Note that you cannot specify the name of the security group.
applySecurityGroupsToLoadBalancer
    :: Text -- ^ 'asgtlbLoadBalancerName'
    -> ApplySecurityGroupsToLoadBalancer
applySecurityGroupsToLoadBalancer pLoadBalancerName_ =
  ApplySecurityGroupsToLoadBalancer'
    { _asgtlbLoadBalancerName = pLoadBalancerName_
    , _asgtlbSecurityGroups = mempty
    }


-- | The name of the load balancer.
asgtlbLoadBalancerName :: Lens' ApplySecurityGroupsToLoadBalancer Text
asgtlbLoadBalancerName = lens _asgtlbLoadBalancerName (\ s a -> s{_asgtlbLoadBalancerName = a})

-- | The IDs of the security groups to associate with the load balancer. Note that you cannot specify the name of the security group.
asgtlbSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancer [Text]
asgtlbSecurityGroups = lens _asgtlbSecurityGroups (\ s a -> s{_asgtlbSecurityGroups = a}) . _Coerce

instance AWSRequest ApplySecurityGroupsToLoadBalancer
         where
        type Rs ApplySecurityGroupsToLoadBalancer =
             ApplySecurityGroupsToLoadBalancerResponse
        request = postQuery elb
        response
          = receiveXMLWrapper
              "ApplySecurityGroupsToLoadBalancerResult"
              (\ s h x ->
                 ApplySecurityGroupsToLoadBalancerResponse' <$>
                   (x .@? "SecurityGroups" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ApplySecurityGroupsToLoadBalancer
         where

instance NFData ApplySecurityGroupsToLoadBalancer
         where

instance ToHeaders ApplySecurityGroupsToLoadBalancer
         where
        toHeaders = const mempty

instance ToPath ApplySecurityGroupsToLoadBalancer
         where
        toPath = const "/"

instance ToQuery ApplySecurityGroupsToLoadBalancer
         where
        toQuery ApplySecurityGroupsToLoadBalancer'{..}
          = mconcat
              ["Action" =:
                 ("ApplySecurityGroupsToLoadBalancer" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _asgtlbLoadBalancerName,
               "SecurityGroups" =:
                 toQueryList "member" _asgtlbSecurityGroups]

-- | Contains the output of ApplySecurityGroupsToLoadBalancer.
--
--
--
-- /See:/ 'applySecurityGroupsToLoadBalancerResponse' smart constructor.
data ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse'
  { _asgtlbrsSecurityGroups :: !(Maybe [Text])
  , _asgtlbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplySecurityGroupsToLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgtlbrsSecurityGroups' - The IDs of the security groups associated with the load balancer.
--
-- * 'asgtlbrsResponseStatus' - -- | The response status code.
applySecurityGroupsToLoadBalancerResponse
    :: Int -- ^ 'asgtlbrsResponseStatus'
    -> ApplySecurityGroupsToLoadBalancerResponse
applySecurityGroupsToLoadBalancerResponse pResponseStatus_ =
  ApplySecurityGroupsToLoadBalancerResponse'
    { _asgtlbrsSecurityGroups = Nothing
    , _asgtlbrsResponseStatus = pResponseStatus_
    }


-- | The IDs of the security groups associated with the load balancer.
asgtlbrsSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancerResponse [Text]
asgtlbrsSecurityGroups = lens _asgtlbrsSecurityGroups (\ s a -> s{_asgtlbrsSecurityGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
asgtlbrsResponseStatus :: Lens' ApplySecurityGroupsToLoadBalancerResponse Int
asgtlbrsResponseStatus = lens _asgtlbrsResponseStatus (\ s a -> s{_asgtlbrsResponseStatus = a})

instance NFData
           ApplySecurityGroupsToLoadBalancerResponse
         where
