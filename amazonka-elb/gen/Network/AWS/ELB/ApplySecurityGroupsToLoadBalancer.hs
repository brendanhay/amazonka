{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
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

-- | Associates one or more security groups with your load balancer in a
-- virtual private cloud (VPC). The specified security groups override the
-- previously associated security groups.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/USVPC_ApplySG.html Manage Security Groups for Amazon VPC>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ApplySecurityGroupsToLoadBalancer.html>
module Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
    (
    -- * Request
      ApplySecurityGroupsToLoadBalancer
    -- ** Request constructor
    , applySecurityGroupsToLoadBalancer
    -- ** Request lenses
    , asgtlbLoadBalancerName
    , asgtlbSecurityGroups

    -- * Response
    , ApplySecurityGroupsToLoadBalancerResponse
    -- ** Response constructor
    , applySecurityGroupsToLoadBalancerResponse
    -- ** Response lenses
    , asgtlbrSecurityGroups
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ELB.Types

-- | /See:/ 'applySecurityGroupsToLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgtlbLoadBalancerName'
--
-- * 'asgtlbSecurityGroups'
data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer'{_asgtlbLoadBalancerName :: Text, _asgtlbSecurityGroups :: [Text]} deriving (Eq, Read, Show)

-- | 'ApplySecurityGroupsToLoadBalancer' smart constructor.
applySecurityGroupsToLoadBalancer :: Text -> ApplySecurityGroupsToLoadBalancer
applySecurityGroupsToLoadBalancer pLoadBalancerName = ApplySecurityGroupsToLoadBalancer'{_asgtlbLoadBalancerName = pLoadBalancerName, _asgtlbSecurityGroups = mempty};

-- | The name of the load balancer.
asgtlbLoadBalancerName :: Lens' ApplySecurityGroupsToLoadBalancer Text
asgtlbLoadBalancerName = lens _asgtlbLoadBalancerName (\ s a -> s{_asgtlbLoadBalancerName = a});

-- | The IDs of the security groups to associate with the load balancer. Note
-- that you cannot specify the name of the security group.
asgtlbSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancer [Text]
asgtlbSecurityGroups = lens _asgtlbSecurityGroups (\ s a -> s{_asgtlbSecurityGroups = a});

instance AWSRequest ApplySecurityGroupsToLoadBalancer
         where
        type Sv ApplySecurityGroupsToLoadBalancer = ELB
        type Rs ApplySecurityGroupsToLoadBalancer =
             ApplySecurityGroupsToLoadBalancerResponse
        request = post
        response
          = receiveXMLWrapper
              "ApplySecurityGroupsToLoadBalancerResult"
              (\ s h x ->
                 ApplySecurityGroupsToLoadBalancerResponse' <$>
                   (x .@? "SecurityGroups" .!@ mempty >>=
                      may (parseXMLList "member")))

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

-- | /See:/ 'applySecurityGroupsToLoadBalancerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgtlbrSecurityGroups'
newtype ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse'{_asgtlbrSecurityGroups :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'ApplySecurityGroupsToLoadBalancerResponse' smart constructor.
applySecurityGroupsToLoadBalancerResponse :: ApplySecurityGroupsToLoadBalancerResponse
applySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse'{_asgtlbrSecurityGroups = Nothing};

-- | The IDs of the security groups associated with the load balancer.
asgtlbrSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancerResponse [Text]
asgtlbrSecurityGroups = lens _asgtlbrSecurityGroups (\ s a -> s{_asgtlbrSecurityGroups = a}) . _Default;
