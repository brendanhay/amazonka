{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more security groups with your load balancer in a
-- virtual private cloud (VPC). The specified security groups override the
-- previously associated security groups.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-security-groups.html#elb-vpc-security-groups Security Groups for Load Balancers in a VPC>
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
    , asgtlbrStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'applySecurityGroupsToLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgtlbLoadBalancerName'
--
-- * 'asgtlbSecurityGroups'
data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer'
    { _asgtlbLoadBalancerName :: !Text
    , _asgtlbSecurityGroups   :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ApplySecurityGroupsToLoadBalancer' smart constructor.
applySecurityGroupsToLoadBalancer :: Text -> ApplySecurityGroupsToLoadBalancer
applySecurityGroupsToLoadBalancer pLoadBalancerName =
    ApplySecurityGroupsToLoadBalancer'
    { _asgtlbLoadBalancerName = pLoadBalancerName
    , _asgtlbSecurityGroups = mempty
    }

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
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
--
-- * 'asgtlbrStatus'
data ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse'
    { _asgtlbrSecurityGroups :: !(Maybe [Text])
    , _asgtlbrStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ApplySecurityGroupsToLoadBalancerResponse' smart constructor.
applySecurityGroupsToLoadBalancerResponse :: Int -> ApplySecurityGroupsToLoadBalancerResponse
applySecurityGroupsToLoadBalancerResponse pStatus =
    ApplySecurityGroupsToLoadBalancerResponse'
    { _asgtlbrSecurityGroups = Nothing
    , _asgtlbrStatus = pStatus
    }

-- | The IDs of the security groups associated with the load balancer.
asgtlbrSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancerResponse [Text]
asgtlbrSecurityGroups = lens _asgtlbrSecurityGroups (\ s a -> s{_asgtlbrSecurityGroups = a}) . _Default;

-- | FIXME: Undocumented member.
asgtlbrStatus :: Lens' ApplySecurityGroupsToLoadBalancerResponse Int
asgtlbrStatus = lens _asgtlbrStatus (\ s a -> s{_asgtlbrStatus = a});
