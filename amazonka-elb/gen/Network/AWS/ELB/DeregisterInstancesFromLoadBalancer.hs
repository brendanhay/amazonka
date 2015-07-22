{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified instances from the specified load balancer.
-- After the instance is deregistered, it no longer receives traffic from
-- the load balancer.
--
-- You can use DescribeLoadBalancers to verify that the instance is
-- deregistered from the load balancer.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/US_DeReg_Reg_Instances.html Deregister and Register Amazon EC2 Instances>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeregisterInstancesFromLoadBalancer.html>
module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
    (
    -- * Request
      DeregisterInstancesFromLoadBalancer
    -- ** Request constructor
    , deregisterInstancesFromLoadBalancer
    -- ** Request lenses
    , diflbrqLoadBalancerName
    , diflbrqInstances

    -- * Response
    , DeregisterInstancesFromLoadBalancerResponse
    -- ** Response constructor
    , deregisterInstancesFromLoadBalancerResponse
    -- ** Response lenses
    , diflbrsInstances
    , diflbrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterInstancesFromLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diflbrqLoadBalancerName'
--
-- * 'diflbrqInstances'
data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer'
    { _diflbrqLoadBalancerName :: !Text
    , _diflbrqInstances        :: ![Instance]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterInstancesFromLoadBalancer' smart constructor.
deregisterInstancesFromLoadBalancer :: Text -> DeregisterInstancesFromLoadBalancer
deregisterInstancesFromLoadBalancer pLoadBalancerName =
    DeregisterInstancesFromLoadBalancer'
    { _diflbrqLoadBalancerName = pLoadBalancerName
    , _diflbrqInstances = mempty
    }

-- | The name of the load balancer.
diflbrqLoadBalancerName :: Lens' DeregisterInstancesFromLoadBalancer Text
diflbrqLoadBalancerName = lens _diflbrqLoadBalancerName (\ s a -> s{_diflbrqLoadBalancerName = a});

-- | The IDs of the instances.
diflbrqInstances :: Lens' DeregisterInstancesFromLoadBalancer [Instance]
diflbrqInstances = lens _diflbrqInstances (\ s a -> s{_diflbrqInstances = a});

instance AWSRequest
         DeregisterInstancesFromLoadBalancer where
        type Sv DeregisterInstancesFromLoadBalancer = ELB
        type Rs DeregisterInstancesFromLoadBalancer =
             DeregisterInstancesFromLoadBalancerResponse
        request = post
        response
          = receiveXMLWrapper
              "DeregisterInstancesFromLoadBalancerResult"
              (\ s h x ->
                 DeregisterInstancesFromLoadBalancerResponse' <$>
                   (x .@? "Instances" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders
         DeregisterInstancesFromLoadBalancer where
        toHeaders = const mempty

instance ToPath DeregisterInstancesFromLoadBalancer
         where
        toPath = const "/"

instance ToQuery DeregisterInstancesFromLoadBalancer
         where
        toQuery DeregisterInstancesFromLoadBalancer'{..}
          = mconcat
              ["Action" =:
                 ("DeregisterInstancesFromLoadBalancer" ::
                    ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _diflbrqLoadBalancerName,
               "Instances" =:
                 toQueryList "member" _diflbrqInstances]

-- | /See:/ 'deregisterInstancesFromLoadBalancerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diflbrsInstances'
--
-- * 'diflbrsStatus'
data DeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse'
    { _diflbrsInstances :: !(Maybe [Instance])
    , _diflbrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterInstancesFromLoadBalancerResponse' smart constructor.
deregisterInstancesFromLoadBalancerResponse :: Int -> DeregisterInstancesFromLoadBalancerResponse
deregisterInstancesFromLoadBalancerResponse pStatus =
    DeregisterInstancesFromLoadBalancerResponse'
    { _diflbrsInstances = Nothing
    , _diflbrsStatus = pStatus
    }

-- | The remaining instances registered with the load balancer.
diflbrsInstances :: Lens' DeregisterInstancesFromLoadBalancerResponse [Instance]
diflbrsInstances = lens _diflbrsInstances (\ s a -> s{_diflbrsInstances = a}) . _Default;

-- | FIXME: Undocumented member.
diflbrsStatus :: Lens' DeregisterInstancesFromLoadBalancerResponse Int
diflbrsStatus = lens _diflbrsStatus (\ s a -> s{_diflbrsStatus = a});
