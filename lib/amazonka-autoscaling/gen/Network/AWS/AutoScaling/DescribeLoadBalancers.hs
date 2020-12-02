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
-- Module      : Network.AWS.AutoScaling.DescribeLoadBalancers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the load balancers for the specified Auto Scaling group.
--
--
-- Note that this operation describes only Classic Load Balancers. If you have Application Load Balancers, use 'DescribeLoadBalancerTargetGroups' instead.
--
module Network.AWS.AutoScaling.DescribeLoadBalancers
    (
    -- * Creating a Request
      describeLoadBalancers
    , DescribeLoadBalancers
    -- * Request Lenses
    , dlbNextToken
    , dlbMaxRecords
    , dlbAutoScalingGroupName

    -- * Destructuring the Response
    , describeLoadBalancersResponse
    , DescribeLoadBalancersResponse
    -- * Response Lenses
    , dlbrsLoadBalancers
    , dlbrsNextToken
    , dlbrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { _dlbNextToken            :: !(Maybe Text)
  , _dlbMaxRecords           :: !(Maybe Int)
  , _dlbAutoScalingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dlbMaxRecords' - The maximum number of items to return with this call. The default value is 100 and the maximum value is 100.
--
-- * 'dlbAutoScalingGroupName' - The name of the Auto Scaling group.
describeLoadBalancers
    :: Text -- ^ 'dlbAutoScalingGroupName'
    -> DescribeLoadBalancers
describeLoadBalancers pAutoScalingGroupName_ =
  DescribeLoadBalancers'
    { _dlbNextToken = Nothing
    , _dlbMaxRecords = Nothing
    , _dlbAutoScalingGroupName = pAutoScalingGroupName_
    }


-- | The token for the next set of items to return. (You received this token from a previous call.)
dlbNextToken :: Lens' DescribeLoadBalancers (Maybe Text)
dlbNextToken = lens _dlbNextToken (\ s a -> s{_dlbNextToken = a})

-- | The maximum number of items to return with this call. The default value is 100 and the maximum value is 100.
dlbMaxRecords :: Lens' DescribeLoadBalancers (Maybe Int)
dlbMaxRecords = lens _dlbMaxRecords (\ s a -> s{_dlbMaxRecords = a})

-- | The name of the Auto Scaling group.
dlbAutoScalingGroupName :: Lens' DescribeLoadBalancers Text
dlbAutoScalingGroupName = lens _dlbAutoScalingGroupName (\ s a -> s{_dlbAutoScalingGroupName = a})

instance AWSRequest DescribeLoadBalancers where
        type Rs DescribeLoadBalancers =
             DescribeLoadBalancersResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "DescribeLoadBalancersResult"
              (\ s h x ->
                 DescribeLoadBalancersResponse' <$>
                   (x .@? "LoadBalancers" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLoadBalancers where

instance NFData DescribeLoadBalancers where

instance ToHeaders DescribeLoadBalancers where
        toHeaders = const mempty

instance ToPath DescribeLoadBalancers where
        toPath = const "/"

instance ToQuery DescribeLoadBalancers where
        toQuery DescribeLoadBalancers'{..}
          = mconcat
              ["Action" =: ("DescribeLoadBalancers" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "NextToken" =: _dlbNextToken,
               "MaxRecords" =: _dlbMaxRecords,
               "AutoScalingGroupName" =: _dlbAutoScalingGroupName]

-- | /See:/ 'describeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { _dlbrsLoadBalancers  :: !(Maybe [LoadBalancerState])
  , _dlbrsNextToken      :: !(Maybe Text)
  , _dlbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbrsLoadBalancers' - The load balancers.
--
-- * 'dlbrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dlbrsResponseStatus' - -- | The response status code.
describeLoadBalancersResponse
    :: Int -- ^ 'dlbrsResponseStatus'
    -> DescribeLoadBalancersResponse
describeLoadBalancersResponse pResponseStatus_ =
  DescribeLoadBalancersResponse'
    { _dlbrsLoadBalancers = Nothing
    , _dlbrsNextToken = Nothing
    , _dlbrsResponseStatus = pResponseStatus_
    }


-- | The load balancers.
dlbrsLoadBalancers :: Lens' DescribeLoadBalancersResponse [LoadBalancerState]
dlbrsLoadBalancers = lens _dlbrsLoadBalancers (\ s a -> s{_dlbrsLoadBalancers = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dlbrsNextToken :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dlbrsNextToken = lens _dlbrsNextToken (\ s a -> s{_dlbrsNextToken = a})

-- | -- | The response status code.
dlbrsResponseStatus :: Lens' DescribeLoadBalancersResponse Int
dlbrsResponseStatus = lens _dlbrsResponseStatus (\ s a -> s{_dlbrsResponseStatus = a})

instance NFData DescribeLoadBalancersResponse where
