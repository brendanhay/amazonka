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
-- Module      : Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the target groups for the specified Auto Scaling group.
--
--
module Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
    (
    -- * Creating a Request
      describeLoadBalancerTargetGroups
    , DescribeLoadBalancerTargetGroups
    -- * Request Lenses
    , dlbtgsNextToken
    , dlbtgsMaxRecords
    , dlbtgsAutoScalingGroupName

    -- * Destructuring the Response
    , describeLoadBalancerTargetGroupsResponse
    , DescribeLoadBalancerTargetGroupsResponse
    -- * Response Lenses
    , dlbtgsrsLoadBalancerTargetGroups
    , dlbtgsrsNextToken
    , dlbtgsrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLoadBalancerTargetGroups' smart constructor.
data DescribeLoadBalancerTargetGroups = DescribeLoadBalancerTargetGroups'
  { _dlbtgsNextToken            :: !(Maybe Text)
  , _dlbtgsMaxRecords           :: !(Maybe Int)
  , _dlbtgsAutoScalingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancerTargetGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbtgsNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dlbtgsMaxRecords' - The maximum number of items to return with this call. The default value is 100 and the maximum value is 100.
--
-- * 'dlbtgsAutoScalingGroupName' - The name of the Auto Scaling group.
describeLoadBalancerTargetGroups
    :: Text -- ^ 'dlbtgsAutoScalingGroupName'
    -> DescribeLoadBalancerTargetGroups
describeLoadBalancerTargetGroups pAutoScalingGroupName_ =
  DescribeLoadBalancerTargetGroups'
    { _dlbtgsNextToken = Nothing
    , _dlbtgsMaxRecords = Nothing
    , _dlbtgsAutoScalingGroupName = pAutoScalingGroupName_
    }


-- | The token for the next set of items to return. (You received this token from a previous call.)
dlbtgsNextToken :: Lens' DescribeLoadBalancerTargetGroups (Maybe Text)
dlbtgsNextToken = lens _dlbtgsNextToken (\ s a -> s{_dlbtgsNextToken = a})

-- | The maximum number of items to return with this call. The default value is 100 and the maximum value is 100.
dlbtgsMaxRecords :: Lens' DescribeLoadBalancerTargetGroups (Maybe Int)
dlbtgsMaxRecords = lens _dlbtgsMaxRecords (\ s a -> s{_dlbtgsMaxRecords = a})

-- | The name of the Auto Scaling group.
dlbtgsAutoScalingGroupName :: Lens' DescribeLoadBalancerTargetGroups Text
dlbtgsAutoScalingGroupName = lens _dlbtgsAutoScalingGroupName (\ s a -> s{_dlbtgsAutoScalingGroupName = a})

instance AWSRequest DescribeLoadBalancerTargetGroups
         where
        type Rs DescribeLoadBalancerTargetGroups =
             DescribeLoadBalancerTargetGroupsResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "DescribeLoadBalancerTargetGroupsResult"
              (\ s h x ->
                 DescribeLoadBalancerTargetGroupsResponse' <$>
                   (x .@? "LoadBalancerTargetGroups" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLoadBalancerTargetGroups
         where

instance NFData DescribeLoadBalancerTargetGroups
         where

instance ToHeaders DescribeLoadBalancerTargetGroups
         where
        toHeaders = const mempty

instance ToPath DescribeLoadBalancerTargetGroups
         where
        toPath = const "/"

instance ToQuery DescribeLoadBalancerTargetGroups
         where
        toQuery DescribeLoadBalancerTargetGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeLoadBalancerTargetGroups" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "NextToken" =: _dlbtgsNextToken,
               "MaxRecords" =: _dlbtgsMaxRecords,
               "AutoScalingGroupName" =:
                 _dlbtgsAutoScalingGroupName]

-- | /See:/ 'describeLoadBalancerTargetGroupsResponse' smart constructor.
data DescribeLoadBalancerTargetGroupsResponse = DescribeLoadBalancerTargetGroupsResponse'
  { _dlbtgsrsLoadBalancerTargetGroups :: !(Maybe [LoadBalancerTargetGroupState])
  , _dlbtgsrsNextToken                :: !(Maybe Text)
  , _dlbtgsrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancerTargetGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbtgsrsLoadBalancerTargetGroups' - Information about the target groups.
--
-- * 'dlbtgsrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dlbtgsrsResponseStatus' - -- | The response status code.
describeLoadBalancerTargetGroupsResponse
    :: Int -- ^ 'dlbtgsrsResponseStatus'
    -> DescribeLoadBalancerTargetGroupsResponse
describeLoadBalancerTargetGroupsResponse pResponseStatus_ =
  DescribeLoadBalancerTargetGroupsResponse'
    { _dlbtgsrsLoadBalancerTargetGroups = Nothing
    , _dlbtgsrsNextToken = Nothing
    , _dlbtgsrsResponseStatus = pResponseStatus_
    }


-- | Information about the target groups.
dlbtgsrsLoadBalancerTargetGroups :: Lens' DescribeLoadBalancerTargetGroupsResponse [LoadBalancerTargetGroupState]
dlbtgsrsLoadBalancerTargetGroups = lens _dlbtgsrsLoadBalancerTargetGroups (\ s a -> s{_dlbtgsrsLoadBalancerTargetGroups = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dlbtgsrsNextToken :: Lens' DescribeLoadBalancerTargetGroupsResponse (Maybe Text)
dlbtgsrsNextToken = lens _dlbtgsrsNextToken (\ s a -> s{_dlbtgsrsNextToken = a})

-- | -- | The response status code.
dlbtgsrsResponseStatus :: Lens' DescribeLoadBalancerTargetGroupsResponse Int
dlbtgsrsResponseStatus = lens _dlbtgsrsResponseStatus (\ s a -> s{_dlbtgsrsResponseStatus = a})

instance NFData
           DescribeLoadBalancerTargetGroupsResponse
         where
