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
-- Module      : Network.AWS.ELBv2.DescribeTargetGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified target groups or all of your target groups. By default, all target groups are described. Alternatively, you can specify one of the following to filter the results: the ARN of the load balancer, the names of one or more target groups, or the ARNs of one or more target groups.
--
--
-- To describe the targets for a target group, use 'DescribeTargetHealth' . To describe the attributes of a target group, use 'DescribeTargetGroupAttributes' .
--
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeTargetGroups
    (
    -- * Creating a Request
      describeTargetGroups
    , DescribeTargetGroups
    -- * Request Lenses
    , dtgTargetGroupARNs
    , dtgNames
    , dtgLoadBalancerARN
    , dtgMarker
    , dtgPageSize

    -- * Destructuring the Response
    , describeTargetGroupsResponse
    , DescribeTargetGroupsResponse
    -- * Response Lenses
    , dtgsrsNextMarker
    , dtgsrsTargetGroups
    , dtgsrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTargetGroups' smart constructor.
data DescribeTargetGroups = DescribeTargetGroups'
  { _dtgTargetGroupARNs :: !(Maybe [Text])
  , _dtgNames           :: !(Maybe [Text])
  , _dtgLoadBalancerARN :: !(Maybe Text)
  , _dtgMarker          :: !(Maybe Text)
  , _dtgPageSize        :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTargetGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgTargetGroupARNs' - The Amazon Resource Names (ARN) of the target groups.
--
-- * 'dtgNames' - The names of the target groups.
--
-- * 'dtgLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'dtgMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'dtgPageSize' - The maximum number of results to return with this call.
describeTargetGroups
    :: DescribeTargetGroups
describeTargetGroups =
  DescribeTargetGroups'
    { _dtgTargetGroupARNs = Nothing
    , _dtgNames = Nothing
    , _dtgLoadBalancerARN = Nothing
    , _dtgMarker = Nothing
    , _dtgPageSize = Nothing
    }


-- | The Amazon Resource Names (ARN) of the target groups.
dtgTargetGroupARNs :: Lens' DescribeTargetGroups [Text]
dtgTargetGroupARNs = lens _dtgTargetGroupARNs (\ s a -> s{_dtgTargetGroupARNs = a}) . _Default . _Coerce

-- | The names of the target groups.
dtgNames :: Lens' DescribeTargetGroups [Text]
dtgNames = lens _dtgNames (\ s a -> s{_dtgNames = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
dtgLoadBalancerARN :: Lens' DescribeTargetGroups (Maybe Text)
dtgLoadBalancerARN = lens _dtgLoadBalancerARN (\ s a -> s{_dtgLoadBalancerARN = a})

-- | The marker for the next set of results. (You received this marker from a previous call.)
dtgMarker :: Lens' DescribeTargetGroups (Maybe Text)
dtgMarker = lens _dtgMarker (\ s a -> s{_dtgMarker = a})

-- | The maximum number of results to return with this call.
dtgPageSize :: Lens' DescribeTargetGroups (Maybe Natural)
dtgPageSize = lens _dtgPageSize (\ s a -> s{_dtgPageSize = a}) . mapping _Nat

instance AWSPager DescribeTargetGroups where
        page rq rs
          | stop (rs ^. dtgsrsNextMarker) = Nothing
          | stop (rs ^. dtgsrsTargetGroups) = Nothing
          | otherwise =
            Just $ rq & dtgMarker .~ rs ^. dtgsrsNextMarker

instance AWSRequest DescribeTargetGroups where
        type Rs DescribeTargetGroups =
             DescribeTargetGroupsResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "DescribeTargetGroupsResult"
              (\ s h x ->
                 DescribeTargetGroupsResponse' <$>
                   (x .@? "NextMarker") <*>
                     (x .@? "TargetGroups" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTargetGroups where

instance NFData DescribeTargetGroups where

instance ToHeaders DescribeTargetGroups where
        toHeaders = const mempty

instance ToPath DescribeTargetGroups where
        toPath = const "/"

instance ToQuery DescribeTargetGroups where
        toQuery DescribeTargetGroups'{..}
          = mconcat
              ["Action" =: ("DescribeTargetGroups" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "TargetGroupArns" =:
                 toQuery
                   (toQueryList "member" <$> _dtgTargetGroupARNs),
               "Names" =:
                 toQuery (toQueryList "member" <$> _dtgNames),
               "LoadBalancerArn" =: _dtgLoadBalancerARN,
               "Marker" =: _dtgMarker, "PageSize" =: _dtgPageSize]

-- | /See:/ 'describeTargetGroupsResponse' smart constructor.
data DescribeTargetGroupsResponse = DescribeTargetGroupsResponse'
  { _dtgsrsNextMarker     :: !(Maybe Text)
  , _dtgsrsTargetGroups   :: !(Maybe [TargetGroup])
  , _dtgsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTargetGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgsrsNextMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dtgsrsTargetGroups' - Information about the target groups.
--
-- * 'dtgsrsResponseStatus' - -- | The response status code.
describeTargetGroupsResponse
    :: Int -- ^ 'dtgsrsResponseStatus'
    -> DescribeTargetGroupsResponse
describeTargetGroupsResponse pResponseStatus_ =
  DescribeTargetGroupsResponse'
    { _dtgsrsNextMarker = Nothing
    , _dtgsrsTargetGroups = Nothing
    , _dtgsrsResponseStatus = pResponseStatus_
    }


-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dtgsrsNextMarker :: Lens' DescribeTargetGroupsResponse (Maybe Text)
dtgsrsNextMarker = lens _dtgsrsNextMarker (\ s a -> s{_dtgsrsNextMarker = a})

-- | Information about the target groups.
dtgsrsTargetGroups :: Lens' DescribeTargetGroupsResponse [TargetGroup]
dtgsrsTargetGroups = lens _dtgsrsTargetGroups (\ s a -> s{_dtgsrsTargetGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
dtgsrsResponseStatus :: Lens' DescribeTargetGroupsResponse Int
dtgsrsResponseStatus = lens _dtgsrsResponseStatus (\ s a -> s{_dtgsrsResponseStatus = a})

instance NFData DescribeTargetGroupsResponse where
