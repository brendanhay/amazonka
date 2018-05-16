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
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Auto Scaling groups.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeAutoScalingGroups
    (
    -- * Creating a Request
      describeAutoScalingGroups
    , DescribeAutoScalingGroups
    -- * Request Lenses
    , dasgAutoScalingGroupNames
    , dasgNextToken
    , dasgMaxRecords

    -- * Destructuring the Response
    , describeAutoScalingGroupsResponse
    , DescribeAutoScalingGroupsResponse
    -- * Response Lenses
    , dasgrsNextToken
    , dasgrsResponseStatus
    , dasgrsAutoScalingGroups
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAutoScalingGroups' smart constructor.
data DescribeAutoScalingGroups = DescribeAutoScalingGroups'
  { _dasgAutoScalingGroupNames :: !(Maybe [Text])
  , _dasgNextToken             :: !(Maybe Text)
  , _dasgMaxRecords            :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAutoScalingGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasgAutoScalingGroupNames' - The names of the Auto Scaling groups. If you omit this parameter, all Auto Scaling groups are described.
--
-- * 'dasgNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dasgMaxRecords' - The maximum number of items to return with this call. The default value is 50 and the maximum value is 100.
describeAutoScalingGroups
    :: DescribeAutoScalingGroups
describeAutoScalingGroups =
  DescribeAutoScalingGroups'
    { _dasgAutoScalingGroupNames = Nothing
    , _dasgNextToken = Nothing
    , _dasgMaxRecords = Nothing
    }


-- | The names of the Auto Scaling groups. If you omit this parameter, all Auto Scaling groups are described.
dasgAutoScalingGroupNames :: Lens' DescribeAutoScalingGroups [Text]
dasgAutoScalingGroupNames = lens _dasgAutoScalingGroupNames (\ s a -> s{_dasgAutoScalingGroupNames = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dasgNextToken :: Lens' DescribeAutoScalingGroups (Maybe Text)
dasgNextToken = lens _dasgNextToken (\ s a -> s{_dasgNextToken = a})

-- | The maximum number of items to return with this call. The default value is 50 and the maximum value is 100.
dasgMaxRecords :: Lens' DescribeAutoScalingGroups (Maybe Int)
dasgMaxRecords = lens _dasgMaxRecords (\ s a -> s{_dasgMaxRecords = a})

instance AWSPager DescribeAutoScalingGroups where
        page rq rs
          | stop (rs ^. dasgrsNextToken) = Nothing
          | stop (rs ^. dasgrsAutoScalingGroups) = Nothing
          | otherwise =
            Just $ rq & dasgNextToken .~ rs ^. dasgrsNextToken

instance AWSRequest DescribeAutoScalingGroups where
        type Rs DescribeAutoScalingGroups =
             DescribeAutoScalingGroupsResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "DescribeAutoScalingGroupsResult"
              (\ s h x ->
                 DescribeAutoScalingGroupsResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "AutoScalingGroups" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable DescribeAutoScalingGroups where

instance NFData DescribeAutoScalingGroups where

instance ToHeaders DescribeAutoScalingGroups where
        toHeaders = const mempty

instance ToPath DescribeAutoScalingGroups where
        toPath = const "/"

instance ToQuery DescribeAutoScalingGroups where
        toQuery DescribeAutoScalingGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAutoScalingGroups" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupNames" =:
                 toQuery
                   (toQueryList "member" <$>
                      _dasgAutoScalingGroupNames),
               "NextToken" =: _dasgNextToken,
               "MaxRecords" =: _dasgMaxRecords]

-- | /See:/ 'describeAutoScalingGroupsResponse' smart constructor.
data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse'
  { _dasgrsNextToken         :: !(Maybe Text)
  , _dasgrsResponseStatus    :: !Int
  , _dasgrsAutoScalingGroups :: ![AutoScalingGroup]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAutoScalingGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasgrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dasgrsResponseStatus' - -- | The response status code.
--
-- * 'dasgrsAutoScalingGroups' - The groups.
describeAutoScalingGroupsResponse
    :: Int -- ^ 'dasgrsResponseStatus'
    -> DescribeAutoScalingGroupsResponse
describeAutoScalingGroupsResponse pResponseStatus_ =
  DescribeAutoScalingGroupsResponse'
    { _dasgrsNextToken = Nothing
    , _dasgrsResponseStatus = pResponseStatus_
    , _dasgrsAutoScalingGroups = mempty
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dasgrsNextToken :: Lens' DescribeAutoScalingGroupsResponse (Maybe Text)
dasgrsNextToken = lens _dasgrsNextToken (\ s a -> s{_dasgrsNextToken = a})

-- | -- | The response status code.
dasgrsResponseStatus :: Lens' DescribeAutoScalingGroupsResponse Int
dasgrsResponseStatus = lens _dasgrsResponseStatus (\ s a -> s{_dasgrsResponseStatus = a})

-- | The groups.
dasgrsAutoScalingGroups :: Lens' DescribeAutoScalingGroupsResponse [AutoScalingGroup]
dasgrsAutoScalingGroups = lens _dasgrsAutoScalingGroups (\ s a -> s{_dasgrsAutoScalingGroups = a}) . _Coerce

instance NFData DescribeAutoScalingGroupsResponse
         where
