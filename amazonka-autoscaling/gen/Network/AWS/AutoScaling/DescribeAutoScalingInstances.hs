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
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Auto Scaling instances.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeAutoScalingInstances
    (
    -- * Creating a Request
      describeAutoScalingInstances
    , DescribeAutoScalingInstances
    -- * Request Lenses
    , dasiNextToken
    , dasiInstanceIds
    , dasiMaxRecords

    -- * Destructuring the Response
    , describeAutoScalingInstancesResponse
    , DescribeAutoScalingInstancesResponse
    -- * Response Lenses
    , dasirsNextToken
    , dasirsAutoScalingInstances
    , dasirsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAutoScalingInstances' smart constructor.
data DescribeAutoScalingInstances = DescribeAutoScalingInstances'
  { _dasiNextToken   :: !(Maybe Text)
  , _dasiInstanceIds :: !(Maybe [Text])
  , _dasiMaxRecords  :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAutoScalingInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasiNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dasiInstanceIds' - The instances to describe; up to 50 instance IDs. If you omit this parameter, all Auto Scaling instances are described. If you specify an ID that does not exist, it is ignored with no error.
--
-- * 'dasiMaxRecords' - The maximum number of items to return with this call. The default value is 50 and the maximum value is 50.
describeAutoScalingInstances
    :: DescribeAutoScalingInstances
describeAutoScalingInstances =
  DescribeAutoScalingInstances'
    { _dasiNextToken = Nothing
    , _dasiInstanceIds = Nothing
    , _dasiMaxRecords = Nothing
    }


-- | The token for the next set of items to return. (You received this token from a previous call.)
dasiNextToken :: Lens' DescribeAutoScalingInstances (Maybe Text)
dasiNextToken = lens _dasiNextToken (\ s a -> s{_dasiNextToken = a})

-- | The instances to describe; up to 50 instance IDs. If you omit this parameter, all Auto Scaling instances are described. If you specify an ID that does not exist, it is ignored with no error.
dasiInstanceIds :: Lens' DescribeAutoScalingInstances [Text]
dasiInstanceIds = lens _dasiInstanceIds (\ s a -> s{_dasiInstanceIds = a}) . _Default . _Coerce

-- | The maximum number of items to return with this call. The default value is 50 and the maximum value is 50.
dasiMaxRecords :: Lens' DescribeAutoScalingInstances (Maybe Int)
dasiMaxRecords = lens _dasiMaxRecords (\ s a -> s{_dasiMaxRecords = a})

instance AWSPager DescribeAutoScalingInstances where
        page rq rs
          | stop (rs ^. dasirsNextToken) = Nothing
          | stop (rs ^. dasirsAutoScalingInstances) = Nothing
          | otherwise =
            Just $ rq & dasiNextToken .~ rs ^. dasirsNextToken

instance AWSRequest DescribeAutoScalingInstances
         where
        type Rs DescribeAutoScalingInstances =
             DescribeAutoScalingInstancesResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "DescribeAutoScalingInstancesResult"
              (\ s h x ->
                 DescribeAutoScalingInstancesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "AutoScalingInstances" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAutoScalingInstances where

instance NFData DescribeAutoScalingInstances where

instance ToHeaders DescribeAutoScalingInstances where
        toHeaders = const mempty

instance ToPath DescribeAutoScalingInstances where
        toPath = const "/"

instance ToQuery DescribeAutoScalingInstances where
        toQuery DescribeAutoScalingInstances'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAutoScalingInstances" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "NextToken" =: _dasiNextToken,
               "InstanceIds" =:
                 toQuery (toQueryList "member" <$> _dasiInstanceIds),
               "MaxRecords" =: _dasiMaxRecords]

-- | /See:/ 'describeAutoScalingInstancesResponse' smart constructor.
data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse'
  { _dasirsNextToken            :: !(Maybe Text)
  , _dasirsAutoScalingInstances :: !(Maybe [AutoScalingInstanceDetails])
  , _dasirsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAutoScalingInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasirsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dasirsAutoScalingInstances' - The instances.
--
-- * 'dasirsResponseStatus' - -- | The response status code.
describeAutoScalingInstancesResponse
    :: Int -- ^ 'dasirsResponseStatus'
    -> DescribeAutoScalingInstancesResponse
describeAutoScalingInstancesResponse pResponseStatus_ =
  DescribeAutoScalingInstancesResponse'
    { _dasirsNextToken = Nothing
    , _dasirsAutoScalingInstances = Nothing
    , _dasirsResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dasirsNextToken :: Lens' DescribeAutoScalingInstancesResponse (Maybe Text)
dasirsNextToken = lens _dasirsNextToken (\ s a -> s{_dasirsNextToken = a})

-- | The instances.
dasirsAutoScalingInstances :: Lens' DescribeAutoScalingInstancesResponse [AutoScalingInstanceDetails]
dasirsAutoScalingInstances = lens _dasirsAutoScalingInstances (\ s a -> s{_dasirsAutoScalingInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
dasirsResponseStatus :: Lens' DescribeAutoScalingInstancesResponse Int
dasirsResponseStatus = lens _dasirsResponseStatus (\ s a -> s{_dasirsResponseStatus = a})

instance NFData DescribeAutoScalingInstancesResponse
         where
