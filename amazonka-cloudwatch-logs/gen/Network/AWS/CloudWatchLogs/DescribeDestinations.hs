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
-- Module      : Network.AWS.CloudWatchLogs.DescribeDestinations
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the destinations that are associated with the AWS account
-- making the request. The list returned in the response is ASCII-sorted by
-- destination name.
--
-- By default, this operation returns up to 50 destinations. If there are
-- more destinations to list, the response would contain a 'nextToken'
-- value in the response body. You can also limit the number of
-- destinations returned in the response by specifying the 'limit'
-- parameter in the request.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeDestinations.html AWS API Reference> for DescribeDestinations.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeDestinations
    (
    -- * Creating a Request
      describeDestinations
    , DescribeDestinations
    -- * Request Lenses
    , ddNextToken
    , ddLimit
    , ddDestinationNamePrefix

    -- * Destructuring the Response
    , describeDestinationsResponse
    , DescribeDestinationsResponse
    -- * Response Lenses
    , ddrsNextToken
    , ddrsDestinations
    , ddrsResponseStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDestinations' smart constructor.
data DescribeDestinations = DescribeDestinations'
    { _ddNextToken             :: !(Maybe Text)
    , _ddLimit                 :: !(Maybe Nat)
    , _ddDestinationNamePrefix :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDestinations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddNextToken'
--
-- * 'ddLimit'
--
-- * 'ddDestinationNamePrefix'
describeDestinations
    :: DescribeDestinations
describeDestinations =
    DescribeDestinations'
    { _ddNextToken = Nothing
    , _ddLimit = Nothing
    , _ddDestinationNamePrefix = Nothing
    }

-- | Undocumented member.
ddNextToken :: Lens' DescribeDestinations (Maybe Text)
ddNextToken = lens _ddNextToken (\ s a -> s{_ddNextToken = a});

-- | Undocumented member.
ddLimit :: Lens' DescribeDestinations (Maybe Natural)
ddLimit = lens _ddLimit (\ s a -> s{_ddLimit = a}) . mapping _Nat;

-- | Will only return destinations that match the provided
-- destinationNamePrefix. If you don\'t specify a value, no prefix is
-- applied.
ddDestinationNamePrefix :: Lens' DescribeDestinations (Maybe Text)
ddDestinationNamePrefix = lens _ddDestinationNamePrefix (\ s a -> s{_ddDestinationNamePrefix = a});

instance AWSPager DescribeDestinations where
        page rq rs
          | stop (rs ^. ddrsNextToken) = Nothing
          | stop (rs ^. ddrsDestinations) = Nothing
          | otherwise =
            Just $ rq & ddNextToken .~ rs ^. ddrsNextToken

instance AWSRequest DescribeDestinations where
        type Rs DescribeDestinations =
             DescribeDestinationsResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDestinationsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "destinations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDestinations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeDestinations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDestinations where
        toJSON DescribeDestinations'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _ddNextToken,
                  ("limit" .=) <$> _ddLimit,
                  ("DestinationNamePrefix" .=) <$>
                    _ddDestinationNamePrefix])

instance ToPath DescribeDestinations where
        toPath = const "/"

instance ToQuery DescribeDestinations where
        toQuery = const mempty

-- | /See:/ 'describeDestinationsResponse' smart constructor.
data DescribeDestinationsResponse = DescribeDestinationsResponse'
    { _ddrsNextToken      :: !(Maybe Text)
    , _ddrsDestinations   :: !(Maybe [Destination])
    , _ddrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDestinationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsNextToken'
--
-- * 'ddrsDestinations'
--
-- * 'ddrsResponseStatus'
describeDestinationsResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DescribeDestinationsResponse
describeDestinationsResponse pResponseStatus_ =
    DescribeDestinationsResponse'
    { _ddrsNextToken = Nothing
    , _ddrsDestinations = Nothing
    , _ddrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ddrsNextToken :: Lens' DescribeDestinationsResponse (Maybe Text)
ddrsNextToken = lens _ddrsNextToken (\ s a -> s{_ddrsNextToken = a});

-- | Undocumented member.
ddrsDestinations :: Lens' DescribeDestinationsResponse [Destination]
ddrsDestinations = lens _ddrsDestinations (\ s a -> s{_ddrsDestinations = a}) . _Default . _Coerce;

-- | The response status code.
ddrsResponseStatus :: Lens' DescribeDestinationsResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a});
