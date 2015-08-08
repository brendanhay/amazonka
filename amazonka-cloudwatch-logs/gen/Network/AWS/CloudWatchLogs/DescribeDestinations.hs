{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
-- more destinations to list, the response would contain a @nextToken@
-- value in the response body. You can also limit the number of
-- destinations returned in the response by specifying the @limit@
-- parameter in the request.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeDestinations.html AWS API Reference> for DescribeDestinations.
module Network.AWS.CloudWatchLogs.DescribeDestinations
    (
    -- * Creating a Request
      DescribeDestinations
    , describeDestinations
    -- * Request Lenses
    , ddNextToken
    , ddLimit
    , ddDestinationNamePrefix

    -- * Destructuring the Response
    , DescribeDestinationsResponse
    , describeDestinationsResponse
    -- * Response Lenses
    , ddrsNextToken
    , ddrsDestinations
    , ddrsStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDestinations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddNextToken'
--
-- * 'ddLimit'
--
-- * 'ddDestinationNamePrefix'
data DescribeDestinations = DescribeDestinations'
    { _ddNextToken             :: !(Maybe Text)
    , _ddLimit                 :: !(Maybe Nat)
    , _ddDestinationNamePrefix :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDestinations' smart constructor.
describeDestinations :: DescribeDestinations
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

instance AWSRequest DescribeDestinations where
        type Sv DescribeDestinations = CloudWatchLogs
        type Rs DescribeDestinations =
             DescribeDestinationsResponse
        request = postJSON
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
              ["nextToken" .= _ddNextToken, "limit" .= _ddLimit,
               "DestinationNamePrefix" .= _ddDestinationNamePrefix]

instance ToPath DescribeDestinations where
        toPath = const "/"

instance ToQuery DescribeDestinations where
        toQuery = const mempty

-- | /See:/ 'describeDestinationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrsNextToken'
--
-- * 'ddrsDestinations'
--
-- * 'ddrsStatus'
data DescribeDestinationsResponse = DescribeDestinationsResponse'
    { _ddrsNextToken    :: !(Maybe Text)
    , _ddrsDestinations :: !(Maybe [Destination])
    , _ddrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDestinationsResponse' smart constructor.
describeDestinationsResponse :: Int -> DescribeDestinationsResponse
describeDestinationsResponse pStatus_ =
    DescribeDestinationsResponse'
    { _ddrsNextToken = Nothing
    , _ddrsDestinations = Nothing
    , _ddrsStatus = pStatus_
    }

-- | Undocumented member.
ddrsNextToken :: Lens' DescribeDestinationsResponse (Maybe Text)
ddrsNextToken = lens _ddrsNextToken (\ s a -> s{_ddrsNextToken = a});

-- | Undocumented member.
ddrsDestinations :: Lens' DescribeDestinationsResponse [Destination]
ddrsDestinations = lens _ddrsDestinations (\ s a -> s{_ddrsDestinations = a}) . _Default . _Coerce;

-- | Undocumented member.
ddrsStatus :: Lens' DescribeDestinationsResponse Int
ddrsStatus = lens _ddrsStatus (\ s a -> s{_ddrsStatus = a});
