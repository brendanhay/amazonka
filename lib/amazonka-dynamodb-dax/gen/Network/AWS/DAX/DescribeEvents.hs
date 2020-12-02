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
-- Module      : Network.AWS.DAX.DescribeEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to DAX clusters and parameter groups. You can obtain events specific to a particular DAX cluster or parameter group by providing the name as a parameter.
--
--
-- By default, only the events occurring within the last hour are returned; however, you can retrieve up to 14 days' worth of events if necessary.
--
module Network.AWS.DAX.DescribeEvents
    (
    -- * Creating a Request
      describeEvents
    , DescribeEvents
    -- * Request Lenses
    , deSourceName
    , deStartTime
    , deSourceType
    , deNextToken
    , deEndTime
    , deDuration
    , deMaxResults

    -- * Destructuring the Response
    , describeEventsResponse
    , DescribeEventsResponse
    -- * Response Lenses
    , dersNextToken
    , dersEvents
    , dersResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { _deSourceName :: !(Maybe Text)
  , _deStartTime  :: !(Maybe POSIX)
  , _deSourceType :: !(Maybe SourceType)
  , _deNextToken  :: !(Maybe Text)
  , _deEndTime    :: !(Maybe POSIX)
  , _deDuration   :: !(Maybe Int)
  , _deMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deSourceName' - The identifier of the event source for which events will be returned. If not specified, then all sources are included in the response.
--
-- * 'deStartTime' - The beginning of the time interval to retrieve events for, specified in ISO 8601 format.
--
-- * 'deSourceType' - The event source to retrieve events for. If no value is specified, all events are returned.
--
-- * 'deNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- * 'deEndTime' - The end of the time interval for which to retrieve events, specified in ISO 8601 format.
--
-- * 'deDuration' - The number of minutes' worth of events to retrieve.
--
-- * 'deMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. The value for @MaxResults@ must be between 20 and 100.
describeEvents
    :: DescribeEvents
describeEvents =
  DescribeEvents'
    { _deSourceName = Nothing
    , _deStartTime = Nothing
    , _deSourceType = Nothing
    , _deNextToken = Nothing
    , _deEndTime = Nothing
    , _deDuration = Nothing
    , _deMaxResults = Nothing
    }


-- | The identifier of the event source for which events will be returned. If not specified, then all sources are included in the response.
deSourceName :: Lens' DescribeEvents (Maybe Text)
deSourceName = lens _deSourceName (\ s a -> s{_deSourceName = a})

-- | The beginning of the time interval to retrieve events for, specified in ISO 8601 format.
deStartTime :: Lens' DescribeEvents (Maybe UTCTime)
deStartTime = lens _deStartTime (\ s a -> s{_deStartTime = a}) . mapping _Time

-- | The event source to retrieve events for. If no value is specified, all events are returned.
deSourceType :: Lens' DescribeEvents (Maybe SourceType)
deSourceType = lens _deSourceType (\ s a -> s{_deSourceType = a})

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
deNextToken :: Lens' DescribeEvents (Maybe Text)
deNextToken = lens _deNextToken (\ s a -> s{_deNextToken = a})

-- | The end of the time interval for which to retrieve events, specified in ISO 8601 format.
deEndTime :: Lens' DescribeEvents (Maybe UTCTime)
deEndTime = lens _deEndTime (\ s a -> s{_deEndTime = a}) . mapping _Time

-- | The number of minutes' worth of events to retrieve.
deDuration :: Lens' DescribeEvents (Maybe Int)
deDuration = lens _deDuration (\ s a -> s{_deDuration = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. The value for @MaxResults@ must be between 20 and 100.
deMaxResults :: Lens' DescribeEvents (Maybe Int)
deMaxResults = lens _deMaxResults (\ s a -> s{_deMaxResults = a})

instance AWSRequest DescribeEvents where
        type Rs DescribeEvents = DescribeEventsResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Events" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEvents where

instance NFData DescribeEvents where

instance ToHeaders DescribeEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.DescribeEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEvents where
        toJSON DescribeEvents'{..}
          = object
              (catMaybes
                 [("SourceName" .=) <$> _deSourceName,
                  ("StartTime" .=) <$> _deStartTime,
                  ("SourceType" .=) <$> _deSourceType,
                  ("NextToken" .=) <$> _deNextToken,
                  ("EndTime" .=) <$> _deEndTime,
                  ("Duration" .=) <$> _deDuration,
                  ("MaxResults" .=) <$> _deMaxResults])

instance ToPath DescribeEvents where
        toPath = const "/"

instance ToQuery DescribeEvents where
        toQuery = const mempty

-- | /See:/ 'describeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { _dersNextToken      :: !(Maybe Text)
  , _dersEvents         :: !(Maybe [Event])
  , _dersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersNextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dersEvents' - An array of events. Each element in the array represents one event.
--
-- * 'dersResponseStatus' - -- | The response status code.
describeEventsResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DescribeEventsResponse
describeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
    { _dersNextToken = Nothing
    , _dersEvents = Nothing
    , _dersResponseStatus = pResponseStatus_
    }


-- | Provides an identifier to allow retrieval of paginated results.
dersNextToken :: Lens' DescribeEventsResponse (Maybe Text)
dersNextToken = lens _dersNextToken (\ s a -> s{_dersNextToken = a})

-- | An array of events. Each element in the array represents one event.
dersEvents :: Lens' DescribeEventsResponse [Event]
dersEvents = lens _dersEvents (\ s a -> s{_dersEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEventsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DescribeEventsResponse where
