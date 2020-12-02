{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseLogEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of log events for a database in Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabaseLogEvents
  ( -- * Creating a Request
    getRelationalDatabaseLogEvents,
    GetRelationalDatabaseLogEvents,

    -- * Request Lenses
    grdleStartTime,
    grdleStartFromHead,
    grdleEndTime,
    grdlePageToken,
    grdleRelationalDatabaseName,
    grdleLogStreamName,

    -- * Destructuring the Response
    getRelationalDatabaseLogEventsResponse,
    GetRelationalDatabaseLogEventsResponse,

    -- * Response Lenses
    grdlersNextBackwardToken,
    grdlersResourceLogEvents,
    grdlersNextForwardToken,
    grdlersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabaseLogEvents' smart constructor.
data GetRelationalDatabaseLogEvents = GetRelationalDatabaseLogEvents'
  { _grdleStartTime ::
      !(Maybe POSIX),
    _grdleStartFromHead ::
      !(Maybe Bool),
    _grdleEndTime ::
      !(Maybe POSIX),
    _grdlePageToken ::
      !(Maybe Text),
    _grdleRelationalDatabaseName ::
      !Text,
    _grdleLogStreamName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRelationalDatabaseLogEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdleStartTime' - The start of the time interval from which to get log events. Constraints:     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the start time.
--
-- * 'grdleStartFromHead' - Parameter to specify if the log should start from head or tail. If @true@ is specified, the log event starts from the head of the log. If @false@ is specified, the log event starts from the tail of the log.
--
-- * 'grdleEndTime' - The end of the time interval from which to get log events. Constraints:     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use an end time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the end time.
--
-- * 'grdlePageToken' - The token to advance to the next or previous page of results from your request. To get a page token, perform an initial @GetRelationalDatabaseLogEvents@ request. If your results are paginated, the response will return a next forward token and/or next backward token that you can specify as the page token in a subsequent request.
--
-- * 'grdleRelationalDatabaseName' - The name of your database for which to get log events.
--
-- * 'grdleLogStreamName' - The name of the log stream. Use the @get relational database log streams@ operation to get a list of available log streams.
getRelationalDatabaseLogEvents ::
  -- | 'grdleRelationalDatabaseName'
  Text ->
  -- | 'grdleLogStreamName'
  Text ->
  GetRelationalDatabaseLogEvents
getRelationalDatabaseLogEvents
  pRelationalDatabaseName_
  pLogStreamName_ =
    GetRelationalDatabaseLogEvents'
      { _grdleStartTime = Nothing,
        _grdleStartFromHead = Nothing,
        _grdleEndTime = Nothing,
        _grdlePageToken = Nothing,
        _grdleRelationalDatabaseName = pRelationalDatabaseName_,
        _grdleLogStreamName = pLogStreamName_
      }

-- | The start of the time interval from which to get log events. Constraints:     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the start time.
grdleStartTime :: Lens' GetRelationalDatabaseLogEvents (Maybe UTCTime)
grdleStartTime = lens _grdleStartTime (\s a -> s {_grdleStartTime = a}) . mapping _Time

-- | Parameter to specify if the log should start from head or tail. If @true@ is specified, the log event starts from the head of the log. If @false@ is specified, the log event starts from the tail of the log.
grdleStartFromHead :: Lens' GetRelationalDatabaseLogEvents (Maybe Bool)
grdleStartFromHead = lens _grdleStartFromHead (\s a -> s {_grdleStartFromHead = a})

-- | The end of the time interval from which to get log events. Constraints:     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use an end time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the end time.
grdleEndTime :: Lens' GetRelationalDatabaseLogEvents (Maybe UTCTime)
grdleEndTime = lens _grdleEndTime (\s a -> s {_grdleEndTime = a}) . mapping _Time

-- | The token to advance to the next or previous page of results from your request. To get a page token, perform an initial @GetRelationalDatabaseLogEvents@ request. If your results are paginated, the response will return a next forward token and/or next backward token that you can specify as the page token in a subsequent request.
grdlePageToken :: Lens' GetRelationalDatabaseLogEvents (Maybe Text)
grdlePageToken = lens _grdlePageToken (\s a -> s {_grdlePageToken = a})

-- | The name of your database for which to get log events.
grdleRelationalDatabaseName :: Lens' GetRelationalDatabaseLogEvents Text
grdleRelationalDatabaseName = lens _grdleRelationalDatabaseName (\s a -> s {_grdleRelationalDatabaseName = a})

-- | The name of the log stream. Use the @get relational database log streams@ operation to get a list of available log streams.
grdleLogStreamName :: Lens' GetRelationalDatabaseLogEvents Text
grdleLogStreamName = lens _grdleLogStreamName (\s a -> s {_grdleLogStreamName = a})

instance AWSRequest GetRelationalDatabaseLogEvents where
  type
    Rs GetRelationalDatabaseLogEvents =
      GetRelationalDatabaseLogEventsResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogEventsResponse'
            <$> (x .?> "nextBackwardToken")
            <*> (x .?> "resourceLogEvents" .!@ mempty)
            <*> (x .?> "nextForwardToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetRelationalDatabaseLogEvents

instance NFData GetRelationalDatabaseLogEvents

instance ToHeaders GetRelationalDatabaseLogEvents where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Lightsail_20161128.GetRelationalDatabaseLogEvents" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetRelationalDatabaseLogEvents where
  toJSON GetRelationalDatabaseLogEvents' {..} =
    object
      ( catMaybes
          [ ("startTime" .=) <$> _grdleStartTime,
            ("startFromHead" .=) <$> _grdleStartFromHead,
            ("endTime" .=) <$> _grdleEndTime,
            ("pageToken" .=) <$> _grdlePageToken,
            Just ("relationalDatabaseName" .= _grdleRelationalDatabaseName),
            Just ("logStreamName" .= _grdleLogStreamName)
          ]
      )

instance ToPath GetRelationalDatabaseLogEvents where
  toPath = const "/"

instance ToQuery GetRelationalDatabaseLogEvents where
  toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseLogEventsResponse' smart constructor.
data GetRelationalDatabaseLogEventsResponse = GetRelationalDatabaseLogEventsResponse'
  { _grdlersNextBackwardToken ::
      !(Maybe Text),
    _grdlersResourceLogEvents ::
      !( Maybe
           [LogEvent]
       ),
    _grdlersNextForwardToken ::
      !(Maybe Text),
    _grdlersResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRelationalDatabaseLogEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdlersNextBackwardToken' - A token used for advancing to the previous page of results from your get relational database log events request.
--
-- * 'grdlersResourceLogEvents' - An object describing the result of your get relational database log events request.
--
-- * 'grdlersNextForwardToken' - A token used for advancing to the next page of results from your get relational database log events request.
--
-- * 'grdlersResponseStatus' - -- | The response status code.
getRelationalDatabaseLogEventsResponse ::
  -- | 'grdlersResponseStatus'
  Int ->
  GetRelationalDatabaseLogEventsResponse
getRelationalDatabaseLogEventsResponse pResponseStatus_ =
  GetRelationalDatabaseLogEventsResponse'
    { _grdlersNextBackwardToken =
        Nothing,
      _grdlersResourceLogEvents = Nothing,
      _grdlersNextForwardToken = Nothing,
      _grdlersResponseStatus = pResponseStatus_
    }

-- | A token used for advancing to the previous page of results from your get relational database log events request.
grdlersNextBackwardToken :: Lens' GetRelationalDatabaseLogEventsResponse (Maybe Text)
grdlersNextBackwardToken = lens _grdlersNextBackwardToken (\s a -> s {_grdlersNextBackwardToken = a})

-- | An object describing the result of your get relational database log events request.
grdlersResourceLogEvents :: Lens' GetRelationalDatabaseLogEventsResponse [LogEvent]
grdlersResourceLogEvents = lens _grdlersResourceLogEvents (\s a -> s {_grdlersResourceLogEvents = a}) . _Default . _Coerce

-- | A token used for advancing to the next page of results from your get relational database log events request.
grdlersNextForwardToken :: Lens' GetRelationalDatabaseLogEventsResponse (Maybe Text)
grdlersNextForwardToken = lens _grdlersNextForwardToken (\s a -> s {_grdlersNextForwardToken = a})

-- | -- | The response status code.
grdlersResponseStatus :: Lens' GetRelationalDatabaseLogEventsResponse Int
grdlersResponseStatus = lens _grdlersResponseStatus (\s a -> s {_grdlersResponseStatus = a})

instance NFData GetRelationalDatabaseLogEventsResponse
