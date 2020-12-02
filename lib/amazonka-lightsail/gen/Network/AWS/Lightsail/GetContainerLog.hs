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
-- Module      : Network.AWS.Lightsail.GetContainerLog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the log events of a container of your Amazon Lightsail container service.
--
--
-- If your container service has more than one node (i.e., a scale greater than 1), then the log events that are returned for the specified container are merged from all nodes on your container service.
module Network.AWS.Lightsail.GetContainerLog
  ( -- * Creating a Request
    getContainerLog,
    GetContainerLog,

    -- * Request Lenses
    gclStartTime,
    gclEndTime,
    gclPageToken,
    gclFilterPattern,
    gclServiceName,
    gclContainerName,

    -- * Destructuring the Response
    getContainerLogResponse,
    GetContainerLogResponse,

    -- * Response Lenses
    gclrsNextPageToken,
    gclrsLogEvents,
    gclrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getContainerLog' smart constructor.
data GetContainerLog = GetContainerLog'
  { _gclStartTime ::
      !(Maybe POSIX),
    _gclEndTime :: !(Maybe POSIX),
    _gclPageToken :: !(Maybe Text),
    _gclFilterPattern :: !(Maybe Text),
    _gclServiceName :: !Text,
    _gclContainerName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetContainerLog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gclStartTime' - The start of the time interval for which to get log data. Constraints:     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, specify @1538424000@ as the start time. You can convert a human-friendly time to Unix time format using a converter like <https://www.epochconverter.com/ Epoch converter> .
--
-- * 'gclEndTime' - The end of the time interval for which to get log data. Constraints:     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use an end time of October 1, 2018, at 9 PM UTC, specify @1538427600@ as the end time. You can convert a human-friendly time to Unix time format using a converter like <https://www.epochconverter.com/ Epoch converter> .
--
-- * 'gclPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetContainerLog@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- * 'gclFilterPattern' - The pattern to use to filter the returned log events to a specific term. The following are a few examples of filter patterns that you can specify:     * To return all log events, specify a filter pattern of @""@ .     * To exclude log events that contain the @ERROR@ term, and return all other log events, specify a filter pattern of @"-ERROR"@ .     * To return log events that contain the @ERROR@ term, specify a filter pattern of @"ERROR"@ .     * To return log events that contain both the @ERROR@ and @Exception@ terms, specify a filter pattern of @"ERROR Exception"@ .     * To return log events that contain the @ERROR@ /or/ the @Exception@ term, specify a filter pattern of @"?ERROR ?Exception"@ .
--
-- * 'gclServiceName' - The name of the container service for which to get a container log.
--
-- * 'gclContainerName' - The name of the container that is either running or previously ran on the container service for which to return a log.
getContainerLog ::
  -- | 'gclServiceName'
  Text ->
  -- | 'gclContainerName'
  Text ->
  GetContainerLog
getContainerLog pServiceName_ pContainerName_ =
  GetContainerLog'
    { _gclStartTime = Nothing,
      _gclEndTime = Nothing,
      _gclPageToken = Nothing,
      _gclFilterPattern = Nothing,
      _gclServiceName = pServiceName_,
      _gclContainerName = pContainerName_
    }

-- | The start of the time interval for which to get log data. Constraints:     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, specify @1538424000@ as the start time. You can convert a human-friendly time to Unix time format using a converter like <https://www.epochconverter.com/ Epoch converter> .
gclStartTime :: Lens' GetContainerLog (Maybe UTCTime)
gclStartTime = lens _gclStartTime (\s a -> s {_gclStartTime = a}) . mapping _Time

-- | The end of the time interval for which to get log data. Constraints:     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use an end time of October 1, 2018, at 9 PM UTC, specify @1538427600@ as the end time. You can convert a human-friendly time to Unix time format using a converter like <https://www.epochconverter.com/ Epoch converter> .
gclEndTime :: Lens' GetContainerLog (Maybe UTCTime)
gclEndTime = lens _gclEndTime (\s a -> s {_gclEndTime = a}) . mapping _Time

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetContainerLog@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
gclPageToken :: Lens' GetContainerLog (Maybe Text)
gclPageToken = lens _gclPageToken (\s a -> s {_gclPageToken = a})

-- | The pattern to use to filter the returned log events to a specific term. The following are a few examples of filter patterns that you can specify:     * To return all log events, specify a filter pattern of @""@ .     * To exclude log events that contain the @ERROR@ term, and return all other log events, specify a filter pattern of @"-ERROR"@ .     * To return log events that contain the @ERROR@ term, specify a filter pattern of @"ERROR"@ .     * To return log events that contain both the @ERROR@ and @Exception@ terms, specify a filter pattern of @"ERROR Exception"@ .     * To return log events that contain the @ERROR@ /or/ the @Exception@ term, specify a filter pattern of @"?ERROR ?Exception"@ .
gclFilterPattern :: Lens' GetContainerLog (Maybe Text)
gclFilterPattern = lens _gclFilterPattern (\s a -> s {_gclFilterPattern = a})

-- | The name of the container service for which to get a container log.
gclServiceName :: Lens' GetContainerLog Text
gclServiceName = lens _gclServiceName (\s a -> s {_gclServiceName = a})

-- | The name of the container that is either running or previously ran on the container service for which to return a log.
gclContainerName :: Lens' GetContainerLog Text
gclContainerName = lens _gclContainerName (\s a -> s {_gclContainerName = a})

instance AWSRequest GetContainerLog where
  type Rs GetContainerLog = GetContainerLogResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetContainerLogResponse'
            <$> (x .?> "nextPageToken")
            <*> (x .?> "logEvents" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetContainerLog

instance NFData GetContainerLog

instance ToHeaders GetContainerLog where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetContainerLog" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetContainerLog where
  toJSON GetContainerLog' {..} =
    object
      ( catMaybes
          [ ("startTime" .=) <$> _gclStartTime,
            ("endTime" .=) <$> _gclEndTime,
            ("pageToken" .=) <$> _gclPageToken,
            ("filterPattern" .=) <$> _gclFilterPattern,
            Just ("serviceName" .= _gclServiceName),
            Just ("containerName" .= _gclContainerName)
          ]
      )

instance ToPath GetContainerLog where
  toPath = const "/"

instance ToQuery GetContainerLog where
  toQuery = const mempty

-- | /See:/ 'getContainerLogResponse' smart constructor.
data GetContainerLogResponse = GetContainerLogResponse'
  { _gclrsNextPageToken ::
      !(Maybe Text),
    _gclrsLogEvents ::
      !(Maybe [ContainerServiceLogEvent]),
    _gclrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetContainerLogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gclrsNextPageToken' - The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetContainerLog@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'gclrsLogEvents' - An array of objects that describe the log events of a container.
--
-- * 'gclrsResponseStatus' - -- | The response status code.
getContainerLogResponse ::
  -- | 'gclrsResponseStatus'
  Int ->
  GetContainerLogResponse
getContainerLogResponse pResponseStatus_ =
  GetContainerLogResponse'
    { _gclrsNextPageToken = Nothing,
      _gclrsLogEvents = Nothing,
      _gclrsResponseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetContainerLog@ request and specify the next page token using the @pageToken@ parameter.
gclrsNextPageToken :: Lens' GetContainerLogResponse (Maybe Text)
gclrsNextPageToken = lens _gclrsNextPageToken (\s a -> s {_gclrsNextPageToken = a})

-- | An array of objects that describe the log events of a container.
gclrsLogEvents :: Lens' GetContainerLogResponse [ContainerServiceLogEvent]
gclrsLogEvents = lens _gclrsLogEvents (\s a -> s {_gclrsLogEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
gclrsResponseStatus :: Lens' GetContainerLogResponse Int
gclrsResponseStatus = lens _gclrsResponseStatus (\s a -> s {_gclrsResponseStatus = a})

instance NFData GetContainerLogResponse
