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
-- Module      : Network.AWS.CostExplorer.GetAnomalyMonitors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly monitor definitions for your account. You can filter using a list of cost anomaly monitor Amazon Resource Names (ARNs).
module Network.AWS.CostExplorer.GetAnomalyMonitors
  ( -- * Creating a Request
    getAnomalyMonitors,
    GetAnomalyMonitors,

    -- * Request Lenses
    gamNextPageToken,
    gamMonitorARNList,
    gamMaxResults,

    -- * Destructuring the Response
    getAnomalyMonitorsResponse,
    GetAnomalyMonitorsResponse,

    -- * Response Lenses
    gamrsNextPageToken,
    gamrsResponseStatus,
    gamrsAnomalyMonitors,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAnomalyMonitors' smart constructor.
data GetAnomalyMonitors = GetAnomalyMonitors'
  { _gamNextPageToken ::
      !(Maybe Text),
    _gamMonitorARNList :: !(Maybe [Text]),
    _gamMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAnomalyMonitors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gamNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gamMonitorARNList' - A list of cost anomaly monitor ARNs.
--
-- * 'gamMaxResults' - The number of entries a paginated response contains.
getAnomalyMonitors ::
  GetAnomalyMonitors
getAnomalyMonitors =
  GetAnomalyMonitors'
    { _gamNextPageToken = Nothing,
      _gamMonitorARNList = Nothing,
      _gamMaxResults = Nothing
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gamNextPageToken :: Lens' GetAnomalyMonitors (Maybe Text)
gamNextPageToken = lens _gamNextPageToken (\s a -> s {_gamNextPageToken = a})

-- | A list of cost anomaly monitor ARNs.
gamMonitorARNList :: Lens' GetAnomalyMonitors [Text]
gamMonitorARNList = lens _gamMonitorARNList (\s a -> s {_gamMonitorARNList = a}) . _Default . _Coerce

-- | The number of entries a paginated response contains.
gamMaxResults :: Lens' GetAnomalyMonitors (Maybe Int)
gamMaxResults = lens _gamMaxResults (\s a -> s {_gamMaxResults = a})

instance AWSRequest GetAnomalyMonitors where
  type Rs GetAnomalyMonitors = GetAnomalyMonitorsResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetAnomalyMonitorsResponse'
            <$> (x .?> "NextPageToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "AnomalyMonitors" .!@ mempty)
      )

instance Hashable GetAnomalyMonitors

instance NFData GetAnomalyMonitors

instance ToHeaders GetAnomalyMonitors where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSInsightsIndexService.GetAnomalyMonitors" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetAnomalyMonitors where
  toJSON GetAnomalyMonitors' {..} =
    object
      ( catMaybes
          [ ("NextPageToken" .=) <$> _gamNextPageToken,
            ("MonitorArnList" .=) <$> _gamMonitorARNList,
            ("MaxResults" .=) <$> _gamMaxResults
          ]
      )

instance ToPath GetAnomalyMonitors where
  toPath = const "/"

instance ToQuery GetAnomalyMonitors where
  toQuery = const mempty

-- | /See:/ 'getAnomalyMonitorsResponse' smart constructor.
data GetAnomalyMonitorsResponse = GetAnomalyMonitorsResponse'
  { _gamrsNextPageToken ::
      !(Maybe Text),
    _gamrsResponseStatus :: !Int,
    _gamrsAnomalyMonitors ::
      ![AnomalyMonitor]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAnomalyMonitorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gamrsNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gamrsResponseStatus' - -- | The response status code.
--
-- * 'gamrsAnomalyMonitors' - A list of cost anomaly monitors that includes the detailed metadata for each monitor.
getAnomalyMonitorsResponse ::
  -- | 'gamrsResponseStatus'
  Int ->
  GetAnomalyMonitorsResponse
getAnomalyMonitorsResponse pResponseStatus_ =
  GetAnomalyMonitorsResponse'
    { _gamrsNextPageToken = Nothing,
      _gamrsResponseStatus = pResponseStatus_,
      _gamrsAnomalyMonitors = mempty
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gamrsNextPageToken :: Lens' GetAnomalyMonitorsResponse (Maybe Text)
gamrsNextPageToken = lens _gamrsNextPageToken (\s a -> s {_gamrsNextPageToken = a})

-- | -- | The response status code.
gamrsResponseStatus :: Lens' GetAnomalyMonitorsResponse Int
gamrsResponseStatus = lens _gamrsResponseStatus (\s a -> s {_gamrsResponseStatus = a})

-- | A list of cost anomaly monitors that includes the detailed metadata for each monitor.
gamrsAnomalyMonitors :: Lens' GetAnomalyMonitorsResponse [AnomalyMonitor]
gamrsAnomalyMonitors = lens _gamrsAnomalyMonitors (\s a -> s {_gamrsAnomalyMonitors = a}) . _Coerce

instance NFData GetAnomalyMonitorsResponse
