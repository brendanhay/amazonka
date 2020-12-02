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
-- Module      : Network.AWS.CostExplorer.GetAnomalies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all of the cost anomalies detected on your account, during the time period specified by the @DateInterval@ object.
module Network.AWS.CostExplorer.GetAnomalies
  ( -- * Creating a Request
    getAnomalies,
    GetAnomalies,

    -- * Request Lenses
    gaNextPageToken,
    gaTotalImpact,
    gaMaxResults,
    gaFeedback,
    gaMonitorARN,
    gaDateInterval,

    -- * Destructuring the Response
    getAnomaliesResponse,
    GetAnomaliesResponse,

    -- * Response Lenses
    garsNextPageToken,
    garsResponseStatus,
    garsAnomalies,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAnomalies' smart constructor.
data GetAnomalies = GetAnomalies'
  { _gaNextPageToken ::
      !(Maybe Text),
    _gaTotalImpact :: !(Maybe TotalImpactFilter),
    _gaMaxResults :: !(Maybe Int),
    _gaFeedback :: !(Maybe AnomalyFeedbackType),
    _gaMonitorARN :: !(Maybe Text),
    _gaDateInterval :: !AnomalyDateInterval
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAnomalies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gaTotalImpact' - Filters anomaly results by the total impact field on the anomaly object. For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve anomalies, with an estimated dollar impact greater than 200.
--
-- * 'gaMaxResults' - The number of entries a paginated response contains.
--
-- * 'gaFeedback' - Filters anomaly results by the feedback field on the anomaly object.
--
-- * 'gaMonitorARN' - Retrieves all of the cost anomalies detected for a specific cost anomaly monitor Amazon Resource Name (ARN).
--
-- * 'gaDateInterval' - Assigns the start and end dates for retrieving cost anomalies. The returned anomaly object will have an @AnomalyEndDate@ in the specified time range.
getAnomalies ::
  -- | 'gaDateInterval'
  AnomalyDateInterval ->
  GetAnomalies
getAnomalies pDateInterval_ =
  GetAnomalies'
    { _gaNextPageToken = Nothing,
      _gaTotalImpact = Nothing,
      _gaMaxResults = Nothing,
      _gaFeedback = Nothing,
      _gaMonitorARN = Nothing,
      _gaDateInterval = pDateInterval_
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gaNextPageToken :: Lens' GetAnomalies (Maybe Text)
gaNextPageToken = lens _gaNextPageToken (\s a -> s {_gaNextPageToken = a})

-- | Filters anomaly results by the total impact field on the anomaly object. For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve anomalies, with an estimated dollar impact greater than 200.
gaTotalImpact :: Lens' GetAnomalies (Maybe TotalImpactFilter)
gaTotalImpact = lens _gaTotalImpact (\s a -> s {_gaTotalImpact = a})

-- | The number of entries a paginated response contains.
gaMaxResults :: Lens' GetAnomalies (Maybe Int)
gaMaxResults = lens _gaMaxResults (\s a -> s {_gaMaxResults = a})

-- | Filters anomaly results by the feedback field on the anomaly object.
gaFeedback :: Lens' GetAnomalies (Maybe AnomalyFeedbackType)
gaFeedback = lens _gaFeedback (\s a -> s {_gaFeedback = a})

-- | Retrieves all of the cost anomalies detected for a specific cost anomaly monitor Amazon Resource Name (ARN).
gaMonitorARN :: Lens' GetAnomalies (Maybe Text)
gaMonitorARN = lens _gaMonitorARN (\s a -> s {_gaMonitorARN = a})

-- | Assigns the start and end dates for retrieving cost anomalies. The returned anomaly object will have an @AnomalyEndDate@ in the specified time range.
gaDateInterval :: Lens' GetAnomalies AnomalyDateInterval
gaDateInterval = lens _gaDateInterval (\s a -> s {_gaDateInterval = a})

instance AWSRequest GetAnomalies where
  type Rs GetAnomalies = GetAnomaliesResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetAnomaliesResponse'
            <$> (x .?> "NextPageToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "Anomalies" .!@ mempty)
      )

instance Hashable GetAnomalies

instance NFData GetAnomalies

instance ToHeaders GetAnomalies where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSInsightsIndexService.GetAnomalies" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetAnomalies where
  toJSON GetAnomalies' {..} =
    object
      ( catMaybes
          [ ("NextPageToken" .=) <$> _gaNextPageToken,
            ("TotalImpact" .=) <$> _gaTotalImpact,
            ("MaxResults" .=) <$> _gaMaxResults,
            ("Feedback" .=) <$> _gaFeedback,
            ("MonitorArn" .=) <$> _gaMonitorARN,
            Just ("DateInterval" .= _gaDateInterval)
          ]
      )

instance ToPath GetAnomalies where
  toPath = const "/"

instance ToQuery GetAnomalies where
  toQuery = const mempty

-- | /See:/ 'getAnomaliesResponse' smart constructor.
data GetAnomaliesResponse = GetAnomaliesResponse'
  { _garsNextPageToken ::
      !(Maybe Text),
    _garsResponseStatus :: !Int,
    _garsAnomalies :: ![Anomaly]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAnomaliesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'garsResponseStatus' - -- | The response status code.
--
-- * 'garsAnomalies' - A list of cost anomalies.
getAnomaliesResponse ::
  -- | 'garsResponseStatus'
  Int ->
  GetAnomaliesResponse
getAnomaliesResponse pResponseStatus_ =
  GetAnomaliesResponse'
    { _garsNextPageToken = Nothing,
      _garsResponseStatus = pResponseStatus_,
      _garsAnomalies = mempty
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
garsNextPageToken :: Lens' GetAnomaliesResponse (Maybe Text)
garsNextPageToken = lens _garsNextPageToken (\s a -> s {_garsNextPageToken = a})

-- | -- | The response status code.
garsResponseStatus :: Lens' GetAnomaliesResponse Int
garsResponseStatus = lens _garsResponseStatus (\s a -> s {_garsResponseStatus = a})

-- | A list of cost anomalies.
garsAnomalies :: Lens' GetAnomaliesResponse [Anomaly]
garsAnomalies = lens _garsAnomalies (\s a -> s {_garsAnomalies = a}) . _Coerce

instance NFData GetAnomaliesResponse
