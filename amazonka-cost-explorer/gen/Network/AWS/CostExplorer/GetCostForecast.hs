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
-- Module      : Network.AWS.CostExplorer.GetCostForecast
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a forecast for how much Amazon Web Services predicts that you will spend over the forecast time period that you select, based on your past costs.
--
--
module Network.AWS.CostExplorer.GetCostForecast
    (
    -- * Creating a Request
      getCostForecast
    , GetCostForecast
    -- * Request Lenses
    , gcfPredictionIntervalLevel
    , gcfFilter
    , gcfTimePeriod
    , gcfMetric
    , gcfGranularity

    -- * Destructuring the Response
    , getCostForecastResponse
    , GetCostForecastResponse
    -- * Response Lenses
    , gcfrsForecastResultsByTime
    , gcfrsTotal
    , gcfrsResponseStatus
    ) where

import Network.AWS.CostExplorer.Types
import Network.AWS.CostExplorer.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCostForecast' smart constructor.
data GetCostForecast = GetCostForecast'
  { _gcfPredictionIntervalLevel :: !(Maybe Nat)
  , _gcfFilter                  :: !(Maybe Expression)
  , _gcfTimePeriod              :: !DateInterval
  , _gcfMetric                  :: !Metric
  , _gcfGranularity             :: !Granularity
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCostForecast' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfPredictionIntervalLevel' - Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
--
-- * 'gcfFilter' - The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
--
-- * 'gcfTimePeriod' - The period of time that you want the forecast to cover.
--
-- * 'gcfMetric' - Which metric Cost Explorer uses to create your forecast. For more information about blended and unblended rates, see <https://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .  Valid values for a @GetCostForecast@ call are the following:     * AmortizedCost     * BlendedCost     * NetAmortizedCost     * NetUnblendedCost     * UnblendedCost
--
-- * 'gcfGranularity' - How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts. The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
getCostForecast
    :: DateInterval -- ^ 'gcfTimePeriod'
    -> Metric -- ^ 'gcfMetric'
    -> Granularity -- ^ 'gcfGranularity'
    -> GetCostForecast
getCostForecast pTimePeriod_ pMetric_ pGranularity_ =
  GetCostForecast'
    { _gcfPredictionIntervalLevel = Nothing
    , _gcfFilter = Nothing
    , _gcfTimePeriod = pTimePeriod_
    , _gcfMetric = pMetric_
    , _gcfGranularity = pGranularity_
    }


-- | Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
gcfPredictionIntervalLevel :: Lens' GetCostForecast (Maybe Natural)
gcfPredictionIntervalLevel = lens _gcfPredictionIntervalLevel (\ s a -> s{_gcfPredictionIntervalLevel = a}) . mapping _Nat

-- | The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
gcfFilter :: Lens' GetCostForecast (Maybe Expression)
gcfFilter = lens _gcfFilter (\ s a -> s{_gcfFilter = a})

-- | The period of time that you want the forecast to cover.
gcfTimePeriod :: Lens' GetCostForecast DateInterval
gcfTimePeriod = lens _gcfTimePeriod (\ s a -> s{_gcfTimePeriod = a})

-- | Which metric Cost Explorer uses to create your forecast. For more information about blended and unblended rates, see <https://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .  Valid values for a @GetCostForecast@ call are the following:     * AmortizedCost     * BlendedCost     * NetAmortizedCost     * NetUnblendedCost     * UnblendedCost
gcfMetric :: Lens' GetCostForecast Metric
gcfMetric = lens _gcfMetric (\ s a -> s{_gcfMetric = a})

-- | How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts. The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
gcfGranularity :: Lens' GetCostForecast Granularity
gcfGranularity = lens _gcfGranularity (\ s a -> s{_gcfGranularity = a})

instance AWSRequest GetCostForecast where
        type Rs GetCostForecast = GetCostForecastResponse
        request = postJSON costExplorer
        response
          = receiveJSON
              (\ s h x ->
                 GetCostForecastResponse' <$>
                   (x .?> "ForecastResultsByTime" .!@ mempty) <*>
                     (x .?> "Total")
                     <*> (pure (fromEnum s)))

instance Hashable GetCostForecast where

instance NFData GetCostForecast where

instance ToHeaders GetCostForecast where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSInsightsIndexService.GetCostForecast" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCostForecast where
        toJSON GetCostForecast'{..}
          = object
              (catMaybes
                 [("PredictionIntervalLevel" .=) <$>
                    _gcfPredictionIntervalLevel,
                  ("Filter" .=) <$> _gcfFilter,
                  Just ("TimePeriod" .= _gcfTimePeriod),
                  Just ("Metric" .= _gcfMetric),
                  Just ("Granularity" .= _gcfGranularity)])

instance ToPath GetCostForecast where
        toPath = const "/"

instance ToQuery GetCostForecast where
        toQuery = const mempty

-- | /See:/ 'getCostForecastResponse' smart constructor.
data GetCostForecastResponse = GetCostForecastResponse'
  { _gcfrsForecastResultsByTime :: !(Maybe [ForecastResult])
  , _gcfrsTotal                 :: !(Maybe MetricValue)
  , _gcfrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCostForecastResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfrsForecastResultsByTime' - The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
--
-- * 'gcfrsTotal' - How much you are forecasted to spend over the forecast period, in @USD@ .
--
-- * 'gcfrsResponseStatus' - -- | The response status code.
getCostForecastResponse
    :: Int -- ^ 'gcfrsResponseStatus'
    -> GetCostForecastResponse
getCostForecastResponse pResponseStatus_ =
  GetCostForecastResponse'
    { _gcfrsForecastResultsByTime = Nothing
    , _gcfrsTotal = Nothing
    , _gcfrsResponseStatus = pResponseStatus_
    }


-- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
gcfrsForecastResultsByTime :: Lens' GetCostForecastResponse [ForecastResult]
gcfrsForecastResultsByTime = lens _gcfrsForecastResultsByTime (\ s a -> s{_gcfrsForecastResultsByTime = a}) . _Default . _Coerce

-- | How much you are forecasted to spend over the forecast period, in @USD@ .
gcfrsTotal :: Lens' GetCostForecastResponse (Maybe MetricValue)
gcfrsTotal = lens _gcfrsTotal (\ s a -> s{_gcfrsTotal = a})

-- | -- | The response status code.
gcfrsResponseStatus :: Lens' GetCostForecastResponse Int
gcfrsResponseStatus = lens _gcfrsResponseStatus (\ s a -> s{_gcfrsResponseStatus = a})

instance NFData GetCostForecastResponse where
