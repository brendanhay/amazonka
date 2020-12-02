{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TerminateRecommendationDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on termination recommendation.
--
--
--
-- /See:/ 'terminateRecommendationDetail' smart constructor.
data TerminateRecommendationDetail = TerminateRecommendationDetail'
  { _trdCurrencyCode ::
      !(Maybe Text),
    _trdEstimatedMonthlySavings ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TerminateRecommendationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trdCurrencyCode' - The currency code that AWS used to calculate the costs for this instance.
--
-- * 'trdEstimatedMonthlySavings' - Estimated savings resulting from modification, on a monthly basis.
terminateRecommendationDetail ::
  TerminateRecommendationDetail
terminateRecommendationDetail =
  TerminateRecommendationDetail'
    { _trdCurrencyCode = Nothing,
      _trdEstimatedMonthlySavings = Nothing
    }

-- | The currency code that AWS used to calculate the costs for this instance.
trdCurrencyCode :: Lens' TerminateRecommendationDetail (Maybe Text)
trdCurrencyCode = lens _trdCurrencyCode (\s a -> s {_trdCurrencyCode = a})

-- | Estimated savings resulting from modification, on a monthly basis.
trdEstimatedMonthlySavings :: Lens' TerminateRecommendationDetail (Maybe Text)
trdEstimatedMonthlySavings = lens _trdEstimatedMonthlySavings (\s a -> s {_trdEstimatedMonthlySavings = a})

instance FromJSON TerminateRecommendationDetail where
  parseJSON =
    withObject
      "TerminateRecommendationDetail"
      ( \x ->
          TerminateRecommendationDetail'
            <$> (x .:? "CurrencyCode") <*> (x .:? "EstimatedMonthlySavings")
      )

instance Hashable TerminateRecommendationDetail

instance NFData TerminateRecommendationDetail
