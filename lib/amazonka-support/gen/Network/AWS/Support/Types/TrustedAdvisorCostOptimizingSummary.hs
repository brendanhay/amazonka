{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The estimated cost savings that might be realized if the recommended operations are taken.
--
--
--
-- /See:/ 'trustedAdvisorCostOptimizingSummary' smart constructor.
data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary'
  { _tacosEstimatedMonthlySavings ::
      !Double,
    _tacosEstimatedPercentMonthlySavings ::
      !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedAdvisorCostOptimizingSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacosEstimatedMonthlySavings' - The estimated monthly savings that might be realized if the recommended operations are taken.
--
-- * 'tacosEstimatedPercentMonthlySavings' - The estimated percentage of savings that might be realized if the recommended operations are taken.
trustedAdvisorCostOptimizingSummary ::
  -- | 'tacosEstimatedMonthlySavings'
  Double ->
  -- | 'tacosEstimatedPercentMonthlySavings'
  Double ->
  TrustedAdvisorCostOptimizingSummary
trustedAdvisorCostOptimizingSummary
  pEstimatedMonthlySavings_
  pEstimatedPercentMonthlySavings_ =
    TrustedAdvisorCostOptimizingSummary'
      { _tacosEstimatedMonthlySavings =
          pEstimatedMonthlySavings_,
        _tacosEstimatedPercentMonthlySavings =
          pEstimatedPercentMonthlySavings_
      }

-- | The estimated monthly savings that might be realized if the recommended operations are taken.
tacosEstimatedMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary Double
tacosEstimatedMonthlySavings = lens _tacosEstimatedMonthlySavings (\s a -> s {_tacosEstimatedMonthlySavings = a})

-- | The estimated percentage of savings that might be realized if the recommended operations are taken.
tacosEstimatedPercentMonthlySavings :: Lens' TrustedAdvisorCostOptimizingSummary Double
tacosEstimatedPercentMonthlySavings = lens _tacosEstimatedPercentMonthlySavings (\s a -> s {_tacosEstimatedPercentMonthlySavings = a})

instance FromJSON TrustedAdvisorCostOptimizingSummary where
  parseJSON =
    withObject
      "TrustedAdvisorCostOptimizingSummary"
      ( \x ->
          TrustedAdvisorCostOptimizingSummary'
            <$> (x .: "estimatedMonthlySavings")
            <*> (x .: "estimatedPercentMonthlySavings")
      )

instance Hashable TrustedAdvisorCostOptimizingSummary

instance NFData TrustedAdvisorCostOptimizingSummary
