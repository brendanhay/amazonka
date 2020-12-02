{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ExponentialRolloutRate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ExponentialRolloutRate where

import Network.AWS.IoT.Types.RateIncreaseCriteria
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Allows you to create an exponential rate of rollout for a job.
--
--
--
-- /See:/ 'exponentialRolloutRate' smart constructor.
data ExponentialRolloutRate = ExponentialRolloutRate'
  { _errBaseRatePerMinute ::
      !Nat,
    _errIncrementFactor :: !Double,
    _errRateIncreaseCriteria ::
      !RateIncreaseCriteria
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExponentialRolloutRate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'errBaseRatePerMinute' - The minimum number of things that will be notified of a pending job, per minute at the start of job rollout. This parameter allows you to define the initial rate of rollout.
--
-- * 'errIncrementFactor' - The exponential factor to increase the rate of rollout for a job. AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
--
-- * 'errRateIncreaseCriteria' - The criteria to initiate the increase in rate of rollout for a job.
exponentialRolloutRate ::
  -- | 'errBaseRatePerMinute'
  Natural ->
  -- | 'errIncrementFactor'
  Double ->
  -- | 'errRateIncreaseCriteria'
  RateIncreaseCriteria ->
  ExponentialRolloutRate
exponentialRolloutRate
  pBaseRatePerMinute_
  pIncrementFactor_
  pRateIncreaseCriteria_ =
    ExponentialRolloutRate'
      { _errBaseRatePerMinute =
          _Nat # pBaseRatePerMinute_,
        _errIncrementFactor = pIncrementFactor_,
        _errRateIncreaseCriteria = pRateIncreaseCriteria_
      }

-- | The minimum number of things that will be notified of a pending job, per minute at the start of job rollout. This parameter allows you to define the initial rate of rollout.
errBaseRatePerMinute :: Lens' ExponentialRolloutRate Natural
errBaseRatePerMinute = lens _errBaseRatePerMinute (\s a -> s {_errBaseRatePerMinute = a}) . _Nat

-- | The exponential factor to increase the rate of rollout for a job. AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
errIncrementFactor :: Lens' ExponentialRolloutRate Double
errIncrementFactor = lens _errIncrementFactor (\s a -> s {_errIncrementFactor = a})

-- | The criteria to initiate the increase in rate of rollout for a job.
errRateIncreaseCriteria :: Lens' ExponentialRolloutRate RateIncreaseCriteria
errRateIncreaseCriteria = lens _errRateIncreaseCriteria (\s a -> s {_errRateIncreaseCriteria = a})

instance FromJSON ExponentialRolloutRate where
  parseJSON =
    withObject
      "ExponentialRolloutRate"
      ( \x ->
          ExponentialRolloutRate'
            <$> (x .: "baseRatePerMinute")
            <*> (x .: "incrementFactor")
            <*> (x .: "rateIncreaseCriteria")
      )

instance Hashable ExponentialRolloutRate

instance NFData ExponentialRolloutRate

instance ToJSON ExponentialRolloutRate where
  toJSON ExponentialRolloutRate' {..} =
    object
      ( catMaybes
          [ Just ("baseRatePerMinute" .= _errBaseRatePerMinute),
            Just ("incrementFactor" .= _errIncrementFactor),
            Just ("rateIncreaseCriteria" .= _errRateIncreaseCriteria)
          ]
      )
