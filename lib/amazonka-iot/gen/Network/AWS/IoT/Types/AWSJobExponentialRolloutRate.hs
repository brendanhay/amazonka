{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobExponentialRolloutRate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobExponentialRolloutRate where

import Network.AWS.IoT.Types.AWSJobRateIncreaseCriteria
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate increase for a job rollout.
--
--
--
-- /See:/ 'awsJobExponentialRolloutRate' smart constructor.
data AWSJobExponentialRolloutRate = AWSJobExponentialRolloutRate'
  { _ajerrBaseRatePerMinute ::
      !Nat,
    _ajerrIncrementFactor :: !Double,
    _ajerrRateIncreaseCriteria ::
      !AWSJobRateIncreaseCriteria
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSJobExponentialRolloutRate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajerrBaseRatePerMinute' - The minimum number of things that will be notified of a pending job, per minute, at the start of the job rollout. This is the initial rate of the rollout.
--
-- * 'ajerrIncrementFactor' - The rate of increase for a job rollout. The number of things notified is multiplied by this factor.
--
-- * 'ajerrRateIncreaseCriteria' - The criteria to initiate the increase in rate of rollout for a job. AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
awsJobExponentialRolloutRate ::
  -- | 'ajerrBaseRatePerMinute'
  Natural ->
  -- | 'ajerrIncrementFactor'
  Double ->
  -- | 'ajerrRateIncreaseCriteria'
  AWSJobRateIncreaseCriteria ->
  AWSJobExponentialRolloutRate
awsJobExponentialRolloutRate
  pBaseRatePerMinute_
  pIncrementFactor_
  pRateIncreaseCriteria_ =
    AWSJobExponentialRolloutRate'
      { _ajerrBaseRatePerMinute =
          _Nat # pBaseRatePerMinute_,
        _ajerrIncrementFactor = pIncrementFactor_,
        _ajerrRateIncreaseCriteria = pRateIncreaseCriteria_
      }

-- | The minimum number of things that will be notified of a pending job, per minute, at the start of the job rollout. This is the initial rate of the rollout.
ajerrBaseRatePerMinute :: Lens' AWSJobExponentialRolloutRate Natural
ajerrBaseRatePerMinute = lens _ajerrBaseRatePerMinute (\s a -> s {_ajerrBaseRatePerMinute = a}) . _Nat

-- | The rate of increase for a job rollout. The number of things notified is multiplied by this factor.
ajerrIncrementFactor :: Lens' AWSJobExponentialRolloutRate Double
ajerrIncrementFactor = lens _ajerrIncrementFactor (\s a -> s {_ajerrIncrementFactor = a})

-- | The criteria to initiate the increase in rate of rollout for a job. AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
ajerrRateIncreaseCriteria :: Lens' AWSJobExponentialRolloutRate AWSJobRateIncreaseCriteria
ajerrRateIncreaseCriteria = lens _ajerrRateIncreaseCriteria (\s a -> s {_ajerrRateIncreaseCriteria = a})

instance FromJSON AWSJobExponentialRolloutRate where
  parseJSON =
    withObject
      "AWSJobExponentialRolloutRate"
      ( \x ->
          AWSJobExponentialRolloutRate'
            <$> (x .: "baseRatePerMinute")
            <*> (x .: "incrementFactor")
            <*> (x .: "rateIncreaseCriteria")
      )

instance Hashable AWSJobExponentialRolloutRate

instance NFData AWSJobExponentialRolloutRate

instance ToJSON AWSJobExponentialRolloutRate where
  toJSON AWSJobExponentialRolloutRate' {..} =
    object
      ( catMaybes
          [ Just ("baseRatePerMinute" .= _ajerrBaseRatePerMinute),
            Just ("incrementFactor" .= _ajerrIncrementFactor),
            Just ("rateIncreaseCriteria" .= _ajerrRateIncreaseCriteria)
          ]
      )
