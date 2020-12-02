{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RateIncreaseCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RateIncreaseCriteria where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Allows you to define a criteria to initiate the increase in rate of rollout for a job.
--
--
--
-- /See:/ 'rateIncreaseCriteria' smart constructor.
data RateIncreaseCriteria = RateIncreaseCriteria'
  { _ricNumberOfNotifiedThings ::
      !(Maybe Nat),
    _ricNumberOfSucceededThings :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RateIncreaseCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ricNumberOfNotifiedThings' - The threshold for number of notified things that will initiate the increase in rate of rollout.
--
-- * 'ricNumberOfSucceededThings' - The threshold for number of succeeded things that will initiate the increase in rate of rollout.
rateIncreaseCriteria ::
  RateIncreaseCriteria
rateIncreaseCriteria =
  RateIncreaseCriteria'
    { _ricNumberOfNotifiedThings = Nothing,
      _ricNumberOfSucceededThings = Nothing
    }

-- | The threshold for number of notified things that will initiate the increase in rate of rollout.
ricNumberOfNotifiedThings :: Lens' RateIncreaseCriteria (Maybe Natural)
ricNumberOfNotifiedThings = lens _ricNumberOfNotifiedThings (\s a -> s {_ricNumberOfNotifiedThings = a}) . mapping _Nat

-- | The threshold for number of succeeded things that will initiate the increase in rate of rollout.
ricNumberOfSucceededThings :: Lens' RateIncreaseCriteria (Maybe Natural)
ricNumberOfSucceededThings = lens _ricNumberOfSucceededThings (\s a -> s {_ricNumberOfSucceededThings = a}) . mapping _Nat

instance FromJSON RateIncreaseCriteria where
  parseJSON =
    withObject
      "RateIncreaseCriteria"
      ( \x ->
          RateIncreaseCriteria'
            <$> (x .:? "numberOfNotifiedThings")
            <*> (x .:? "numberOfSucceededThings")
      )

instance Hashable RateIncreaseCriteria

instance NFData RateIncreaseCriteria

instance ToJSON RateIncreaseCriteria where
  toJSON RateIncreaseCriteria' {..} =
    object
      ( catMaybes
          [ ("numberOfNotifiedThings" .=) <$> _ricNumberOfNotifiedThings,
            ("numberOfSucceededThings" .=) <$> _ricNumberOfSucceededThings
          ]
      )
