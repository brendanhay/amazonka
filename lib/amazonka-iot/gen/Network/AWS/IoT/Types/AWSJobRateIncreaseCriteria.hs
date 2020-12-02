{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobRateIncreaseCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobRateIncreaseCriteria where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The criteria to initiate the increase in rate of rollout for a job.
--
--
--
-- /See:/ 'awsJobRateIncreaseCriteria' smart constructor.
data AWSJobRateIncreaseCriteria = AWSJobRateIncreaseCriteria'
  { _ajricNumberOfNotifiedThings ::
      !(Maybe Nat),
    _ajricNumberOfSucceededThings ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSJobRateIncreaseCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajricNumberOfNotifiedThings' - When this number of things have been notified, it will initiate an increase in the rollout rate.
--
-- * 'ajricNumberOfSucceededThings' - When this number of things have succeeded in their job execution, it will initiate an increase in the rollout rate.
awsJobRateIncreaseCriteria ::
  AWSJobRateIncreaseCriteria
awsJobRateIncreaseCriteria =
  AWSJobRateIncreaseCriteria'
    { _ajricNumberOfNotifiedThings =
        Nothing,
      _ajricNumberOfSucceededThings = Nothing
    }

-- | When this number of things have been notified, it will initiate an increase in the rollout rate.
ajricNumberOfNotifiedThings :: Lens' AWSJobRateIncreaseCriteria (Maybe Natural)
ajricNumberOfNotifiedThings = lens _ajricNumberOfNotifiedThings (\s a -> s {_ajricNumberOfNotifiedThings = a}) . mapping _Nat

-- | When this number of things have succeeded in their job execution, it will initiate an increase in the rollout rate.
ajricNumberOfSucceededThings :: Lens' AWSJobRateIncreaseCriteria (Maybe Natural)
ajricNumberOfSucceededThings = lens _ajricNumberOfSucceededThings (\s a -> s {_ajricNumberOfSucceededThings = a}) . mapping _Nat

instance FromJSON AWSJobRateIncreaseCriteria where
  parseJSON =
    withObject
      "AWSJobRateIncreaseCriteria"
      ( \x ->
          AWSJobRateIncreaseCriteria'
            <$> (x .:? "numberOfNotifiedThings")
            <*> (x .:? "numberOfSucceededThings")
      )

instance Hashable AWSJobRateIncreaseCriteria

instance NFData AWSJobRateIncreaseCriteria

instance ToJSON AWSJobRateIncreaseCriteria where
  toJSON AWSJobRateIncreaseCriteria' {..} =
    object
      ( catMaybes
          [ ("numberOfNotifiedThings" .=) <$> _ajricNumberOfNotifiedThings,
            ("numberOfSucceededThings" .=) <$> _ajricNumberOfSucceededThings
          ]
      )
