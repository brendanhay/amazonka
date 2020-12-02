{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FeatureActivations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FeatureActivations where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions
import Network.AWS.Prelude

-- | Feature Activations
--
-- /See:/ 'featureActivations' smart constructor.
newtype FeatureActivations = FeatureActivations'
  { _faInputPrepareScheduleActions ::
      Maybe
        FeatureActivationsInputPrepareScheduleActions
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FeatureActivations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faInputPrepareScheduleActions' - Enables the Input Prepare feature. You can create Input Prepare actions in the schedule only if this feature is enabled. If you disable the feature on an existing schedule, make sure that you first delete all input prepare actions from the schedule.
featureActivations ::
  FeatureActivations
featureActivations =
  FeatureActivations' {_faInputPrepareScheduleActions = Nothing}

-- | Enables the Input Prepare feature. You can create Input Prepare actions in the schedule only if this feature is enabled. If you disable the feature on an existing schedule, make sure that you first delete all input prepare actions from the schedule.
faInputPrepareScheduleActions :: Lens' FeatureActivations (Maybe FeatureActivationsInputPrepareScheduleActions)
faInputPrepareScheduleActions = lens _faInputPrepareScheduleActions (\s a -> s {_faInputPrepareScheduleActions = a})

instance FromJSON FeatureActivations where
  parseJSON =
    withObject
      "FeatureActivations"
      ( \x ->
          FeatureActivations' <$> (x .:? "inputPrepareScheduleActions")
      )

instance Hashable FeatureActivations

instance NFData FeatureActivations

instance ToJSON FeatureActivations where
  toJSON FeatureActivations' {..} =
    object
      ( catMaybes
          [ ("inputPrepareScheduleActions" .=)
              <$> _faInputPrepareScheduleActions
          ]
      )
