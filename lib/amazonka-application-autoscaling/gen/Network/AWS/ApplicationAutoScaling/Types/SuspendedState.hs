{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.SuspendedState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.SuspendedState where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies whether the scaling activities for a scalable target are in a suspended state.
--
--
--
-- /See:/ 'suspendedState' smart constructor.
data SuspendedState = SuspendedState'
  { _ssDynamicScalingInSuspended ::
      !(Maybe Bool),
    _ssScheduledScalingSuspended :: !(Maybe Bool),
    _ssDynamicScalingOutSuspended :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuspendedState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssDynamicScalingInSuspended' - Whether scale in by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to remove capacity when a scaling policy is triggered. The default is @false@ .
--
-- * 'ssScheduledScalingSuspended' - Whether scheduled scaling is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add or remove capacity by initiating scheduled actions. The default is @false@ .
--
-- * 'ssDynamicScalingOutSuspended' - Whether scale out by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add capacity when a scaling policy is triggered. The default is @false@ .
suspendedState ::
  SuspendedState
suspendedState =
  SuspendedState'
    { _ssDynamicScalingInSuspended = Nothing,
      _ssScheduledScalingSuspended = Nothing,
      _ssDynamicScalingOutSuspended = Nothing
    }

-- | Whether scale in by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to remove capacity when a scaling policy is triggered. The default is @false@ .
ssDynamicScalingInSuspended :: Lens' SuspendedState (Maybe Bool)
ssDynamicScalingInSuspended = lens _ssDynamicScalingInSuspended (\s a -> s {_ssDynamicScalingInSuspended = a})

-- | Whether scheduled scaling is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add or remove capacity by initiating scheduled actions. The default is @false@ .
ssScheduledScalingSuspended :: Lens' SuspendedState (Maybe Bool)
ssScheduledScalingSuspended = lens _ssScheduledScalingSuspended (\s a -> s {_ssScheduledScalingSuspended = a})

-- | Whether scale out by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add capacity when a scaling policy is triggered. The default is @false@ .
ssDynamicScalingOutSuspended :: Lens' SuspendedState (Maybe Bool)
ssDynamicScalingOutSuspended = lens _ssDynamicScalingOutSuspended (\s a -> s {_ssDynamicScalingOutSuspended = a})

instance FromJSON SuspendedState where
  parseJSON =
    withObject
      "SuspendedState"
      ( \x ->
          SuspendedState'
            <$> (x .:? "DynamicScalingInSuspended")
            <*> (x .:? "ScheduledScalingSuspended")
            <*> (x .:? "DynamicScalingOutSuspended")
      )

instance Hashable SuspendedState

instance NFData SuspendedState

instance ToJSON SuspendedState where
  toJSON SuspendedState' {..} =
    object
      ( catMaybes
          [ ("DynamicScalingInSuspended" .=) <$> _ssDynamicScalingInSuspended,
            ("ScheduledScalingSuspended" .=) <$> _ssScheduledScalingSuspended,
            ("DynamicScalingOutSuspended" .=)
              <$> _ssDynamicScalingOutSuspended
          ]
      )
