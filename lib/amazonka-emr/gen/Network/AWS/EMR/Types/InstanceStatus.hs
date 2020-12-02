{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceStatus where

import Network.AWS.EMR.Types.InstanceState
import Network.AWS.EMR.Types.InstanceStateChangeReason
import Network.AWS.EMR.Types.InstanceTimeline
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The instance status details.
--
--
--
-- /See:/ 'instanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { _isState ::
      !(Maybe InstanceState),
    _isStateChangeReason :: !(Maybe InstanceStateChangeReason),
    _isTimeline :: !(Maybe InstanceTimeline)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isState' - The current state of the instance.
--
-- * 'isStateChangeReason' - The details of the status change reason for the instance.
--
-- * 'isTimeline' - The timeline of the instance status over time.
instanceStatus ::
  InstanceStatus
instanceStatus =
  InstanceStatus'
    { _isState = Nothing,
      _isStateChangeReason = Nothing,
      _isTimeline = Nothing
    }

-- | The current state of the instance.
isState :: Lens' InstanceStatus (Maybe InstanceState)
isState = lens _isState (\s a -> s {_isState = a})

-- | The details of the status change reason for the instance.
isStateChangeReason :: Lens' InstanceStatus (Maybe InstanceStateChangeReason)
isStateChangeReason = lens _isStateChangeReason (\s a -> s {_isStateChangeReason = a})

-- | The timeline of the instance status over time.
isTimeline :: Lens' InstanceStatus (Maybe InstanceTimeline)
isTimeline = lens _isTimeline (\s a -> s {_isTimeline = a})

instance FromJSON InstanceStatus where
  parseJSON =
    withObject
      "InstanceStatus"
      ( \x ->
          InstanceStatus'
            <$> (x .:? "State")
            <*> (x .:? "StateChangeReason")
            <*> (x .:? "Timeline")
      )

instance Hashable InstanceStatus

instance NFData InstanceStatus
