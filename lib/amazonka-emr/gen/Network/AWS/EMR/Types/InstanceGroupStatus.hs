{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupStatus where

import Network.AWS.EMR.Types.InstanceGroupState
import Network.AWS.EMR.Types.InstanceGroupStateChangeReason
import Network.AWS.EMR.Types.InstanceGroupTimeline
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of the instance group status.
--
--
--
-- /See:/ 'instanceGroupStatus' smart constructor.
data InstanceGroupStatus = InstanceGroupStatus'
  { _igsState ::
      !(Maybe InstanceGroupState),
    _igsStateChangeReason ::
      !(Maybe InstanceGroupStateChangeReason),
    _igsTimeline :: !(Maybe InstanceGroupTimeline)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igsState' - The current state of the instance group.
--
-- * 'igsStateChangeReason' - The status change reason details for the instance group.
--
-- * 'igsTimeline' - The timeline of the instance group status over time.
instanceGroupStatus ::
  InstanceGroupStatus
instanceGroupStatus =
  InstanceGroupStatus'
    { _igsState = Nothing,
      _igsStateChangeReason = Nothing,
      _igsTimeline = Nothing
    }

-- | The current state of the instance group.
igsState :: Lens' InstanceGroupStatus (Maybe InstanceGroupState)
igsState = lens _igsState (\s a -> s {_igsState = a})

-- | The status change reason details for the instance group.
igsStateChangeReason :: Lens' InstanceGroupStatus (Maybe InstanceGroupStateChangeReason)
igsStateChangeReason = lens _igsStateChangeReason (\s a -> s {_igsStateChangeReason = a})

-- | The timeline of the instance group status over time.
igsTimeline :: Lens' InstanceGroupStatus (Maybe InstanceGroupTimeline)
igsTimeline = lens _igsTimeline (\s a -> s {_igsTimeline = a})

instance FromJSON InstanceGroupStatus where
  parseJSON =
    withObject
      "InstanceGroupStatus"
      ( \x ->
          InstanceGroupStatus'
            <$> (x .:? "State")
            <*> (x .:? "StateChangeReason")
            <*> (x .:? "Timeline")
      )

instance Hashable InstanceGroupStatus

instance NFData InstanceGroupStatus
