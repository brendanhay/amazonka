{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupTimeline where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The timeline of the instance group lifecycle.
--
--
--
-- /See:/ 'instanceGroupTimeline' smart constructor.
data InstanceGroupTimeline = InstanceGroupTimeline'
  { _igtReadyDateTime ::
      !(Maybe POSIX),
    _igtCreationDateTime :: !(Maybe POSIX),
    _igtEndDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceGroupTimeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igtReadyDateTime' - The date and time when the instance group became ready to perform tasks.
--
-- * 'igtCreationDateTime' - The creation date and time of the instance group.
--
-- * 'igtEndDateTime' - The date and time when the instance group terminated.
instanceGroupTimeline ::
  InstanceGroupTimeline
instanceGroupTimeline =
  InstanceGroupTimeline'
    { _igtReadyDateTime = Nothing,
      _igtCreationDateTime = Nothing,
      _igtEndDateTime = Nothing
    }

-- | The date and time when the instance group became ready to perform tasks.
igtReadyDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtReadyDateTime = lens _igtReadyDateTime (\s a -> s {_igtReadyDateTime = a}) . mapping _Time

-- | The creation date and time of the instance group.
igtCreationDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtCreationDateTime = lens _igtCreationDateTime (\s a -> s {_igtCreationDateTime = a}) . mapping _Time

-- | The date and time when the instance group terminated.
igtEndDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtEndDateTime = lens _igtEndDateTime (\s a -> s {_igtEndDateTime = a}) . mapping _Time

instance FromJSON InstanceGroupTimeline where
  parseJSON =
    withObject
      "InstanceGroupTimeline"
      ( \x ->
          InstanceGroupTimeline'
            <$> (x .:? "ReadyDateTime")
            <*> (x .:? "CreationDateTime")
            <*> (x .:? "EndDateTime")
      )

instance Hashable InstanceGroupTimeline

instance NFData InstanceGroupTimeline
