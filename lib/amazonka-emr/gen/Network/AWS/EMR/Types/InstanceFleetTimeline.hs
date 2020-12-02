{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetTimeline where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
--
--
--
-- /See:/ 'instanceFleetTimeline' smart constructor.
data InstanceFleetTimeline = InstanceFleetTimeline'
  { _iftReadyDateTime ::
      !(Maybe POSIX),
    _iftCreationDateTime :: !(Maybe POSIX),
    _iftEndDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceFleetTimeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iftReadyDateTime' - The time and date the instance fleet was ready to run jobs.
--
-- * 'iftCreationDateTime' - The time and date the instance fleet was created.
--
-- * 'iftEndDateTime' - The time and date the instance fleet terminated.
instanceFleetTimeline ::
  InstanceFleetTimeline
instanceFleetTimeline =
  InstanceFleetTimeline'
    { _iftReadyDateTime = Nothing,
      _iftCreationDateTime = Nothing,
      _iftEndDateTime = Nothing
    }

-- | The time and date the instance fleet was ready to run jobs.
iftReadyDateTime :: Lens' InstanceFleetTimeline (Maybe UTCTime)
iftReadyDateTime = lens _iftReadyDateTime (\s a -> s {_iftReadyDateTime = a}) . mapping _Time

-- | The time and date the instance fleet was created.
iftCreationDateTime :: Lens' InstanceFleetTimeline (Maybe UTCTime)
iftCreationDateTime = lens _iftCreationDateTime (\s a -> s {_iftCreationDateTime = a}) . mapping _Time

-- | The time and date the instance fleet terminated.
iftEndDateTime :: Lens' InstanceFleetTimeline (Maybe UTCTime)
iftEndDateTime = lens _iftEndDateTime (\s a -> s {_iftEndDateTime = a}) . mapping _Time

instance FromJSON InstanceFleetTimeline where
  parseJSON =
    withObject
      "InstanceFleetTimeline"
      ( \x ->
          InstanceFleetTimeline'
            <$> (x .:? "ReadyDateTime")
            <*> (x .:? "CreationDateTime")
            <*> (x .:? "EndDateTime")
      )

instance Hashable InstanceFleetTimeline

instance NFData InstanceFleetTimeline
