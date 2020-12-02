{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTimeline where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The timeline of the instance lifecycle.
--
--
--
-- /See:/ 'instanceTimeline' smart constructor.
data InstanceTimeline = InstanceTimeline'
  { _itReadyDateTime ::
      !(Maybe POSIX),
    _itCreationDateTime :: !(Maybe POSIX),
    _itEndDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceTimeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itReadyDateTime' - The date and time when the instance was ready to perform tasks.
--
-- * 'itCreationDateTime' - The creation date and time of the instance.
--
-- * 'itEndDateTime' - The date and time when the instance was terminated.
instanceTimeline ::
  InstanceTimeline
instanceTimeline =
  InstanceTimeline'
    { _itReadyDateTime = Nothing,
      _itCreationDateTime = Nothing,
      _itEndDateTime = Nothing
    }

-- | The date and time when the instance was ready to perform tasks.
itReadyDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itReadyDateTime = lens _itReadyDateTime (\s a -> s {_itReadyDateTime = a}) . mapping _Time

-- | The creation date and time of the instance.
itCreationDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itCreationDateTime = lens _itCreationDateTime (\s a -> s {_itCreationDateTime = a}) . mapping _Time

-- | The date and time when the instance was terminated.
itEndDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itEndDateTime = lens _itEndDateTime (\s a -> s {_itEndDateTime = a}) . mapping _Time

instance FromJSON InstanceTimeline where
  parseJSON =
    withObject
      "InstanceTimeline"
      ( \x ->
          InstanceTimeline'
            <$> (x .:? "ReadyDateTime")
            <*> (x .:? "CreationDateTime")
            <*> (x .:? "EndDateTime")
      )

instance Hashable InstanceTimeline

instance NFData InstanceTimeline
