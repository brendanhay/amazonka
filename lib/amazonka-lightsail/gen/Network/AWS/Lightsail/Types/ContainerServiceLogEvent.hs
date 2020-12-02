{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceLogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceLogEvent where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the log events of a container of an Amazon Lightsail container service.
--
--
--
-- /See:/ 'containerServiceLogEvent' smart constructor.
data ContainerServiceLogEvent = ContainerServiceLogEvent'
  { _csleCreatedAt ::
      !(Maybe POSIX),
    _csleMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerServiceLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csleCreatedAt' - The timestamp when the container service log event was created.
--
-- * 'csleMessage' - The message of the container service log event.
containerServiceLogEvent ::
  ContainerServiceLogEvent
containerServiceLogEvent =
  ContainerServiceLogEvent'
    { _csleCreatedAt = Nothing,
      _csleMessage = Nothing
    }

-- | The timestamp when the container service log event was created.
csleCreatedAt :: Lens' ContainerServiceLogEvent (Maybe UTCTime)
csleCreatedAt = lens _csleCreatedAt (\s a -> s {_csleCreatedAt = a}) . mapping _Time

-- | The message of the container service log event.
csleMessage :: Lens' ContainerServiceLogEvent (Maybe Text)
csleMessage = lens _csleMessage (\s a -> s {_csleMessage = a})

instance FromJSON ContainerServiceLogEvent where
  parseJSON =
    withObject
      "ContainerServiceLogEvent"
      ( \x ->
          ContainerServiceLogEvent'
            <$> (x .:? "createdAt") <*> (x .:? "message")
      )

instance Hashable ContainerServiceLogEvent

instance NFData ContainerServiceLogEvent
