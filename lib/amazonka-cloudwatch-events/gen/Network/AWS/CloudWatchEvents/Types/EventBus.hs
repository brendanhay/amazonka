{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.EventBus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.EventBus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An event bus receives events from a source and routes them to rules associated with that event bus. Your account's default event bus receives rules from AWS services. A custom event bus can receive rules from AWS services as well as your custom applications and services. A partner event bus receives events from an event source created by an SaaS partner. These events come from the partners services or applications.
--
--
--
-- /See:/ 'eventBus' smart constructor.
data EventBus = EventBus'
  { _ebARN :: !(Maybe Text),
    _ebName :: !(Maybe Text),
    _ebPolicy :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventBus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebARN' - The ARN of the event bus.
--
-- * 'ebName' - The name of the event bus.
--
-- * 'ebPolicy' - The permissions policy of the event bus, describing which other AWS accounts can write events to this event bus.
eventBus ::
  EventBus
eventBus =
  EventBus'
    { _ebARN = Nothing,
      _ebName = Nothing,
      _ebPolicy = Nothing
    }

-- | The ARN of the event bus.
ebARN :: Lens' EventBus (Maybe Text)
ebARN = lens _ebARN (\s a -> s {_ebARN = a})

-- | The name of the event bus.
ebName :: Lens' EventBus (Maybe Text)
ebName = lens _ebName (\s a -> s {_ebName = a})

-- | The permissions policy of the event bus, describing which other AWS accounts can write events to this event bus.
ebPolicy :: Lens' EventBus (Maybe Text)
ebPolicy = lens _ebPolicy (\s a -> s {_ebPolicy = a})

instance FromJSON EventBus where
  parseJSON =
    withObject
      "EventBus"
      ( \x ->
          EventBus'
            <$> (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Policy")
      )

instance Hashable EventBus

instance NFData EventBus
