{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventCondition where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EventDimensions
import Network.AWS.Prelude

-- | Specifies the conditions to evaluate for an event that applies to an activity in a journey.
--
--
--
-- /See:/ 'eventCondition' smart constructor.
data EventCondition = EventCondition'
  { _ecDimensions ::
      !(Maybe EventDimensions),
    _ecMessageActivity :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecDimensions' - The dimensions for the event filter to use for the activity.
--
-- * 'ecMessageActivity' - The message identifier (message_id) for the message to use when determining whether message events meet the condition.
eventCondition ::
  EventCondition
eventCondition =
  EventCondition'
    { _ecDimensions = Nothing,
      _ecMessageActivity = Nothing
    }

-- | The dimensions for the event filter to use for the activity.
ecDimensions :: Lens' EventCondition (Maybe EventDimensions)
ecDimensions = lens _ecDimensions (\s a -> s {_ecDimensions = a})

-- | The message identifier (message_id) for the message to use when determining whether message events meet the condition.
ecMessageActivity :: Lens' EventCondition (Maybe Text)
ecMessageActivity = lens _ecMessageActivity (\s a -> s {_ecMessageActivity = a})

instance FromJSON EventCondition where
  parseJSON =
    withObject
      "EventCondition"
      ( \x ->
          EventCondition'
            <$> (x .:? "Dimensions") <*> (x .:? "MessageActivity")
      )

instance Hashable EventCondition

instance NFData EventCondition

instance ToJSON EventCondition where
  toJSON EventCondition' {..} =
    object
      ( catMaybes
          [ ("Dimensions" .=) <$> _ecDimensions,
            ("MessageActivity" .=) <$> _ecMessageActivity
          ]
      )
