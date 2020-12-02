{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventDetails where

import Network.AWS.AWSHealth.Types.Event
import Network.AWS.AWSHealth.Types.EventDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detailed information about an event. A combination of an <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> object, an <https://docs.aws.amazon.com/health/latest/APIReference/API_EventDescription.html EventDescription> object, and additional metadata about the event. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails> operation.
--
--
--
-- /See:/ 'eventDetails' smart constructor.
data EventDetails = EventDetails'
  { _edEvent :: !(Maybe Event),
    _edEventDescription :: !(Maybe EventDescription),
    _edEventMetadata :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edEvent' - Summary information about the event.
--
-- * 'edEventDescription' - The most recent description of the event.
--
-- * 'edEventMetadata' - Additional metadata about the event.
eventDetails ::
  EventDetails
eventDetails =
  EventDetails'
    { _edEvent = Nothing,
      _edEventDescription = Nothing,
      _edEventMetadata = Nothing
    }

-- | Summary information about the event.
edEvent :: Lens' EventDetails (Maybe Event)
edEvent = lens _edEvent (\s a -> s {_edEvent = a})

-- | The most recent description of the event.
edEventDescription :: Lens' EventDetails (Maybe EventDescription)
edEventDescription = lens _edEventDescription (\s a -> s {_edEventDescription = a})

-- | Additional metadata about the event.
edEventMetadata :: Lens' EventDetails (HashMap Text (Text))
edEventMetadata = lens _edEventMetadata (\s a -> s {_edEventMetadata = a}) . _Default . _Map

instance FromJSON EventDetails where
  parseJSON =
    withObject
      "EventDetails"
      ( \x ->
          EventDetails'
            <$> (x .:? "event")
            <*> (x .:? "eventDescription")
            <*> (x .:? "eventMetadata" .!= mempty)
      )

instance Hashable EventDetails

instance NFData EventDetails
