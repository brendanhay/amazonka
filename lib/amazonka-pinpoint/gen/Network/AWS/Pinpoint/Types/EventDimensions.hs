{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventDimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventDimensions where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.AttributeDimension
import Network.AWS.Pinpoint.Types.MetricDimension
import Network.AWS.Pinpoint.Types.SetDimension
import Network.AWS.Prelude

-- | Specifies the dimensions for an event filter that determines when a campaign is sent or a journey activity is performed.
--
--
--
-- /See:/ 'eventDimensions' smart constructor.
data EventDimensions = EventDimensions'
  { _edMetrics ::
      !(Maybe (Map Text (MetricDimension))),
    _edEventType :: !(Maybe SetDimension),
    _edAttributes :: !(Maybe (Map Text (AttributeDimension)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventDimensions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edMetrics' - One or more custom metrics that your application reports to Amazon Pinpoint. You can use these metrics as selection criteria when you create an event filter.
--
-- * 'edEventType' - The name of the event that causes the campaign to be sent or the journey activity to be performed. This can be a standard event that Amazon Pinpoint generates, such as _email.delivered. For campaigns, this can also be a custom event that's specific to your application. For information about standard events, see <https://docs.aws.amazon.com/pinpoint/latest/developerguide/event-streams.html Streaming Amazon Pinpoint Events> in the /Amazon Pinpoint Developer Guide/ .
--
-- * 'edAttributes' - One or more custom attributes that your application reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create an event filter.
eventDimensions ::
  EventDimensions
eventDimensions =
  EventDimensions'
    { _edMetrics = Nothing,
      _edEventType = Nothing,
      _edAttributes = Nothing
    }

-- | One or more custom metrics that your application reports to Amazon Pinpoint. You can use these metrics as selection criteria when you create an event filter.
edMetrics :: Lens' EventDimensions (HashMap Text (MetricDimension))
edMetrics = lens _edMetrics (\s a -> s {_edMetrics = a}) . _Default . _Map

-- | The name of the event that causes the campaign to be sent or the journey activity to be performed. This can be a standard event that Amazon Pinpoint generates, such as _email.delivered. For campaigns, this can also be a custom event that's specific to your application. For information about standard events, see <https://docs.aws.amazon.com/pinpoint/latest/developerguide/event-streams.html Streaming Amazon Pinpoint Events> in the /Amazon Pinpoint Developer Guide/ .
edEventType :: Lens' EventDimensions (Maybe SetDimension)
edEventType = lens _edEventType (\s a -> s {_edEventType = a})

-- | One or more custom attributes that your application reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create an event filter.
edAttributes :: Lens' EventDimensions (HashMap Text (AttributeDimension))
edAttributes = lens _edAttributes (\s a -> s {_edAttributes = a}) . _Default . _Map

instance FromJSON EventDimensions where
  parseJSON =
    withObject
      "EventDimensions"
      ( \x ->
          EventDimensions'
            <$> (x .:? "Metrics" .!= mempty)
            <*> (x .:? "EventType")
            <*> (x .:? "Attributes" .!= mempty)
      )

instance Hashable EventDimensions

instance NFData EventDimensions

instance ToJSON EventDimensions where
  toJSON EventDimensions' {..} =
    object
      ( catMaybes
          [ ("Metrics" .=) <$> _edMetrics,
            ("EventType" .=) <$> _edEventType,
            ("Attributes" .=) <$> _edAttributes
          ]
      )
