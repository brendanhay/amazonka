{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Event where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SourceType

-- | Describes an event.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eSourceType :: !(Maybe SourceType),
    _eSeverity :: !(Maybe Text),
    _eSourceIdentifier :: !(Maybe Text),
    _eDate :: !(Maybe ISO8601),
    _eEventCategories :: !(Maybe [Text]),
    _eMessage :: !(Maybe Text),
    _eEventId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceType' - The source type for this event.
--
-- * 'eSeverity' - The severity of the event. Values: ERROR, INFO
--
-- * 'eSourceIdentifier' - The identifier for the source of the event.
--
-- * 'eDate' - The date and time of the event.
--
-- * 'eEventCategories' - A list of the event categories. Values: Configuration, Management, Monitoring, Security
--
-- * 'eMessage' - The text of this event.
--
-- * 'eEventId' - The identifier of the event.
event ::
  Event
event =
  Event'
    { _eSourceType = Nothing,
      _eSeverity = Nothing,
      _eSourceIdentifier = Nothing,
      _eDate = Nothing,
      _eEventCategories = Nothing,
      _eMessage = Nothing,
      _eEventId = Nothing
    }

-- | The source type for this event.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\s a -> s {_eSourceType = a})

-- | The severity of the event. Values: ERROR, INFO
eSeverity :: Lens' Event (Maybe Text)
eSeverity = lens _eSeverity (\s a -> s {_eSeverity = a})

-- | The identifier for the source of the event.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier = lens _eSourceIdentifier (\s a -> s {_eSourceIdentifier = a})

-- | The date and time of the event.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\s a -> s {_eDate = a}) . mapping _Time

-- | A list of the event categories. Values: Configuration, Management, Monitoring, Security
eEventCategories :: Lens' Event [Text]
eEventCategories = lens _eEventCategories (\s a -> s {_eEventCategories = a}) . _Default . _Coerce

-- | The text of this event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\s a -> s {_eMessage = a})

-- | The identifier of the event.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\s a -> s {_eEventId = a})

instance FromXML Event where
  parseXML x =
    Event'
      <$> (x .@? "SourceType")
      <*> (x .@? "Severity")
      <*> (x .@? "SourceIdentifier")
      <*> (x .@? "Date")
      <*> ( x .@? "EventCategories" .!@ mempty
              >>= may (parseXMLList "EventCategory")
          )
      <*> (x .@? "Message")
      <*> (x .@? "EventId")

instance Hashable Event

instance NFData Event
