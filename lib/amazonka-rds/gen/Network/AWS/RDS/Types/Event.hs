{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Event where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.SourceType

-- | This data type is used as a response element in the @DescribeEvents@ action.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eSourceType :: !(Maybe SourceType),
    _eSourceARN :: !(Maybe Text),
    _eSourceIdentifier :: !(Maybe Text),
    _eDate :: !(Maybe ISO8601),
    _eEventCategories :: !(Maybe [Text]),
    _eMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceType' - Specifies the source type for this event.
--
-- * 'eSourceARN' - The Amazon Resource Name (ARN) for the event.
--
-- * 'eSourceIdentifier' - Provides the identifier for the source of the event.
--
-- * 'eDate' - Specifies the date and time of the event.
--
-- * 'eEventCategories' - Specifies the category for the event.
--
-- * 'eMessage' - Provides the text of this event.
event ::
  Event
event =
  Event'
    { _eSourceType = Nothing,
      _eSourceARN = Nothing,
      _eSourceIdentifier = Nothing,
      _eDate = Nothing,
      _eEventCategories = Nothing,
      _eMessage = Nothing
    }

-- | Specifies the source type for this event.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\s a -> s {_eSourceType = a})

-- | The Amazon Resource Name (ARN) for the event.
eSourceARN :: Lens' Event (Maybe Text)
eSourceARN = lens _eSourceARN (\s a -> s {_eSourceARN = a})

-- | Provides the identifier for the source of the event.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier = lens _eSourceIdentifier (\s a -> s {_eSourceIdentifier = a})

-- | Specifies the date and time of the event.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\s a -> s {_eDate = a}) . mapping _Time

-- | Specifies the category for the event.
eEventCategories :: Lens' Event [Text]
eEventCategories = lens _eEventCategories (\s a -> s {_eEventCategories = a}) . _Default . _Coerce

-- | Provides the text of this event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\s a -> s {_eMessage = a})

instance FromXML Event where
  parseXML x =
    Event'
      <$> (x .@? "SourceType")
      <*> (x .@? "SourceArn")
      <*> (x .@? "SourceIdentifier")
      <*> (x .@? "Date")
      <*> ( x .@? "EventCategories" .!@ mempty
              >>= may (parseXMLList "EventCategory")
          )
      <*> (x .@? "Message")

instance Hashable Event

instance NFData Event
