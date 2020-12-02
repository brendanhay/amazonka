{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.EventInfoMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EventInfoMap where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes event information.
--
--
--
-- /See:/ 'eventInfoMap' smart constructor.
data EventInfoMap = EventInfoMap'
  { _eimEventDescription ::
      !(Maybe Text),
    _eimSeverity :: !(Maybe Text),
    _eimEventCategories :: !(Maybe [Text]),
    _eimEventId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventInfoMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eimEventDescription' - The description of an Amazon Redshift event.
--
-- * 'eimSeverity' - The severity of the event. Values: ERROR, INFO
--
-- * 'eimEventCategories' - The category of an Amazon Redshift event.
--
-- * 'eimEventId' - The identifier of an Amazon Redshift event.
eventInfoMap ::
  EventInfoMap
eventInfoMap =
  EventInfoMap'
    { _eimEventDescription = Nothing,
      _eimSeverity = Nothing,
      _eimEventCategories = Nothing,
      _eimEventId = Nothing
    }

-- | The description of an Amazon Redshift event.
eimEventDescription :: Lens' EventInfoMap (Maybe Text)
eimEventDescription = lens _eimEventDescription (\s a -> s {_eimEventDescription = a})

-- | The severity of the event. Values: ERROR, INFO
eimSeverity :: Lens' EventInfoMap (Maybe Text)
eimSeverity = lens _eimSeverity (\s a -> s {_eimSeverity = a})

-- | The category of an Amazon Redshift event.
eimEventCategories :: Lens' EventInfoMap [Text]
eimEventCategories = lens _eimEventCategories (\s a -> s {_eimEventCategories = a}) . _Default . _Coerce

-- | The identifier of an Amazon Redshift event.
eimEventId :: Lens' EventInfoMap (Maybe Text)
eimEventId = lens _eimEventId (\s a -> s {_eimEventId = a})

instance FromXML EventInfoMap where
  parseXML x =
    EventInfoMap'
      <$> (x .@? "EventDescription")
      <*> (x .@? "Severity")
      <*> ( x .@? "EventCategories" .!@ mempty
              >>= may (parseXMLList "EventCategory")
          )
      <*> (x .@? "EventId")

instance Hashable EventInfoMap

instance NFData EventInfoMap
