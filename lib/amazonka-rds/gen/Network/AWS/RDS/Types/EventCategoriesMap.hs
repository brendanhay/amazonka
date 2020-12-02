{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EventCategoriesMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EventCategoriesMap where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the results of a successful invocation of the @DescribeEventCategories@ operation.
--
--
--
-- /See:/ 'eventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { _ecmSourceType ::
      !(Maybe Text),
    _ecmEventCategories :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventCategoriesMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecmSourceType' - The source type that the returned categories belong to
--
-- * 'ecmEventCategories' - The event categories for the specified source type
eventCategoriesMap ::
  EventCategoriesMap
eventCategoriesMap =
  EventCategoriesMap'
    { _ecmSourceType = Nothing,
      _ecmEventCategories = Nothing
    }

-- | The source type that the returned categories belong to
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\s a -> s {_ecmSourceType = a})

-- | The event categories for the specified source type
ecmEventCategories :: Lens' EventCategoriesMap [Text]
ecmEventCategories = lens _ecmEventCategories (\s a -> s {_ecmEventCategories = a}) . _Default . _Coerce

instance FromXML EventCategoriesMap where
  parseXML x =
    EventCategoriesMap'
      <$> (x .@? "SourceType")
      <*> ( x .@? "EventCategories" .!@ mempty
              >>= may (parseXMLList "EventCategory")
          )

instance Hashable EventCategoriesMap

instance NFData EventCategoriesMap
