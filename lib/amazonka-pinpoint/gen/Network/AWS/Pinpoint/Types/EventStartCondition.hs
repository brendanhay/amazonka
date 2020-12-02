{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventStartCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventStartCondition where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EventFilter
import Network.AWS.Prelude

-- | Specifies the settings for an event that causes a journey activity to start.
--
--
--
-- /See:/ 'eventStartCondition' smart constructor.
data EventStartCondition = EventStartCondition'
  { _escEventFilter ::
      !(Maybe EventFilter),
    _escSegmentId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventStartCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'escEventFilter' - Undocumented member.
--
-- * 'escSegmentId' - Undocumented member.
eventStartCondition ::
  EventStartCondition
eventStartCondition =
  EventStartCondition'
    { _escEventFilter = Nothing,
      _escSegmentId = Nothing
    }

-- | Undocumented member.
escEventFilter :: Lens' EventStartCondition (Maybe EventFilter)
escEventFilter = lens _escEventFilter (\s a -> s {_escEventFilter = a})

-- | Undocumented member.
escSegmentId :: Lens' EventStartCondition (Maybe Text)
escSegmentId = lens _escSegmentId (\s a -> s {_escSegmentId = a})

instance FromJSON EventStartCondition where
  parseJSON =
    withObject
      "EventStartCondition"
      ( \x ->
          EventStartCondition'
            <$> (x .:? "EventFilter") <*> (x .:? "SegmentId")
      )

instance Hashable EventStartCondition

instance NFData EventStartCondition

instance ToJSON EventStartCondition where
  toJSON EventStartCondition' {..} =
    object
      ( catMaybes
          [ ("EventFilter" .=) <$> _escEventFilter,
            ("SegmentId" .=) <$> _escSegmentId
          ]
      )
