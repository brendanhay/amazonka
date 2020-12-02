{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventFilter where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EventDimensions
import Network.AWS.Pinpoint.Types.FilterType
import Network.AWS.Prelude

-- | Specifies the settings for an event that causes a campaign to be sent or a journey activity to be performed.
--
--
--
-- /See:/ 'eventFilter' smart constructor.
data EventFilter = EventFilter'
  { _efFilterType :: !FilterType,
    _efDimensions :: !EventDimensions
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efFilterType' - The type of event that causes the campaign to be sent or the journey activity to be performed. Valid values are: SYSTEM, sends the campaign or performs the activity when a system event occurs; and, ENDPOINT, sends the campaign or performs the activity when an endpoint event (<link>Events resource) occurs.
--
-- * 'efDimensions' - The dimensions for the event filter to use for the campaign or the journey activity.
eventFilter ::
  -- | 'efFilterType'
  FilterType ->
  -- | 'efDimensions'
  EventDimensions ->
  EventFilter
eventFilter pFilterType_ pDimensions_ =
  EventFilter'
    { _efFilterType = pFilterType_,
      _efDimensions = pDimensions_
    }

-- | The type of event that causes the campaign to be sent or the journey activity to be performed. Valid values are: SYSTEM, sends the campaign or performs the activity when a system event occurs; and, ENDPOINT, sends the campaign or performs the activity when an endpoint event (<link>Events resource) occurs.
efFilterType :: Lens' EventFilter FilterType
efFilterType = lens _efFilterType (\s a -> s {_efFilterType = a})

-- | The dimensions for the event filter to use for the campaign or the journey activity.
efDimensions :: Lens' EventFilter EventDimensions
efDimensions = lens _efDimensions (\s a -> s {_efDimensions = a})

instance FromJSON EventFilter where
  parseJSON =
    withObject
      "EventFilter"
      ( \x ->
          EventFilter' <$> (x .: "FilterType") <*> (x .: "Dimensions")
      )

instance Hashable EventFilter

instance NFData EventFilter

instance ToJSON EventFilter where
  toJSON EventFilter' {..} =
    object
      ( catMaybes
          [ Just ("FilterType" .= _efFilterType),
            Just ("Dimensions" .= _efDimensions)
          ]
      )
