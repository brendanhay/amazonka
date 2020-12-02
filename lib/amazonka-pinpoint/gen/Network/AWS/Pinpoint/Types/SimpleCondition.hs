{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SimpleCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SimpleCondition where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EventCondition
import Network.AWS.Pinpoint.Types.SegmentCondition
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Prelude

-- | Specifies a condition to evaluate for an activity in a journey.
--
--
--
-- /See:/ 'simpleCondition' smart constructor.
data SimpleCondition = SimpleCondition'
  { _scSegmentDimensions ::
      !(Maybe SegmentDimensions),
    _scEventCondition :: !(Maybe EventCondition),
    _scSegmentCondition :: !(Maybe SegmentCondition)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SimpleCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scSegmentDimensions' - The dimension settings for the segment that's associated with the activity.
--
-- * 'scEventCondition' - The dimension settings for the event that's associated with the activity.
--
-- * 'scSegmentCondition' - The segment that's associated with the activity.
simpleCondition ::
  SimpleCondition
simpleCondition =
  SimpleCondition'
    { _scSegmentDimensions = Nothing,
      _scEventCondition = Nothing,
      _scSegmentCondition = Nothing
    }

-- | The dimension settings for the segment that's associated with the activity.
scSegmentDimensions :: Lens' SimpleCondition (Maybe SegmentDimensions)
scSegmentDimensions = lens _scSegmentDimensions (\s a -> s {_scSegmentDimensions = a})

-- | The dimension settings for the event that's associated with the activity.
scEventCondition :: Lens' SimpleCondition (Maybe EventCondition)
scEventCondition = lens _scEventCondition (\s a -> s {_scEventCondition = a})

-- | The segment that's associated with the activity.
scSegmentCondition :: Lens' SimpleCondition (Maybe SegmentCondition)
scSegmentCondition = lens _scSegmentCondition (\s a -> s {_scSegmentCondition = a})

instance FromJSON SimpleCondition where
  parseJSON =
    withObject
      "SimpleCondition"
      ( \x ->
          SimpleCondition'
            <$> (x .:? "segmentDimensions")
            <*> (x .:? "EventCondition")
            <*> (x .:? "SegmentCondition")
      )

instance Hashable SimpleCondition

instance NFData SimpleCondition

instance ToJSON SimpleCondition where
  toJSON SimpleCondition' {..} =
    object
      ( catMaybes
          [ ("segmentDimensions" .=) <$> _scSegmentDimensions,
            ("EventCondition" .=) <$> _scEventCondition,
            ("SegmentCondition" .=) <$> _scSegmentCondition
          ]
      )
