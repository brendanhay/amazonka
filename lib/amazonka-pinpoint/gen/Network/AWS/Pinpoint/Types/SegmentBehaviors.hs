{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentBehaviors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentBehaviors where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.RecencyDimension
import Network.AWS.Prelude

-- | Specifies dimension settings for including or excluding endpoints from a segment based on how recently an endpoint was active.
--
--
--
-- /See:/ 'segmentBehaviors' smart constructor.
newtype SegmentBehaviors = SegmentBehaviors'
  { _sbRecency ::
      Maybe RecencyDimension
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentBehaviors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbRecency' - The dimension settings that are based on how recently an endpoint was active.
segmentBehaviors ::
  SegmentBehaviors
segmentBehaviors = SegmentBehaviors' {_sbRecency = Nothing}

-- | The dimension settings that are based on how recently an endpoint was active.
sbRecency :: Lens' SegmentBehaviors (Maybe RecencyDimension)
sbRecency = lens _sbRecency (\s a -> s {_sbRecency = a})

instance FromJSON SegmentBehaviors where
  parseJSON =
    withObject
      "SegmentBehaviors"
      (\x -> SegmentBehaviors' <$> (x .:? "Recency"))

instance Hashable SegmentBehaviors

instance NFData SegmentBehaviors

instance ToJSON SegmentBehaviors where
  toJSON SegmentBehaviors' {..} =
    object (catMaybes [("Recency" .=) <$> _sbRecency])
