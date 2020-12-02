{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentCondition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a segment to associate with an activity in a journey.
--
--
--
-- /See:/ 'segmentCondition' smart constructor.
newtype SegmentCondition = SegmentCondition' {_scSegmentId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scSegmentId' - The unique identifier for the segment to associate with the activity.
segmentCondition ::
  -- | 'scSegmentId'
  Text ->
  SegmentCondition
segmentCondition pSegmentId_ =
  SegmentCondition' {_scSegmentId = pSegmentId_}

-- | The unique identifier for the segment to associate with the activity.
scSegmentId :: Lens' SegmentCondition Text
scSegmentId = lens _scSegmentId (\s a -> s {_scSegmentId = a})

instance FromJSON SegmentCondition where
  parseJSON =
    withObject
      "SegmentCondition"
      (\x -> SegmentCondition' <$> (x .: "SegmentId"))

instance Hashable SegmentCondition

instance NFData SegmentCondition

instance ToJSON SegmentCondition where
  toJSON SegmentCondition' {..} =
    object (catMaybes [Just ("SegmentId" .= _scSegmentId)])
