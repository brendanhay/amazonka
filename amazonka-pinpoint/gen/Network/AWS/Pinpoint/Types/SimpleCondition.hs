{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SimpleCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SimpleCondition where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventCondition
import Network.AWS.Pinpoint.Types.SegmentCondition
import Network.AWS.Pinpoint.Types.SegmentDimensions
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a condition to evaluate for an activity in a journey.
--
-- /See:/ 'newSimpleCondition' smart constructor.
data SimpleCondition = SimpleCondition'
  { -- | The dimension settings for the event that\'s associated with the
    -- activity.
    eventCondition :: Prelude.Maybe EventCondition,
    -- | The dimension settings for the segment that\'s associated with the
    -- activity.
    segmentDimensions :: Prelude.Maybe SegmentDimensions,
    -- | The segment that\'s associated with the activity.
    segmentCondition :: Prelude.Maybe SegmentCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SimpleCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventCondition', 'simpleCondition_eventCondition' - The dimension settings for the event that\'s associated with the
-- activity.
--
-- 'segmentDimensions', 'simpleCondition_segmentDimensions' - The dimension settings for the segment that\'s associated with the
-- activity.
--
-- 'segmentCondition', 'simpleCondition_segmentCondition' - The segment that\'s associated with the activity.
newSimpleCondition ::
  SimpleCondition
newSimpleCondition =
  SimpleCondition'
    { eventCondition = Prelude.Nothing,
      segmentDimensions = Prelude.Nothing,
      segmentCondition = Prelude.Nothing
    }

-- | The dimension settings for the event that\'s associated with the
-- activity.
simpleCondition_eventCondition :: Lens.Lens' SimpleCondition (Prelude.Maybe EventCondition)
simpleCondition_eventCondition = Lens.lens (\SimpleCondition' {eventCondition} -> eventCondition) (\s@SimpleCondition' {} a -> s {eventCondition = a} :: SimpleCondition)

-- | The dimension settings for the segment that\'s associated with the
-- activity.
simpleCondition_segmentDimensions :: Lens.Lens' SimpleCondition (Prelude.Maybe SegmentDimensions)
simpleCondition_segmentDimensions = Lens.lens (\SimpleCondition' {segmentDimensions} -> segmentDimensions) (\s@SimpleCondition' {} a -> s {segmentDimensions = a} :: SimpleCondition)

-- | The segment that\'s associated with the activity.
simpleCondition_segmentCondition :: Lens.Lens' SimpleCondition (Prelude.Maybe SegmentCondition)
simpleCondition_segmentCondition = Lens.lens (\SimpleCondition' {segmentCondition} -> segmentCondition) (\s@SimpleCondition' {} a -> s {segmentCondition = a} :: SimpleCondition)

instance Prelude.FromJSON SimpleCondition where
  parseJSON =
    Prelude.withObject
      "SimpleCondition"
      ( \x ->
          SimpleCondition'
            Prelude.<$> (x Prelude..:? "EventCondition")
            Prelude.<*> (x Prelude..:? "segmentDimensions")
            Prelude.<*> (x Prelude..:? "SegmentCondition")
      )

instance Prelude.Hashable SimpleCondition

instance Prelude.NFData SimpleCondition

instance Prelude.ToJSON SimpleCondition where
  toJSON SimpleCondition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EventCondition" Prelude..=)
              Prelude.<$> eventCondition,
            ("segmentDimensions" Prelude..=)
              Prelude.<$> segmentDimensions,
            ("SegmentCondition" Prelude..=)
              Prelude.<$> segmentCondition
          ]
      )
