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
-- Module      : Amazonka.Pinpoint.Types.SimpleCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SimpleCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.EventCondition
import Amazonka.Pinpoint.Types.SegmentCondition
import Amazonka.Pinpoint.Types.SegmentDimensions
import qualified Amazonka.Prelude as Prelude

-- | Specifies a condition to evaluate for an activity in a journey.
--
-- /See:/ 'newSimpleCondition' smart constructor.
data SimpleCondition = SimpleCondition'
  { -- | The dimension settings for the event that\'s associated with the
    -- activity.
    eventCondition :: Prelude.Maybe EventCondition,
    -- | The segment that\'s associated with the activity.
    segmentCondition :: Prelude.Maybe SegmentCondition,
    -- | The dimension settings for the segment that\'s associated with the
    -- activity.
    segmentDimensions :: Prelude.Maybe SegmentDimensions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'segmentCondition', 'simpleCondition_segmentCondition' - The segment that\'s associated with the activity.
--
-- 'segmentDimensions', 'simpleCondition_segmentDimensions' - The dimension settings for the segment that\'s associated with the
-- activity.
newSimpleCondition ::
  SimpleCondition
newSimpleCondition =
  SimpleCondition'
    { eventCondition = Prelude.Nothing,
      segmentCondition = Prelude.Nothing,
      segmentDimensions = Prelude.Nothing
    }

-- | The dimension settings for the event that\'s associated with the
-- activity.
simpleCondition_eventCondition :: Lens.Lens' SimpleCondition (Prelude.Maybe EventCondition)
simpleCondition_eventCondition = Lens.lens (\SimpleCondition' {eventCondition} -> eventCondition) (\s@SimpleCondition' {} a -> s {eventCondition = a} :: SimpleCondition)

-- | The segment that\'s associated with the activity.
simpleCondition_segmentCondition :: Lens.Lens' SimpleCondition (Prelude.Maybe SegmentCondition)
simpleCondition_segmentCondition = Lens.lens (\SimpleCondition' {segmentCondition} -> segmentCondition) (\s@SimpleCondition' {} a -> s {segmentCondition = a} :: SimpleCondition)

-- | The dimension settings for the segment that\'s associated with the
-- activity.
simpleCondition_segmentDimensions :: Lens.Lens' SimpleCondition (Prelude.Maybe SegmentDimensions)
simpleCondition_segmentDimensions = Lens.lens (\SimpleCondition' {segmentDimensions} -> segmentDimensions) (\s@SimpleCondition' {} a -> s {segmentDimensions = a} :: SimpleCondition)

instance Data.FromJSON SimpleCondition where
  parseJSON =
    Data.withObject
      "SimpleCondition"
      ( \x ->
          SimpleCondition'
            Prelude.<$> (x Data..:? "EventCondition")
            Prelude.<*> (x Data..:? "SegmentCondition")
            Prelude.<*> (x Data..:? "segmentDimensions")
      )

instance Prelude.Hashable SimpleCondition where
  hashWithSalt _salt SimpleCondition' {..} =
    _salt
      `Prelude.hashWithSalt` eventCondition
      `Prelude.hashWithSalt` segmentCondition
      `Prelude.hashWithSalt` segmentDimensions

instance Prelude.NFData SimpleCondition where
  rnf SimpleCondition' {..} =
    Prelude.rnf eventCondition
      `Prelude.seq` Prelude.rnf segmentCondition
      `Prelude.seq` Prelude.rnf segmentDimensions

instance Data.ToJSON SimpleCondition where
  toJSON SimpleCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventCondition" Data..=)
              Prelude.<$> eventCondition,
            ("SegmentCondition" Data..=)
              Prelude.<$> segmentCondition,
            ("segmentDimensions" Data..=)
              Prelude.<$> segmentDimensions
          ]
      )
