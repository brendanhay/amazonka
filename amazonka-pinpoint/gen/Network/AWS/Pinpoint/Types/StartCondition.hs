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
-- Module      : Network.AWS.Pinpoint.Types.StartCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.StartCondition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventStartCondition
import Network.AWS.Pinpoint.Types.SegmentCondition

-- | Specifies the conditions for the first activity in a journey. This
-- activity and its conditions determine which users are participants in a
-- journey.
--
-- /See:/ 'newStartCondition' smart constructor.
data StartCondition = StartCondition'
  { eventStartCondition :: Core.Maybe EventStartCondition,
    -- | The custom description of the condition.
    description :: Core.Maybe Core.Text,
    -- | The segment that\'s associated with the first activity in the journey.
    -- This segment determines which users are participants in the journey.
    segmentStartCondition :: Core.Maybe SegmentCondition
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventStartCondition', 'startCondition_eventStartCondition' - Undocumented member.
--
-- 'description', 'startCondition_description' - The custom description of the condition.
--
-- 'segmentStartCondition', 'startCondition_segmentStartCondition' - The segment that\'s associated with the first activity in the journey.
-- This segment determines which users are participants in the journey.
newStartCondition ::
  StartCondition
newStartCondition =
  StartCondition'
    { eventStartCondition = Core.Nothing,
      description = Core.Nothing,
      segmentStartCondition = Core.Nothing
    }

-- | Undocumented member.
startCondition_eventStartCondition :: Lens.Lens' StartCondition (Core.Maybe EventStartCondition)
startCondition_eventStartCondition = Lens.lens (\StartCondition' {eventStartCondition} -> eventStartCondition) (\s@StartCondition' {} a -> s {eventStartCondition = a} :: StartCondition)

-- | The custom description of the condition.
startCondition_description :: Lens.Lens' StartCondition (Core.Maybe Core.Text)
startCondition_description = Lens.lens (\StartCondition' {description} -> description) (\s@StartCondition' {} a -> s {description = a} :: StartCondition)

-- | The segment that\'s associated with the first activity in the journey.
-- This segment determines which users are participants in the journey.
startCondition_segmentStartCondition :: Lens.Lens' StartCondition (Core.Maybe SegmentCondition)
startCondition_segmentStartCondition = Lens.lens (\StartCondition' {segmentStartCondition} -> segmentStartCondition) (\s@StartCondition' {} a -> s {segmentStartCondition = a} :: StartCondition)

instance Core.FromJSON StartCondition where
  parseJSON =
    Core.withObject
      "StartCondition"
      ( \x ->
          StartCondition'
            Core.<$> (x Core..:? "EventStartCondition")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "SegmentStartCondition")
      )

instance Core.Hashable StartCondition

instance Core.NFData StartCondition

instance Core.ToJSON StartCondition where
  toJSON StartCondition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventStartCondition" Core..=)
              Core.<$> eventStartCondition,
            ("Description" Core..=) Core.<$> description,
            ("SegmentStartCondition" Core..=)
              Core.<$> segmentStartCondition
          ]
      )
