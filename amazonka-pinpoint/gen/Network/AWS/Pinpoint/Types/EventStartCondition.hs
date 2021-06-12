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
-- Module      : Network.AWS.Pinpoint.Types.EventStartCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventStartCondition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventFilter

-- | Specifies the settings for an event that causes a journey activity to
-- start.
--
-- /See:/ 'newEventStartCondition' smart constructor.
data EventStartCondition = EventStartCondition'
  { eventFilter :: Core.Maybe EventFilter,
    segmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventStartCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventFilter', 'eventStartCondition_eventFilter' - Undocumented member.
--
-- 'segmentId', 'eventStartCondition_segmentId' - Undocumented member.
newEventStartCondition ::
  EventStartCondition
newEventStartCondition =
  EventStartCondition'
    { eventFilter = Core.Nothing,
      segmentId = Core.Nothing
    }

-- | Undocumented member.
eventStartCondition_eventFilter :: Lens.Lens' EventStartCondition (Core.Maybe EventFilter)
eventStartCondition_eventFilter = Lens.lens (\EventStartCondition' {eventFilter} -> eventFilter) (\s@EventStartCondition' {} a -> s {eventFilter = a} :: EventStartCondition)

-- | Undocumented member.
eventStartCondition_segmentId :: Lens.Lens' EventStartCondition (Core.Maybe Core.Text)
eventStartCondition_segmentId = Lens.lens (\EventStartCondition' {segmentId} -> segmentId) (\s@EventStartCondition' {} a -> s {segmentId = a} :: EventStartCondition)

instance Core.FromJSON EventStartCondition where
  parseJSON =
    Core.withObject
      "EventStartCondition"
      ( \x ->
          EventStartCondition'
            Core.<$> (x Core..:? "EventFilter")
            Core.<*> (x Core..:? "SegmentId")
      )

instance Core.Hashable EventStartCondition

instance Core.NFData EventStartCondition

instance Core.ToJSON EventStartCondition where
  toJSON EventStartCondition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventFilter" Core..=) Core.<$> eventFilter,
            ("SegmentId" Core..=) Core.<$> segmentId
          ]
      )
