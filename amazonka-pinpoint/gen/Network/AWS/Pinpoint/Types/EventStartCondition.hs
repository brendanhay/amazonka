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
-- Module      : Network.AWS.Pinpoint.Types.EventStartCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventStartCondition where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventFilter
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for an event that causes a journey activity to
-- start.
--
-- /See:/ 'newEventStartCondition' smart constructor.
data EventStartCondition = EventStartCondition'
  { eventFilter :: Prelude.Maybe EventFilter,
    segmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { eventFilter = Prelude.Nothing,
      segmentId = Prelude.Nothing
    }

-- | Undocumented member.
eventStartCondition_eventFilter :: Lens.Lens' EventStartCondition (Prelude.Maybe EventFilter)
eventStartCondition_eventFilter = Lens.lens (\EventStartCondition' {eventFilter} -> eventFilter) (\s@EventStartCondition' {} a -> s {eventFilter = a} :: EventStartCondition)

-- | Undocumented member.
eventStartCondition_segmentId :: Lens.Lens' EventStartCondition (Prelude.Maybe Prelude.Text)
eventStartCondition_segmentId = Lens.lens (\EventStartCondition' {segmentId} -> segmentId) (\s@EventStartCondition' {} a -> s {segmentId = a} :: EventStartCondition)

instance Prelude.FromJSON EventStartCondition where
  parseJSON =
    Prelude.withObject
      "EventStartCondition"
      ( \x ->
          EventStartCondition'
            Prelude.<$> (x Prelude..:? "EventFilter")
            Prelude.<*> (x Prelude..:? "SegmentId")
      )

instance Prelude.Hashable EventStartCondition

instance Prelude.NFData EventStartCondition

instance Prelude.ToJSON EventStartCondition where
  toJSON EventStartCondition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EventFilter" Prelude..=) Prelude.<$> eventFilter,
            ("SegmentId" Prelude..=) Prelude.<$> segmentId
          ]
      )
