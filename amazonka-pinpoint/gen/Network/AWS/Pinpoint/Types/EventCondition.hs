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
-- Module      : Network.AWS.Pinpoint.Types.EventCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventCondition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventDimensions

-- | Specifies the conditions to evaluate for an event that applies to an
-- activity in a journey.
--
-- /See:/ 'newEventCondition' smart constructor.
data EventCondition = EventCondition'
  { -- | The message identifier (message_id) for the message to use when
    -- determining whether message events meet the condition.
    messageActivity :: Core.Maybe Core.Text,
    -- | The dimensions for the event filter to use for the activity.
    dimensions :: Core.Maybe EventDimensions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageActivity', 'eventCondition_messageActivity' - The message identifier (message_id) for the message to use when
-- determining whether message events meet the condition.
--
-- 'dimensions', 'eventCondition_dimensions' - The dimensions for the event filter to use for the activity.
newEventCondition ::
  EventCondition
newEventCondition =
  EventCondition'
    { messageActivity = Core.Nothing,
      dimensions = Core.Nothing
    }

-- | The message identifier (message_id) for the message to use when
-- determining whether message events meet the condition.
eventCondition_messageActivity :: Lens.Lens' EventCondition (Core.Maybe Core.Text)
eventCondition_messageActivity = Lens.lens (\EventCondition' {messageActivity} -> messageActivity) (\s@EventCondition' {} a -> s {messageActivity = a} :: EventCondition)

-- | The dimensions for the event filter to use for the activity.
eventCondition_dimensions :: Lens.Lens' EventCondition (Core.Maybe EventDimensions)
eventCondition_dimensions = Lens.lens (\EventCondition' {dimensions} -> dimensions) (\s@EventCondition' {} a -> s {dimensions = a} :: EventCondition)

instance Core.FromJSON EventCondition where
  parseJSON =
    Core.withObject
      "EventCondition"
      ( \x ->
          EventCondition'
            Core.<$> (x Core..:? "MessageActivity")
            Core.<*> (x Core..:? "Dimensions")
      )

instance Core.Hashable EventCondition

instance Core.NFData EventCondition

instance Core.ToJSON EventCondition where
  toJSON EventCondition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MessageActivity" Core..=)
              Core.<$> messageActivity,
            ("Dimensions" Core..=) Core.<$> dimensions
          ]
      )
