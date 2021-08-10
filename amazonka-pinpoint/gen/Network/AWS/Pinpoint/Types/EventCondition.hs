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
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the conditions to evaluate for an event that applies to an
-- activity in a journey.
--
-- /See:/ 'newEventCondition' smart constructor.
data EventCondition = EventCondition'
  { -- | The message identifier (message_id) for the message to use when
    -- determining whether message events meet the condition.
    messageActivity :: Prelude.Maybe Prelude.Text,
    -- | The dimensions for the event filter to use for the activity.
    dimensions :: Prelude.Maybe EventDimensions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { messageActivity = Prelude.Nothing,
      dimensions = Prelude.Nothing
    }

-- | The message identifier (message_id) for the message to use when
-- determining whether message events meet the condition.
eventCondition_messageActivity :: Lens.Lens' EventCondition (Prelude.Maybe Prelude.Text)
eventCondition_messageActivity = Lens.lens (\EventCondition' {messageActivity} -> messageActivity) (\s@EventCondition' {} a -> s {messageActivity = a} :: EventCondition)

-- | The dimensions for the event filter to use for the activity.
eventCondition_dimensions :: Lens.Lens' EventCondition (Prelude.Maybe EventDimensions)
eventCondition_dimensions = Lens.lens (\EventCondition' {dimensions} -> dimensions) (\s@EventCondition' {} a -> s {dimensions = a} :: EventCondition)

instance Core.FromJSON EventCondition where
  parseJSON =
    Core.withObject
      "EventCondition"
      ( \x ->
          EventCondition'
            Prelude.<$> (x Core..:? "MessageActivity")
            Prelude.<*> (x Core..:? "Dimensions")
      )

instance Prelude.Hashable EventCondition

instance Prelude.NFData EventCondition

instance Core.ToJSON EventCondition where
  toJSON EventCondition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MessageActivity" Core..=)
              Prelude.<$> messageActivity,
            ("Dimensions" Core..=) Prelude.<$> dimensions
          ]
      )
