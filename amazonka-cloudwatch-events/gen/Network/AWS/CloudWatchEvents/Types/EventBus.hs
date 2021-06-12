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
-- Module      : Network.AWS.CloudWatchEvents.Types.EventBus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.EventBus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An event bus receives events from a source and routes them to rules
-- associated with that event bus. Your account\'s default event bus
-- receives rules from AWS services. A custom event bus can receive rules
-- from AWS services as well as your custom applications and services. A
-- partner event bus receives events from an event source created by an
-- SaaS partner. These events come from the partners services or
-- applications.
--
-- /See:/ 'newEventBus' smart constructor.
data EventBus = EventBus'
  { -- | The ARN of the event bus.
    arn :: Core.Maybe Core.Text,
    -- | The name of the event bus.
    name :: Core.Maybe Core.Text,
    -- | The permissions policy of the event bus, describing which other AWS
    -- accounts can write events to this event bus.
    policy :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventBus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'eventBus_arn' - The ARN of the event bus.
--
-- 'name', 'eventBus_name' - The name of the event bus.
--
-- 'policy', 'eventBus_policy' - The permissions policy of the event bus, describing which other AWS
-- accounts can write events to this event bus.
newEventBus ::
  EventBus
newEventBus =
  EventBus'
    { arn = Core.Nothing,
      name = Core.Nothing,
      policy = Core.Nothing
    }

-- | The ARN of the event bus.
eventBus_arn :: Lens.Lens' EventBus (Core.Maybe Core.Text)
eventBus_arn = Lens.lens (\EventBus' {arn} -> arn) (\s@EventBus' {} a -> s {arn = a} :: EventBus)

-- | The name of the event bus.
eventBus_name :: Lens.Lens' EventBus (Core.Maybe Core.Text)
eventBus_name = Lens.lens (\EventBus' {name} -> name) (\s@EventBus' {} a -> s {name = a} :: EventBus)

-- | The permissions policy of the event bus, describing which other AWS
-- accounts can write events to this event bus.
eventBus_policy :: Lens.Lens' EventBus (Core.Maybe Core.Text)
eventBus_policy = Lens.lens (\EventBus' {policy} -> policy) (\s@EventBus' {} a -> s {policy = a} :: EventBus)

instance Core.FromJSON EventBus where
  parseJSON =
    Core.withObject
      "EventBus"
      ( \x ->
          EventBus'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Policy")
      )

instance Core.Hashable EventBus

instance Core.NFData EventBus
