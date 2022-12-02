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
-- Module      : Amazonka.CloudWatchEvents.Types.EventBus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.EventBus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An event bus receives events from a source and routes them to rules
-- associated with that event bus. Your account\'s default event bus
-- receives events from Amazon Web Services services. A custom event bus
-- can receive events from your custom applications and services. A partner
-- event bus receives events from an event source created by an SaaS
-- partner. These events come from the partners services or applications.
--
-- /See:/ 'newEventBus' smart constructor.
data EventBus = EventBus'
  { -- | The permissions policy of the event bus, describing which other Amazon
    -- Web Services accounts can write events to this event bus.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The name of the event bus.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event bus.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventBus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'eventBus_policy' - The permissions policy of the event bus, describing which other Amazon
-- Web Services accounts can write events to this event bus.
--
-- 'name', 'eventBus_name' - The name of the event bus.
--
-- 'arn', 'eventBus_arn' - The ARN of the event bus.
newEventBus ::
  EventBus
newEventBus =
  EventBus'
    { policy = Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The permissions policy of the event bus, describing which other Amazon
-- Web Services accounts can write events to this event bus.
eventBus_policy :: Lens.Lens' EventBus (Prelude.Maybe Prelude.Text)
eventBus_policy = Lens.lens (\EventBus' {policy} -> policy) (\s@EventBus' {} a -> s {policy = a} :: EventBus)

-- | The name of the event bus.
eventBus_name :: Lens.Lens' EventBus (Prelude.Maybe Prelude.Text)
eventBus_name = Lens.lens (\EventBus' {name} -> name) (\s@EventBus' {} a -> s {name = a} :: EventBus)

-- | The ARN of the event bus.
eventBus_arn :: Lens.Lens' EventBus (Prelude.Maybe Prelude.Text)
eventBus_arn = Lens.lens (\EventBus' {arn} -> arn) (\s@EventBus' {} a -> s {arn = a} :: EventBus)

instance Data.FromJSON EventBus where
  parseJSON =
    Data.withObject
      "EventBus"
      ( \x ->
          EventBus'
            Prelude.<$> (x Data..:? "Policy")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Arn")
      )

instance Prelude.Hashable EventBus where
  hashWithSalt _salt EventBus' {..} =
    _salt `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData EventBus where
  rnf EventBus' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
