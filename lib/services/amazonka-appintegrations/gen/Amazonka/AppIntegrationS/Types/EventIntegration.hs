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
-- Module      : Amazonka.AppIntegrationS.Types.EventIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppIntegrationS.Types.EventIntegration where

import Amazonka.AppIntegrationS.Types.EventFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The event integration.
--
-- /See:/ 'newEventIntegration' smart constructor.
data EventIntegration = EventIntegration'
  { -- | The tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the event integration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EventBridge bus for the event integration.
    eventBridgeBus :: Prelude.Maybe Prelude.Text,
    -- | The event integration filter.
    eventFilter :: Prelude.Maybe EventFilter,
    -- | The event integration description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the event integration.
    eventIntegrationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'eventIntegration_tags' - The tags.
--
-- 'name', 'eventIntegration_name' - The name of the event integration.
--
-- 'eventBridgeBus', 'eventIntegration_eventBridgeBus' - The Amazon EventBridge bus for the event integration.
--
-- 'eventFilter', 'eventIntegration_eventFilter' - The event integration filter.
--
-- 'description', 'eventIntegration_description' - The event integration description.
--
-- 'eventIntegrationArn', 'eventIntegration_eventIntegrationArn' - The Amazon Resource Name (ARN) of the event integration.
newEventIntegration ::
  EventIntegration
newEventIntegration =
  EventIntegration'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      eventBridgeBus = Prelude.Nothing,
      eventFilter = Prelude.Nothing,
      description = Prelude.Nothing,
      eventIntegrationArn = Prelude.Nothing
    }

-- | The tags.
eventIntegration_tags :: Lens.Lens' EventIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
eventIntegration_tags = Lens.lens (\EventIntegration' {tags} -> tags) (\s@EventIntegration' {} a -> s {tags = a} :: EventIntegration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the event integration.
eventIntegration_name :: Lens.Lens' EventIntegration (Prelude.Maybe Prelude.Text)
eventIntegration_name = Lens.lens (\EventIntegration' {name} -> name) (\s@EventIntegration' {} a -> s {name = a} :: EventIntegration)

-- | The Amazon EventBridge bus for the event integration.
eventIntegration_eventBridgeBus :: Lens.Lens' EventIntegration (Prelude.Maybe Prelude.Text)
eventIntegration_eventBridgeBus = Lens.lens (\EventIntegration' {eventBridgeBus} -> eventBridgeBus) (\s@EventIntegration' {} a -> s {eventBridgeBus = a} :: EventIntegration)

-- | The event integration filter.
eventIntegration_eventFilter :: Lens.Lens' EventIntegration (Prelude.Maybe EventFilter)
eventIntegration_eventFilter = Lens.lens (\EventIntegration' {eventFilter} -> eventFilter) (\s@EventIntegration' {} a -> s {eventFilter = a} :: EventIntegration)

-- | The event integration description.
eventIntegration_description :: Lens.Lens' EventIntegration (Prelude.Maybe Prelude.Text)
eventIntegration_description = Lens.lens (\EventIntegration' {description} -> description) (\s@EventIntegration' {} a -> s {description = a} :: EventIntegration)

-- | The Amazon Resource Name (ARN) of the event integration.
eventIntegration_eventIntegrationArn :: Lens.Lens' EventIntegration (Prelude.Maybe Prelude.Text)
eventIntegration_eventIntegrationArn = Lens.lens (\EventIntegration' {eventIntegrationArn} -> eventIntegrationArn) (\s@EventIntegration' {} a -> s {eventIntegrationArn = a} :: EventIntegration)

instance Data.FromJSON EventIntegration where
  parseJSON =
    Data.withObject
      "EventIntegration"
      ( \x ->
          EventIntegration'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "EventBridgeBus")
            Prelude.<*> (x Data..:? "EventFilter")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EventIntegrationArn")
      )

instance Prelude.Hashable EventIntegration where
  hashWithSalt _salt EventIntegration' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` eventBridgeBus
      `Prelude.hashWithSalt` eventFilter
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventIntegrationArn

instance Prelude.NFData EventIntegration where
  rnf EventIntegration' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf eventBridgeBus
      `Prelude.seq` Prelude.rnf eventFilter
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eventIntegrationArn
