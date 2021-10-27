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
-- Module      : Network.AWS.AppIntegrationS.Types.EventIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppIntegrationS.Types.EventIntegration where

import Network.AWS.AppIntegrationS.Types.EventFilter
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The event integration.
--
-- /See:/ 'newEventIntegration' smart constructor.
data EventIntegration = EventIntegration'
  { -- | The Amazon EventBridge bus for the event integration.
    eventBridgeBus :: Prelude.Maybe Prelude.Text,
    -- | The event integration filter.
    eventFilter :: Prelude.Maybe EventFilter,
    -- | The Amazon Resource Name (ARN) of the event integration.
    eventIntegrationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the event integration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The event integration description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'eventBridgeBus', 'eventIntegration_eventBridgeBus' - The Amazon EventBridge bus for the event integration.
--
-- 'eventFilter', 'eventIntegration_eventFilter' - The event integration filter.
--
-- 'eventIntegrationArn', 'eventIntegration_eventIntegrationArn' - The Amazon Resource Name (ARN) of the event integration.
--
-- 'name', 'eventIntegration_name' - The name of the event integration.
--
-- 'description', 'eventIntegration_description' - The event integration description.
--
-- 'tags', 'eventIntegration_tags' - The tags.
newEventIntegration ::
  EventIntegration
newEventIntegration =
  EventIntegration'
    { eventBridgeBus = Prelude.Nothing,
      eventFilter = Prelude.Nothing,
      eventIntegrationArn = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon EventBridge bus for the event integration.
eventIntegration_eventBridgeBus :: Lens.Lens' EventIntegration (Prelude.Maybe Prelude.Text)
eventIntegration_eventBridgeBus = Lens.lens (\EventIntegration' {eventBridgeBus} -> eventBridgeBus) (\s@EventIntegration' {} a -> s {eventBridgeBus = a} :: EventIntegration)

-- | The event integration filter.
eventIntegration_eventFilter :: Lens.Lens' EventIntegration (Prelude.Maybe EventFilter)
eventIntegration_eventFilter = Lens.lens (\EventIntegration' {eventFilter} -> eventFilter) (\s@EventIntegration' {} a -> s {eventFilter = a} :: EventIntegration)

-- | The Amazon Resource Name (ARN) of the event integration.
eventIntegration_eventIntegrationArn :: Lens.Lens' EventIntegration (Prelude.Maybe Prelude.Text)
eventIntegration_eventIntegrationArn = Lens.lens (\EventIntegration' {eventIntegrationArn} -> eventIntegrationArn) (\s@EventIntegration' {} a -> s {eventIntegrationArn = a} :: EventIntegration)

-- | The name of the event integration.
eventIntegration_name :: Lens.Lens' EventIntegration (Prelude.Maybe Prelude.Text)
eventIntegration_name = Lens.lens (\EventIntegration' {name} -> name) (\s@EventIntegration' {} a -> s {name = a} :: EventIntegration)

-- | The event integration description.
eventIntegration_description :: Lens.Lens' EventIntegration (Prelude.Maybe Prelude.Text)
eventIntegration_description = Lens.lens (\EventIntegration' {description} -> description) (\s@EventIntegration' {} a -> s {description = a} :: EventIntegration)

-- | The tags.
eventIntegration_tags :: Lens.Lens' EventIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
eventIntegration_tags = Lens.lens (\EventIntegration' {tags} -> tags) (\s@EventIntegration' {} a -> s {tags = a} :: EventIntegration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON EventIntegration where
  parseJSON =
    Core.withObject
      "EventIntegration"
      ( \x ->
          EventIntegration'
            Prelude.<$> (x Core..:? "EventBridgeBus")
            Prelude.<*> (x Core..:? "EventFilter")
            Prelude.<*> (x Core..:? "EventIntegrationArn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable EventIntegration

instance Prelude.NFData EventIntegration
