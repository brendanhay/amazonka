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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentEvent where

import Amazonka.AmplifyUiBuilder.Types.ActionParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of an event. You can bind an event and a
-- corresponding action to a @Component@ or a @ComponentChild@. A button
-- click is an example of an event.
--
-- /See:/ 'newComponentEvent' smart constructor.
data ComponentEvent = ComponentEvent'
  { -- | Binds an event to an action on a component. When you specify a
    -- @bindingEvent@, the event is called when the action is performed.
    bindingEvent :: Prelude.Maybe Prelude.Text,
    -- | The action to perform when a specific event is raised.
    action :: Prelude.Maybe Prelude.Text,
    -- | Describes information about the action.
    parameters :: Prelude.Maybe ActionParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bindingEvent', 'componentEvent_bindingEvent' - Binds an event to an action on a component. When you specify a
-- @bindingEvent@, the event is called when the action is performed.
--
-- 'action', 'componentEvent_action' - The action to perform when a specific event is raised.
--
-- 'parameters', 'componentEvent_parameters' - Describes information about the action.
newComponentEvent ::
  ComponentEvent
newComponentEvent =
  ComponentEvent'
    { bindingEvent = Prelude.Nothing,
      action = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | Binds an event to an action on a component. When you specify a
-- @bindingEvent@, the event is called when the action is performed.
componentEvent_bindingEvent :: Lens.Lens' ComponentEvent (Prelude.Maybe Prelude.Text)
componentEvent_bindingEvent = Lens.lens (\ComponentEvent' {bindingEvent} -> bindingEvent) (\s@ComponentEvent' {} a -> s {bindingEvent = a} :: ComponentEvent)

-- | The action to perform when a specific event is raised.
componentEvent_action :: Lens.Lens' ComponentEvent (Prelude.Maybe Prelude.Text)
componentEvent_action = Lens.lens (\ComponentEvent' {action} -> action) (\s@ComponentEvent' {} a -> s {action = a} :: ComponentEvent)

-- | Describes information about the action.
componentEvent_parameters :: Lens.Lens' ComponentEvent (Prelude.Maybe ActionParameters)
componentEvent_parameters = Lens.lens (\ComponentEvent' {parameters} -> parameters) (\s@ComponentEvent' {} a -> s {parameters = a} :: ComponentEvent)

instance Data.FromJSON ComponentEvent where
  parseJSON =
    Data.withObject
      "ComponentEvent"
      ( \x ->
          ComponentEvent'
            Prelude.<$> (x Data..:? "bindingEvent")
            Prelude.<*> (x Data..:? "action")
            Prelude.<*> (x Data..:? "parameters")
      )

instance Prelude.Hashable ComponentEvent where
  hashWithSalt _salt ComponentEvent' {..} =
    _salt `Prelude.hashWithSalt` bindingEvent
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData ComponentEvent where
  rnf ComponentEvent' {..} =
    Prelude.rnf bindingEvent
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf parameters

instance Data.ToJSON ComponentEvent where
  toJSON ComponentEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bindingEvent" Data..=) Prelude.<$> bindingEvent,
            ("action" Data..=) Prelude.<$> action,
            ("parameters" Data..=) Prelude.<$> parameters
          ]
      )
