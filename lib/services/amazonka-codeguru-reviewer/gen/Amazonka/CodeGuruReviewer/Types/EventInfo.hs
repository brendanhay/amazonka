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
-- Module      : Amazonka.CodeGuruReviewer.Types.EventInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.EventInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an event. The event might be a push, pull request,
-- scheduled request, or another type of event.
--
-- /See:/ 'newEventInfo' smart constructor.
data EventInfo = EventInfo'
  { -- | The name of the event. The possible names are @pull_request@,
    -- @workflow_dispatch@, @schedule@, and @push@
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of an event. The state might be open, closed, or another
    -- state.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eventInfo_name' - The name of the event. The possible names are @pull_request@,
-- @workflow_dispatch@, @schedule@, and @push@
--
-- 'state', 'eventInfo_state' - The state of an event. The state might be open, closed, or another
-- state.
newEventInfo ::
  EventInfo
newEventInfo =
  EventInfo'
    { name = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The name of the event. The possible names are @pull_request@,
-- @workflow_dispatch@, @schedule@, and @push@
eventInfo_name :: Lens.Lens' EventInfo (Prelude.Maybe Prelude.Text)
eventInfo_name = Lens.lens (\EventInfo' {name} -> name) (\s@EventInfo' {} a -> s {name = a} :: EventInfo)

-- | The state of an event. The state might be open, closed, or another
-- state.
eventInfo_state :: Lens.Lens' EventInfo (Prelude.Maybe Prelude.Text)
eventInfo_state = Lens.lens (\EventInfo' {state} -> state) (\s@EventInfo' {} a -> s {state = a} :: EventInfo)

instance Core.FromJSON EventInfo where
  parseJSON =
    Core.withObject
      "EventInfo"
      ( \x ->
          EventInfo'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "State")
      )

instance Prelude.Hashable EventInfo where
  hashWithSalt _salt EventInfo' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData EventInfo where
  rnf EventInfo' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf state

instance Core.ToJSON EventInfo where
  toJSON EventInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("State" Core..=) Prelude.<$> state
          ]
      )
