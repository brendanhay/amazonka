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
-- Module      : Amazonka.DLM.Types.EventSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.EventSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.EventParameters
import Amazonka.DLM.Types.EventSourceValues
import qualified Amazonka.Prelude as Prelude

-- | __[Event-based policies only]__ Specifies an event that activates an
-- event-based policy.
--
-- /See:/ 'newEventSource' smart constructor.
data EventSource = EventSource'
  { -- | Information about the event.
    parameters :: Prelude.Maybe EventParameters,
    -- | The source of the event. Currently only managed CloudWatch Events rules
    -- are supported.
    type' :: EventSourceValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'eventSource_parameters' - Information about the event.
--
-- 'type'', 'eventSource_type' - The source of the event. Currently only managed CloudWatch Events rules
-- are supported.
newEventSource ::
  -- | 'type''
  EventSourceValues ->
  EventSource
newEventSource pType_ =
  EventSource'
    { parameters = Prelude.Nothing,
      type' = pType_
    }

-- | Information about the event.
eventSource_parameters :: Lens.Lens' EventSource (Prelude.Maybe EventParameters)
eventSource_parameters = Lens.lens (\EventSource' {parameters} -> parameters) (\s@EventSource' {} a -> s {parameters = a} :: EventSource)

-- | The source of the event. Currently only managed CloudWatch Events rules
-- are supported.
eventSource_type :: Lens.Lens' EventSource EventSourceValues
eventSource_type = Lens.lens (\EventSource' {type'} -> type') (\s@EventSource' {} a -> s {type' = a} :: EventSource)

instance Core.FromJSON EventSource where
  parseJSON =
    Core.withObject
      "EventSource"
      ( \x ->
          EventSource'
            Prelude.<$> (x Core..:? "Parameters")
            Prelude.<*> (x Core..: "Type")
      )

instance Prelude.Hashable EventSource where
  hashWithSalt _salt EventSource' {..} =
    _salt `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EventSource where
  rnf EventSource' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON EventSource where
  toJSON EventSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("Type" Core..= type')
          ]
      )
