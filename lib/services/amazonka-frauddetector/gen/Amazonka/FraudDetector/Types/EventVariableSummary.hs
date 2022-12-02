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
-- Module      : Amazonka.FraudDetector.Types.EventVariableSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EventVariableSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the summary of an event variable that was evaluated
-- for generating prediction.
--
-- /See:/ 'newEventVariableSummary' smart constructor.
data EventVariableSummary = EventVariableSummary'
  { -- | The event variable name.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The event variable source.
    source :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The value of the event variable.
    value :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventVariableSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eventVariableSummary_name' - The event variable name.
--
-- 'source', 'eventVariableSummary_source' - The event variable source.
--
-- 'value', 'eventVariableSummary_value' - The value of the event variable.
newEventVariableSummary ::
  EventVariableSummary
newEventVariableSummary =
  EventVariableSummary'
    { name = Prelude.Nothing,
      source = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The event variable name.
eventVariableSummary_name :: Lens.Lens' EventVariableSummary (Prelude.Maybe Prelude.Text)
eventVariableSummary_name = Lens.lens (\EventVariableSummary' {name} -> name) (\s@EventVariableSummary' {} a -> s {name = a} :: EventVariableSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The event variable source.
eventVariableSummary_source :: Lens.Lens' EventVariableSummary (Prelude.Maybe Prelude.Text)
eventVariableSummary_source = Lens.lens (\EventVariableSummary' {source} -> source) (\s@EventVariableSummary' {} a -> s {source = a} :: EventVariableSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The value of the event variable.
eventVariableSummary_value :: Lens.Lens' EventVariableSummary (Prelude.Maybe Prelude.Text)
eventVariableSummary_value = Lens.lens (\EventVariableSummary' {value} -> value) (\s@EventVariableSummary' {} a -> s {value = a} :: EventVariableSummary) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON EventVariableSummary where
  parseJSON =
    Data.withObject
      "EventVariableSummary"
      ( \x ->
          EventVariableSummary'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "source")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable EventVariableSummary where
  hashWithSalt _salt EventVariableSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` value

instance Prelude.NFData EventVariableSummary where
  rnf EventVariableSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf value
