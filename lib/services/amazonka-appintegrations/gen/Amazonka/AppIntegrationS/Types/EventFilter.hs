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
-- Module      : Amazonka.AppIntegrationS.Types.EventFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppIntegrationS.Types.EventFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The event filter.
--
-- /See:/ 'newEventFilter' smart constructor.
data EventFilter = EventFilter'
  { -- | The source of the events.
    source :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'eventFilter_source' - The source of the events.
newEventFilter ::
  -- | 'source'
  Prelude.Text ->
  EventFilter
newEventFilter pSource_ =
  EventFilter' {source = pSource_}

-- | The source of the events.
eventFilter_source :: Lens.Lens' EventFilter Prelude.Text
eventFilter_source = Lens.lens (\EventFilter' {source} -> source) (\s@EventFilter' {} a -> s {source = a} :: EventFilter)

instance Core.FromJSON EventFilter where
  parseJSON =
    Core.withObject
      "EventFilter"
      ( \x ->
          EventFilter' Prelude.<$> (x Core..: "Source")
      )

instance Prelude.Hashable EventFilter where
  hashWithSalt _salt EventFilter' {..} =
    _salt `Prelude.hashWithSalt` source

instance Prelude.NFData EventFilter where
  rnf EventFilter' {..} = Prelude.rnf source

instance Core.ToJSON EventFilter where
  toJSON EventFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Source" Core..= source)]
      )
