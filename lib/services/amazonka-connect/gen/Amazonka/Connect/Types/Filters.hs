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
-- Module      : Amazonka.Connect.Types.Filters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Filters where

import Amazonka.Connect.Types.Channel
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the filter to apply when retrieving metrics.
--
-- /See:/ 'newFilters' smart constructor.
data Filters = Filters'
  { -- | The queues to use to filter the metrics. You can specify up to 100
    -- queues per request.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The channel to use to filter the metrics.
    channels :: Prelude.Maybe [Channel]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queues', 'filters_queues' - The queues to use to filter the metrics. You can specify up to 100
-- queues per request.
--
-- 'channels', 'filters_channels' - The channel to use to filter the metrics.
newFilters ::
  Filters
newFilters =
  Filters'
    { queues = Prelude.Nothing,
      channels = Prelude.Nothing
    }

-- | The queues to use to filter the metrics. You can specify up to 100
-- queues per request.
filters_queues :: Lens.Lens' Filters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filters_queues = Lens.lens (\Filters' {queues} -> queues) (\s@Filters' {} a -> s {queues = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

-- | The channel to use to filter the metrics.
filters_channels :: Lens.Lens' Filters (Prelude.Maybe [Channel])
filters_channels = Lens.lens (\Filters' {channels} -> channels) (\s@Filters' {} a -> s {channels = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Filters where
  hashWithSalt salt' Filters' {..} =
    salt' `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` queues

instance Prelude.NFData Filters where
  rnf Filters' {..} =
    Prelude.rnf queues
      `Prelude.seq` Prelude.rnf channels

instance Core.ToJSON Filters where
  toJSON Filters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Queues" Core..=) Prelude.<$> queues,
            ("Channels" Core..=) Prelude.<$> channels
          ]
      )
