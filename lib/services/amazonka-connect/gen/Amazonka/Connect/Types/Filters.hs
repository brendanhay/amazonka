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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Filters where

import Amazonka.Connect.Types.Channel
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the filter to apply when retrieving metrics.
--
-- /See:/ 'newFilters' smart constructor.
data Filters = Filters'
  { -- | The channel to use to filter the metrics.
    channels :: Prelude.Maybe [Channel],
    -- | The queues to use to filter the metrics. You should specify at least one
    -- queue, and can specify up to 100 queues per request. The
    -- @GetCurrentMetricsData@ API in particular requires a queue when you
    -- include a @Filter@ in your request.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of up to 100 routing profile IDs or ARNs.
    routingProfiles :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'channels', 'filters_channels' - The channel to use to filter the metrics.
--
-- 'queues', 'filters_queues' - The queues to use to filter the metrics. You should specify at least one
-- queue, and can specify up to 100 queues per request. The
-- @GetCurrentMetricsData@ API in particular requires a queue when you
-- include a @Filter@ in your request.
--
-- 'routingProfiles', 'filters_routingProfiles' - A list of up to 100 routing profile IDs or ARNs.
newFilters ::
  Filters
newFilters =
  Filters'
    { channels = Prelude.Nothing,
      queues = Prelude.Nothing,
      routingProfiles = Prelude.Nothing
    }

-- | The channel to use to filter the metrics.
filters_channels :: Lens.Lens' Filters (Prelude.Maybe [Channel])
filters_channels = Lens.lens (\Filters' {channels} -> channels) (\s@Filters' {} a -> s {channels = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

-- | The queues to use to filter the metrics. You should specify at least one
-- queue, and can specify up to 100 queues per request. The
-- @GetCurrentMetricsData@ API in particular requires a queue when you
-- include a @Filter@ in your request.
filters_queues :: Lens.Lens' Filters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filters_queues = Lens.lens (\Filters' {queues} -> queues) (\s@Filters' {} a -> s {queues = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

-- | A list of up to 100 routing profile IDs or ARNs.
filters_routingProfiles :: Lens.Lens' Filters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filters_routingProfiles = Lens.lens (\Filters' {routingProfiles} -> routingProfiles) (\s@Filters' {} a -> s {routingProfiles = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Filters where
  hashWithSalt _salt Filters' {..} =
    _salt `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` queues
      `Prelude.hashWithSalt` routingProfiles

instance Prelude.NFData Filters where
  rnf Filters' {..} =
    Prelude.rnf channels
      `Prelude.seq` Prelude.rnf queues
      `Prelude.seq` Prelude.rnf routingProfiles

instance Data.ToJSON Filters where
  toJSON Filters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Channels" Data..=) Prelude.<$> channels,
            ("Queues" Data..=) Prelude.<$> queues,
            ("RoutingProfiles" Data..=)
              Prelude.<$> routingProfiles
          ]
      )
