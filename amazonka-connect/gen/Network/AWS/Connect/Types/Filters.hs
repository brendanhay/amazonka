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
-- Module      : Network.AWS.Connect.Types.Filters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Filters where

import Network.AWS.Connect.Types.Channel
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the filter to apply when retrieving metrics.
--
-- /See:/ 'newFilters' smart constructor.
data Filters = Filters'
  { -- | The channel to use to filter the metrics.
    channels :: Core.Maybe [Channel],
    -- | The queues to use to filter the metrics. You can specify up to 100
    -- queues per request.
    queues :: Core.Maybe (Core.NonEmpty Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'queues', 'filters_queues' - The queues to use to filter the metrics. You can specify up to 100
-- queues per request.
newFilters ::
  Filters
newFilters =
  Filters'
    { channels = Core.Nothing,
      queues = Core.Nothing
    }

-- | The channel to use to filter the metrics.
filters_channels :: Lens.Lens' Filters (Core.Maybe [Channel])
filters_channels = Lens.lens (\Filters' {channels} -> channels) (\s@Filters' {} a -> s {channels = a} :: Filters) Core.. Lens.mapping Lens._Coerce

-- | The queues to use to filter the metrics. You can specify up to 100
-- queues per request.
filters_queues :: Lens.Lens' Filters (Core.Maybe (Core.NonEmpty Core.Text))
filters_queues = Lens.lens (\Filters' {queues} -> queues) (\s@Filters' {} a -> s {queues = a} :: Filters) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable Filters

instance Core.NFData Filters

instance Core.ToJSON Filters where
  toJSON Filters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Channels" Core..=) Core.<$> channels,
            ("Queues" Core..=) Core.<$> queues
          ]
      )
