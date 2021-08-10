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
import qualified Network.AWS.Prelude as Prelude

-- | Contains the filter to apply when retrieving metrics.
--
-- /See:/ 'newFilters' smart constructor.
data Filters = Filters'
  { -- | The channel to use to filter the metrics.
    channels :: Prelude.Maybe [Channel],
    -- | The queues to use to filter the metrics. You can specify up to 100
    -- queues per request.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'queues', 'filters_queues' - The queues to use to filter the metrics. You can specify up to 100
-- queues per request.
newFilters ::
  Filters
newFilters =
  Filters'
    { channels = Prelude.Nothing,
      queues = Prelude.Nothing
    }

-- | The channel to use to filter the metrics.
filters_channels :: Lens.Lens' Filters (Prelude.Maybe [Channel])
filters_channels = Lens.lens (\Filters' {channels} -> channels) (\s@Filters' {} a -> s {channels = a} :: Filters) Prelude.. Lens.mapping Lens._Coerce

-- | The queues to use to filter the metrics. You can specify up to 100
-- queues per request.
filters_queues :: Lens.Lens' Filters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filters_queues = Lens.lens (\Filters' {queues} -> queues) (\s@Filters' {} a -> s {queues = a} :: Filters) Prelude.. Lens.mapping Lens._Coerce

instance Prelude.Hashable Filters

instance Prelude.NFData Filters

instance Core.ToJSON Filters where
  toJSON Filters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Channels" Core..=) Prelude.<$> channels,
            ("Queues" Core..=) Prelude.<$> queues
          ]
      )
