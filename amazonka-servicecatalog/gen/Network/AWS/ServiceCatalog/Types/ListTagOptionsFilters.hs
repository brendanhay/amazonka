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
-- Module      : Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Filters to use when listing TagOptions.
--
-- /See:/ 'newListTagOptionsFilters' smart constructor.
data ListTagOptionsFilters = ListTagOptionsFilters'
  { -- | The TagOption key.
    key :: Core.Maybe Core.Text,
    -- | The active state.
    active :: Core.Maybe Core.Bool,
    -- | The TagOption value.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagOptionsFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'listTagOptionsFilters_key' - The TagOption key.
--
-- 'active', 'listTagOptionsFilters_active' - The active state.
--
-- 'value', 'listTagOptionsFilters_value' - The TagOption value.
newListTagOptionsFilters ::
  ListTagOptionsFilters
newListTagOptionsFilters =
  ListTagOptionsFilters'
    { key = Core.Nothing,
      active = Core.Nothing,
      value = Core.Nothing
    }

-- | The TagOption key.
listTagOptionsFilters_key :: Lens.Lens' ListTagOptionsFilters (Core.Maybe Core.Text)
listTagOptionsFilters_key = Lens.lens (\ListTagOptionsFilters' {key} -> key) (\s@ListTagOptionsFilters' {} a -> s {key = a} :: ListTagOptionsFilters)

-- | The active state.
listTagOptionsFilters_active :: Lens.Lens' ListTagOptionsFilters (Core.Maybe Core.Bool)
listTagOptionsFilters_active = Lens.lens (\ListTagOptionsFilters' {active} -> active) (\s@ListTagOptionsFilters' {} a -> s {active = a} :: ListTagOptionsFilters)

-- | The TagOption value.
listTagOptionsFilters_value :: Lens.Lens' ListTagOptionsFilters (Core.Maybe Core.Text)
listTagOptionsFilters_value = Lens.lens (\ListTagOptionsFilters' {value} -> value) (\s@ListTagOptionsFilters' {} a -> s {value = a} :: ListTagOptionsFilters)

instance Core.Hashable ListTagOptionsFilters

instance Core.NFData ListTagOptionsFilters

instance Core.ToJSON ListTagOptionsFilters where
  toJSON ListTagOptionsFilters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Active" Core..=) Core.<$> active,
            ("Value" Core..=) Core.<$> value
          ]
      )
