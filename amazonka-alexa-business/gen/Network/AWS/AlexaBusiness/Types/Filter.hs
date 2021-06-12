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
-- Module      : Network.AWS.AlexaBusiness.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Filter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A filter name and value pair that is used to return a more specific list
-- of results. Filters can be used to match a set of resources by various
-- criteria.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The key of a filter.
    key :: Core.Text,
    -- | The values of a filter.
    values :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'filter_key' - The key of a filter.
--
-- 'values', 'filter_values' - The values of a filter.
newFilter ::
  -- | 'key'
  Core.Text ->
  Filter
newFilter pKey_ =
  Filter' {key = pKey_, values = Core.mempty}

-- | The key of a filter.
filter_key :: Lens.Lens' Filter Core.Text
filter_key = Lens.lens (\Filter' {key} -> key) (\s@Filter' {} a -> s {key = a} :: Filter)

-- | The values of a filter.
filter_values :: Lens.Lens' Filter [Core.Text]
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Core.. Lens._Coerce

instance Core.Hashable Filter

instance Core.NFData Filter

instance Core.ToJSON Filter where
  toJSON Filter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values)
          ]
      )
