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
-- Module      : Network.AWS.SecretsManager.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.Filter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SecretsManager.Types.FilterNameStringType

-- | Allows you to add filters when you use the search function in Secrets
-- Manager.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | Filters your list of secrets by a specific key.
    key :: Core.Maybe FilterNameStringType,
    -- | Filters your list of secrets by a specific value.
    --
    -- You can prefix your search value with an exclamation mark (@!@) in order
    -- to perform negation filters.
    values :: Core.Maybe (Core.NonEmpty Core.Text)
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
-- 'key', 'filter_key' - Filters your list of secrets by a specific key.
--
-- 'values', 'filter_values' - Filters your list of secrets by a specific value.
--
-- You can prefix your search value with an exclamation mark (@!@) in order
-- to perform negation filters.
newFilter ::
  Filter
newFilter =
  Filter' {key = Core.Nothing, values = Core.Nothing}

-- | Filters your list of secrets by a specific key.
filter_key :: Lens.Lens' Filter (Core.Maybe FilterNameStringType)
filter_key = Lens.lens (\Filter' {key} -> key) (\s@Filter' {} a -> s {key = a} :: Filter)

-- | Filters your list of secrets by a specific value.
--
-- You can prefix your search value with an exclamation mark (@!@) in order
-- to perform negation filters.
filter_values :: Lens.Lens' Filter (Core.Maybe (Core.NonEmpty Core.Text))
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable Filter

instance Core.NFData Filter

instance Core.ToJSON Filter where
  toJSON Filter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values
          ]
      )
