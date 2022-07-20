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
-- Module      : Amazonka.SecretsManager.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecretsManager.Types.FilterNameStringType

-- | Allows you to add filters when you use the search function in Secrets
-- Manager.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | Filters your list of secrets by a specific key.
    key :: Prelude.Maybe FilterNameStringType,
    -- | Filters your list of secrets by a specific value.
    --
    -- You can prefix your search value with an exclamation mark (@!@) in order
    -- to perform negation filters.
    values :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Filter'
    { key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | Filters your list of secrets by a specific key.
filter_key :: Lens.Lens' Filter (Prelude.Maybe FilterNameStringType)
filter_key = Lens.lens (\Filter' {key} -> key) (\s@Filter' {} a -> s {key = a} :: Filter)

-- | Filters your list of secrets by a specific value.
--
-- You can prefix your search value with an exclamation mark (@!@) in order
-- to perform negation filters.
filter_values :: Lens.Lens' Filter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Core.ToJSON Filter where
  toJSON Filter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("Values" Core..=) Prelude.<$> values
          ]
      )
