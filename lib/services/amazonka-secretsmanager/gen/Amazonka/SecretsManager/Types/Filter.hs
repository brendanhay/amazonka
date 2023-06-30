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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecretsManager.Types.FilterNameStringType

-- | Allows you to add filters when you use the search function in Secrets
-- Manager. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/manage_search-secret.html Find secrets in Secrets Manager>.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The following are keys you can use:
    --
    -- -   __description__: Prefix match, not case-sensitive.
    --
    -- -   __name__: Prefix match, case-sensitive.
    --
    -- -   __tag-key__: Prefix match, case-sensitive.
    --
    -- -   __tag-value__: Prefix match, case-sensitive.
    --
    -- -   __primary-region__: Prefix match, case-sensitive.
    --
    -- -   __all__: Breaks the filter value string into words and then searches
    --     all attributes for matches. Not case-sensitive.
    key :: Prelude.Maybe FilterNameStringType,
    -- | The keyword to filter for.
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
-- 'key', 'filter_key' - The following are keys you can use:
--
-- -   __description__: Prefix match, not case-sensitive.
--
-- -   __name__: Prefix match, case-sensitive.
--
-- -   __tag-key__: Prefix match, case-sensitive.
--
-- -   __tag-value__: Prefix match, case-sensitive.
--
-- -   __primary-region__: Prefix match, case-sensitive.
--
-- -   __all__: Breaks the filter value string into words and then searches
--     all attributes for matches. Not case-sensitive.
--
-- 'values', 'filter_values' - The keyword to filter for.
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

-- | The following are keys you can use:
--
-- -   __description__: Prefix match, not case-sensitive.
--
-- -   __name__: Prefix match, case-sensitive.
--
-- -   __tag-key__: Prefix match, case-sensitive.
--
-- -   __tag-value__: Prefix match, case-sensitive.
--
-- -   __primary-region__: Prefix match, case-sensitive.
--
-- -   __all__: Breaks the filter value string into words and then searches
--     all attributes for matches. Not case-sensitive.
filter_key :: Lens.Lens' Filter (Prelude.Maybe FilterNameStringType)
filter_key = Lens.lens (\Filter' {key} -> key) (\s@Filter' {} a -> s {key = a} :: Filter)

-- | The keyword to filter for.
--
-- You can prefix your search value with an exclamation mark (@!@) in order
-- to perform negation filters.
filter_values :: Lens.Lens' Filter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
