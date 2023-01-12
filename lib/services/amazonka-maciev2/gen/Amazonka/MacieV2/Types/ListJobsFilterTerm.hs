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
-- Module      : Amazonka.MacieV2.Types.ListJobsFilterTerm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ListJobsFilterTerm where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.JobComparator
import Amazonka.MacieV2.Types.ListJobsFilterKey
import qualified Amazonka.Prelude as Prelude

-- | Specifies a condition that filters the results of a request for
-- information about classification jobs. Each condition consists of a
-- property, an operator, and one or more values.
--
-- /See:/ 'newListJobsFilterTerm' smart constructor.
data ListJobsFilterTerm = ListJobsFilterTerm'
  { -- | The operator to use to filter the results.
    comparator :: Prelude.Maybe JobComparator,
    -- | The property to use to filter the results.
    key :: Prelude.Maybe ListJobsFilterKey,
    -- | An array that lists one or more values to use to filter the results.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobsFilterTerm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparator', 'listJobsFilterTerm_comparator' - The operator to use to filter the results.
--
-- 'key', 'listJobsFilterTerm_key' - The property to use to filter the results.
--
-- 'values', 'listJobsFilterTerm_values' - An array that lists one or more values to use to filter the results.
newListJobsFilterTerm ::
  ListJobsFilterTerm
newListJobsFilterTerm =
  ListJobsFilterTerm'
    { comparator = Prelude.Nothing,
      key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The operator to use to filter the results.
listJobsFilterTerm_comparator :: Lens.Lens' ListJobsFilterTerm (Prelude.Maybe JobComparator)
listJobsFilterTerm_comparator = Lens.lens (\ListJobsFilterTerm' {comparator} -> comparator) (\s@ListJobsFilterTerm' {} a -> s {comparator = a} :: ListJobsFilterTerm)

-- | The property to use to filter the results.
listJobsFilterTerm_key :: Lens.Lens' ListJobsFilterTerm (Prelude.Maybe ListJobsFilterKey)
listJobsFilterTerm_key = Lens.lens (\ListJobsFilterTerm' {key} -> key) (\s@ListJobsFilterTerm' {} a -> s {key = a} :: ListJobsFilterTerm)

-- | An array that lists one or more values to use to filter the results.
listJobsFilterTerm_values :: Lens.Lens' ListJobsFilterTerm (Prelude.Maybe [Prelude.Text])
listJobsFilterTerm_values = Lens.lens (\ListJobsFilterTerm' {values} -> values) (\s@ListJobsFilterTerm' {} a -> s {values = a} :: ListJobsFilterTerm) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ListJobsFilterTerm where
  hashWithSalt _salt ListJobsFilterTerm' {..} =
    _salt `Prelude.hashWithSalt` comparator
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData ListJobsFilterTerm where
  rnf ListJobsFilterTerm' {..} =
    Prelude.rnf comparator
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON ListJobsFilterTerm where
  toJSON ListJobsFilterTerm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comparator" Data..=) Prelude.<$> comparator,
            ("key" Data..=) Prelude.<$> key,
            ("values" Data..=) Prelude.<$> values
          ]
      )
