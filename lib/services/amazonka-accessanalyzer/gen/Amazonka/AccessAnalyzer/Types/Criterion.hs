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
-- Module      : Amazonka.AccessAnalyzer.Types.Criterion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.Criterion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The criteria to use in the filter that defines the archive rule. For
-- more information on available filter keys, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-reference-filter-keys.html IAM Access Analyzer filter keys>.
--
-- /See:/ 'newCriterion' smart constructor.
data Criterion = Criterion'
  { -- | A \"contains\" operator to match for the filter used to create the rule.
    contains :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An \"equals\" operator to match for the filter used to create the rule.
    eq :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An \"exists\" operator to match for the filter used to create the rule.
    exists :: Prelude.Maybe Prelude.Bool,
    -- | A \"not equals\" operator to match for the filter used to create the
    -- rule.
    neq :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Criterion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contains', 'criterion_contains' - A \"contains\" operator to match for the filter used to create the rule.
--
-- 'eq', 'criterion_eq' - An \"equals\" operator to match for the filter used to create the rule.
--
-- 'exists', 'criterion_exists' - An \"exists\" operator to match for the filter used to create the rule.
--
-- 'neq', 'criterion_neq' - A \"not equals\" operator to match for the filter used to create the
-- rule.
newCriterion ::
  Criterion
newCriterion =
  Criterion'
    { contains = Prelude.Nothing,
      eq = Prelude.Nothing,
      exists = Prelude.Nothing,
      neq = Prelude.Nothing
    }

-- | A \"contains\" operator to match for the filter used to create the rule.
criterion_contains :: Lens.Lens' Criterion (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
criterion_contains = Lens.lens (\Criterion' {contains} -> contains) (\s@Criterion' {} a -> s {contains = a} :: Criterion) Prelude.. Lens.mapping Lens.coerced

-- | An \"equals\" operator to match for the filter used to create the rule.
criterion_eq :: Lens.Lens' Criterion (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
criterion_eq = Lens.lens (\Criterion' {eq} -> eq) (\s@Criterion' {} a -> s {eq = a} :: Criterion) Prelude.. Lens.mapping Lens.coerced

-- | An \"exists\" operator to match for the filter used to create the rule.
criterion_exists :: Lens.Lens' Criterion (Prelude.Maybe Prelude.Bool)
criterion_exists = Lens.lens (\Criterion' {exists} -> exists) (\s@Criterion' {} a -> s {exists = a} :: Criterion)

-- | A \"not equals\" operator to match for the filter used to create the
-- rule.
criterion_neq :: Lens.Lens' Criterion (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
criterion_neq = Lens.lens (\Criterion' {neq} -> neq) (\s@Criterion' {} a -> s {neq = a} :: Criterion) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Criterion where
  parseJSON =
    Data.withObject
      "Criterion"
      ( \x ->
          Criterion'
            Prelude.<$> (x Data..:? "contains")
            Prelude.<*> (x Data..:? "eq")
            Prelude.<*> (x Data..:? "exists")
            Prelude.<*> (x Data..:? "neq")
      )

instance Prelude.Hashable Criterion where
  hashWithSalt _salt Criterion' {..} =
    _salt
      `Prelude.hashWithSalt` contains
      `Prelude.hashWithSalt` eq
      `Prelude.hashWithSalt` exists
      `Prelude.hashWithSalt` neq

instance Prelude.NFData Criterion where
  rnf Criterion' {..} =
    Prelude.rnf contains
      `Prelude.seq` Prelude.rnf eq
      `Prelude.seq` Prelude.rnf exists
      `Prelude.seq` Prelude.rnf neq

instance Data.ToJSON Criterion where
  toJSON Criterion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contains" Data..=) Prelude.<$> contains,
            ("eq" Data..=) Prelude.<$> eq,
            ("exists" Data..=) Prelude.<$> exists,
            ("neq" Data..=) Prelude.<$> neq
          ]
      )
