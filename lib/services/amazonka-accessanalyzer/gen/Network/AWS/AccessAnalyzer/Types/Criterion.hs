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
-- Module      : Network.AWS.AccessAnalyzer.Types.Criterion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.Criterion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The criteria to use in the filter that defines the archive rule.
--
-- /See:/ 'newCriterion' smart constructor.
data Criterion = Criterion'
  { -- | An \"equals\" operator to match for the filter used to create the rule.
    eq :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An \"exists\" operator to match for the filter used to create the rule.
    exists :: Prelude.Maybe Prelude.Bool,
    -- | A \"not equals\" operator to match for the filter used to create the
    -- rule.
    neq :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A \"contains\" operator to match for the filter used to create the rule.
    contains :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'eq', 'criterion_eq' - An \"equals\" operator to match for the filter used to create the rule.
--
-- 'exists', 'criterion_exists' - An \"exists\" operator to match for the filter used to create the rule.
--
-- 'neq', 'criterion_neq' - A \"not equals\" operator to match for the filter used to create the
-- rule.
--
-- 'contains', 'criterion_contains' - A \"contains\" operator to match for the filter used to create the rule.
newCriterion ::
  Criterion
newCriterion =
  Criterion'
    { eq = Prelude.Nothing,
      exists = Prelude.Nothing,
      neq = Prelude.Nothing,
      contains = Prelude.Nothing
    }

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

-- | A \"contains\" operator to match for the filter used to create the rule.
criterion_contains :: Lens.Lens' Criterion (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
criterion_contains = Lens.lens (\Criterion' {contains} -> contains) (\s@Criterion' {} a -> s {contains = a} :: Criterion) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Criterion where
  parseJSON =
    Core.withObject
      "Criterion"
      ( \x ->
          Criterion'
            Prelude.<$> (x Core..:? "eq")
            Prelude.<*> (x Core..:? "exists")
            Prelude.<*> (x Core..:? "neq")
            Prelude.<*> (x Core..:? "contains")
      )

instance Prelude.Hashable Criterion

instance Prelude.NFData Criterion

instance Core.ToJSON Criterion where
  toJSON Criterion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("eq" Core..=) Prelude.<$> eq,
            ("exists" Core..=) Prelude.<$> exists,
            ("neq" Core..=) Prelude.<$> neq,
            ("contains" Core..=) Prelude.<$> contains
          ]
      )
