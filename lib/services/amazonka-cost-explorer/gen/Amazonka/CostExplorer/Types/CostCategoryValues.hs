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
-- Module      : Amazonka.CostExplorer.Types.CostCategoryValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategoryValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.MatchOption
import qualified Amazonka.Prelude as Prelude

-- | The Cost Categories values used for filtering the costs.
--
-- If @Values@ and @Key@ are not specified, the @ABSENT@ @MatchOption@ is
-- applied to all Cost Categories. That is, it filters on resources that
-- aren\'t mapped to any Cost Categories.
--
-- If @Values@ is provided and @Key@ isn\'t specified, the @ABSENT@
-- @MatchOption@ is applied to the Cost Categories @Key@ only. That is, it
-- filters on resources without the given Cost Categories key.
--
-- /See:/ 'newCostCategoryValues' smart constructor.
data CostCategoryValues = CostCategoryValues'
  { key :: Prelude.Maybe Prelude.Text,
    -- | The match options that you can use to filter your results. MatchOptions
    -- is only applicable for actions related to cost category. The default
    -- values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Prelude.Maybe [MatchOption],
    -- | The specific value of the Cost Category.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostCategoryValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'costCategoryValues_key' - Undocumented member.
--
-- 'matchOptions', 'costCategoryValues_matchOptions' - The match options that you can use to filter your results. MatchOptions
-- is only applicable for actions related to cost category. The default
-- values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@.
--
-- 'values', 'costCategoryValues_values' - The specific value of the Cost Category.
newCostCategoryValues ::
  CostCategoryValues
newCostCategoryValues =
  CostCategoryValues'
    { key = Prelude.Nothing,
      matchOptions = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | Undocumented member.
costCategoryValues_key :: Lens.Lens' CostCategoryValues (Prelude.Maybe Prelude.Text)
costCategoryValues_key = Lens.lens (\CostCategoryValues' {key} -> key) (\s@CostCategoryValues' {} a -> s {key = a} :: CostCategoryValues)

-- | The match options that you can use to filter your results. MatchOptions
-- is only applicable for actions related to cost category. The default
-- values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@.
costCategoryValues_matchOptions :: Lens.Lens' CostCategoryValues (Prelude.Maybe [MatchOption])
costCategoryValues_matchOptions = Lens.lens (\CostCategoryValues' {matchOptions} -> matchOptions) (\s@CostCategoryValues' {} a -> s {matchOptions = a} :: CostCategoryValues) Prelude.. Lens.mapping Lens.coerced

-- | The specific value of the Cost Category.
costCategoryValues_values :: Lens.Lens' CostCategoryValues (Prelude.Maybe [Prelude.Text])
costCategoryValues_values = Lens.lens (\CostCategoryValues' {values} -> values) (\s@CostCategoryValues' {} a -> s {values = a} :: CostCategoryValues) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CostCategoryValues where
  parseJSON =
    Core.withObject
      "CostCategoryValues"
      ( \x ->
          CostCategoryValues'
            Prelude.<$> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "MatchOptions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Values" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable CostCategoryValues where
  hashWithSalt _salt CostCategoryValues' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` matchOptions
      `Prelude.hashWithSalt` values

instance Prelude.NFData CostCategoryValues where
  rnf CostCategoryValues' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf matchOptions
      `Prelude.seq` Prelude.rnf values

instance Core.ToJSON CostCategoryValues where
  toJSON CostCategoryValues' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("MatchOptions" Core..=) Prelude.<$> matchOptions,
            ("Values" Core..=) Prelude.<$> values
          ]
      )
