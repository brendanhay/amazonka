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
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryValues where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.MatchOption
import qualified Network.AWS.Lens as Lens

-- | The Cost Categories values used for filtering the costs.
--
-- If @Values@ and @Key@ are not specified, the @ABSENT@ @MatchOption@ is
-- applied to all Cost Categories. That is, filtering on resources that are
-- not mapped to any Cost Categories.
--
-- If @Values@ is provided and @Key@ is not specified, the @ABSENT@
-- @MatchOption@ is applied to the Cost Categories @Key@ only. That is,
-- filtering on resources without the given Cost Categories key.
--
-- /See:/ 'newCostCategoryValues' smart constructor.
data CostCategoryValues = CostCategoryValues'
  { key :: Core.Maybe Core.Text,
    -- | The specific value of the Cost Category.
    values :: Core.Maybe [Core.Text],
    -- | The match options that you can use to filter your results. MatchOptions
    -- is only applicable for actions related to cost category. The default
    -- values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Core.Maybe [MatchOption]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'values', 'costCategoryValues_values' - The specific value of the Cost Category.
--
-- 'matchOptions', 'costCategoryValues_matchOptions' - The match options that you can use to filter your results. MatchOptions
-- is only applicable for actions related to cost category. The default
-- values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@.
newCostCategoryValues ::
  CostCategoryValues
newCostCategoryValues =
  CostCategoryValues'
    { key = Core.Nothing,
      values = Core.Nothing,
      matchOptions = Core.Nothing
    }

-- | Undocumented member.
costCategoryValues_key :: Lens.Lens' CostCategoryValues (Core.Maybe Core.Text)
costCategoryValues_key = Lens.lens (\CostCategoryValues' {key} -> key) (\s@CostCategoryValues' {} a -> s {key = a} :: CostCategoryValues)

-- | The specific value of the Cost Category.
costCategoryValues_values :: Lens.Lens' CostCategoryValues (Core.Maybe [Core.Text])
costCategoryValues_values = Lens.lens (\CostCategoryValues' {values} -> values) (\s@CostCategoryValues' {} a -> s {values = a} :: CostCategoryValues) Core.. Lens.mapping Lens._Coerce

-- | The match options that you can use to filter your results. MatchOptions
-- is only applicable for actions related to cost category. The default
-- values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@.
costCategoryValues_matchOptions :: Lens.Lens' CostCategoryValues (Core.Maybe [MatchOption])
costCategoryValues_matchOptions = Lens.lens (\CostCategoryValues' {matchOptions} -> matchOptions) (\s@CostCategoryValues' {} a -> s {matchOptions = a} :: CostCategoryValues) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON CostCategoryValues where
  parseJSON =
    Core.withObject
      "CostCategoryValues"
      ( \x ->
          CostCategoryValues'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Values" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MatchOptions" Core..!= Core.mempty)
      )

instance Core.Hashable CostCategoryValues

instance Core.NFData CostCategoryValues

instance Core.ToJSON CostCategoryValues where
  toJSON CostCategoryValues' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values,
            ("MatchOptions" Core..=) Core.<$> matchOptions
          ]
      )
