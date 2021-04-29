{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.CostExplorer.Types.MatchOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { key :: Prelude.Maybe Prelude.Text,
    -- | The specific value of the Cost Category.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The match options that you can use to filter your results. MatchOptions
    -- is only applicable for actions related to cost category. The default
    -- values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Prelude.Maybe [MatchOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { key = Prelude.Nothing,
      values = Prelude.Nothing,
      matchOptions = Prelude.Nothing
    }

-- | Undocumented member.
costCategoryValues_key :: Lens.Lens' CostCategoryValues (Prelude.Maybe Prelude.Text)
costCategoryValues_key = Lens.lens (\CostCategoryValues' {key} -> key) (\s@CostCategoryValues' {} a -> s {key = a} :: CostCategoryValues)

-- | The specific value of the Cost Category.
costCategoryValues_values :: Lens.Lens' CostCategoryValues (Prelude.Maybe [Prelude.Text])
costCategoryValues_values = Lens.lens (\CostCategoryValues' {values} -> values) (\s@CostCategoryValues' {} a -> s {values = a} :: CostCategoryValues) Prelude.. Lens.mapping Prelude._Coerce

-- | The match options that you can use to filter your results. MatchOptions
-- is only applicable for actions related to cost category. The default
-- values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@.
costCategoryValues_matchOptions :: Lens.Lens' CostCategoryValues (Prelude.Maybe [MatchOption])
costCategoryValues_matchOptions = Lens.lens (\CostCategoryValues' {matchOptions} -> matchOptions) (\s@CostCategoryValues' {} a -> s {matchOptions = a} :: CostCategoryValues) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON CostCategoryValues where
  parseJSON =
    Prelude.withObject
      "CostCategoryValues"
      ( \x ->
          CostCategoryValues'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Values" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "MatchOptions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CostCategoryValues

instance Prelude.NFData CostCategoryValues

instance Prelude.ToJSON CostCategoryValues where
  toJSON CostCategoryValues' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Values" Prelude..=) Prelude.<$> values,
            ("MatchOptions" Prelude..=)
              Prelude.<$> matchOptions
          ]
      )
