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
-- Module      : Amazonka.QuickSight.Types.ItemsLimitConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ItemsLimitConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.OtherCategories

-- | The limit configuration of the visual display for an axis.
--
-- /See:/ 'newItemsLimitConfiguration' smart constructor.
data ItemsLimitConfiguration = ItemsLimitConfiguration'
  { -- | The limit on how many items of a field are showed in the chart. For
    -- example, the number of slices that are displayed in a pie chart.
    itemsLimit :: Prelude.Maybe Prelude.Integer,
    -- | The @Show other@ of an axis in the chart. Choose one of the following
    -- options:
    --
    -- -   @INCLUDE@
    --
    -- -   @EXCLUDE@
    otherCategories :: Prelude.Maybe OtherCategories
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ItemsLimitConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemsLimit', 'itemsLimitConfiguration_itemsLimit' - The limit on how many items of a field are showed in the chart. For
-- example, the number of slices that are displayed in a pie chart.
--
-- 'otherCategories', 'itemsLimitConfiguration_otherCategories' - The @Show other@ of an axis in the chart. Choose one of the following
-- options:
--
-- -   @INCLUDE@
--
-- -   @EXCLUDE@
newItemsLimitConfiguration ::
  ItemsLimitConfiguration
newItemsLimitConfiguration =
  ItemsLimitConfiguration'
    { itemsLimit =
        Prelude.Nothing,
      otherCategories = Prelude.Nothing
    }

-- | The limit on how many items of a field are showed in the chart. For
-- example, the number of slices that are displayed in a pie chart.
itemsLimitConfiguration_itemsLimit :: Lens.Lens' ItemsLimitConfiguration (Prelude.Maybe Prelude.Integer)
itemsLimitConfiguration_itemsLimit = Lens.lens (\ItemsLimitConfiguration' {itemsLimit} -> itemsLimit) (\s@ItemsLimitConfiguration' {} a -> s {itemsLimit = a} :: ItemsLimitConfiguration)

-- | The @Show other@ of an axis in the chart. Choose one of the following
-- options:
--
-- -   @INCLUDE@
--
-- -   @EXCLUDE@
itemsLimitConfiguration_otherCategories :: Lens.Lens' ItemsLimitConfiguration (Prelude.Maybe OtherCategories)
itemsLimitConfiguration_otherCategories = Lens.lens (\ItemsLimitConfiguration' {otherCategories} -> otherCategories) (\s@ItemsLimitConfiguration' {} a -> s {otherCategories = a} :: ItemsLimitConfiguration)

instance Data.FromJSON ItemsLimitConfiguration where
  parseJSON =
    Data.withObject
      "ItemsLimitConfiguration"
      ( \x ->
          ItemsLimitConfiguration'
            Prelude.<$> (x Data..:? "ItemsLimit")
            Prelude.<*> (x Data..:? "OtherCategories")
      )

instance Prelude.Hashable ItemsLimitConfiguration where
  hashWithSalt _salt ItemsLimitConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` itemsLimit
      `Prelude.hashWithSalt` otherCategories

instance Prelude.NFData ItemsLimitConfiguration where
  rnf ItemsLimitConfiguration' {..} =
    Prelude.rnf itemsLimit `Prelude.seq`
      Prelude.rnf otherCategories

instance Data.ToJSON ItemsLimitConfiguration where
  toJSON ItemsLimitConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ItemsLimit" Data..=) Prelude.<$> itemsLimit,
            ("OtherCategories" Data..=)
              Prelude.<$> otherCategories
          ]
      )
