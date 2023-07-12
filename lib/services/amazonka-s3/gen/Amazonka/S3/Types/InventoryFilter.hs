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
-- Module      : Amazonka.S3.Types.InventoryFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.InventoryFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Specifies an inventory filter. The inventory only includes objects that
-- meet the filter\'s criteria.
--
-- /See:/ 'newInventoryFilter' smart constructor.
data InventoryFilter = InventoryFilter'
  { -- | The prefix that an object must have to be included in the inventory
    -- results.
    prefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'inventoryFilter_prefix' - The prefix that an object must have to be included in the inventory
-- results.
newInventoryFilter ::
  -- | 'prefix'
  Prelude.Text ->
  InventoryFilter
newInventoryFilter pPrefix_ =
  InventoryFilter' {prefix = pPrefix_}

-- | The prefix that an object must have to be included in the inventory
-- results.
inventoryFilter_prefix :: Lens.Lens' InventoryFilter Prelude.Text
inventoryFilter_prefix = Lens.lens (\InventoryFilter' {prefix} -> prefix) (\s@InventoryFilter' {} a -> s {prefix = a} :: InventoryFilter)

instance Data.FromXML InventoryFilter where
  parseXML x =
    InventoryFilter' Prelude.<$> (x Data..@ "Prefix")

instance Prelude.Hashable InventoryFilter where
  hashWithSalt _salt InventoryFilter' {..} =
    _salt `Prelude.hashWithSalt` prefix

instance Prelude.NFData InventoryFilter where
  rnf InventoryFilter' {..} = Prelude.rnf prefix

instance Data.ToXML InventoryFilter where
  toXML InventoryFilter' {..} =
    Prelude.mconcat ["Prefix" Data.@= prefix]
