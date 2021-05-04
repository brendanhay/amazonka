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
-- Module      : Network.AWS.S3.Types.InventoryFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Specifies an inventory filter. The inventory only includes objects that
-- meet the filter\'s criteria.
--
-- /See:/ 'newInventoryFilter' smart constructor.
data InventoryFilter = InventoryFilter'
  { -- | The prefix that an object must have to be included in the inventory
    -- results.
    prefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML InventoryFilter where
  parseXML x =
    InventoryFilter'
      Prelude.<$> (x Prelude..@ "Prefix")

instance Prelude.Hashable InventoryFilter

instance Prelude.NFData InventoryFilter

instance Prelude.ToXML InventoryFilter where
  toXML InventoryFilter' {..} =
    Prelude.mconcat ["Prefix" Prelude.@= prefix]
