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
-- Module      : Amazonka.Outposts.Types.CatalogItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.CatalogItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Outposts.Types.CatalogItemStatus
import Amazonka.Outposts.Types.EC2Capacity
import Amazonka.Outposts.Types.SupportedStorageEnum
import qualified Amazonka.Prelude as Prelude

-- | Information about a catalog item.
--
-- /See:/ 'newCatalogItem' smart constructor.
data CatalogItem = CatalogItem'
  { -- | Information about the power draw of an item.
    powerKva :: Prelude.Maybe Prelude.Double,
    -- | The weight of the item in pounds.
    weightLbs :: Prelude.Maybe Prelude.Int,
    -- | Information about the EC2 capacity of an item.
    eC2Capacities :: Prelude.Maybe [EC2Capacity],
    -- | The ID of the catalog item.
    catalogItemId :: Prelude.Maybe Prelude.Text,
    -- | The status of a catalog item.
    itemStatus :: Prelude.Maybe CatalogItemStatus,
    -- | The uplink speed this catalog item requires for the connection to the
    -- Region.
    supportedUplinkGbps :: Prelude.Maybe [Prelude.Int],
    -- | The supported storage options for the catalog item.
    supportedStorage :: Prelude.Maybe [SupportedStorageEnum]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'powerKva', 'catalogItem_powerKva' - Information about the power draw of an item.
--
-- 'weightLbs', 'catalogItem_weightLbs' - The weight of the item in pounds.
--
-- 'eC2Capacities', 'catalogItem_eC2Capacities' - Information about the EC2 capacity of an item.
--
-- 'catalogItemId', 'catalogItem_catalogItemId' - The ID of the catalog item.
--
-- 'itemStatus', 'catalogItem_itemStatus' - The status of a catalog item.
--
-- 'supportedUplinkGbps', 'catalogItem_supportedUplinkGbps' - The uplink speed this catalog item requires for the connection to the
-- Region.
--
-- 'supportedStorage', 'catalogItem_supportedStorage' - The supported storage options for the catalog item.
newCatalogItem ::
  CatalogItem
newCatalogItem =
  CatalogItem'
    { powerKva = Prelude.Nothing,
      weightLbs = Prelude.Nothing,
      eC2Capacities = Prelude.Nothing,
      catalogItemId = Prelude.Nothing,
      itemStatus = Prelude.Nothing,
      supportedUplinkGbps = Prelude.Nothing,
      supportedStorage = Prelude.Nothing
    }

-- | Information about the power draw of an item.
catalogItem_powerKva :: Lens.Lens' CatalogItem (Prelude.Maybe Prelude.Double)
catalogItem_powerKva = Lens.lens (\CatalogItem' {powerKva} -> powerKva) (\s@CatalogItem' {} a -> s {powerKva = a} :: CatalogItem)

-- | The weight of the item in pounds.
catalogItem_weightLbs :: Lens.Lens' CatalogItem (Prelude.Maybe Prelude.Int)
catalogItem_weightLbs = Lens.lens (\CatalogItem' {weightLbs} -> weightLbs) (\s@CatalogItem' {} a -> s {weightLbs = a} :: CatalogItem)

-- | Information about the EC2 capacity of an item.
catalogItem_eC2Capacities :: Lens.Lens' CatalogItem (Prelude.Maybe [EC2Capacity])
catalogItem_eC2Capacities = Lens.lens (\CatalogItem' {eC2Capacities} -> eC2Capacities) (\s@CatalogItem' {} a -> s {eC2Capacities = a} :: CatalogItem) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the catalog item.
catalogItem_catalogItemId :: Lens.Lens' CatalogItem (Prelude.Maybe Prelude.Text)
catalogItem_catalogItemId = Lens.lens (\CatalogItem' {catalogItemId} -> catalogItemId) (\s@CatalogItem' {} a -> s {catalogItemId = a} :: CatalogItem)

-- | The status of a catalog item.
catalogItem_itemStatus :: Lens.Lens' CatalogItem (Prelude.Maybe CatalogItemStatus)
catalogItem_itemStatus = Lens.lens (\CatalogItem' {itemStatus} -> itemStatus) (\s@CatalogItem' {} a -> s {itemStatus = a} :: CatalogItem)

-- | The uplink speed this catalog item requires for the connection to the
-- Region.
catalogItem_supportedUplinkGbps :: Lens.Lens' CatalogItem (Prelude.Maybe [Prelude.Int])
catalogItem_supportedUplinkGbps = Lens.lens (\CatalogItem' {supportedUplinkGbps} -> supportedUplinkGbps) (\s@CatalogItem' {} a -> s {supportedUplinkGbps = a} :: CatalogItem) Prelude.. Lens.mapping Lens.coerced

-- | The supported storage options for the catalog item.
catalogItem_supportedStorage :: Lens.Lens' CatalogItem (Prelude.Maybe [SupportedStorageEnum])
catalogItem_supportedStorage = Lens.lens (\CatalogItem' {supportedStorage} -> supportedStorage) (\s@CatalogItem' {} a -> s {supportedStorage = a} :: CatalogItem) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CatalogItem where
  parseJSON =
    Core.withObject
      "CatalogItem"
      ( \x ->
          CatalogItem'
            Prelude.<$> (x Core..:? "PowerKva")
            Prelude.<*> (x Core..:? "WeightLbs")
            Prelude.<*> (x Core..:? "EC2Capacities" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CatalogItemId")
            Prelude.<*> (x Core..:? "ItemStatus")
            Prelude.<*> ( x Core..:? "SupportedUplinkGbps"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "SupportedStorage"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CatalogItem where
  hashWithSalt _salt CatalogItem' {..} =
    _salt `Prelude.hashWithSalt` powerKva
      `Prelude.hashWithSalt` weightLbs
      `Prelude.hashWithSalt` eC2Capacities
      `Prelude.hashWithSalt` catalogItemId
      `Prelude.hashWithSalt` itemStatus
      `Prelude.hashWithSalt` supportedUplinkGbps
      `Prelude.hashWithSalt` supportedStorage

instance Prelude.NFData CatalogItem where
  rnf CatalogItem' {..} =
    Prelude.rnf powerKva
      `Prelude.seq` Prelude.rnf weightLbs
      `Prelude.seq` Prelude.rnf eC2Capacities
      `Prelude.seq` Prelude.rnf catalogItemId
      `Prelude.seq` Prelude.rnf itemStatus
      `Prelude.seq` Prelude.rnf supportedUplinkGbps
      `Prelude.seq` Prelude.rnf supportedStorage
