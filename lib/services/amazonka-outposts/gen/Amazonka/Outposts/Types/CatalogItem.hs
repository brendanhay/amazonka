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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.CatalogItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.CatalogItemStatus
import Amazonka.Outposts.Types.EC2Capacity
import Amazonka.Outposts.Types.SupportedStorageEnum
import qualified Amazonka.Prelude as Prelude

-- | Information about a catalog item.
--
-- /See:/ 'newCatalogItem' smart constructor.
data CatalogItem = CatalogItem'
  { -- | The ID of the catalog item.
    catalogItemId :: Prelude.Maybe Prelude.Text,
    -- | Information about the EC2 capacity of an item.
    eC2Capacities :: Prelude.Maybe [EC2Capacity],
    -- | The status of a catalog item.
    itemStatus :: Prelude.Maybe CatalogItemStatus,
    -- | Information about the power draw of an item.
    powerKva :: Prelude.Maybe Prelude.Double,
    -- | The supported storage options for the catalog item.
    supportedStorage :: Prelude.Maybe [SupportedStorageEnum],
    -- | The uplink speed this catalog item requires for the connection to the
    -- Region.
    supportedUplinkGbps :: Prelude.Maybe [Prelude.Int],
    -- | The weight of the item in pounds.
    weightLbs :: Prelude.Maybe Prelude.Int
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
-- 'catalogItemId', 'catalogItem_catalogItemId' - The ID of the catalog item.
--
-- 'eC2Capacities', 'catalogItem_eC2Capacities' - Information about the EC2 capacity of an item.
--
-- 'itemStatus', 'catalogItem_itemStatus' - The status of a catalog item.
--
-- 'powerKva', 'catalogItem_powerKva' - Information about the power draw of an item.
--
-- 'supportedStorage', 'catalogItem_supportedStorage' - The supported storage options for the catalog item.
--
-- 'supportedUplinkGbps', 'catalogItem_supportedUplinkGbps' - The uplink speed this catalog item requires for the connection to the
-- Region.
--
-- 'weightLbs', 'catalogItem_weightLbs' - The weight of the item in pounds.
newCatalogItem ::
  CatalogItem
newCatalogItem =
  CatalogItem'
    { catalogItemId = Prelude.Nothing,
      eC2Capacities = Prelude.Nothing,
      itemStatus = Prelude.Nothing,
      powerKva = Prelude.Nothing,
      supportedStorage = Prelude.Nothing,
      supportedUplinkGbps = Prelude.Nothing,
      weightLbs = Prelude.Nothing
    }

-- | The ID of the catalog item.
catalogItem_catalogItemId :: Lens.Lens' CatalogItem (Prelude.Maybe Prelude.Text)
catalogItem_catalogItemId = Lens.lens (\CatalogItem' {catalogItemId} -> catalogItemId) (\s@CatalogItem' {} a -> s {catalogItemId = a} :: CatalogItem)

-- | Information about the EC2 capacity of an item.
catalogItem_eC2Capacities :: Lens.Lens' CatalogItem (Prelude.Maybe [EC2Capacity])
catalogItem_eC2Capacities = Lens.lens (\CatalogItem' {eC2Capacities} -> eC2Capacities) (\s@CatalogItem' {} a -> s {eC2Capacities = a} :: CatalogItem) Prelude.. Lens.mapping Lens.coerced

-- | The status of a catalog item.
catalogItem_itemStatus :: Lens.Lens' CatalogItem (Prelude.Maybe CatalogItemStatus)
catalogItem_itemStatus = Lens.lens (\CatalogItem' {itemStatus} -> itemStatus) (\s@CatalogItem' {} a -> s {itemStatus = a} :: CatalogItem)

-- | Information about the power draw of an item.
catalogItem_powerKva :: Lens.Lens' CatalogItem (Prelude.Maybe Prelude.Double)
catalogItem_powerKva = Lens.lens (\CatalogItem' {powerKva} -> powerKva) (\s@CatalogItem' {} a -> s {powerKva = a} :: CatalogItem)

-- | The supported storage options for the catalog item.
catalogItem_supportedStorage :: Lens.Lens' CatalogItem (Prelude.Maybe [SupportedStorageEnum])
catalogItem_supportedStorage = Lens.lens (\CatalogItem' {supportedStorage} -> supportedStorage) (\s@CatalogItem' {} a -> s {supportedStorage = a} :: CatalogItem) Prelude.. Lens.mapping Lens.coerced

-- | The uplink speed this catalog item requires for the connection to the
-- Region.
catalogItem_supportedUplinkGbps :: Lens.Lens' CatalogItem (Prelude.Maybe [Prelude.Int])
catalogItem_supportedUplinkGbps = Lens.lens (\CatalogItem' {supportedUplinkGbps} -> supportedUplinkGbps) (\s@CatalogItem' {} a -> s {supportedUplinkGbps = a} :: CatalogItem) Prelude.. Lens.mapping Lens.coerced

-- | The weight of the item in pounds.
catalogItem_weightLbs :: Lens.Lens' CatalogItem (Prelude.Maybe Prelude.Int)
catalogItem_weightLbs = Lens.lens (\CatalogItem' {weightLbs} -> weightLbs) (\s@CatalogItem' {} a -> s {weightLbs = a} :: CatalogItem)

instance Data.FromJSON CatalogItem where
  parseJSON =
    Data.withObject
      "CatalogItem"
      ( \x ->
          CatalogItem'
            Prelude.<$> (x Data..:? "CatalogItemId")
            Prelude.<*> (x Data..:? "EC2Capacities" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ItemStatus")
            Prelude.<*> (x Data..:? "PowerKva")
            Prelude.<*> ( x
                            Data..:? "SupportedStorage"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SupportedUplinkGbps"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "WeightLbs")
      )

instance Prelude.Hashable CatalogItem where
  hashWithSalt _salt CatalogItem' {..} =
    _salt
      `Prelude.hashWithSalt` catalogItemId
      `Prelude.hashWithSalt` eC2Capacities
      `Prelude.hashWithSalt` itemStatus
      `Prelude.hashWithSalt` powerKva
      `Prelude.hashWithSalt` supportedStorage
      `Prelude.hashWithSalt` supportedUplinkGbps
      `Prelude.hashWithSalt` weightLbs

instance Prelude.NFData CatalogItem where
  rnf CatalogItem' {..} =
    Prelude.rnf catalogItemId
      `Prelude.seq` Prelude.rnf eC2Capacities
      `Prelude.seq` Prelude.rnf itemStatus
      `Prelude.seq` Prelude.rnf powerKva
      `Prelude.seq` Prelude.rnf supportedStorage
      `Prelude.seq` Prelude.rnf supportedUplinkGbps
      `Prelude.seq` Prelude.rnf weightLbs
