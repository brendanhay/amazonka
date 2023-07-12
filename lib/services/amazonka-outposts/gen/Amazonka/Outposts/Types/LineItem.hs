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
-- Module      : Amazonka.Outposts.Types.LineItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.LineItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.LineItemAssetInformation
import Amazonka.Outposts.Types.LineItemStatus
import Amazonka.Outposts.Types.ShipmentInformation
import qualified Amazonka.Prelude as Prelude

-- | Information about a line item.
--
-- /See:/ 'newLineItem' smart constructor.
data LineItem = LineItem'
  { -- | Information about assets.
    assetInformationList :: Prelude.Maybe [LineItemAssetInformation],
    -- | The ID of the catalog item.
    catalogItemId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the line item.
    lineItemId :: Prelude.Maybe Prelude.Text,
    -- | The quantity of the line item.
    quantity :: Prelude.Maybe Prelude.Natural,
    -- | Information about a line item shipment.
    shipmentInformation :: Prelude.Maybe ShipmentInformation,
    -- | The status of the line item.
    status :: Prelude.Maybe LineItemStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetInformationList', 'lineItem_assetInformationList' - Information about assets.
--
-- 'catalogItemId', 'lineItem_catalogItemId' - The ID of the catalog item.
--
-- 'lineItemId', 'lineItem_lineItemId' - The ID of the line item.
--
-- 'quantity', 'lineItem_quantity' - The quantity of the line item.
--
-- 'shipmentInformation', 'lineItem_shipmentInformation' - Information about a line item shipment.
--
-- 'status', 'lineItem_status' - The status of the line item.
newLineItem ::
  LineItem
newLineItem =
  LineItem'
    { assetInformationList = Prelude.Nothing,
      catalogItemId = Prelude.Nothing,
      lineItemId = Prelude.Nothing,
      quantity = Prelude.Nothing,
      shipmentInformation = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Information about assets.
lineItem_assetInformationList :: Lens.Lens' LineItem (Prelude.Maybe [LineItemAssetInformation])
lineItem_assetInformationList = Lens.lens (\LineItem' {assetInformationList} -> assetInformationList) (\s@LineItem' {} a -> s {assetInformationList = a} :: LineItem) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the catalog item.
lineItem_catalogItemId :: Lens.Lens' LineItem (Prelude.Maybe Prelude.Text)
lineItem_catalogItemId = Lens.lens (\LineItem' {catalogItemId} -> catalogItemId) (\s@LineItem' {} a -> s {catalogItemId = a} :: LineItem)

-- | The ID of the line item.
lineItem_lineItemId :: Lens.Lens' LineItem (Prelude.Maybe Prelude.Text)
lineItem_lineItemId = Lens.lens (\LineItem' {lineItemId} -> lineItemId) (\s@LineItem' {} a -> s {lineItemId = a} :: LineItem)

-- | The quantity of the line item.
lineItem_quantity :: Lens.Lens' LineItem (Prelude.Maybe Prelude.Natural)
lineItem_quantity = Lens.lens (\LineItem' {quantity} -> quantity) (\s@LineItem' {} a -> s {quantity = a} :: LineItem)

-- | Information about a line item shipment.
lineItem_shipmentInformation :: Lens.Lens' LineItem (Prelude.Maybe ShipmentInformation)
lineItem_shipmentInformation = Lens.lens (\LineItem' {shipmentInformation} -> shipmentInformation) (\s@LineItem' {} a -> s {shipmentInformation = a} :: LineItem)

-- | The status of the line item.
lineItem_status :: Lens.Lens' LineItem (Prelude.Maybe LineItemStatus)
lineItem_status = Lens.lens (\LineItem' {status} -> status) (\s@LineItem' {} a -> s {status = a} :: LineItem)

instance Data.FromJSON LineItem where
  parseJSON =
    Data.withObject
      "LineItem"
      ( \x ->
          LineItem'
            Prelude.<$> ( x
                            Data..:? "AssetInformationList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CatalogItemId")
            Prelude.<*> (x Data..:? "LineItemId")
            Prelude.<*> (x Data..:? "Quantity")
            Prelude.<*> (x Data..:? "ShipmentInformation")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable LineItem where
  hashWithSalt _salt LineItem' {..} =
    _salt
      `Prelude.hashWithSalt` assetInformationList
      `Prelude.hashWithSalt` catalogItemId
      `Prelude.hashWithSalt` lineItemId
      `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` shipmentInformation
      `Prelude.hashWithSalt` status

instance Prelude.NFData LineItem where
  rnf LineItem' {..} =
    Prelude.rnf assetInformationList
      `Prelude.seq` Prelude.rnf catalogItemId
      `Prelude.seq` Prelude.rnf lineItemId
      `Prelude.seq` Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf shipmentInformation
      `Prelude.seq` Prelude.rnf status
