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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.LineItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Outposts.Types.LineItemAssetInformation
import Amazonka.Outposts.Types.LineItemStatus
import Amazonka.Outposts.Types.ShipmentInformation
import qualified Amazonka.Prelude as Prelude

-- | Information about a line item.
--
-- /See:/ 'newLineItem' smart constructor.
data LineItem = LineItem'
  { -- | The quantity of the line item.
    quantity :: Prelude.Maybe Prelude.Natural,
    -- | Information about assets.
    assetInformationList :: Prelude.Maybe [LineItemAssetInformation],
    -- | The status of the line item.
    status :: Prelude.Maybe LineItemStatus,
    -- | The ID of the catalog item.
    catalogItemId :: Prelude.Maybe Prelude.Text,
    -- | Information about a line item shipment.
    shipmentInformation :: Prelude.Maybe ShipmentInformation,
    -- | The ID of the line item.
    lineItemId :: Prelude.Maybe Prelude.Text
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
-- 'quantity', 'lineItem_quantity' - The quantity of the line item.
--
-- 'assetInformationList', 'lineItem_assetInformationList' - Information about assets.
--
-- 'status', 'lineItem_status' - The status of the line item.
--
-- 'catalogItemId', 'lineItem_catalogItemId' - The ID of the catalog item.
--
-- 'shipmentInformation', 'lineItem_shipmentInformation' - Information about a line item shipment.
--
-- 'lineItemId', 'lineItem_lineItemId' - The ID of the line item.
newLineItem ::
  LineItem
newLineItem =
  LineItem'
    { quantity = Prelude.Nothing,
      assetInformationList = Prelude.Nothing,
      status = Prelude.Nothing,
      catalogItemId = Prelude.Nothing,
      shipmentInformation = Prelude.Nothing,
      lineItemId = Prelude.Nothing
    }

-- | The quantity of the line item.
lineItem_quantity :: Lens.Lens' LineItem (Prelude.Maybe Prelude.Natural)
lineItem_quantity = Lens.lens (\LineItem' {quantity} -> quantity) (\s@LineItem' {} a -> s {quantity = a} :: LineItem)

-- | Information about assets.
lineItem_assetInformationList :: Lens.Lens' LineItem (Prelude.Maybe [LineItemAssetInformation])
lineItem_assetInformationList = Lens.lens (\LineItem' {assetInformationList} -> assetInformationList) (\s@LineItem' {} a -> s {assetInformationList = a} :: LineItem) Prelude.. Lens.mapping Lens.coerced

-- | The status of the line item.
lineItem_status :: Lens.Lens' LineItem (Prelude.Maybe LineItemStatus)
lineItem_status = Lens.lens (\LineItem' {status} -> status) (\s@LineItem' {} a -> s {status = a} :: LineItem)

-- | The ID of the catalog item.
lineItem_catalogItemId :: Lens.Lens' LineItem (Prelude.Maybe Prelude.Text)
lineItem_catalogItemId = Lens.lens (\LineItem' {catalogItemId} -> catalogItemId) (\s@LineItem' {} a -> s {catalogItemId = a} :: LineItem)

-- | Information about a line item shipment.
lineItem_shipmentInformation :: Lens.Lens' LineItem (Prelude.Maybe ShipmentInformation)
lineItem_shipmentInformation = Lens.lens (\LineItem' {shipmentInformation} -> shipmentInformation) (\s@LineItem' {} a -> s {shipmentInformation = a} :: LineItem)

-- | The ID of the line item.
lineItem_lineItemId :: Lens.Lens' LineItem (Prelude.Maybe Prelude.Text)
lineItem_lineItemId = Lens.lens (\LineItem' {lineItemId} -> lineItemId) (\s@LineItem' {} a -> s {lineItemId = a} :: LineItem)

instance Core.FromJSON LineItem where
  parseJSON =
    Core.withObject
      "LineItem"
      ( \x ->
          LineItem'
            Prelude.<$> (x Core..:? "Quantity")
            Prelude.<*> ( x Core..:? "AssetInformationList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CatalogItemId")
            Prelude.<*> (x Core..:? "ShipmentInformation")
            Prelude.<*> (x Core..:? "LineItemId")
      )

instance Prelude.Hashable LineItem where
  hashWithSalt _salt LineItem' {..} =
    _salt `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` assetInformationList
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` catalogItemId
      `Prelude.hashWithSalt` shipmentInformation
      `Prelude.hashWithSalt` lineItemId

instance Prelude.NFData LineItem where
  rnf LineItem' {..} =
    Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf assetInformationList
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf catalogItemId
      `Prelude.seq` Prelude.rnf shipmentInformation
      `Prelude.seq` Prelude.rnf lineItemId
