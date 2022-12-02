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
-- Module      : Amazonka.Outposts.Types.LineItemRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.LineItemRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a line item request.
--
-- /See:/ 'newLineItemRequest' smart constructor.
data LineItemRequest = LineItemRequest'
  { -- | The quantity of a line item request.
    quantity :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the catalog item.
    catalogItemId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineItemRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'lineItemRequest_quantity' - The quantity of a line item request.
--
-- 'catalogItemId', 'lineItemRequest_catalogItemId' - The ID of the catalog item.
newLineItemRequest ::
  LineItemRequest
newLineItemRequest =
  LineItemRequest'
    { quantity = Prelude.Nothing,
      catalogItemId = Prelude.Nothing
    }

-- | The quantity of a line item request.
lineItemRequest_quantity :: Lens.Lens' LineItemRequest (Prelude.Maybe Prelude.Natural)
lineItemRequest_quantity = Lens.lens (\LineItemRequest' {quantity} -> quantity) (\s@LineItemRequest' {} a -> s {quantity = a} :: LineItemRequest)

-- | The ID of the catalog item.
lineItemRequest_catalogItemId :: Lens.Lens' LineItemRequest (Prelude.Maybe Prelude.Text)
lineItemRequest_catalogItemId = Lens.lens (\LineItemRequest' {catalogItemId} -> catalogItemId) (\s@LineItemRequest' {} a -> s {catalogItemId = a} :: LineItemRequest)

instance Prelude.Hashable LineItemRequest where
  hashWithSalt _salt LineItemRequest' {..} =
    _salt `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` catalogItemId

instance Prelude.NFData LineItemRequest where
  rnf LineItemRequest' {..} =
    Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf catalogItemId

instance Data.ToJSON LineItemRequest where
  toJSON LineItemRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Quantity" Data..=) Prelude.<$> quantity,
            ("CatalogItemId" Data..=) Prelude.<$> catalogItemId
          ]
      )
