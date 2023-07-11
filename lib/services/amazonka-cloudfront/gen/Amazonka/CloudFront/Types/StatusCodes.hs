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
-- Module      : Amazonka.CloudFront.Types.StatusCodes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.StatusCodes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex data type for the status codes that you specify that, when
-- returned by a primary origin, trigger CloudFront to failover to a second
-- origin.
--
-- /See:/ 'newStatusCodes' smart constructor.
data StatusCodes = StatusCodes'
  { -- | The number of status codes.
    quantity :: Prelude.Int,
    -- | The items (status codes) for an origin group.
    items :: Prelude.NonEmpty Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatusCodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'statusCodes_quantity' - The number of status codes.
--
-- 'items', 'statusCodes_items' - The items (status codes) for an origin group.
newStatusCodes ::
  -- | 'quantity'
  Prelude.Int ->
  -- | 'items'
  Prelude.NonEmpty Prelude.Int ->
  StatusCodes
newStatusCodes pQuantity_ pItems_ =
  StatusCodes'
    { quantity = pQuantity_,
      items = Lens.coerced Lens.# pItems_
    }

-- | The number of status codes.
statusCodes_quantity :: Lens.Lens' StatusCodes Prelude.Int
statusCodes_quantity = Lens.lens (\StatusCodes' {quantity} -> quantity) (\s@StatusCodes' {} a -> s {quantity = a} :: StatusCodes)

-- | The items (status codes) for an origin group.
statusCodes_items :: Lens.Lens' StatusCodes (Prelude.NonEmpty Prelude.Int)
statusCodes_items = Lens.lens (\StatusCodes' {items} -> items) (\s@StatusCodes' {} a -> s {items = a} :: StatusCodes) Prelude.. Lens.coerced

instance Data.FromXML StatusCodes where
  parseXML x =
    StatusCodes'
      Prelude.<$> (x Data..@ "Quantity")
      Prelude.<*> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList1 "StatusCode"
                  )

instance Prelude.Hashable StatusCodes where
  hashWithSalt _salt StatusCodes' {..} =
    _salt
      `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` items

instance Prelude.NFData StatusCodes where
  rnf StatusCodes' {..} =
    Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf items

instance Data.ToXML StatusCodes where
  toXML StatusCodes' {..} =
    Prelude.mconcat
      [ "Quantity" Data.@= quantity,
        "Items" Data.@= Data.toXMLList "StatusCode" items
      ]
