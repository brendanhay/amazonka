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
-- Module      : Amazonka.CloudFront.Types.OriginGroupMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginGroupMembers where

import Amazonka.CloudFront.Types.OriginGroupMember
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex data type for the origins included in an origin group.
--
-- /See:/ 'newOriginGroupMembers' smart constructor.
data OriginGroupMembers = OriginGroupMembers'
  { -- | The number of origins in an origin group.
    quantity :: Prelude.Int,
    -- | Items (origins) in an origin group.
    items :: Prelude.NonEmpty OriginGroupMember
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginGroupMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'originGroupMembers_quantity' - The number of origins in an origin group.
--
-- 'items', 'originGroupMembers_items' - Items (origins) in an origin group.
newOriginGroupMembers ::
  -- | 'quantity'
  Prelude.Int ->
  -- | 'items'
  Prelude.NonEmpty OriginGroupMember ->
  OriginGroupMembers
newOriginGroupMembers pQuantity_ pItems_ =
  OriginGroupMembers'
    { quantity = pQuantity_,
      items = Lens.coerced Lens.# pItems_
    }

-- | The number of origins in an origin group.
originGroupMembers_quantity :: Lens.Lens' OriginGroupMembers Prelude.Int
originGroupMembers_quantity = Lens.lens (\OriginGroupMembers' {quantity} -> quantity) (\s@OriginGroupMembers' {} a -> s {quantity = a} :: OriginGroupMembers)

-- | Items (origins) in an origin group.
originGroupMembers_items :: Lens.Lens' OriginGroupMembers (Prelude.NonEmpty OriginGroupMember)
originGroupMembers_items = Lens.lens (\OriginGroupMembers' {items} -> items) (\s@OriginGroupMembers' {} a -> s {items = a} :: OriginGroupMembers) Prelude.. Lens.coerced

instance Data.FromXML OriginGroupMembers where
  parseXML x =
    OriginGroupMembers'
      Prelude.<$> (x Data..@ "Quantity")
      Prelude.<*> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList1 "OriginGroupMember"
                  )

instance Prelude.Hashable OriginGroupMembers where
  hashWithSalt _salt OriginGroupMembers' {..} =
    _salt `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` items

instance Prelude.NFData OriginGroupMembers where
  rnf OriginGroupMembers' {..} =
    Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf items

instance Data.ToXML OriginGroupMembers where
  toXML OriginGroupMembers' {..} =
    Prelude.mconcat
      [ "Quantity" Data.@= quantity,
        "Items"
          Data.@= Data.toXMLList "OriginGroupMember" items
      ]
