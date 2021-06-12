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
-- Module      : Network.AWS.CloudFront.Types.OriginGroupMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupMembers where

import Network.AWS.CloudFront.Types.OriginGroupMember
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex data type for the origins included in an origin group.
--
-- /See:/ 'newOriginGroupMembers' smart constructor.
data OriginGroupMembers = OriginGroupMembers'
  { -- | The number of origins in an origin group.
    quantity :: Core.Int,
    -- | Items (origins) in an origin group.
    items :: Core.NonEmpty OriginGroupMember
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'items'
  Core.NonEmpty OriginGroupMember ->
  OriginGroupMembers
newOriginGroupMembers pQuantity_ pItems_ =
  OriginGroupMembers'
    { quantity = pQuantity_,
      items = Lens._Coerce Lens.# pItems_
    }

-- | The number of origins in an origin group.
originGroupMembers_quantity :: Lens.Lens' OriginGroupMembers Core.Int
originGroupMembers_quantity = Lens.lens (\OriginGroupMembers' {quantity} -> quantity) (\s@OriginGroupMembers' {} a -> s {quantity = a} :: OriginGroupMembers)

-- | Items (origins) in an origin group.
originGroupMembers_items :: Lens.Lens' OriginGroupMembers (Core.NonEmpty OriginGroupMember)
originGroupMembers_items = Lens.lens (\OriginGroupMembers' {items} -> items) (\s@OriginGroupMembers' {} a -> s {items = a} :: OriginGroupMembers) Core.. Lens._Coerce

instance Core.FromXML OriginGroupMembers where
  parseXML x =
    OriginGroupMembers'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.parseXMLList1 "OriginGroupMember"
               )

instance Core.Hashable OriginGroupMembers

instance Core.NFData OriginGroupMembers

instance Core.ToXML OriginGroupMembers where
  toXML OriginGroupMembers' {..} =
    Core.mconcat
      [ "Quantity" Core.@= quantity,
        "Items"
          Core.@= Core.toXMLList "OriginGroupMember" items
      ]
