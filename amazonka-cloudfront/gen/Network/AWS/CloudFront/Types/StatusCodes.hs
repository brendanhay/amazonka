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
-- Module      : Network.AWS.CloudFront.Types.StatusCodes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StatusCodes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex data type for the status codes that you specify that, when
-- returned by a primary origin, trigger CloudFront to failover to a second
-- origin.
--
-- /See:/ 'newStatusCodes' smart constructor.
data StatusCodes = StatusCodes'
  { -- | The number of status codes.
    quantity :: Core.Int,
    -- | The items (status codes) for an origin group.
    items :: Core.NonEmpty Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'items'
  Core.NonEmpty Core.Int ->
  StatusCodes
newStatusCodes pQuantity_ pItems_ =
  StatusCodes'
    { quantity = pQuantity_,
      items = Lens._Coerce Lens.# pItems_
    }

-- | The number of status codes.
statusCodes_quantity :: Lens.Lens' StatusCodes Core.Int
statusCodes_quantity = Lens.lens (\StatusCodes' {quantity} -> quantity) (\s@StatusCodes' {} a -> s {quantity = a} :: StatusCodes)

-- | The items (status codes) for an origin group.
statusCodes_items :: Lens.Lens' StatusCodes (Core.NonEmpty Core.Int)
statusCodes_items = Lens.lens (\StatusCodes' {items} -> items) (\s@StatusCodes' {} a -> s {items = a} :: StatusCodes) Core.. Lens._Coerce

instance Core.FromXML StatusCodes where
  parseXML x =
    StatusCodes'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.parseXMLList1 "StatusCode"
               )

instance Core.Hashable StatusCodes

instance Core.NFData StatusCodes

instance Core.ToXML StatusCodes where
  toXML StatusCodes' {..} =
    Core.mconcat
      [ "Quantity" Core.@= quantity,
        "Items" Core.@= Core.toXMLList "StatusCode" items
      ]
