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
-- Module      : Network.AWS.CloudFront.Types.QueryStringNames
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryStringNames where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains a list of query string names.
--
-- /See:/ 'newQueryStringNames' smart constructor.
data QueryStringNames = QueryStringNames'
  { -- | A list of query string names.
    items :: Core.Maybe [Core.Text],
    -- | The number of query string names in the @Items@ list.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueryStringNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'queryStringNames_items' - A list of query string names.
--
-- 'quantity', 'queryStringNames_quantity' - The number of query string names in the @Items@ list.
newQueryStringNames ::
  -- | 'quantity'
  Core.Int ->
  QueryStringNames
newQueryStringNames pQuantity_ =
  QueryStringNames'
    { items = Core.Nothing,
      quantity = pQuantity_
    }

-- | A list of query string names.
queryStringNames_items :: Lens.Lens' QueryStringNames (Core.Maybe [Core.Text])
queryStringNames_items = Lens.lens (\QueryStringNames' {items} -> items) (\s@QueryStringNames' {} a -> s {items = a} :: QueryStringNames) Core.. Lens.mapping Lens._Coerce

-- | The number of query string names in the @Items@ list.
queryStringNames_quantity :: Lens.Lens' QueryStringNames Core.Int
queryStringNames_quantity = Lens.lens (\QueryStringNames' {quantity} -> quantity) (\s@QueryStringNames' {} a -> s {quantity = a} :: QueryStringNames)

instance Core.FromXML QueryStringNames where
  parseXML x =
    QueryStringNames'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Name")
               )
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable QueryStringNames

instance Core.NFData QueryStringNames

instance Core.ToXML QueryStringNames where
  toXML QueryStringNames' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML (Core.toXMLList "Name" Core.<$> items),
        "Quantity" Core.@= quantity
      ]
