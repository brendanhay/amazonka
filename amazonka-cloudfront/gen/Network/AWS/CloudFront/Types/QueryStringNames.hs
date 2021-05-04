{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains a list of query string names.
--
-- /See:/ 'newQueryStringNames' smart constructor.
data QueryStringNames = QueryStringNames'
  { -- | A list of query string names.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of query string names in the @Items@ list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  QueryStringNames
newQueryStringNames pQuantity_ =
  QueryStringNames'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | A list of query string names.
queryStringNames_items :: Lens.Lens' QueryStringNames (Prelude.Maybe [Prelude.Text])
queryStringNames_items = Lens.lens (\QueryStringNames' {items} -> items) (\s@QueryStringNames' {} a -> s {items = a} :: QueryStringNames) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of query string names in the @Items@ list.
queryStringNames_quantity :: Lens.Lens' QueryStringNames Prelude.Int
queryStringNames_quantity = Lens.lens (\QueryStringNames' {quantity} -> quantity) (\s@QueryStringNames' {} a -> s {quantity = a} :: QueryStringNames)

instance Prelude.FromXML QueryStringNames where
  parseXML x =
    QueryStringNames'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Name")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable QueryStringNames

instance Prelude.NFData QueryStringNames

instance Prelude.ToXML QueryStringNames where
  toXML QueryStringNames' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Name" Prelude.<$> items),
        "Quantity" Prelude.@= quantity
      ]
