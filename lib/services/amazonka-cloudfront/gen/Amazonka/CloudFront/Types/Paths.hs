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
-- Module      : Amazonka.CloudFront.Types.Paths
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.Paths where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains information about the objects that you want
-- to invalidate. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newPaths' smart constructor.
data Paths = Paths'
  { -- | A complex type that contains a list of the paths that you want to
    -- invalidate.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of invalidation paths specified for the objects that you want
    -- to invalidate.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Paths' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'paths_items' - A complex type that contains a list of the paths that you want to
-- invalidate.
--
-- 'quantity', 'paths_quantity' - The number of invalidation paths specified for the objects that you want
-- to invalidate.
newPaths ::
  -- | 'quantity'
  Prelude.Int ->
  Paths
newPaths pQuantity_ =
  Paths'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | A complex type that contains a list of the paths that you want to
-- invalidate.
paths_items :: Lens.Lens' Paths (Prelude.Maybe [Prelude.Text])
paths_items = Lens.lens (\Paths' {items} -> items) (\s@Paths' {} a -> s {items = a} :: Paths) Prelude.. Lens.mapping Lens.coerced

-- | The number of invalidation paths specified for the objects that you want
-- to invalidate.
paths_quantity :: Lens.Lens' Paths Prelude.Int
paths_quantity = Lens.lens (\Paths' {quantity} -> quantity) (\s@Paths' {} a -> s {quantity = a} :: Paths)

instance Data.FromXML Paths where
  parseXML x =
    Paths'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Path")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable Paths where
  hashWithSalt _salt Paths' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData Paths where
  rnf Paths' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Data.ToXML Paths where
  toXML Paths' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML (Data.toXMLList "Path" Prelude.<$> items),
        "Quantity" Data.@= quantity
      ]
