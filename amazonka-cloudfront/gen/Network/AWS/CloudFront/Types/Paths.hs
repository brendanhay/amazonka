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
-- Module      : Network.AWS.CloudFront.Types.Paths
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Paths where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
paths_items = Lens.lens (\Paths' {items} -> items) (\s@Paths' {} a -> s {items = a} :: Paths) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of invalidation paths specified for the objects that you want
-- to invalidate.
paths_quantity :: Lens.Lens' Paths Prelude.Int
paths_quantity = Lens.lens (\Paths' {quantity} -> quantity) (\s@Paths' {} a -> s {quantity = a} :: Paths)

instance Prelude.FromXML Paths where
  parseXML x =
    Paths'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Path")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable Paths

instance Prelude.NFData Paths

instance Prelude.ToXML Paths where
  toXML Paths' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Path" Prelude.<$> items),
        "Quantity" Prelude.@= quantity
      ]
