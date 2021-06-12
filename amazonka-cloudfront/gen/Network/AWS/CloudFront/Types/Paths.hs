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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex type that contains information about the objects that you want
-- to invalidate. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newPaths' smart constructor.
data Paths = Paths'
  { -- | A complex type that contains a list of the paths that you want to
    -- invalidate.
    items :: Core.Maybe [Core.Text],
    -- | The number of invalidation paths specified for the objects that you want
    -- to invalidate.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  Paths
newPaths pQuantity_ =
  Paths' {items = Core.Nothing, quantity = pQuantity_}

-- | A complex type that contains a list of the paths that you want to
-- invalidate.
paths_items :: Lens.Lens' Paths (Core.Maybe [Core.Text])
paths_items = Lens.lens (\Paths' {items} -> items) (\s@Paths' {} a -> s {items = a} :: Paths) Core.. Lens.mapping Lens._Coerce

-- | The number of invalidation paths specified for the objects that you want
-- to invalidate.
paths_quantity :: Lens.Lens' Paths Core.Int
paths_quantity = Lens.lens (\Paths' {quantity} -> quantity) (\s@Paths' {} a -> s {quantity = a} :: Paths)

instance Core.FromXML Paths where
  parseXML x =
    Paths'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Path")
               )
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable Paths

instance Core.NFData Paths

instance Core.ToXML Paths where
  toXML Paths' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML (Core.toXMLList "Path" Core.<$> items),
        "Quantity" Core.@= quantity
      ]
