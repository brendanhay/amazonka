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
-- Module      : Network.AWS.CloudFront.Types.Aliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Aliases where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
--
-- /See:/ 'newAliases' smart constructor.
data Aliases = Aliases'
  { -- | A complex type that contains the CNAME aliases, if any, that you want to
    -- associate with this distribution.
    items :: Core.Maybe [Core.Text],
    -- | The number of CNAME aliases, if any, that you want to associate with
    -- this distribution.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Aliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'aliases_items' - A complex type that contains the CNAME aliases, if any, that you want to
-- associate with this distribution.
--
-- 'quantity', 'aliases_quantity' - The number of CNAME aliases, if any, that you want to associate with
-- this distribution.
newAliases ::
  -- | 'quantity'
  Core.Int ->
  Aliases
newAliases pQuantity_ =
  Aliases'
    { items = Core.Nothing,
      quantity = pQuantity_
    }

-- | A complex type that contains the CNAME aliases, if any, that you want to
-- associate with this distribution.
aliases_items :: Lens.Lens' Aliases (Core.Maybe [Core.Text])
aliases_items = Lens.lens (\Aliases' {items} -> items) (\s@Aliases' {} a -> s {items = a} :: Aliases) Core.. Lens.mapping Lens._Coerce

-- | The number of CNAME aliases, if any, that you want to associate with
-- this distribution.
aliases_quantity :: Lens.Lens' Aliases Core.Int
aliases_quantity = Lens.lens (\Aliases' {quantity} -> quantity) (\s@Aliases' {} a -> s {quantity = a} :: Aliases)

instance Core.FromXML Aliases where
  parseXML x =
    Aliases'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "CNAME")
               )
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable Aliases

instance Core.NFData Aliases

instance Core.ToXML Aliases where
  toXML Aliases' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML (Core.toXMLList "CNAME" Core.<$> items),
        "Quantity" Core.@= quantity
      ]
