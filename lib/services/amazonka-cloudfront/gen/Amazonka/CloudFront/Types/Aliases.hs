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
-- Module      : Amazonka.CloudFront.Types.Aliases
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.Aliases where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
--
-- /See:/ 'newAliases' smart constructor.
data Aliases = Aliases'
  { -- | A complex type that contains the CNAME aliases, if any, that you want to
    -- associate with this distribution.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of CNAME aliases, if any, that you want to associate with
    -- this distribution.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  Aliases
newAliases pQuantity_ =
  Aliases'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | A complex type that contains the CNAME aliases, if any, that you want to
-- associate with this distribution.
aliases_items :: Lens.Lens' Aliases (Prelude.Maybe [Prelude.Text])
aliases_items = Lens.lens (\Aliases' {items} -> items) (\s@Aliases' {} a -> s {items = a} :: Aliases) Prelude.. Lens.mapping Lens.coerced

-- | The number of CNAME aliases, if any, that you want to associate with
-- this distribution.
aliases_quantity :: Lens.Lens' Aliases Prelude.Int
aliases_quantity = Lens.lens (\Aliases' {quantity} -> quantity) (\s@Aliases' {} a -> s {quantity = a} :: Aliases)

instance Core.FromXML Aliases where
  parseXML x =
    Aliases'
      Prelude.<$> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "CNAME")
                  )
      Prelude.<*> (x Core..@ "Quantity")

instance Prelude.Hashable Aliases where
  hashWithSalt _salt Aliases' {..} =
    _salt `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData Aliases where
  rnf Aliases' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Core.ToXML Aliases where
  toXML Aliases' {..} =
    Prelude.mconcat
      [ "Items"
          Core.@= Core.toXML
            (Core.toXMLList "CNAME" Prelude.<$> items),
        "Quantity" Core.@= quantity
      ]
