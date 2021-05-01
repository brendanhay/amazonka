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
-- Module      : Network.AWS.CloudFront.Types.Aliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Aliases where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
aliases_items = Lens.lens (\Aliases' {items} -> items) (\s@Aliases' {} a -> s {items = a} :: Aliases) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of CNAME aliases, if any, that you want to associate with
-- this distribution.
aliases_quantity :: Lens.Lens' Aliases Prelude.Int
aliases_quantity = Lens.lens (\Aliases' {quantity} -> quantity) (\s@Aliases' {} a -> s {quantity = a} :: Aliases)

instance Prelude.FromXML Aliases where
  parseXML x =
    Aliases'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "CNAME")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable Aliases

instance Prelude.NFData Aliases

instance Prelude.ToXML Aliases where
  toXML Aliases' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "CNAME" Prelude.<$> items),
        "Quantity" Prelude.@= quantity
      ]
