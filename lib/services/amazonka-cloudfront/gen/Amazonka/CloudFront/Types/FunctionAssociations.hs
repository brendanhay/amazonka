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
-- Module      : Amazonka.CloudFront.Types.FunctionAssociations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FunctionAssociations where

import Amazonka.CloudFront.Types.FunctionAssociation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of CloudFront functions that are associated with a cache behavior
-- in a CloudFront distribution. CloudFront functions must be published to
-- the @LIVE@ stage to associate them with a cache behavior.
--
-- /See:/ 'newFunctionAssociations' smart constructor.
data FunctionAssociations = FunctionAssociations'
  { -- | The CloudFront functions that are associated with a cache behavior in a
    -- CloudFront distribution. CloudFront functions must be published to the
    -- @LIVE@ stage to associate them with a cache behavior.
    items :: Prelude.Maybe [FunctionAssociation],
    -- | The number of CloudFront functions in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'functionAssociations_items' - The CloudFront functions that are associated with a cache behavior in a
-- CloudFront distribution. CloudFront functions must be published to the
-- @LIVE@ stage to associate them with a cache behavior.
--
-- 'quantity', 'functionAssociations_quantity' - The number of CloudFront functions in the list.
newFunctionAssociations ::
  -- | 'quantity'
  Prelude.Int ->
  FunctionAssociations
newFunctionAssociations pQuantity_ =
  FunctionAssociations'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | The CloudFront functions that are associated with a cache behavior in a
-- CloudFront distribution. CloudFront functions must be published to the
-- @LIVE@ stage to associate them with a cache behavior.
functionAssociations_items :: Lens.Lens' FunctionAssociations (Prelude.Maybe [FunctionAssociation])
functionAssociations_items = Lens.lens (\FunctionAssociations' {items} -> items) (\s@FunctionAssociations' {} a -> s {items = a} :: FunctionAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The number of CloudFront functions in the list.
functionAssociations_quantity :: Lens.Lens' FunctionAssociations Prelude.Int
functionAssociations_quantity = Lens.lens (\FunctionAssociations' {quantity} -> quantity) (\s@FunctionAssociations' {} a -> s {quantity = a} :: FunctionAssociations)

instance Core.FromXML FunctionAssociations where
  parseXML x =
    FunctionAssociations'
      Prelude.<$> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "FunctionAssociation")
                  )
      Prelude.<*> (x Core..@ "Quantity")

instance Prelude.Hashable FunctionAssociations where
  hashWithSalt _salt FunctionAssociations' {..} =
    _salt `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData FunctionAssociations where
  rnf FunctionAssociations' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Core.ToXML FunctionAssociations where
  toXML FunctionAssociations' {..} =
    Prelude.mconcat
      [ "Items"
          Core.@= Core.toXML
            ( Core.toXMLList "FunctionAssociation"
                Prelude.<$> items
            ),
        "Quantity" Core.@= quantity
      ]
