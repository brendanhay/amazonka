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
-- Module      : Amazonka.CloudFront.Types.LambdaFunctionAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.LambdaFunctionAssociations where

import Amazonka.CloudFront.Types.LambdaFunctionAssociation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that specifies a list of Lambda\@Edge functions
-- associations for a cache behavior.
--
-- If you want to invoke one or more Lambda\@Edge functions triggered by
-- requests that match the @PathPattern@ of the cache behavior, specify the
-- applicable values for @Quantity@ and @Items@. Note that there can be up
-- to 4 @LambdaFunctionAssociation@ items in this list (one for each
-- possible value of @EventType@) and each @EventType@ can be associated
-- with only one function.
--
-- If you don\'t want to invoke any Lambda\@Edge functions for the requests
-- that match @PathPattern@, specify @0@ for @Quantity@ and omit @Items@.
--
-- /See:/ 'newLambdaFunctionAssociations' smart constructor.
data LambdaFunctionAssociations = LambdaFunctionAssociations'
  { -- | __Optional__: A complex type that contains @LambdaFunctionAssociation@
    -- items for this cache behavior. If @Quantity@ is @0@, you can omit
    -- @Items@.
    items :: Prelude.Maybe [LambdaFunctionAssociation],
    -- | The number of Lambda\@Edge function associations for this cache
    -- behavior.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'lambdaFunctionAssociations_items' - __Optional__: A complex type that contains @LambdaFunctionAssociation@
-- items for this cache behavior. If @Quantity@ is @0@, you can omit
-- @Items@.
--
-- 'quantity', 'lambdaFunctionAssociations_quantity' - The number of Lambda\@Edge function associations for this cache
-- behavior.
newLambdaFunctionAssociations ::
  -- | 'quantity'
  Prelude.Int ->
  LambdaFunctionAssociations
newLambdaFunctionAssociations pQuantity_ =
  LambdaFunctionAssociations'
    { items =
        Prelude.Nothing,
      quantity = pQuantity_
    }

-- | __Optional__: A complex type that contains @LambdaFunctionAssociation@
-- items for this cache behavior. If @Quantity@ is @0@, you can omit
-- @Items@.
lambdaFunctionAssociations_items :: Lens.Lens' LambdaFunctionAssociations (Prelude.Maybe [LambdaFunctionAssociation])
lambdaFunctionAssociations_items = Lens.lens (\LambdaFunctionAssociations' {items} -> items) (\s@LambdaFunctionAssociations' {} a -> s {items = a} :: LambdaFunctionAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The number of Lambda\@Edge function associations for this cache
-- behavior.
lambdaFunctionAssociations_quantity :: Lens.Lens' LambdaFunctionAssociations Prelude.Int
lambdaFunctionAssociations_quantity = Lens.lens (\LambdaFunctionAssociations' {quantity} -> quantity) (\s@LambdaFunctionAssociations' {} a -> s {quantity = a} :: LambdaFunctionAssociations)

instance Data.FromXML LambdaFunctionAssociations where
  parseXML x =
    LambdaFunctionAssociations'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "LambdaFunctionAssociation")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable LambdaFunctionAssociations where
  hashWithSalt _salt LambdaFunctionAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData LambdaFunctionAssociations where
  rnf LambdaFunctionAssociations' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Data.ToXML LambdaFunctionAssociations where
  toXML LambdaFunctionAssociations' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML
            ( Data.toXMLList "LambdaFunctionAssociation"
                Prelude.<$> items
            ),
        "Quantity" Data.@= quantity
      ]
