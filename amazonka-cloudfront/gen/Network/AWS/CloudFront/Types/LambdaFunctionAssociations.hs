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
-- Module      : Network.AWS.CloudFront.Types.LambdaFunctionAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.LambdaFunctionAssociations where

import Network.AWS.CloudFront.Types.LambdaFunctionAssociation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that specifies a list of Lambda functions associations
-- for a cache behavior.
--
-- If you want to invoke one or more Lambda functions triggered by requests
-- that match the @PathPattern@ of the cache behavior, specify the
-- applicable values for @Quantity@ and @Items@. Note that there can be up
-- to 4 @LambdaFunctionAssociation@ items in this list (one for each
-- possible value of @EventType@) and each @EventType@ can be associated
-- with the Lambda function only once.
--
-- If you don\'t want to invoke any Lambda functions for the requests that
-- match @PathPattern@, specify @0@ for @Quantity@ and omit @Items@.
--
-- /See:/ 'newLambdaFunctionAssociations' smart constructor.
data LambdaFunctionAssociations = LambdaFunctionAssociations'
  { -- | __Optional__: A complex type that contains @LambdaFunctionAssociation@
    -- items for this cache behavior. If @Quantity@ is @0@, you can omit
    -- @Items@.
    items :: Prelude.Maybe [LambdaFunctionAssociation],
    -- | The number of Lambda function associations for this cache behavior.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'quantity', 'lambdaFunctionAssociations_quantity' - The number of Lambda function associations for this cache behavior.
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
lambdaFunctionAssociations_items = Lens.lens (\LambdaFunctionAssociations' {items} -> items) (\s@LambdaFunctionAssociations' {} a -> s {items = a} :: LambdaFunctionAssociations) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of Lambda function associations for this cache behavior.
lambdaFunctionAssociations_quantity :: Lens.Lens' LambdaFunctionAssociations Prelude.Int
lambdaFunctionAssociations_quantity = Lens.lens (\LambdaFunctionAssociations' {quantity} -> quantity) (\s@LambdaFunctionAssociations' {} a -> s {quantity = a} :: LambdaFunctionAssociations)

instance Prelude.FromXML LambdaFunctionAssociations where
  parseXML x =
    LambdaFunctionAssociations'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "LambdaFunctionAssociation")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable LambdaFunctionAssociations

instance Prelude.NFData LambdaFunctionAssociations

instance Prelude.ToXML LambdaFunctionAssociations where
  toXML LambdaFunctionAssociations' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            ( Prelude.toXMLList "LambdaFunctionAssociation"
                Prelude.<$> items
            ),
        "Quantity" Prelude.@= quantity
      ]
