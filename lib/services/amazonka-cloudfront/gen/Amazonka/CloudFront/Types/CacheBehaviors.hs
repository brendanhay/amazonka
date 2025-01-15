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
-- Module      : Amazonka.CloudFront.Types.CacheBehaviors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CacheBehaviors where

import Amazonka.CloudFront.Types.CacheBehavior
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /See:/ 'newCacheBehaviors' smart constructor.
data CacheBehaviors = CacheBehaviors'
  { -- | Optional: A complex type that contains cache behaviors for this
    -- distribution. If @Quantity@ is @0@, you can omit @Items@.
    items :: Prelude.Maybe [CacheBehavior],
    -- | The number of cache behaviors for this distribution.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheBehaviors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'cacheBehaviors_items' - Optional: A complex type that contains cache behaviors for this
-- distribution. If @Quantity@ is @0@, you can omit @Items@.
--
-- 'quantity', 'cacheBehaviors_quantity' - The number of cache behaviors for this distribution.
newCacheBehaviors ::
  -- | 'quantity'
  Prelude.Int ->
  CacheBehaviors
newCacheBehaviors pQuantity_ =
  CacheBehaviors'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | Optional: A complex type that contains cache behaviors for this
-- distribution. If @Quantity@ is @0@, you can omit @Items@.
cacheBehaviors_items :: Lens.Lens' CacheBehaviors (Prelude.Maybe [CacheBehavior])
cacheBehaviors_items = Lens.lens (\CacheBehaviors' {items} -> items) (\s@CacheBehaviors' {} a -> s {items = a} :: CacheBehaviors) Prelude.. Lens.mapping Lens.coerced

-- | The number of cache behaviors for this distribution.
cacheBehaviors_quantity :: Lens.Lens' CacheBehaviors Prelude.Int
cacheBehaviors_quantity = Lens.lens (\CacheBehaviors' {quantity} -> quantity) (\s@CacheBehaviors' {} a -> s {quantity = a} :: CacheBehaviors)

instance Data.FromXML CacheBehaviors where
  parseXML x =
    CacheBehaviors'
      Prelude.<$> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CacheBehavior")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable CacheBehaviors where
  hashWithSalt _salt CacheBehaviors' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData CacheBehaviors where
  rnf CacheBehaviors' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf quantity

instance Data.ToXML CacheBehaviors where
  toXML CacheBehaviors' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML
            (Data.toXMLList "CacheBehavior" Prelude.<$> items),
        "Quantity" Data.@= quantity
      ]
