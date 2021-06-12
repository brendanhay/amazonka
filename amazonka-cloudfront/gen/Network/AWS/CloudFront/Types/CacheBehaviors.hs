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
-- Module      : Network.AWS.CloudFront.Types.CacheBehaviors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CacheBehaviors where

import Network.AWS.CloudFront.Types.CacheBehavior
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /See:/ 'newCacheBehaviors' smart constructor.
data CacheBehaviors = CacheBehaviors'
  { -- | Optional: A complex type that contains cache behaviors for this
    -- distribution. If @Quantity@ is @0@, you can omit @Items@.
    items :: Core.Maybe [CacheBehavior],
    -- | The number of cache behaviors for this distribution.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CacheBehaviors
newCacheBehaviors pQuantity_ =
  CacheBehaviors'
    { items = Core.Nothing,
      quantity = pQuantity_
    }

-- | Optional: A complex type that contains cache behaviors for this
-- distribution. If @Quantity@ is @0@, you can omit @Items@.
cacheBehaviors_items :: Lens.Lens' CacheBehaviors (Core.Maybe [CacheBehavior])
cacheBehaviors_items = Lens.lens (\CacheBehaviors' {items} -> items) (\s@CacheBehaviors' {} a -> s {items = a} :: CacheBehaviors) Core.. Lens.mapping Lens._Coerce

-- | The number of cache behaviors for this distribution.
cacheBehaviors_quantity :: Lens.Lens' CacheBehaviors Core.Int
cacheBehaviors_quantity = Lens.lens (\CacheBehaviors' {quantity} -> quantity) (\s@CacheBehaviors' {} a -> s {quantity = a} :: CacheBehaviors)

instance Core.FromXML CacheBehaviors where
  parseXML x =
    CacheBehaviors'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "CacheBehavior")
               )
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable CacheBehaviors

instance Core.NFData CacheBehaviors

instance Core.ToXML CacheBehaviors where
  toXML CacheBehaviors' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML
            (Core.toXMLList "CacheBehavior" Core.<$> items),
        "Quantity" Core.@= quantity
      ]
