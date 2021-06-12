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
-- Module      : Network.AWS.CloudFront.Types.OriginGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroups where

import Network.AWS.CloudFront.Types.OriginGroup
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex data type for the origin groups specified for a distribution.
--
-- /See:/ 'newOriginGroups' smart constructor.
data OriginGroups = OriginGroups'
  { -- | The items (origin groups) in a distribution.
    items :: Core.Maybe [OriginGroup],
    -- | The number of origin groups.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OriginGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'originGroups_items' - The items (origin groups) in a distribution.
--
-- 'quantity', 'originGroups_quantity' - The number of origin groups.
newOriginGroups ::
  -- | 'quantity'
  Core.Int ->
  OriginGroups
newOriginGroups pQuantity_ =
  OriginGroups'
    { items = Core.Nothing,
      quantity = pQuantity_
    }

-- | The items (origin groups) in a distribution.
originGroups_items :: Lens.Lens' OriginGroups (Core.Maybe [OriginGroup])
originGroups_items = Lens.lens (\OriginGroups' {items} -> items) (\s@OriginGroups' {} a -> s {items = a} :: OriginGroups) Core.. Lens.mapping Lens._Coerce

-- | The number of origin groups.
originGroups_quantity :: Lens.Lens' OriginGroups Core.Int
originGroups_quantity = Lens.lens (\OriginGroups' {quantity} -> quantity) (\s@OriginGroups' {} a -> s {quantity = a} :: OriginGroups)

instance Core.FromXML OriginGroups where
  parseXML x =
    OriginGroups'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "OriginGroup")
               )
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable OriginGroups

instance Core.NFData OriginGroups

instance Core.ToXML OriginGroups where
  toXML OriginGroups' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML
            (Core.toXMLList "OriginGroup" Core.<$> items),
        "Quantity" Core.@= quantity
      ]
