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
-- Module      : Network.AWS.CloudFront.Types.OriginGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroups where

import Network.AWS.CloudFront.Types.OriginGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex data type for the origin groups specified for a distribution.
--
-- /See:/ 'newOriginGroups' smart constructor.
data OriginGroups = OriginGroups'
  { -- | The items (origin groups) in a distribution.
    items :: Prelude.Maybe [OriginGroup],
    -- | The number of origin groups.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  OriginGroups
newOriginGroups pQuantity_ =
  OriginGroups'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | The items (origin groups) in a distribution.
originGroups_items :: Lens.Lens' OriginGroups (Prelude.Maybe [OriginGroup])
originGroups_items = Lens.lens (\OriginGroups' {items} -> items) (\s@OriginGroups' {} a -> s {items = a} :: OriginGroups) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of origin groups.
originGroups_quantity :: Lens.Lens' OriginGroups Prelude.Int
originGroups_quantity = Lens.lens (\OriginGroups' {quantity} -> quantity) (\s@OriginGroups' {} a -> s {quantity = a} :: OriginGroups)

instance Prelude.FromXML OriginGroups where
  parseXML x =
    OriginGroups'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "OriginGroup")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable OriginGroups

instance Prelude.NFData OriginGroups

instance Prelude.ToXML OriginGroups where
  toXML OriginGroups' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "OriginGroup" Prelude.<$> items),
        "Quantity" Prelude.@= quantity
      ]
