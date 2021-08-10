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
-- Module      : Network.AWS.CloudFront.Types.Origins
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Origins where

import Network.AWS.CloudFront.Types.Origin
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the origins for this distribution.
--
-- /See:/ 'newOrigins' smart constructor.
data Origins = Origins'
  { -- | The number of origins for this distribution.
    quantity :: Prelude.Int,
    -- | A list of origins.
    items :: Prelude.NonEmpty Origin
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Origins' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'origins_quantity' - The number of origins for this distribution.
--
-- 'items', 'origins_items' - A list of origins.
newOrigins ::
  -- | 'quantity'
  Prelude.Int ->
  -- | 'items'
  Prelude.NonEmpty Origin ->
  Origins
newOrigins pQuantity_ pItems_ =
  Origins'
    { quantity = pQuantity_,
      items = Lens._Coerce Lens.# pItems_
    }

-- | The number of origins for this distribution.
origins_quantity :: Lens.Lens' Origins Prelude.Int
origins_quantity = Lens.lens (\Origins' {quantity} -> quantity) (\s@Origins' {} a -> s {quantity = a} :: Origins)

-- | A list of origins.
origins_items :: Lens.Lens' Origins (Prelude.NonEmpty Origin)
origins_items = Lens.lens (\Origins' {items} -> items) (\s@Origins' {} a -> s {items = a} :: Origins) Prelude.. Lens._Coerce

instance Core.FromXML Origins where
  parseXML x =
    Origins'
      Prelude.<$> (x Core..@ "Quantity")
      Prelude.<*> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList1 "Origin"
                  )

instance Prelude.Hashable Origins

instance Prelude.NFData Origins

instance Core.ToXML Origins where
  toXML Origins' {..} =
    Prelude.mconcat
      [ "Quantity" Core.@= quantity,
        "Items" Core.@= Core.toXMLList "Origin" items
      ]
