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
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfiles where

import Network.AWS.CloudFront.Types.QueryArgProfile
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Query argument-profile mapping for field-level encryption.
--
-- /See:/ 'newQueryArgProfiles' smart constructor.
data QueryArgProfiles = QueryArgProfiles'
  { -- | Number of items for query argument-profile mapping for field-level
    -- encryption.
    items :: Core.Maybe [QueryArgProfile],
    -- | Number of profiles for query argument-profile mapping for field-level
    -- encryption.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueryArgProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'queryArgProfiles_items' - Number of items for query argument-profile mapping for field-level
-- encryption.
--
-- 'quantity', 'queryArgProfiles_quantity' - Number of profiles for query argument-profile mapping for field-level
-- encryption.
newQueryArgProfiles ::
  -- | 'quantity'
  Core.Int ->
  QueryArgProfiles
newQueryArgProfiles pQuantity_ =
  QueryArgProfiles'
    { items = Core.Nothing,
      quantity = pQuantity_
    }

-- | Number of items for query argument-profile mapping for field-level
-- encryption.
queryArgProfiles_items :: Lens.Lens' QueryArgProfiles (Core.Maybe [QueryArgProfile])
queryArgProfiles_items = Lens.lens (\QueryArgProfiles' {items} -> items) (\s@QueryArgProfiles' {} a -> s {items = a} :: QueryArgProfiles) Core.. Lens.mapping Lens._Coerce

-- | Number of profiles for query argument-profile mapping for field-level
-- encryption.
queryArgProfiles_quantity :: Lens.Lens' QueryArgProfiles Core.Int
queryArgProfiles_quantity = Lens.lens (\QueryArgProfiles' {quantity} -> quantity) (\s@QueryArgProfiles' {} a -> s {quantity = a} :: QueryArgProfiles)

instance Core.FromXML QueryArgProfiles where
  parseXML x =
    QueryArgProfiles'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "QueryArgProfile")
               )
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable QueryArgProfiles

instance Core.NFData QueryArgProfiles

instance Core.ToXML QueryArgProfiles where
  toXML QueryArgProfiles' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML
            (Core.toXMLList "QueryArgProfile" Core.<$> items),
        "Quantity" Core.@= quantity
      ]
