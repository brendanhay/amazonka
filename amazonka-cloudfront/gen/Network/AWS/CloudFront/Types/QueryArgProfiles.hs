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
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfiles where

import Network.AWS.CloudFront.Types.QueryArgProfile
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Query argument-profile mapping for field-level encryption.
--
-- /See:/ 'newQueryArgProfiles' smart constructor.
data QueryArgProfiles = QueryArgProfiles'
  { -- | Number of items for query argument-profile mapping for field-level
    -- encryption.
    items :: Prelude.Maybe [QueryArgProfile],
    -- | Number of profiles for query argument-profile mapping for field-level
    -- encryption.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  QueryArgProfiles
newQueryArgProfiles pQuantity_ =
  QueryArgProfiles'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | Number of items for query argument-profile mapping for field-level
-- encryption.
queryArgProfiles_items :: Lens.Lens' QueryArgProfiles (Prelude.Maybe [QueryArgProfile])
queryArgProfiles_items = Lens.lens (\QueryArgProfiles' {items} -> items) (\s@QueryArgProfiles' {} a -> s {items = a} :: QueryArgProfiles) Prelude.. Lens.mapping Prelude._Coerce

-- | Number of profiles for query argument-profile mapping for field-level
-- encryption.
queryArgProfiles_quantity :: Lens.Lens' QueryArgProfiles Prelude.Int
queryArgProfiles_quantity = Lens.lens (\QueryArgProfiles' {quantity} -> quantity) (\s@QueryArgProfiles' {} a -> s {quantity = a} :: QueryArgProfiles)

instance Prelude.FromXML QueryArgProfiles where
  parseXML x =
    QueryArgProfiles'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "QueryArgProfile")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable QueryArgProfiles

instance Prelude.NFData QueryArgProfiles

instance Prelude.ToXML QueryArgProfiles where
  toXML QueryArgProfiles' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            ( Prelude.toXMLList "QueryArgProfile"
                Prelude.<$> items
            ),
        "Quantity" Prelude.@= quantity
      ]
