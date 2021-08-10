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
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfiles where

import Network.AWS.CloudFront.Types.ContentTypeProfile
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Field-level encryption content type-profile.
--
-- /See:/ 'newContentTypeProfiles' smart constructor.
data ContentTypeProfiles = ContentTypeProfiles'
  { -- | Items in a field-level encryption content type-profile mapping.
    items :: Prelude.Maybe [ContentTypeProfile],
    -- | The number of field-level encryption content type-profile mappings.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContentTypeProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'contentTypeProfiles_items' - Items in a field-level encryption content type-profile mapping.
--
-- 'quantity', 'contentTypeProfiles_quantity' - The number of field-level encryption content type-profile mappings.
newContentTypeProfiles ::
  -- | 'quantity'
  Prelude.Int ->
  ContentTypeProfiles
newContentTypeProfiles pQuantity_ =
  ContentTypeProfiles'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | Items in a field-level encryption content type-profile mapping.
contentTypeProfiles_items :: Lens.Lens' ContentTypeProfiles (Prelude.Maybe [ContentTypeProfile])
contentTypeProfiles_items = Lens.lens (\ContentTypeProfiles' {items} -> items) (\s@ContentTypeProfiles' {} a -> s {items = a} :: ContentTypeProfiles) Prelude.. Lens.mapping Lens._Coerce

-- | The number of field-level encryption content type-profile mappings.
contentTypeProfiles_quantity :: Lens.Lens' ContentTypeProfiles Prelude.Int
contentTypeProfiles_quantity = Lens.lens (\ContentTypeProfiles' {quantity} -> quantity) (\s@ContentTypeProfiles' {} a -> s {quantity = a} :: ContentTypeProfiles)

instance Core.FromXML ContentTypeProfiles where
  parseXML x =
    ContentTypeProfiles'
      Prelude.<$> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ContentTypeProfile")
                  )
      Prelude.<*> (x Core..@ "Quantity")

instance Prelude.Hashable ContentTypeProfiles

instance Prelude.NFData ContentTypeProfiles

instance Core.ToXML ContentTypeProfiles where
  toXML ContentTypeProfiles' {..} =
    Prelude.mconcat
      [ "Items"
          Core.@= Core.toXML
            ( Core.toXMLList "ContentTypeProfile"
                Prelude.<$> items
            ),
        "Quantity" Core.@= quantity
      ]
