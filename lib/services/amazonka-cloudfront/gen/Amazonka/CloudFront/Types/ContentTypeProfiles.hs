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
-- Module      : Amazonka.CloudFront.Types.ContentTypeProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ContentTypeProfiles where

import Amazonka.CloudFront.Types.ContentTypeProfile
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
contentTypeProfiles_items = Lens.lens (\ContentTypeProfiles' {items} -> items) (\s@ContentTypeProfiles' {} a -> s {items = a} :: ContentTypeProfiles) Prelude.. Lens.mapping Lens.coerced

-- | The number of field-level encryption content type-profile mappings.
contentTypeProfiles_quantity :: Lens.Lens' ContentTypeProfiles Prelude.Int
contentTypeProfiles_quantity = Lens.lens (\ContentTypeProfiles' {quantity} -> quantity) (\s@ContentTypeProfiles' {} a -> s {quantity = a} :: ContentTypeProfiles)

instance Data.FromXML ContentTypeProfiles where
  parseXML x =
    ContentTypeProfiles'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "ContentTypeProfile")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable ContentTypeProfiles where
  hashWithSalt _salt ContentTypeProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData ContentTypeProfiles where
  rnf ContentTypeProfiles' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Data.ToXML ContentTypeProfiles where
  toXML ContentTypeProfiles' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML
            ( Data.toXMLList "ContentTypeProfile"
                Prelude.<$> items
            ),
        "Quantity" Data.@= quantity
      ]
