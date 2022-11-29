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
-- Module      : Amazonka.CloudFront.Types.EncryptionEntities
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.EncryptionEntities where

import Amazonka.CloudFront.Types.EncryptionEntity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Complex data type for field-level encryption profiles that includes all
-- of the encryption entities.
--
-- /See:/ 'newEncryptionEntities' smart constructor.
data EncryptionEntities = EncryptionEntities'
  { -- | An array of field patterns in a field-level encryption content
    -- type-profile mapping.
    items :: Prelude.Maybe [EncryptionEntity],
    -- | Number of field pattern items in a field-level encryption content
    -- type-profile mapping.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'encryptionEntities_items' - An array of field patterns in a field-level encryption content
-- type-profile mapping.
--
-- 'quantity', 'encryptionEntities_quantity' - Number of field pattern items in a field-level encryption content
-- type-profile mapping.
newEncryptionEntities ::
  -- | 'quantity'
  Prelude.Int ->
  EncryptionEntities
newEncryptionEntities pQuantity_ =
  EncryptionEntities'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | An array of field patterns in a field-level encryption content
-- type-profile mapping.
encryptionEntities_items :: Lens.Lens' EncryptionEntities (Prelude.Maybe [EncryptionEntity])
encryptionEntities_items = Lens.lens (\EncryptionEntities' {items} -> items) (\s@EncryptionEntities' {} a -> s {items = a} :: EncryptionEntities) Prelude.. Lens.mapping Lens.coerced

-- | Number of field pattern items in a field-level encryption content
-- type-profile mapping.
encryptionEntities_quantity :: Lens.Lens' EncryptionEntities Prelude.Int
encryptionEntities_quantity = Lens.lens (\EncryptionEntities' {quantity} -> quantity) (\s@EncryptionEntities' {} a -> s {quantity = a} :: EncryptionEntities)

instance Core.FromXML EncryptionEntities where
  parseXML x =
    EncryptionEntities'
      Prelude.<$> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "EncryptionEntity")
                  )
      Prelude.<*> (x Core..@ "Quantity")

instance Prelude.Hashable EncryptionEntities where
  hashWithSalt _salt EncryptionEntities' {..} =
    _salt `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData EncryptionEntities where
  rnf EncryptionEntities' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Core.ToXML EncryptionEntities where
  toXML EncryptionEntities' {..} =
    Prelude.mconcat
      [ "Items"
          Core.@= Core.toXML
            ( Core.toXMLList "EncryptionEntity"
                Prelude.<$> items
            ),
        "Quantity" Core.@= quantity
      ]
