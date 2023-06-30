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
-- Module      : Amazonka.CloudFront.Types.FieldLevelEncryptionList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FieldLevelEncryptionList where

import Amazonka.CloudFront.Types.FieldLevelEncryptionSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of field-level encrpytion configurations.
--
-- /See:/ 'newFieldLevelEncryptionList' smart constructor.
data FieldLevelEncryptionList = FieldLevelEncryptionList'
  { -- | An array of field-level encryption items.
    items :: Prelude.Maybe [FieldLevelEncryptionSummary],
    -- | If there are more elements to be listed, this element is present and
    -- contains the value that you can use for the @Marker@ request parameter
    -- to continue listing your configurations where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of elements you want in the response body.
    maxItems :: Prelude.Int,
    -- | The number of field-level encryption items.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldLevelEncryptionList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'fieldLevelEncryptionList_items' - An array of field-level encryption items.
--
-- 'nextMarker', 'fieldLevelEncryptionList_nextMarker' - If there are more elements to be listed, this element is present and
-- contains the value that you can use for the @Marker@ request parameter
-- to continue listing your configurations where you left off.
--
-- 'maxItems', 'fieldLevelEncryptionList_maxItems' - The maximum number of elements you want in the response body.
--
-- 'quantity', 'fieldLevelEncryptionList_quantity' - The number of field-level encryption items.
newFieldLevelEncryptionList ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  FieldLevelEncryptionList
newFieldLevelEncryptionList pMaxItems_ pQuantity_ =
  FieldLevelEncryptionList'
    { items = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | An array of field-level encryption items.
fieldLevelEncryptionList_items :: Lens.Lens' FieldLevelEncryptionList (Prelude.Maybe [FieldLevelEncryptionSummary])
fieldLevelEncryptionList_items = Lens.lens (\FieldLevelEncryptionList' {items} -> items) (\s@FieldLevelEncryptionList' {} a -> s {items = a} :: FieldLevelEncryptionList) Prelude.. Lens.mapping Lens.coerced

-- | If there are more elements to be listed, this element is present and
-- contains the value that you can use for the @Marker@ request parameter
-- to continue listing your configurations where you left off.
fieldLevelEncryptionList_nextMarker :: Lens.Lens' FieldLevelEncryptionList (Prelude.Maybe Prelude.Text)
fieldLevelEncryptionList_nextMarker = Lens.lens (\FieldLevelEncryptionList' {nextMarker} -> nextMarker) (\s@FieldLevelEncryptionList' {} a -> s {nextMarker = a} :: FieldLevelEncryptionList)

-- | The maximum number of elements you want in the response body.
fieldLevelEncryptionList_maxItems :: Lens.Lens' FieldLevelEncryptionList Prelude.Int
fieldLevelEncryptionList_maxItems = Lens.lens (\FieldLevelEncryptionList' {maxItems} -> maxItems) (\s@FieldLevelEncryptionList' {} a -> s {maxItems = a} :: FieldLevelEncryptionList)

-- | The number of field-level encryption items.
fieldLevelEncryptionList_quantity :: Lens.Lens' FieldLevelEncryptionList Prelude.Int
fieldLevelEncryptionList_quantity = Lens.lens (\FieldLevelEncryptionList' {quantity} -> quantity) (\s@FieldLevelEncryptionList' {} a -> s {quantity = a} :: FieldLevelEncryptionList)

instance Data.FromXML FieldLevelEncryptionList where
  parseXML x =
    FieldLevelEncryptionList'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "FieldLevelEncryptionSummary")
                  )
      Prelude.<*> (x Data..@? "NextMarker")
      Prelude.<*> (x Data..@ "MaxItems")
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable FieldLevelEncryptionList where
  hashWithSalt _salt FieldLevelEncryptionList' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData FieldLevelEncryptionList where
  rnf FieldLevelEncryptionList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf quantity
