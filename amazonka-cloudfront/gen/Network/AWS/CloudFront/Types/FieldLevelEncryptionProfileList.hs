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
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | List of field-level encryption profiles.
--
-- /See:/ 'newFieldLevelEncryptionProfileList' smart constructor.
data FieldLevelEncryptionProfileList = FieldLevelEncryptionProfileList'
  { -- | The field-level encryption profile items.
    items :: Prelude.Maybe [FieldLevelEncryptionProfileSummary],
    -- | If there are more elements to be listed, this element is present and
    -- contains the value that you can use for the @Marker@ request parameter
    -- to continue listing your profiles where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of field-level encryption profiles you want in the
    -- response body.
    maxItems :: Prelude.Int,
    -- | The number of field-level encryption profiles.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FieldLevelEncryptionProfileList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'fieldLevelEncryptionProfileList_items' - The field-level encryption profile items.
--
-- 'nextMarker', 'fieldLevelEncryptionProfileList_nextMarker' - If there are more elements to be listed, this element is present and
-- contains the value that you can use for the @Marker@ request parameter
-- to continue listing your profiles where you left off.
--
-- 'maxItems', 'fieldLevelEncryptionProfileList_maxItems' - The maximum number of field-level encryption profiles you want in the
-- response body.
--
-- 'quantity', 'fieldLevelEncryptionProfileList_quantity' - The number of field-level encryption profiles.
newFieldLevelEncryptionProfileList ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  FieldLevelEncryptionProfileList
newFieldLevelEncryptionProfileList
  pMaxItems_
  pQuantity_ =
    FieldLevelEncryptionProfileList'
      { items =
          Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        maxItems = pMaxItems_,
        quantity = pQuantity_
      }

-- | The field-level encryption profile items.
fieldLevelEncryptionProfileList_items :: Lens.Lens' FieldLevelEncryptionProfileList (Prelude.Maybe [FieldLevelEncryptionProfileSummary])
fieldLevelEncryptionProfileList_items = Lens.lens (\FieldLevelEncryptionProfileList' {items} -> items) (\s@FieldLevelEncryptionProfileList' {} a -> s {items = a} :: FieldLevelEncryptionProfileList) Prelude.. Lens.mapping Prelude._Coerce

-- | If there are more elements to be listed, this element is present and
-- contains the value that you can use for the @Marker@ request parameter
-- to continue listing your profiles where you left off.
fieldLevelEncryptionProfileList_nextMarker :: Lens.Lens' FieldLevelEncryptionProfileList (Prelude.Maybe Prelude.Text)
fieldLevelEncryptionProfileList_nextMarker = Lens.lens (\FieldLevelEncryptionProfileList' {nextMarker} -> nextMarker) (\s@FieldLevelEncryptionProfileList' {} a -> s {nextMarker = a} :: FieldLevelEncryptionProfileList)

-- | The maximum number of field-level encryption profiles you want in the
-- response body.
fieldLevelEncryptionProfileList_maxItems :: Lens.Lens' FieldLevelEncryptionProfileList Prelude.Int
fieldLevelEncryptionProfileList_maxItems = Lens.lens (\FieldLevelEncryptionProfileList' {maxItems} -> maxItems) (\s@FieldLevelEncryptionProfileList' {} a -> s {maxItems = a} :: FieldLevelEncryptionProfileList)

-- | The number of field-level encryption profiles.
fieldLevelEncryptionProfileList_quantity :: Lens.Lens' FieldLevelEncryptionProfileList Prelude.Int
fieldLevelEncryptionProfileList_quantity = Lens.lens (\FieldLevelEncryptionProfileList' {quantity} -> quantity) (\s@FieldLevelEncryptionProfileList' {} a -> s {quantity = a} :: FieldLevelEncryptionProfileList)

instance
  Prelude.FromXML
    FieldLevelEncryptionProfileList
  where
  parseXML x =
    FieldLevelEncryptionProfileList'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        ( Prelude.parseXMLList
                            "FieldLevelEncryptionProfileSummary"
                        )
                  )
      Prelude.<*> (x Prelude..@? "NextMarker")
      Prelude.<*> (x Prelude..@ "MaxItems")
      Prelude.<*> (x Prelude..@ "Quantity")

instance
  Prelude.Hashable
    FieldLevelEncryptionProfileList

instance
  Prelude.NFData
    FieldLevelEncryptionProfileList
