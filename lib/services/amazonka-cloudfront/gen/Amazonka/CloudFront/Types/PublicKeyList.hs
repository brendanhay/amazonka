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
-- Module      : Amazonka.CloudFront.Types.PublicKeyList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.PublicKeyList where

import Amazonka.CloudFront.Types.PublicKeySummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of public keys that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
--
-- /See:/ 'newPublicKeyList' smart constructor.
data PublicKeyList = PublicKeyList'
  { -- | A list of public keys.
    items :: Prelude.Maybe [PublicKeySummary],
    -- | If there are more elements to be listed, this element is present and
    -- contains the value that you can use for the @Marker@ request parameter
    -- to continue listing your public keys where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of public keys you want in the response.
    maxItems :: Prelude.Int,
    -- | The number of public keys in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicKeyList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'publicKeyList_items' - A list of public keys.
--
-- 'nextMarker', 'publicKeyList_nextMarker' - If there are more elements to be listed, this element is present and
-- contains the value that you can use for the @Marker@ request parameter
-- to continue listing your public keys where you left off.
--
-- 'maxItems', 'publicKeyList_maxItems' - The maximum number of public keys you want in the response.
--
-- 'quantity', 'publicKeyList_quantity' - The number of public keys in the list.
newPublicKeyList ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  PublicKeyList
newPublicKeyList pMaxItems_ pQuantity_ =
  PublicKeyList'
    { items = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | A list of public keys.
publicKeyList_items :: Lens.Lens' PublicKeyList (Prelude.Maybe [PublicKeySummary])
publicKeyList_items = Lens.lens (\PublicKeyList' {items} -> items) (\s@PublicKeyList' {} a -> s {items = a} :: PublicKeyList) Prelude.. Lens.mapping Lens.coerced

-- | If there are more elements to be listed, this element is present and
-- contains the value that you can use for the @Marker@ request parameter
-- to continue listing your public keys where you left off.
publicKeyList_nextMarker :: Lens.Lens' PublicKeyList (Prelude.Maybe Prelude.Text)
publicKeyList_nextMarker = Lens.lens (\PublicKeyList' {nextMarker} -> nextMarker) (\s@PublicKeyList' {} a -> s {nextMarker = a} :: PublicKeyList)

-- | The maximum number of public keys you want in the response.
publicKeyList_maxItems :: Lens.Lens' PublicKeyList Prelude.Int
publicKeyList_maxItems = Lens.lens (\PublicKeyList' {maxItems} -> maxItems) (\s@PublicKeyList' {} a -> s {maxItems = a} :: PublicKeyList)

-- | The number of public keys in the list.
publicKeyList_quantity :: Lens.Lens' PublicKeyList Prelude.Int
publicKeyList_quantity = Lens.lens (\PublicKeyList' {quantity} -> quantity) (\s@PublicKeyList' {} a -> s {quantity = a} :: PublicKeyList)

instance Data.FromXML PublicKeyList where
  parseXML x =
    PublicKeyList'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "PublicKeySummary")
                  )
      Prelude.<*> (x Data..@? "NextMarker")
      Prelude.<*> (x Data..@ "MaxItems")
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable PublicKeyList where
  hashWithSalt _salt PublicKeyList' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData PublicKeyList where
  rnf PublicKeyList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf quantity
