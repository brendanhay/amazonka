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
-- Module      : Network.AWS.CloudFront.Types.PublicKeyList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKeyList where

import Network.AWS.CloudFront.Types.PublicKeySummary
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of public keys that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
--
-- /See:/ 'newPublicKeyList' smart constructor.
data PublicKeyList = PublicKeyList'
  { -- | A list of public keys.
    items :: Core.Maybe [PublicKeySummary],
    -- | If there are more elements to be listed, this element is present and
    -- contains the value that you can use for the @Marker@ request parameter
    -- to continue listing your public keys where you left off.
    nextMarker :: Core.Maybe Core.Text,
    -- | The maximum number of public keys you want in the response.
    maxItems :: Core.Int,
    -- | The number of public keys in the list.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'quantity'
  Core.Int ->
  PublicKeyList
newPublicKeyList pMaxItems_ pQuantity_ =
  PublicKeyList'
    { items = Core.Nothing,
      nextMarker = Core.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | A list of public keys.
publicKeyList_items :: Lens.Lens' PublicKeyList (Core.Maybe [PublicKeySummary])
publicKeyList_items = Lens.lens (\PublicKeyList' {items} -> items) (\s@PublicKeyList' {} a -> s {items = a} :: PublicKeyList) Core.. Lens.mapping Lens._Coerce

-- | If there are more elements to be listed, this element is present and
-- contains the value that you can use for the @Marker@ request parameter
-- to continue listing your public keys where you left off.
publicKeyList_nextMarker :: Lens.Lens' PublicKeyList (Core.Maybe Core.Text)
publicKeyList_nextMarker = Lens.lens (\PublicKeyList' {nextMarker} -> nextMarker) (\s@PublicKeyList' {} a -> s {nextMarker = a} :: PublicKeyList)

-- | The maximum number of public keys you want in the response.
publicKeyList_maxItems :: Lens.Lens' PublicKeyList Core.Int
publicKeyList_maxItems = Lens.lens (\PublicKeyList' {maxItems} -> maxItems) (\s@PublicKeyList' {} a -> s {maxItems = a} :: PublicKeyList)

-- | The number of public keys in the list.
publicKeyList_quantity :: Lens.Lens' PublicKeyList Core.Int
publicKeyList_quantity = Lens.lens (\PublicKeyList' {quantity} -> quantity) (\s@PublicKeyList' {} a -> s {quantity = a} :: PublicKeyList)

instance Core.FromXML PublicKeyList where
  parseXML x =
    PublicKeyList'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "PublicKeySummary")
               )
      Core.<*> (x Core..@? "NextMarker")
      Core.<*> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable PublicKeyList

instance Core.NFData PublicKeyList
