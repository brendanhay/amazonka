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
-- Module      : Network.AWS.CloudFront.Types.KeyPairIds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyPairIds where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of CloudFront key pair identifiers.
--
-- /See:/ 'newKeyPairIds' smart constructor.
data KeyPairIds = KeyPairIds'
  { -- | A list of CloudFront key pair identifiers.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of key pair identifiers in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KeyPairIds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'keyPairIds_items' - A list of CloudFront key pair identifiers.
--
-- 'quantity', 'keyPairIds_quantity' - The number of key pair identifiers in the list.
newKeyPairIds ::
  -- | 'quantity'
  Prelude.Int ->
  KeyPairIds
newKeyPairIds pQuantity_ =
  KeyPairIds'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | A list of CloudFront key pair identifiers.
keyPairIds_items :: Lens.Lens' KeyPairIds (Prelude.Maybe [Prelude.Text])
keyPairIds_items = Lens.lens (\KeyPairIds' {items} -> items) (\s@KeyPairIds' {} a -> s {items = a} :: KeyPairIds) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of key pair identifiers in the list.
keyPairIds_quantity :: Lens.Lens' KeyPairIds Prelude.Int
keyPairIds_quantity = Lens.lens (\KeyPairIds' {quantity} -> quantity) (\s@KeyPairIds' {} a -> s {quantity = a} :: KeyPairIds)

instance Prelude.FromXML KeyPairIds where
  parseXML x =
    KeyPairIds'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "KeyPairId")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable KeyPairIds

instance Prelude.NFData KeyPairIds
