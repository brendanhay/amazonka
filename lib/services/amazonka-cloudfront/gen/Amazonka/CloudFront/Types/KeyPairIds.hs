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
-- Module      : Amazonka.CloudFront.Types.KeyPairIds
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.KeyPairIds where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of CloudFront key pair identifiers.
--
-- /See:/ 'newKeyPairIds' smart constructor.
data KeyPairIds = KeyPairIds'
  { -- | A list of CloudFront key pair identifiers.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of key pair identifiers in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
keyPairIds_items = Lens.lens (\KeyPairIds' {items} -> items) (\s@KeyPairIds' {} a -> s {items = a} :: KeyPairIds) Prelude.. Lens.mapping Lens.coerced

-- | The number of key pair identifiers in the list.
keyPairIds_quantity :: Lens.Lens' KeyPairIds Prelude.Int
keyPairIds_quantity = Lens.lens (\KeyPairIds' {quantity} -> quantity) (\s@KeyPairIds' {} a -> s {quantity = a} :: KeyPairIds)

instance Data.FromXML KeyPairIds where
  parseXML x =
    KeyPairIds'
      Prelude.<$> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "KeyPairId")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable KeyPairIds where
  hashWithSalt _salt KeyPairIds' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData KeyPairIds where
  rnf KeyPairIds' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf quantity
