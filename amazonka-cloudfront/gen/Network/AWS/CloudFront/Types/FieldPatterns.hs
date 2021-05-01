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
-- Module      : Network.AWS.CloudFront.Types.FieldPatterns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldPatterns where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex data type that includes the field patterns to match for
-- field-level encryption.
--
-- /See:/ 'newFieldPatterns' smart constructor.
data FieldPatterns = FieldPatterns'
  { -- | An array of the field-level encryption field patterns.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of field-level encryption field patterns.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FieldPatterns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'fieldPatterns_items' - An array of the field-level encryption field patterns.
--
-- 'quantity', 'fieldPatterns_quantity' - The number of field-level encryption field patterns.
newFieldPatterns ::
  -- | 'quantity'
  Prelude.Int ->
  FieldPatterns
newFieldPatterns pQuantity_ =
  FieldPatterns'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | An array of the field-level encryption field patterns.
fieldPatterns_items :: Lens.Lens' FieldPatterns (Prelude.Maybe [Prelude.Text])
fieldPatterns_items = Lens.lens (\FieldPatterns' {items} -> items) (\s@FieldPatterns' {} a -> s {items = a} :: FieldPatterns) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of field-level encryption field patterns.
fieldPatterns_quantity :: Lens.Lens' FieldPatterns Prelude.Int
fieldPatterns_quantity = Lens.lens (\FieldPatterns' {quantity} -> quantity) (\s@FieldPatterns' {} a -> s {quantity = a} :: FieldPatterns)

instance Prelude.FromXML FieldPatterns where
  parseXML x =
    FieldPatterns'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "FieldPattern")
                  )
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable FieldPatterns

instance Prelude.NFData FieldPatterns

instance Prelude.ToXML FieldPatterns where
  toXML FieldPatterns' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "FieldPattern" Prelude.<$> items),
        "Quantity" Prelude.@= quantity
      ]
