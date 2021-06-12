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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex data type that includes the field patterns to match for
-- field-level encryption.
--
-- /See:/ 'newFieldPatterns' smart constructor.
data FieldPatterns = FieldPatterns'
  { -- | An array of the field-level encryption field patterns.
    items :: Core.Maybe [Core.Text],
    -- | The number of field-level encryption field patterns.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  FieldPatterns
newFieldPatterns pQuantity_ =
  FieldPatterns'
    { items = Core.Nothing,
      quantity = pQuantity_
    }

-- | An array of the field-level encryption field patterns.
fieldPatterns_items :: Lens.Lens' FieldPatterns (Core.Maybe [Core.Text])
fieldPatterns_items = Lens.lens (\FieldPatterns' {items} -> items) (\s@FieldPatterns' {} a -> s {items = a} :: FieldPatterns) Core.. Lens.mapping Lens._Coerce

-- | The number of field-level encryption field patterns.
fieldPatterns_quantity :: Lens.Lens' FieldPatterns Core.Int
fieldPatterns_quantity = Lens.lens (\FieldPatterns' {quantity} -> quantity) (\s@FieldPatterns' {} a -> s {quantity = a} :: FieldPatterns)

instance Core.FromXML FieldPatterns where
  parseXML x =
    FieldPatterns'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "FieldPattern")
               )
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable FieldPatterns

instance Core.NFData FieldPatterns

instance Core.ToXML FieldPatterns where
  toXML FieldPatterns' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML
            (Core.toXMLList "FieldPattern" Core.<$> items),
        "Quantity" Core.@= quantity
      ]
