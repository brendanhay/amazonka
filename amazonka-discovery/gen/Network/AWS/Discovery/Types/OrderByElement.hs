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
-- Module      : Network.AWS.Discovery.Types.OrderByElement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.OrderByElement where

import Network.AWS.Discovery.Types.OrderString
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A field and direction for ordered output.
--
-- /See:/ 'newOrderByElement' smart constructor.
data OrderByElement = OrderByElement'
  { -- | Ordering direction.
    sortOrder :: Prelude.Maybe OrderString,
    -- | The field on which to order.
    fieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrderByElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'orderByElement_sortOrder' - Ordering direction.
--
-- 'fieldName', 'orderByElement_fieldName' - The field on which to order.
newOrderByElement ::
  -- | 'fieldName'
  Prelude.Text ->
  OrderByElement
newOrderByElement pFieldName_ =
  OrderByElement'
    { sortOrder = Prelude.Nothing,
      fieldName = pFieldName_
    }

-- | Ordering direction.
orderByElement_sortOrder :: Lens.Lens' OrderByElement (Prelude.Maybe OrderString)
orderByElement_sortOrder = Lens.lens (\OrderByElement' {sortOrder} -> sortOrder) (\s@OrderByElement' {} a -> s {sortOrder = a} :: OrderByElement)

-- | The field on which to order.
orderByElement_fieldName :: Lens.Lens' OrderByElement Prelude.Text
orderByElement_fieldName = Lens.lens (\OrderByElement' {fieldName} -> fieldName) (\s@OrderByElement' {} a -> s {fieldName = a} :: OrderByElement)

instance Prelude.Hashable OrderByElement

instance Prelude.NFData OrderByElement

instance Prelude.ToJSON OrderByElement where
  toJSON OrderByElement' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("sortOrder" Prelude..=) Prelude.<$> sortOrder,
            Prelude.Just ("fieldName" Prelude..= fieldName)
          ]
      )
