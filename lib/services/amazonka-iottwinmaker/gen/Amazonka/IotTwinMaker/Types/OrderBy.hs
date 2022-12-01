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
-- Module      : Amazonka.IotTwinMaker.Types.OrderBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.OrderBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types.Order
import qualified Amazonka.Prelude as Prelude

-- | Filter criteria that orders the return output. It can be sorted in
-- ascending or descending order.
--
-- /See:/ 'newOrderBy' smart constructor.
data OrderBy = OrderBy'
  { -- | The set order that filters results.
    order :: Prelude.Maybe Order,
    -- | The property name.
    propertyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrderBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'order', 'orderBy_order' - The set order that filters results.
--
-- 'propertyName', 'orderBy_propertyName' - The property name.
newOrderBy ::
  -- | 'propertyName'
  Prelude.Text ->
  OrderBy
newOrderBy pPropertyName_ =
  OrderBy'
    { order = Prelude.Nothing,
      propertyName = pPropertyName_
    }

-- | The set order that filters results.
orderBy_order :: Lens.Lens' OrderBy (Prelude.Maybe Order)
orderBy_order = Lens.lens (\OrderBy' {order} -> order) (\s@OrderBy' {} a -> s {order = a} :: OrderBy)

-- | The property name.
orderBy_propertyName :: Lens.Lens' OrderBy Prelude.Text
orderBy_propertyName = Lens.lens (\OrderBy' {propertyName} -> propertyName) (\s@OrderBy' {} a -> s {propertyName = a} :: OrderBy)

instance Prelude.Hashable OrderBy where
  hashWithSalt _salt OrderBy' {..} =
    _salt `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` propertyName

instance Prelude.NFData OrderBy where
  rnf OrderBy' {..} =
    Prelude.rnf order
      `Prelude.seq` Prelude.rnf propertyName

instance Core.ToJSON OrderBy where
  toJSON OrderBy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("order" Core..=) Prelude.<$> order,
            Prelude.Just ("propertyName" Core..= propertyName)
          ]
      )
