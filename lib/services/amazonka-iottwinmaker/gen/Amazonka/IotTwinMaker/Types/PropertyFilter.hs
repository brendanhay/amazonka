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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataValue
import qualified Amazonka.Prelude as Prelude

-- | An object that filters items returned by a property request.
--
-- /See:/ 'newPropertyFilter' smart constructor.
data PropertyFilter = PropertyFilter'
  { -- | The operator associated with this property filter.
    operator :: Prelude.Maybe Prelude.Text,
    -- | The property name associated with this property filter.
    propertyName :: Prelude.Maybe Prelude.Text,
    -- | The value associated with this property filter.
    value :: Prelude.Maybe DataValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operator', 'propertyFilter_operator' - The operator associated with this property filter.
--
-- 'propertyName', 'propertyFilter_propertyName' - The property name associated with this property filter.
--
-- 'value', 'propertyFilter_value' - The value associated with this property filter.
newPropertyFilter ::
  PropertyFilter
newPropertyFilter =
  PropertyFilter'
    { operator = Prelude.Nothing,
      propertyName = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The operator associated with this property filter.
propertyFilter_operator :: Lens.Lens' PropertyFilter (Prelude.Maybe Prelude.Text)
propertyFilter_operator = Lens.lens (\PropertyFilter' {operator} -> operator) (\s@PropertyFilter' {} a -> s {operator = a} :: PropertyFilter)

-- | The property name associated with this property filter.
propertyFilter_propertyName :: Lens.Lens' PropertyFilter (Prelude.Maybe Prelude.Text)
propertyFilter_propertyName = Lens.lens (\PropertyFilter' {propertyName} -> propertyName) (\s@PropertyFilter' {} a -> s {propertyName = a} :: PropertyFilter)

-- | The value associated with this property filter.
propertyFilter_value :: Lens.Lens' PropertyFilter (Prelude.Maybe DataValue)
propertyFilter_value = Lens.lens (\PropertyFilter' {value} -> value) (\s@PropertyFilter' {} a -> s {value = a} :: PropertyFilter)

instance Prelude.Hashable PropertyFilter where
  hashWithSalt _salt PropertyFilter' {..} =
    _salt `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` propertyName
      `Prelude.hashWithSalt` value

instance Prelude.NFData PropertyFilter where
  rnf PropertyFilter' {..} =
    Prelude.rnf operator
      `Prelude.seq` Prelude.rnf propertyName
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON PropertyFilter where
  toJSON PropertyFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("operator" Data..=) Prelude.<$> operator,
            ("propertyName" Data..=) Prelude.<$> propertyName,
            ("value" Data..=) Prelude.<$> value
          ]
      )
