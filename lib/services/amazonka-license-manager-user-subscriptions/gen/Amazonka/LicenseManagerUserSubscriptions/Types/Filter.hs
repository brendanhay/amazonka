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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types.Filter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A filter name and value pair that is used to return more specific
-- results from a describe operation. Filters can be used to match a set of
-- resources by specific criteria, such as tags, attributes, or IDs.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The name of an attribute to use as a filter.
    attribute :: Prelude.Maybe Prelude.Text,
    -- | The type of search (For example, eq, geq, leq)
    operation :: Prelude.Maybe Prelude.Text,
    -- | Value of the filter.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'filter_attribute' - The name of an attribute to use as a filter.
--
-- 'operation', 'filter_operation' - The type of search (For example, eq, geq, leq)
--
-- 'value', 'filter_value' - Value of the filter.
newFilter ::
  Filter
newFilter =
  Filter'
    { attribute = Prelude.Nothing,
      operation = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of an attribute to use as a filter.
filter_attribute :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_attribute = Lens.lens (\Filter' {attribute} -> attribute) (\s@Filter' {} a -> s {attribute = a} :: Filter)

-- | The type of search (For example, eq, geq, leq)
filter_operation :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_operation = Lens.lens (\Filter' {operation} -> operation) (\s@Filter' {} a -> s {operation = a} :: Filter)

-- | Value of the filter.
filter_value :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_value = Lens.lens (\Filter' {value} -> value) (\s@Filter' {} a -> s {value = a} :: Filter)

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` value

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON Filter where
  toJSON Filter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Attribute" Core..=) Prelude.<$> attribute,
            ("Operation" Core..=) Prelude.<$> operation,
            ("Value" Core..=) Prelude.<$> value
          ]
      )
