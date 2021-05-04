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
-- Module      : Network.AWS.DMS.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Filter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifies the name and value of a filter object. This filter is used to
-- limit the number and type of AWS DMS objects that are returned for a
-- particular @Describe*@ call or similar operation. Filters are used as an
-- optional parameter for certain API operations.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The name of the filter as specified for a @Describe*@ or similar
    -- operation.
    name :: Prelude.Text,
    -- | The filter value, which can specify one or more values used to narrow
    -- the returned results.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filter_name' - The name of the filter as specified for a @Describe*@ or similar
-- operation.
--
-- 'values', 'filter_values' - The filter value, which can specify one or more values used to narrow
-- the returned results.
newFilter ::
  -- | 'name'
  Prelude.Text ->
  Filter
newFilter pName_ =
  Filter' {name = pName_, values = Prelude.mempty}

-- | The name of the filter as specified for a @Describe*@ or similar
-- operation.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | The filter value, which can specify one or more values used to narrow
-- the returned results.
filter_values :: Lens.Lens' Filter [Prelude.Text]
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Prelude._Coerce

instance Prelude.Hashable Filter

instance Prelude.NFData Filter

instance Prelude.ToJSON Filter where
  toJSON Filter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Values" Prelude..= values)
          ]
      )
