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
-- Module      : Amazonka.ResourceGroups.Types.GroupFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.GroupFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroups.Types.GroupFilterName

-- | A filter collection that you can use to restrict the results from a
-- @List@ operation to only those you want to include.
--
-- /See:/ 'newGroupFilter' smart constructor.
data GroupFilter = GroupFilter'
  { -- | The name of the filter. Filter names are case-sensitive.
    name :: GroupFilterName,
    -- | One or more filter values. Allowed filter values vary by group filter
    -- name, and are case-sensitive.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'groupFilter_name' - The name of the filter. Filter names are case-sensitive.
--
-- 'values', 'groupFilter_values' - One or more filter values. Allowed filter values vary by group filter
-- name, and are case-sensitive.
newGroupFilter ::
  -- | 'name'
  GroupFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  GroupFilter
newGroupFilter pName_ pValues_ =
  GroupFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the filter. Filter names are case-sensitive.
groupFilter_name :: Lens.Lens' GroupFilter GroupFilterName
groupFilter_name = Lens.lens (\GroupFilter' {name} -> name) (\s@GroupFilter' {} a -> s {name = a} :: GroupFilter)

-- | One or more filter values. Allowed filter values vary by group filter
-- name, and are case-sensitive.
groupFilter_values :: Lens.Lens' GroupFilter (Prelude.NonEmpty Prelude.Text)
groupFilter_values = Lens.lens (\GroupFilter' {values} -> values) (\s@GroupFilter' {} a -> s {values = a} :: GroupFilter) Prelude.. Lens.coerced

instance Prelude.Hashable GroupFilter where
  hashWithSalt _salt GroupFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData GroupFilter where
  rnf GroupFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON GroupFilter where
  toJSON GroupFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
