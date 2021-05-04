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
-- Module      : Network.AWS.ResourceGroups.Types.GroupFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ResourceGroups.Types.GroupFilterName

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      values = Prelude._Coerce Lens.# pValues_
    }

-- | The name of the filter. Filter names are case-sensitive.
groupFilter_name :: Lens.Lens' GroupFilter GroupFilterName
groupFilter_name = Lens.lens (\GroupFilter' {name} -> name) (\s@GroupFilter' {} a -> s {name = a} :: GroupFilter)

-- | One or more filter values. Allowed filter values vary by group filter
-- name, and are case-sensitive.
groupFilter_values :: Lens.Lens' GroupFilter (Prelude.NonEmpty Prelude.Text)
groupFilter_values = Lens.lens (\GroupFilter' {values} -> values) (\s@GroupFilter' {} a -> s {values = a} :: GroupFilter) Prelude.. Prelude._Coerce

instance Prelude.Hashable GroupFilter

instance Prelude.NFData GroupFilter

instance Prelude.ToJSON GroupFilter where
  toJSON GroupFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Values" Prelude..= values)
          ]
      )
