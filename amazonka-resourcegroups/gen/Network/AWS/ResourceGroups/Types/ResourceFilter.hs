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
-- Module      : Network.AWS.ResourceGroups.Types.ResourceFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ResourceFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ResourceGroups.Types.ResourceFilterName

-- | A filter name and value pair that is used to obtain more specific
-- results from a list of resources.
--
-- /See:/ 'newResourceFilter' smart constructor.
data ResourceFilter = ResourceFilter'
  { -- | The name of the filter. Filter names are case-sensitive.
    name :: ResourceFilterName,
    -- | One or more filter values. Allowed filter values vary by resource filter
    -- name, and are case-sensitive.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resourceFilter_name' - The name of the filter. Filter names are case-sensitive.
--
-- 'values', 'resourceFilter_values' - One or more filter values. Allowed filter values vary by resource filter
-- name, and are case-sensitive.
newResourceFilter ::
  -- | 'name'
  ResourceFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  ResourceFilter
newResourceFilter pName_ pValues_ =
  ResourceFilter'
    { name = pName_,
      values = Prelude._Coerce Lens.# pValues_
    }

-- | The name of the filter. Filter names are case-sensitive.
resourceFilter_name :: Lens.Lens' ResourceFilter ResourceFilterName
resourceFilter_name = Lens.lens (\ResourceFilter' {name} -> name) (\s@ResourceFilter' {} a -> s {name = a} :: ResourceFilter)

-- | One or more filter values. Allowed filter values vary by resource filter
-- name, and are case-sensitive.
resourceFilter_values :: Lens.Lens' ResourceFilter (Prelude.NonEmpty Prelude.Text)
resourceFilter_values = Lens.lens (\ResourceFilter' {values} -> values) (\s@ResourceFilter' {} a -> s {values = a} :: ResourceFilter) Prelude.. Prelude._Coerce

instance Prelude.Hashable ResourceFilter

instance Prelude.NFData ResourceFilter

instance Prelude.ToJSON ResourceFilter where
  toJSON ResourceFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Values" Prelude..= values)
          ]
      )
