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
-- Module      : Amazonka.ResourceGroups.Types.ResourceFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.ResourceFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroups.Types.ResourceFilterName

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the filter. Filter names are case-sensitive.
resourceFilter_name :: Lens.Lens' ResourceFilter ResourceFilterName
resourceFilter_name = Lens.lens (\ResourceFilter' {name} -> name) (\s@ResourceFilter' {} a -> s {name = a} :: ResourceFilter)

-- | One or more filter values. Allowed filter values vary by resource filter
-- name, and are case-sensitive.
resourceFilter_values :: Lens.Lens' ResourceFilter (Prelude.NonEmpty Prelude.Text)
resourceFilter_values = Lens.lens (\ResourceFilter' {values} -> values) (\s@ResourceFilter' {} a -> s {values = a} :: ResourceFilter) Prelude.. Lens.coerced

instance Prelude.Hashable ResourceFilter where
  hashWithSalt _salt ResourceFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData ResourceFilter where
  rnf ResourceFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Core.ToJSON ResourceFilter where
  toJSON ResourceFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Values" Core..= values)
          ]
      )
