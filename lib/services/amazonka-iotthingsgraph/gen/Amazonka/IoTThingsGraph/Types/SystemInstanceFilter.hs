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
-- Module      : Amazonka.IoTThingsGraph.Types.SystemInstanceFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.SystemInstanceFilter where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.SystemInstanceFilterName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that filters a system instance search. Multiple filters
-- function as OR criteria in the search. For example a search that
-- includes a GREENGRASS_GROUP_NAME and a STATUS filter searches for system
-- instances in the specified Greengrass group that have the specified
-- status.
--
-- /See:/ 'newSystemInstanceFilter' smart constructor.
data SystemInstanceFilter = SystemInstanceFilter'
  { -- | An array of string values for the search filter field. Multiple values
    -- function as AND criteria in the search.
    value :: Prelude.Maybe [Prelude.Text],
    -- | The name of the search filter field.
    name :: Prelude.Maybe SystemInstanceFilterName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemInstanceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'systemInstanceFilter_value' - An array of string values for the search filter field. Multiple values
-- function as AND criteria in the search.
--
-- 'name', 'systemInstanceFilter_name' - The name of the search filter field.
newSystemInstanceFilter ::
  SystemInstanceFilter
newSystemInstanceFilter =
  SystemInstanceFilter'
    { value = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | An array of string values for the search filter field. Multiple values
-- function as AND criteria in the search.
systemInstanceFilter_value :: Lens.Lens' SystemInstanceFilter (Prelude.Maybe [Prelude.Text])
systemInstanceFilter_value = Lens.lens (\SystemInstanceFilter' {value} -> value) (\s@SystemInstanceFilter' {} a -> s {value = a} :: SystemInstanceFilter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the search filter field.
systemInstanceFilter_name :: Lens.Lens' SystemInstanceFilter (Prelude.Maybe SystemInstanceFilterName)
systemInstanceFilter_name = Lens.lens (\SystemInstanceFilter' {name} -> name) (\s@SystemInstanceFilter' {} a -> s {name = a} :: SystemInstanceFilter)

instance Prelude.Hashable SystemInstanceFilter where
  hashWithSalt salt' SystemInstanceFilter' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData SystemInstanceFilter where
  rnf SystemInstanceFilter' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Core.ToJSON SystemInstanceFilter where
  toJSON SystemInstanceFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("name" Core..=) Prelude.<$> name
          ]
      )
