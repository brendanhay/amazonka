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
-- Module      : Amazonka.IoTThingsGraph.Types.EntityFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.EntityFilter where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.EntityFilterName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that filters an entity search. Multiple filters function as OR
-- criteria in the search. For example a search that includes a @NAMESPACE@
-- and a @REFERENCED_ENTITY_ID@ filter searches for entities in the
-- specified namespace that use the entity specified by the value of
-- @REFERENCED_ENTITY_ID@.
--
-- /See:/ 'newEntityFilter' smart constructor.
data EntityFilter = EntityFilter'
  { -- | An array of string values for the search filter field. Multiple values
    -- function as AND criteria in the search.
    value :: Prelude.Maybe [Prelude.Text],
    -- | The name of the entity search filter field. @REFERENCED_ENTITY_ID@
    -- filters on entities that are used by the entity in the result set. For
    -- example, you can filter on the ID of a property that is used in a state.
    name :: Prelude.Maybe EntityFilterName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'entityFilter_value' - An array of string values for the search filter field. Multiple values
-- function as AND criteria in the search.
--
-- 'name', 'entityFilter_name' - The name of the entity search filter field. @REFERENCED_ENTITY_ID@
-- filters on entities that are used by the entity in the result set. For
-- example, you can filter on the ID of a property that is used in a state.
newEntityFilter ::
  EntityFilter
newEntityFilter =
  EntityFilter'
    { value = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | An array of string values for the search filter field. Multiple values
-- function as AND criteria in the search.
entityFilter_value :: Lens.Lens' EntityFilter (Prelude.Maybe [Prelude.Text])
entityFilter_value = Lens.lens (\EntityFilter' {value} -> value) (\s@EntityFilter' {} a -> s {value = a} :: EntityFilter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the entity search filter field. @REFERENCED_ENTITY_ID@
-- filters on entities that are used by the entity in the result set. For
-- example, you can filter on the ID of a property that is used in a state.
entityFilter_name :: Lens.Lens' EntityFilter (Prelude.Maybe EntityFilterName)
entityFilter_name = Lens.lens (\EntityFilter' {name} -> name) (\s@EntityFilter' {} a -> s {name = a} :: EntityFilter)

instance Prelude.Hashable EntityFilter where
  hashWithSalt _salt EntityFilter' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name

instance Prelude.NFData EntityFilter where
  rnf EntityFilter' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Core.ToJSON EntityFilter where
  toJSON EntityFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("name" Core..=) Prelude.<$> name
          ]
      )
