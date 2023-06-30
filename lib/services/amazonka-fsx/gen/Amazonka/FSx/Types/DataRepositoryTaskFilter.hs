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
-- Module      : Amazonka.FSx.Types.DataRepositoryTaskFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryTaskFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DataRepositoryTaskFilterName
import qualified Amazonka.Prelude as Prelude

-- | (Optional) An array of filter objects you can use to filter the response
-- of data repository tasks you will see in the the response. You can
-- filter the tasks returned in the response by one or more file system
-- IDs, task lifecycles, and by task type. A filter object consists of a
-- filter @Name@, and one or more @Values@ for the filter.
--
-- /See:/ 'newDataRepositoryTaskFilter' smart constructor.
data DataRepositoryTaskFilter = DataRepositoryTaskFilter'
  { -- | Name of the task property to use in filtering the tasks returned in the
    -- response.
    --
    -- -   Use @file-system-id@ to retrieve data repository tasks for specific
    --     file systems.
    --
    -- -   Use @task-lifecycle@ to retrieve data repository tasks with one or
    --     more specific lifecycle states, as follows: CANCELED, EXECUTING,
    --     FAILED, PENDING, and SUCCEEDED.
    name :: Prelude.Maybe DataRepositoryTaskFilterName,
    -- | Use Values to include the specific file system IDs and task lifecycle
    -- states for the filters you are using.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataRepositoryTaskFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dataRepositoryTaskFilter_name' - Name of the task property to use in filtering the tasks returned in the
-- response.
--
-- -   Use @file-system-id@ to retrieve data repository tasks for specific
--     file systems.
--
-- -   Use @task-lifecycle@ to retrieve data repository tasks with one or
--     more specific lifecycle states, as follows: CANCELED, EXECUTING,
--     FAILED, PENDING, and SUCCEEDED.
--
-- 'values', 'dataRepositoryTaskFilter_values' - Use Values to include the specific file system IDs and task lifecycle
-- states for the filters you are using.
newDataRepositoryTaskFilter ::
  DataRepositoryTaskFilter
newDataRepositoryTaskFilter =
  DataRepositoryTaskFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | Name of the task property to use in filtering the tasks returned in the
-- response.
--
-- -   Use @file-system-id@ to retrieve data repository tasks for specific
--     file systems.
--
-- -   Use @task-lifecycle@ to retrieve data repository tasks with one or
--     more specific lifecycle states, as follows: CANCELED, EXECUTING,
--     FAILED, PENDING, and SUCCEEDED.
dataRepositoryTaskFilter_name :: Lens.Lens' DataRepositoryTaskFilter (Prelude.Maybe DataRepositoryTaskFilterName)
dataRepositoryTaskFilter_name = Lens.lens (\DataRepositoryTaskFilter' {name} -> name) (\s@DataRepositoryTaskFilter' {} a -> s {name = a} :: DataRepositoryTaskFilter)

-- | Use Values to include the specific file system IDs and task lifecycle
-- states for the filters you are using.
dataRepositoryTaskFilter_values :: Lens.Lens' DataRepositoryTaskFilter (Prelude.Maybe [Prelude.Text])
dataRepositoryTaskFilter_values = Lens.lens (\DataRepositoryTaskFilter' {values} -> values) (\s@DataRepositoryTaskFilter' {} a -> s {values = a} :: DataRepositoryTaskFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable DataRepositoryTaskFilter where
  hashWithSalt _salt DataRepositoryTaskFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData DataRepositoryTaskFilter where
  rnf DataRepositoryTaskFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON DataRepositoryTaskFilter where
  toJSON DataRepositoryTaskFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
