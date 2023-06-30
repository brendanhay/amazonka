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
-- Module      : Amazonka.SWF.Types.WorkflowTypeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowTypeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used to filter workflow execution query results by type. Each parameter,
-- if specified, defines a rule that must be satisfied by each returned
-- result.
--
-- /See:/ 'newWorkflowTypeFilter' smart constructor.
data WorkflowTypeFilter = WorkflowTypeFilter'
  { -- | Version of the workflow type.
    version :: Prelude.Maybe Prelude.Text,
    -- | Name of the workflow type.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowTypeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'workflowTypeFilter_version' - Version of the workflow type.
--
-- 'name', 'workflowTypeFilter_name' - Name of the workflow type.
newWorkflowTypeFilter ::
  -- | 'name'
  Prelude.Text ->
  WorkflowTypeFilter
newWorkflowTypeFilter pName_ =
  WorkflowTypeFilter'
    { version = Prelude.Nothing,
      name = pName_
    }

-- | Version of the workflow type.
workflowTypeFilter_version :: Lens.Lens' WorkflowTypeFilter (Prelude.Maybe Prelude.Text)
workflowTypeFilter_version = Lens.lens (\WorkflowTypeFilter' {version} -> version) (\s@WorkflowTypeFilter' {} a -> s {version = a} :: WorkflowTypeFilter)

-- | Name of the workflow type.
workflowTypeFilter_name :: Lens.Lens' WorkflowTypeFilter Prelude.Text
workflowTypeFilter_name = Lens.lens (\WorkflowTypeFilter' {name} -> name) (\s@WorkflowTypeFilter' {} a -> s {name = a} :: WorkflowTypeFilter)

instance Prelude.Hashable WorkflowTypeFilter where
  hashWithSalt _salt WorkflowTypeFilter' {..} =
    _salt
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name

instance Prelude.NFData WorkflowTypeFilter where
  rnf WorkflowTypeFilter' {..} =
    Prelude.rnf version `Prelude.seq` Prelude.rnf name

instance Data.ToJSON WorkflowTypeFilter where
  toJSON WorkflowTypeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("version" Data..=) Prelude.<$> version,
            Prelude.Just ("name" Data..= name)
          ]
      )
