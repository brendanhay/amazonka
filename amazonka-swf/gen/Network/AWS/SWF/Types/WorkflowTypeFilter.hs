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
-- Module      : Network.AWS.SWF.Types.WorkflowTypeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowTypeFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Used to filter workflow execution query results by type. Each parameter,
-- if specified, defines a rule that must be satisfied by each returned
-- result.
--
-- /See:/ 'newWorkflowTypeFilter' smart constructor.
data WorkflowTypeFilter = WorkflowTypeFilter'
  { -- | Version of the workflow type.
    version :: Core.Maybe Core.Text,
    -- | Name of the workflow type.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  WorkflowTypeFilter
newWorkflowTypeFilter pName_ =
  WorkflowTypeFilter'
    { version = Core.Nothing,
      name = pName_
    }

-- | Version of the workflow type.
workflowTypeFilter_version :: Lens.Lens' WorkflowTypeFilter (Core.Maybe Core.Text)
workflowTypeFilter_version = Lens.lens (\WorkflowTypeFilter' {version} -> version) (\s@WorkflowTypeFilter' {} a -> s {version = a} :: WorkflowTypeFilter)

-- | Name of the workflow type.
workflowTypeFilter_name :: Lens.Lens' WorkflowTypeFilter Core.Text
workflowTypeFilter_name = Lens.lens (\WorkflowTypeFilter' {name} -> name) (\s@WorkflowTypeFilter' {} a -> s {name = a} :: WorkflowTypeFilter)

instance Core.Hashable WorkflowTypeFilter

instance Core.NFData WorkflowTypeFilter

instance Core.ToJSON WorkflowTypeFilter where
  toJSON WorkflowTypeFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("version" Core..=) Core.<$> version,
            Core.Just ("name" Core..= name)
          ]
      )
