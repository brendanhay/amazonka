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
-- Module      : Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about a resource timeout that occurred during an
-- execution.
--
-- /See:/ 'newTaskTimedOutEventDetails' smart constructor.
data TaskTimedOutEventDetails = TaskTimedOutEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The error code of the failure.
    error :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The action of the resource called by a task state.
    resourceType :: Core.Text,
    -- | The service name of the resource in a task state.
    resource :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'TaskTimedOutEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'taskTimedOutEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'taskTimedOutEventDetails_error' - The error code of the failure.
--
-- 'resourceType', 'taskTimedOutEventDetails_resourceType' - The action of the resource called by a task state.
--
-- 'resource', 'taskTimedOutEventDetails_resource' - The service name of the resource in a task state.
newTaskTimedOutEventDetails ::
  -- | 'resourceType'
  Core.Text ->
  -- | 'resource'
  Core.Text ->
  TaskTimedOutEventDetails
newTaskTimedOutEventDetails pResourceType_ pResource_ =
  TaskTimedOutEventDetails'
    { cause = Core.Nothing,
      error = Core.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | A more detailed explanation of the cause of the failure.
taskTimedOutEventDetails_cause :: Lens.Lens' TaskTimedOutEventDetails (Core.Maybe Core.Text)
taskTimedOutEventDetails_cause = Lens.lens (\TaskTimedOutEventDetails' {cause} -> cause) (\s@TaskTimedOutEventDetails' {} a -> s {cause = a} :: TaskTimedOutEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The error code of the failure.
taskTimedOutEventDetails_error :: Lens.Lens' TaskTimedOutEventDetails (Core.Maybe Core.Text)
taskTimedOutEventDetails_error = Lens.lens (\TaskTimedOutEventDetails' {error} -> error) (\s@TaskTimedOutEventDetails' {} a -> s {error = a} :: TaskTimedOutEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The action of the resource called by a task state.
taskTimedOutEventDetails_resourceType :: Lens.Lens' TaskTimedOutEventDetails Core.Text
taskTimedOutEventDetails_resourceType = Lens.lens (\TaskTimedOutEventDetails' {resourceType} -> resourceType) (\s@TaskTimedOutEventDetails' {} a -> s {resourceType = a} :: TaskTimedOutEventDetails)

-- | The service name of the resource in a task state.
taskTimedOutEventDetails_resource :: Lens.Lens' TaskTimedOutEventDetails Core.Text
taskTimedOutEventDetails_resource = Lens.lens (\TaskTimedOutEventDetails' {resource} -> resource) (\s@TaskTimedOutEventDetails' {} a -> s {resource = a} :: TaskTimedOutEventDetails)

instance Core.FromJSON TaskTimedOutEventDetails where
  parseJSON =
    Core.withObject
      "TaskTimedOutEventDetails"
      ( \x ->
          TaskTimedOutEventDetails'
            Core.<$> (x Core..:? "cause")
            Core.<*> (x Core..:? "error")
            Core.<*> (x Core..: "resourceType")
            Core.<*> (x Core..: "resource")
      )

instance Core.Hashable TaskTimedOutEventDetails

instance Core.NFData TaskTimedOutEventDetails
