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
-- Module      : Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about a task that failed to submit during an execution.
--
-- /See:/ 'newTaskSubmitFailedEventDetails' smart constructor.
data TaskSubmitFailedEventDetails = TaskSubmitFailedEventDetails'
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
-- Create a value of 'TaskSubmitFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'taskSubmitFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'taskSubmitFailedEventDetails_error' - The error code of the failure.
--
-- 'resourceType', 'taskSubmitFailedEventDetails_resourceType' - The action of the resource called by a task state.
--
-- 'resource', 'taskSubmitFailedEventDetails_resource' - The service name of the resource in a task state.
newTaskSubmitFailedEventDetails ::
  -- | 'resourceType'
  Core.Text ->
  -- | 'resource'
  Core.Text ->
  TaskSubmitFailedEventDetails
newTaskSubmitFailedEventDetails
  pResourceType_
  pResource_ =
    TaskSubmitFailedEventDetails'
      { cause = Core.Nothing,
        error = Core.Nothing,
        resourceType = pResourceType_,
        resource = pResource_
      }

-- | A more detailed explanation of the cause of the failure.
taskSubmitFailedEventDetails_cause :: Lens.Lens' TaskSubmitFailedEventDetails (Core.Maybe Core.Text)
taskSubmitFailedEventDetails_cause = Lens.lens (\TaskSubmitFailedEventDetails' {cause} -> cause) (\s@TaskSubmitFailedEventDetails' {} a -> s {cause = a} :: TaskSubmitFailedEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The error code of the failure.
taskSubmitFailedEventDetails_error :: Lens.Lens' TaskSubmitFailedEventDetails (Core.Maybe Core.Text)
taskSubmitFailedEventDetails_error = Lens.lens (\TaskSubmitFailedEventDetails' {error} -> error) (\s@TaskSubmitFailedEventDetails' {} a -> s {error = a} :: TaskSubmitFailedEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The action of the resource called by a task state.
taskSubmitFailedEventDetails_resourceType :: Lens.Lens' TaskSubmitFailedEventDetails Core.Text
taskSubmitFailedEventDetails_resourceType = Lens.lens (\TaskSubmitFailedEventDetails' {resourceType} -> resourceType) (\s@TaskSubmitFailedEventDetails' {} a -> s {resourceType = a} :: TaskSubmitFailedEventDetails)

-- | The service name of the resource in a task state.
taskSubmitFailedEventDetails_resource :: Lens.Lens' TaskSubmitFailedEventDetails Core.Text
taskSubmitFailedEventDetails_resource = Lens.lens (\TaskSubmitFailedEventDetails' {resource} -> resource) (\s@TaskSubmitFailedEventDetails' {} a -> s {resource = a} :: TaskSubmitFailedEventDetails)

instance Core.FromJSON TaskSubmitFailedEventDetails where
  parseJSON =
    Core.withObject
      "TaskSubmitFailedEventDetails"
      ( \x ->
          TaskSubmitFailedEventDetails'
            Core.<$> (x Core..:? "cause")
            Core.<*> (x Core..:? "error")
            Core.<*> (x Core..: "resourceType")
            Core.<*> (x Core..: "resource")
      )

instance Core.Hashable TaskSubmitFailedEventDetails

instance Core.NFData TaskSubmitFailedEventDetails
