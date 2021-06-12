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
-- Module      : Network.AWS.StepFunctions.Types.TaskFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskFailedEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about a task failure event.
--
-- /See:/ 'newTaskFailedEventDetails' smart constructor.
data TaskFailedEventDetails = TaskFailedEventDetails'
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
-- Create a value of 'TaskFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'taskFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'taskFailedEventDetails_error' - The error code of the failure.
--
-- 'resourceType', 'taskFailedEventDetails_resourceType' - The action of the resource called by a task state.
--
-- 'resource', 'taskFailedEventDetails_resource' - The service name of the resource in a task state.
newTaskFailedEventDetails ::
  -- | 'resourceType'
  Core.Text ->
  -- | 'resource'
  Core.Text ->
  TaskFailedEventDetails
newTaskFailedEventDetails pResourceType_ pResource_ =
  TaskFailedEventDetails'
    { cause = Core.Nothing,
      error = Core.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | A more detailed explanation of the cause of the failure.
taskFailedEventDetails_cause :: Lens.Lens' TaskFailedEventDetails (Core.Maybe Core.Text)
taskFailedEventDetails_cause = Lens.lens (\TaskFailedEventDetails' {cause} -> cause) (\s@TaskFailedEventDetails' {} a -> s {cause = a} :: TaskFailedEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The error code of the failure.
taskFailedEventDetails_error :: Lens.Lens' TaskFailedEventDetails (Core.Maybe Core.Text)
taskFailedEventDetails_error = Lens.lens (\TaskFailedEventDetails' {error} -> error) (\s@TaskFailedEventDetails' {} a -> s {error = a} :: TaskFailedEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The action of the resource called by a task state.
taskFailedEventDetails_resourceType :: Lens.Lens' TaskFailedEventDetails Core.Text
taskFailedEventDetails_resourceType = Lens.lens (\TaskFailedEventDetails' {resourceType} -> resourceType) (\s@TaskFailedEventDetails' {} a -> s {resourceType = a} :: TaskFailedEventDetails)

-- | The service name of the resource in a task state.
taskFailedEventDetails_resource :: Lens.Lens' TaskFailedEventDetails Core.Text
taskFailedEventDetails_resource = Lens.lens (\TaskFailedEventDetails' {resource} -> resource) (\s@TaskFailedEventDetails' {} a -> s {resource = a} :: TaskFailedEventDetails)

instance Core.FromJSON TaskFailedEventDetails where
  parseJSON =
    Core.withObject
      "TaskFailedEventDetails"
      ( \x ->
          TaskFailedEventDetails'
            Core.<$> (x Core..:? "cause")
            Core.<*> (x Core..:? "error")
            Core.<*> (x Core..: "resourceType")
            Core.<*> (x Core..: "resource")
      )

instance Core.Hashable TaskFailedEventDetails

instance Core.NFData TaskFailedEventDetails
