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
-- Module      : Network.AWS.StepFunctions.Types.TaskStartedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskStartedEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about the start of a task during an execution.
--
-- /See:/ 'newTaskStartedEventDetails' smart constructor.
data TaskStartedEventDetails = TaskStartedEventDetails'
  { -- | The action of the resource called by a task state.
    resourceType :: Core.Text,
    -- | The service name of the resource in a task state.
    resource :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TaskStartedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'taskStartedEventDetails_resourceType' - The action of the resource called by a task state.
--
-- 'resource', 'taskStartedEventDetails_resource' - The service name of the resource in a task state.
newTaskStartedEventDetails ::
  -- | 'resourceType'
  Core.Text ->
  -- | 'resource'
  Core.Text ->
  TaskStartedEventDetails
newTaskStartedEventDetails pResourceType_ pResource_ =
  TaskStartedEventDetails'
    { resourceType =
        pResourceType_,
      resource = pResource_
    }

-- | The action of the resource called by a task state.
taskStartedEventDetails_resourceType :: Lens.Lens' TaskStartedEventDetails Core.Text
taskStartedEventDetails_resourceType = Lens.lens (\TaskStartedEventDetails' {resourceType} -> resourceType) (\s@TaskStartedEventDetails' {} a -> s {resourceType = a} :: TaskStartedEventDetails)

-- | The service name of the resource in a task state.
taskStartedEventDetails_resource :: Lens.Lens' TaskStartedEventDetails Core.Text
taskStartedEventDetails_resource = Lens.lens (\TaskStartedEventDetails' {resource} -> resource) (\s@TaskStartedEventDetails' {} a -> s {resource = a} :: TaskStartedEventDetails)

instance Core.FromJSON TaskStartedEventDetails where
  parseJSON =
    Core.withObject
      "TaskStartedEventDetails"
      ( \x ->
          TaskStartedEventDetails'
            Core.<$> (x Core..: "resourceType")
            Core.<*> (x Core..: "resource")
      )

instance Core.Hashable TaskStartedEventDetails

instance Core.NFData TaskStartedEventDetails
