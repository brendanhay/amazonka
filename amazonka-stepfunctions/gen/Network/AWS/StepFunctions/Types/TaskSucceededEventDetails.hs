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
-- Module      : Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskSucceededEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the successful completion of a task state.
--
-- /See:/ 'newTaskSucceededEventDetails' smart constructor.
data TaskSucceededEventDetails = TaskSucceededEventDetails'
  { -- | The full JSON response from a resource when a task has succeeded. This
    -- response becomes the output of the related task. Length constraints
    -- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Core.Maybe HistoryEventExecutionDataDetails,
    -- | The action of the resource called by a task state.
    resourceType :: Core.Text,
    -- | The service name of the resource in a task state.
    resource :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'TaskSucceededEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'taskSucceededEventDetails_output' - The full JSON response from a resource when a task has succeeded. This
-- response becomes the output of the related task. Length constraints
-- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'outputDetails', 'taskSucceededEventDetails_outputDetails' - Contains details about the output of an execution history event.
--
-- 'resourceType', 'taskSucceededEventDetails_resourceType' - The action of the resource called by a task state.
--
-- 'resource', 'taskSucceededEventDetails_resource' - The service name of the resource in a task state.
newTaskSucceededEventDetails ::
  -- | 'resourceType'
  Core.Text ->
  -- | 'resource'
  Core.Text ->
  TaskSucceededEventDetails
newTaskSucceededEventDetails
  pResourceType_
  pResource_ =
    TaskSucceededEventDetails'
      { output = Core.Nothing,
        outputDetails = Core.Nothing,
        resourceType = pResourceType_,
        resource = pResource_
      }

-- | The full JSON response from a resource when a task has succeeded. This
-- response becomes the output of the related task. Length constraints
-- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
taskSucceededEventDetails_output :: Lens.Lens' TaskSucceededEventDetails (Core.Maybe Core.Text)
taskSucceededEventDetails_output = Lens.lens (\TaskSucceededEventDetails' {output} -> output) (\s@TaskSucceededEventDetails' {} a -> s {output = a} :: TaskSucceededEventDetails) Core.. Lens.mapping Core._Sensitive

-- | Contains details about the output of an execution history event.
taskSucceededEventDetails_outputDetails :: Lens.Lens' TaskSucceededEventDetails (Core.Maybe HistoryEventExecutionDataDetails)
taskSucceededEventDetails_outputDetails = Lens.lens (\TaskSucceededEventDetails' {outputDetails} -> outputDetails) (\s@TaskSucceededEventDetails' {} a -> s {outputDetails = a} :: TaskSucceededEventDetails)

-- | The action of the resource called by a task state.
taskSucceededEventDetails_resourceType :: Lens.Lens' TaskSucceededEventDetails Core.Text
taskSucceededEventDetails_resourceType = Lens.lens (\TaskSucceededEventDetails' {resourceType} -> resourceType) (\s@TaskSucceededEventDetails' {} a -> s {resourceType = a} :: TaskSucceededEventDetails)

-- | The service name of the resource in a task state.
taskSucceededEventDetails_resource :: Lens.Lens' TaskSucceededEventDetails Core.Text
taskSucceededEventDetails_resource = Lens.lens (\TaskSucceededEventDetails' {resource} -> resource) (\s@TaskSucceededEventDetails' {} a -> s {resource = a} :: TaskSucceededEventDetails)

instance Core.FromJSON TaskSucceededEventDetails where
  parseJSON =
    Core.withObject
      "TaskSucceededEventDetails"
      ( \x ->
          TaskSucceededEventDetails'
            Core.<$> (x Core..:? "output")
            Core.<*> (x Core..:? "outputDetails")
            Core.<*> (x Core..: "resourceType")
            Core.<*> (x Core..: "resource")
      )

instance Core.Hashable TaskSucceededEventDetails

instance Core.NFData TaskSucceededEventDetails
