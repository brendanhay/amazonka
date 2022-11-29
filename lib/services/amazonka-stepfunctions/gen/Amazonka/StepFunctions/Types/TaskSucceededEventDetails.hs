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
-- Module      : Amazonka.StepFunctions.Types.TaskSucceededEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TaskSucceededEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the successful completion of a task state.
--
-- /See:/ 'newTaskSucceededEventDetails' smart constructor.
data TaskSucceededEventDetails = TaskSucceededEventDetails'
  { -- | Contains details about the output of an execution history event.
    outputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The full JSON response from a resource when a task has succeeded. This
    -- response becomes the output of the related task. Length constraints
    -- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The service name of the resource in a task state.
    resourceType :: Prelude.Text,
    -- | The action of the resource called by a task state.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskSucceededEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputDetails', 'taskSucceededEventDetails_outputDetails' - Contains details about the output of an execution history event.
--
-- 'output', 'taskSucceededEventDetails_output' - The full JSON response from a resource when a task has succeeded. This
-- response becomes the output of the related task. Length constraints
-- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'resourceType', 'taskSucceededEventDetails_resourceType' - The service name of the resource in a task state.
--
-- 'resource', 'taskSucceededEventDetails_resource' - The action of the resource called by a task state.
newTaskSucceededEventDetails ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  TaskSucceededEventDetails
newTaskSucceededEventDetails
  pResourceType_
  pResource_ =
    TaskSucceededEventDetails'
      { outputDetails =
          Prelude.Nothing,
        output = Prelude.Nothing,
        resourceType = pResourceType_,
        resource = pResource_
      }

-- | Contains details about the output of an execution history event.
taskSucceededEventDetails_outputDetails :: Lens.Lens' TaskSucceededEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
taskSucceededEventDetails_outputDetails = Lens.lens (\TaskSucceededEventDetails' {outputDetails} -> outputDetails) (\s@TaskSucceededEventDetails' {} a -> s {outputDetails = a} :: TaskSucceededEventDetails)

-- | The full JSON response from a resource when a task has succeeded. This
-- response becomes the output of the related task. Length constraints
-- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
taskSucceededEventDetails_output :: Lens.Lens' TaskSucceededEventDetails (Prelude.Maybe Prelude.Text)
taskSucceededEventDetails_output = Lens.lens (\TaskSucceededEventDetails' {output} -> output) (\s@TaskSucceededEventDetails' {} a -> s {output = a} :: TaskSucceededEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | The service name of the resource in a task state.
taskSucceededEventDetails_resourceType :: Lens.Lens' TaskSucceededEventDetails Prelude.Text
taskSucceededEventDetails_resourceType = Lens.lens (\TaskSucceededEventDetails' {resourceType} -> resourceType) (\s@TaskSucceededEventDetails' {} a -> s {resourceType = a} :: TaskSucceededEventDetails)

-- | The action of the resource called by a task state.
taskSucceededEventDetails_resource :: Lens.Lens' TaskSucceededEventDetails Prelude.Text
taskSucceededEventDetails_resource = Lens.lens (\TaskSucceededEventDetails' {resource} -> resource) (\s@TaskSucceededEventDetails' {} a -> s {resource = a} :: TaskSucceededEventDetails)

instance Core.FromJSON TaskSucceededEventDetails where
  parseJSON =
    Core.withObject
      "TaskSucceededEventDetails"
      ( \x ->
          TaskSucceededEventDetails'
            Prelude.<$> (x Core..:? "outputDetails")
            Prelude.<*> (x Core..:? "output")
            Prelude.<*> (x Core..: "resourceType")
            Prelude.<*> (x Core..: "resource")
      )

instance Prelude.Hashable TaskSucceededEventDetails where
  hashWithSalt _salt TaskSucceededEventDetails' {..} =
    _salt `Prelude.hashWithSalt` outputDetails
      `Prelude.hashWithSalt` output
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData TaskSucceededEventDetails where
  rnf TaskSucceededEventDetails' {..} =
    Prelude.rnf outputDetails
      `Prelude.seq` Prelude.rnf output
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resource
