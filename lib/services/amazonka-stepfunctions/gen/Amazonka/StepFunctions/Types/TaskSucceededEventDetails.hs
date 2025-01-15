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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TaskSucceededEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the successful completion of a task state.
--
-- /See:/ 'newTaskSucceededEventDetails' smart constructor.
data TaskSucceededEventDetails = TaskSucceededEventDetails'
  { -- | The full JSON response from a resource when a task has succeeded. This
    -- response becomes the output of the related task. Length constraints
    -- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
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
-- 'output', 'taskSucceededEventDetails_output' - The full JSON response from a resource when a task has succeeded. This
-- response becomes the output of the related task. Length constraints
-- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'outputDetails', 'taskSucceededEventDetails_outputDetails' - Contains details about the output of an execution history event.
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
      { output =
          Prelude.Nothing,
        outputDetails = Prelude.Nothing,
        resourceType = pResourceType_,
        resource = pResource_
      }

-- | The full JSON response from a resource when a task has succeeded. This
-- response becomes the output of the related task. Length constraints
-- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
taskSucceededEventDetails_output :: Lens.Lens' TaskSucceededEventDetails (Prelude.Maybe Prelude.Text)
taskSucceededEventDetails_output = Lens.lens (\TaskSucceededEventDetails' {output} -> output) (\s@TaskSucceededEventDetails' {} a -> s {output = a} :: TaskSucceededEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | Contains details about the output of an execution history event.
taskSucceededEventDetails_outputDetails :: Lens.Lens' TaskSucceededEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
taskSucceededEventDetails_outputDetails = Lens.lens (\TaskSucceededEventDetails' {outputDetails} -> outputDetails) (\s@TaskSucceededEventDetails' {} a -> s {outputDetails = a} :: TaskSucceededEventDetails)

-- | The service name of the resource in a task state.
taskSucceededEventDetails_resourceType :: Lens.Lens' TaskSucceededEventDetails Prelude.Text
taskSucceededEventDetails_resourceType = Lens.lens (\TaskSucceededEventDetails' {resourceType} -> resourceType) (\s@TaskSucceededEventDetails' {} a -> s {resourceType = a} :: TaskSucceededEventDetails)

-- | The action of the resource called by a task state.
taskSucceededEventDetails_resource :: Lens.Lens' TaskSucceededEventDetails Prelude.Text
taskSucceededEventDetails_resource = Lens.lens (\TaskSucceededEventDetails' {resource} -> resource) (\s@TaskSucceededEventDetails' {} a -> s {resource = a} :: TaskSucceededEventDetails)

instance Data.FromJSON TaskSucceededEventDetails where
  parseJSON =
    Data.withObject
      "TaskSucceededEventDetails"
      ( \x ->
          TaskSucceededEventDetails'
            Prelude.<$> (x Data..:? "output")
            Prelude.<*> (x Data..:? "outputDetails")
            Prelude.<*> (x Data..: "resourceType")
            Prelude.<*> (x Data..: "resource")
      )

instance Prelude.Hashable TaskSucceededEventDetails where
  hashWithSalt _salt TaskSucceededEventDetails' {..} =
    _salt
      `Prelude.hashWithSalt` output
      `Prelude.hashWithSalt` outputDetails
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData TaskSucceededEventDetails where
  rnf TaskSucceededEventDetails' {..} =
    Prelude.rnf output `Prelude.seq`
      Prelude.rnf outputDetails `Prelude.seq`
        Prelude.rnf resourceType `Prelude.seq`
          Prelude.rnf resource
