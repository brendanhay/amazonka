{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a task submitted to a resource .
--
-- /See:/ 'newTaskSubmittedEventDetails' smart constructor.
data TaskSubmittedEventDetails = TaskSubmittedEventDetails'
  { -- | The response from a resource when a task has started. Length constraints
    -- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The action of the resource called by a task state.
    resourceType :: Prelude.Text,
    -- | The service name of the resource in a task state.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TaskSubmittedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'taskSubmittedEventDetails_output' - The response from a resource when a task has started. Length constraints
-- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'outputDetails', 'taskSubmittedEventDetails_outputDetails' - Contains details about the output of an execution history event.
--
-- 'resourceType', 'taskSubmittedEventDetails_resourceType' - The action of the resource called by a task state.
--
-- 'resource', 'taskSubmittedEventDetails_resource' - The service name of the resource in a task state.
newTaskSubmittedEventDetails ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  TaskSubmittedEventDetails
newTaskSubmittedEventDetails
  pResourceType_
  pResource_ =
    TaskSubmittedEventDetails'
      { output =
          Prelude.Nothing,
        outputDetails = Prelude.Nothing,
        resourceType = pResourceType_,
        resource = pResource_
      }

-- | The response from a resource when a task has started. Length constraints
-- apply to the payload size, and are expressed as bytes in UTF-8 encoding.
taskSubmittedEventDetails_output :: Lens.Lens' TaskSubmittedEventDetails (Prelude.Maybe Prelude.Text)
taskSubmittedEventDetails_output = Lens.lens (\TaskSubmittedEventDetails' {output} -> output) (\s@TaskSubmittedEventDetails' {} a -> s {output = a} :: TaskSubmittedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | Contains details about the output of an execution history event.
taskSubmittedEventDetails_outputDetails :: Lens.Lens' TaskSubmittedEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
taskSubmittedEventDetails_outputDetails = Lens.lens (\TaskSubmittedEventDetails' {outputDetails} -> outputDetails) (\s@TaskSubmittedEventDetails' {} a -> s {outputDetails = a} :: TaskSubmittedEventDetails)

-- | The action of the resource called by a task state.
taskSubmittedEventDetails_resourceType :: Lens.Lens' TaskSubmittedEventDetails Prelude.Text
taskSubmittedEventDetails_resourceType = Lens.lens (\TaskSubmittedEventDetails' {resourceType} -> resourceType) (\s@TaskSubmittedEventDetails' {} a -> s {resourceType = a} :: TaskSubmittedEventDetails)

-- | The service name of the resource in a task state.
taskSubmittedEventDetails_resource :: Lens.Lens' TaskSubmittedEventDetails Prelude.Text
taskSubmittedEventDetails_resource = Lens.lens (\TaskSubmittedEventDetails' {resource} -> resource) (\s@TaskSubmittedEventDetails' {} a -> s {resource = a} :: TaskSubmittedEventDetails)

instance Prelude.FromJSON TaskSubmittedEventDetails where
  parseJSON =
    Prelude.withObject
      "TaskSubmittedEventDetails"
      ( \x ->
          TaskSubmittedEventDetails'
            Prelude.<$> (x Prelude..:? "output")
            Prelude.<*> (x Prelude..:? "outputDetails")
            Prelude.<*> (x Prelude..: "resourceType")
            Prelude.<*> (x Prelude..: "resource")
      )

instance Prelude.Hashable TaskSubmittedEventDetails

instance Prelude.NFData TaskSubmittedEventDetails
