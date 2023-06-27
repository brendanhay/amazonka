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
-- Module      : Amazonka.StepFunctions.Types.TaskStartFailedEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TaskStartFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a task that failed to start during an execution.
--
-- /See:/ 'newTaskStartFailedEventDetails' smart constructor.
data TaskStartFailedEventDetails = TaskStartFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The service name of the resource in a task state.
    resourceType :: Prelude.Text,
    -- | The action of the resource called by a task state.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskStartFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'taskStartFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'taskStartFailedEventDetails_error' - The error code of the failure.
--
-- 'resourceType', 'taskStartFailedEventDetails_resourceType' - The service name of the resource in a task state.
--
-- 'resource', 'taskStartFailedEventDetails_resource' - The action of the resource called by a task state.
newTaskStartFailedEventDetails ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  TaskStartFailedEventDetails
newTaskStartFailedEventDetails
  pResourceType_
  pResource_ =
    TaskStartFailedEventDetails'
      { cause =
          Prelude.Nothing,
        error = Prelude.Nothing,
        resourceType = pResourceType_,
        resource = pResource_
      }

-- | A more detailed explanation of the cause of the failure.
taskStartFailedEventDetails_cause :: Lens.Lens' TaskStartFailedEventDetails (Prelude.Maybe Prelude.Text)
taskStartFailedEventDetails_cause = Lens.lens (\TaskStartFailedEventDetails' {cause} -> cause) (\s@TaskStartFailedEventDetails' {} a -> s {cause = a} :: TaskStartFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | The error code of the failure.
taskStartFailedEventDetails_error :: Lens.Lens' TaskStartFailedEventDetails (Prelude.Maybe Prelude.Text)
taskStartFailedEventDetails_error = Lens.lens (\TaskStartFailedEventDetails' {error} -> error) (\s@TaskStartFailedEventDetails' {} a -> s {error = a} :: TaskStartFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | The service name of the resource in a task state.
taskStartFailedEventDetails_resourceType :: Lens.Lens' TaskStartFailedEventDetails Prelude.Text
taskStartFailedEventDetails_resourceType = Lens.lens (\TaskStartFailedEventDetails' {resourceType} -> resourceType) (\s@TaskStartFailedEventDetails' {} a -> s {resourceType = a} :: TaskStartFailedEventDetails)

-- | The action of the resource called by a task state.
taskStartFailedEventDetails_resource :: Lens.Lens' TaskStartFailedEventDetails Prelude.Text
taskStartFailedEventDetails_resource = Lens.lens (\TaskStartFailedEventDetails' {resource} -> resource) (\s@TaskStartFailedEventDetails' {} a -> s {resource = a} :: TaskStartFailedEventDetails)

instance Data.FromJSON TaskStartFailedEventDetails where
  parseJSON =
    Data.withObject
      "TaskStartFailedEventDetails"
      ( \x ->
          TaskStartFailedEventDetails'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "error")
            Prelude.<*> (x Data..: "resourceType")
            Prelude.<*> (x Data..: "resource")
      )

instance Prelude.Hashable TaskStartFailedEventDetails where
  hashWithSalt _salt TaskStartFailedEventDetails' {..} =
    _salt
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData TaskStartFailedEventDetails where
  rnf TaskStartFailedEventDetails' {..} =
    Prelude.rnf cause
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resource
