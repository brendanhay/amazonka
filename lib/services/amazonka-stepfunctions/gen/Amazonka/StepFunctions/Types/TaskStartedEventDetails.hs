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
-- Module      : Amazonka.StepFunctions.Types.TaskStartedEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TaskStartedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the start of a task during an execution.
--
-- /See:/ 'newTaskStartedEventDetails' smart constructor.
data TaskStartedEventDetails = TaskStartedEventDetails'
  { -- | The service name of the resource in a task state.
    resourceType :: Prelude.Text,
    -- | The action of the resource called by a task state.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskStartedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'taskStartedEventDetails_resourceType' - The service name of the resource in a task state.
--
-- 'resource', 'taskStartedEventDetails_resource' - The action of the resource called by a task state.
newTaskStartedEventDetails ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  TaskStartedEventDetails
newTaskStartedEventDetails pResourceType_ pResource_ =
  TaskStartedEventDetails'
    { resourceType =
        pResourceType_,
      resource = pResource_
    }

-- | The service name of the resource in a task state.
taskStartedEventDetails_resourceType :: Lens.Lens' TaskStartedEventDetails Prelude.Text
taskStartedEventDetails_resourceType = Lens.lens (\TaskStartedEventDetails' {resourceType} -> resourceType) (\s@TaskStartedEventDetails' {} a -> s {resourceType = a} :: TaskStartedEventDetails)

-- | The action of the resource called by a task state.
taskStartedEventDetails_resource :: Lens.Lens' TaskStartedEventDetails Prelude.Text
taskStartedEventDetails_resource = Lens.lens (\TaskStartedEventDetails' {resource} -> resource) (\s@TaskStartedEventDetails' {} a -> s {resource = a} :: TaskStartedEventDetails)

instance Data.FromJSON TaskStartedEventDetails where
  parseJSON =
    Data.withObject
      "TaskStartedEventDetails"
      ( \x ->
          TaskStartedEventDetails'
            Prelude.<$> (x Data..: "resourceType")
            Prelude.<*> (x Data..: "resource")
      )

instance Prelude.Hashable TaskStartedEventDetails where
  hashWithSalt _salt TaskStartedEventDetails' {..} =
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData TaskStartedEventDetails where
  rnf TaskStartedEventDetails' {..} =
    Prelude.rnf resourceType `Prelude.seq`
      Prelude.rnf resource
