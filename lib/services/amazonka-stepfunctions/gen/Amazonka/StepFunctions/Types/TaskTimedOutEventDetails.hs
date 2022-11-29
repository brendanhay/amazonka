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
-- Module      : Amazonka.StepFunctions.Types.TaskTimedOutEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TaskTimedOutEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a resource timeout that occurred during an
-- execution.
--
-- /See:/ 'newTaskTimedOutEventDetails' smart constructor.
data TaskTimedOutEventDetails = TaskTimedOutEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The service name of the resource in a task state.
    resourceType :: Prelude.Text,
    -- | The action of the resource called by a task state.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskTimedOutEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'taskTimedOutEventDetails_error' - The error code of the failure.
--
-- 'cause', 'taskTimedOutEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'resourceType', 'taskTimedOutEventDetails_resourceType' - The service name of the resource in a task state.
--
-- 'resource', 'taskTimedOutEventDetails_resource' - The action of the resource called by a task state.
newTaskTimedOutEventDetails ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  TaskTimedOutEventDetails
newTaskTimedOutEventDetails pResourceType_ pResource_ =
  TaskTimedOutEventDetails'
    { error = Prelude.Nothing,
      cause = Prelude.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | The error code of the failure.
taskTimedOutEventDetails_error :: Lens.Lens' TaskTimedOutEventDetails (Prelude.Maybe Prelude.Text)
taskTimedOutEventDetails_error = Lens.lens (\TaskTimedOutEventDetails' {error} -> error) (\s@TaskTimedOutEventDetails' {} a -> s {error = a} :: TaskTimedOutEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
taskTimedOutEventDetails_cause :: Lens.Lens' TaskTimedOutEventDetails (Prelude.Maybe Prelude.Text)
taskTimedOutEventDetails_cause = Lens.lens (\TaskTimedOutEventDetails' {cause} -> cause) (\s@TaskTimedOutEventDetails' {} a -> s {cause = a} :: TaskTimedOutEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | The service name of the resource in a task state.
taskTimedOutEventDetails_resourceType :: Lens.Lens' TaskTimedOutEventDetails Prelude.Text
taskTimedOutEventDetails_resourceType = Lens.lens (\TaskTimedOutEventDetails' {resourceType} -> resourceType) (\s@TaskTimedOutEventDetails' {} a -> s {resourceType = a} :: TaskTimedOutEventDetails)

-- | The action of the resource called by a task state.
taskTimedOutEventDetails_resource :: Lens.Lens' TaskTimedOutEventDetails Prelude.Text
taskTimedOutEventDetails_resource = Lens.lens (\TaskTimedOutEventDetails' {resource} -> resource) (\s@TaskTimedOutEventDetails' {} a -> s {resource = a} :: TaskTimedOutEventDetails)

instance Core.FromJSON TaskTimedOutEventDetails where
  parseJSON =
    Core.withObject
      "TaskTimedOutEventDetails"
      ( \x ->
          TaskTimedOutEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
            Prelude.<*> (x Core..: "resourceType")
            Prelude.<*> (x Core..: "resource")
      )

instance Prelude.Hashable TaskTimedOutEventDetails where
  hashWithSalt _salt TaskTimedOutEventDetails' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData TaskTimedOutEventDetails where
  rnf TaskTimedOutEventDetails' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf cause
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resource
