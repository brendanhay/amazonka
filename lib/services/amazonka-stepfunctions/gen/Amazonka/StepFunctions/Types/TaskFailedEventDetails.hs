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
-- Module      : Amazonka.StepFunctions.Types.TaskFailedEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TaskFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a task failure event.
--
-- /See:/ 'newTaskFailedEventDetails' smart constructor.
data TaskFailedEventDetails = TaskFailedEventDetails'
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
-- Create a value of 'TaskFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'taskFailedEventDetails_error' - The error code of the failure.
--
-- 'cause', 'taskFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'resourceType', 'taskFailedEventDetails_resourceType' - The service name of the resource in a task state.
--
-- 'resource', 'taskFailedEventDetails_resource' - The action of the resource called by a task state.
newTaskFailedEventDetails ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  TaskFailedEventDetails
newTaskFailedEventDetails pResourceType_ pResource_ =
  TaskFailedEventDetails'
    { error = Prelude.Nothing,
      cause = Prelude.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | The error code of the failure.
taskFailedEventDetails_error :: Lens.Lens' TaskFailedEventDetails (Prelude.Maybe Prelude.Text)
taskFailedEventDetails_error = Lens.lens (\TaskFailedEventDetails' {error} -> error) (\s@TaskFailedEventDetails' {} a -> s {error = a} :: TaskFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
taskFailedEventDetails_cause :: Lens.Lens' TaskFailedEventDetails (Prelude.Maybe Prelude.Text)
taskFailedEventDetails_cause = Lens.lens (\TaskFailedEventDetails' {cause} -> cause) (\s@TaskFailedEventDetails' {} a -> s {cause = a} :: TaskFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | The service name of the resource in a task state.
taskFailedEventDetails_resourceType :: Lens.Lens' TaskFailedEventDetails Prelude.Text
taskFailedEventDetails_resourceType = Lens.lens (\TaskFailedEventDetails' {resourceType} -> resourceType) (\s@TaskFailedEventDetails' {} a -> s {resourceType = a} :: TaskFailedEventDetails)

-- | The action of the resource called by a task state.
taskFailedEventDetails_resource :: Lens.Lens' TaskFailedEventDetails Prelude.Text
taskFailedEventDetails_resource = Lens.lens (\TaskFailedEventDetails' {resource} -> resource) (\s@TaskFailedEventDetails' {} a -> s {resource = a} :: TaskFailedEventDetails)

instance Core.FromJSON TaskFailedEventDetails where
  parseJSON =
    Core.withObject
      "TaskFailedEventDetails"
      ( \x ->
          TaskFailedEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
            Prelude.<*> (x Core..: "resourceType")
            Prelude.<*> (x Core..: "resource")
      )

instance Prelude.Hashable TaskFailedEventDetails where
  hashWithSalt _salt TaskFailedEventDetails' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData TaskFailedEventDetails where
  rnf TaskFailedEventDetails' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf cause
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resource
